package streams

import zio._
import zio.console.putStrLn
import zio.stream._

import java.io.{BufferedWriter, FileWriter}
import java.nio.file.Paths
import java.time.Instant

object ZStreamObfuscate extends App {
  case class UnconstraintExact(mt: String, nv: String, tid: String, instant: Instant) {
    override def toString: String = {
      s"$mt,$nv,$tid,${instant.toString}"
    }
  }

  object UnconstraintExact {
    def make(s: String): UnconstraintExact = {
      val sl = s.split(',')

      UnconstraintExact(sl(0), sl(1), sl(2), Instant.parse(sl(3)))
    }
  }

  import scala.util.Random

  def numericString(length: Int): String = {
    @annotation.tailrec
    def go(acc: List[Int], l: Int): String = {
      if (l == 0) acc.mkString
      else go(Random.nextInt(10) :: acc, l - 1)
    }

    go(List.empty[Int], length)
  }

  def alphanumericString(length: Int): String = Random.alphanumeric.take(length).mkString

  trait Persistence {
    def modify(f: Map[String, String] => (String, Map[String, String])): ZIO[Any, Nothing, String]
  }

  case class MapPersistence(rm: Ref[Map[String, String]]) extends Persistence {
    override def modify(f: Map[String, String] => (String, Map[String, String])): ZIO[Any, Nothing, String] = rm.modify(f)
  }

  trait Obfuscator {
    def obfuscateUE(ue: UnconstraintExact): ZIO[Any, Nothing, UnconstraintExact]
  }

  case class PersObfuscator(pers: Persistence) extends Obfuscator {
    /**
     * @param s value we want to have a unique obfuscated value for
     * @param e extractor, which part of of 's' is the key in the map
     * @param f obfuscator function
     * @param m the map, which hold already obfuscated values, with a key the extractor provides
     * @return a tuple with the currently obfuscated value and the map containing all values
     */
    private def modifyMap(s: String, e: String => String, f: String => String, m: Map[String, String]): (String, Map[String, String]) = {
      val ext = e(s)
      m.get(ext).fold {
        val v = f(s)
        (v, m + (ext -> v))
      } (v => (v, m))
    }

    private def createNumericString(s: String): String = numericString(s.length)
    private def createAddr(a: String): String = {
      val sa = a.split(';')
      val na = alphanumericString(sa(0).length)
      s"$na;${sa(1)};${sa(2)}"
    }
    private def createEmail(e: String): String = {
      val se = e.split('@')
      val na = alphanumericString(se(0).length)
      s"$na@${se(1)}"
    }
    private def createAlphanumericString(s: String): String = alphanumericString(s.length)

    private def modify(ue: UnconstraintExact)(m: Map[String, String]): (String, Map[String, String]) = {
      ue.mt match {
        case "SameCustomerNumberMatcher$"  => modifyMap(ue.nv, identity, createNumericString, m)
        case "SamePhoneNumberMatcher$"     => modifyMap(ue.nv, identity, createNumericString, m)
        case "SameShipmentAddressMatcher$" => modifyMap(ue.nv, s => s.split(';')(0), createAddr, m)
        case "SameBillingAddressMatcher$"  => modifyMap(ue.nv, s => s.split(';')(0), createAddr, m)
        case "SameEmailMatcher$"           => modifyMap(ue.nv, s => s.split('@')(0), createEmail, m)
        case "SameDIExactIdMatcher$"       => modifyMap(ue.nv, identity, createAlphanumericString, m)
        case "SameDISmartIdMatcher$"       => modifyMap(ue.nv, identity, createAlphanumericString, m)
      }
    }

    override def obfuscateUE(ue: UnconstraintExact): ZIO[Any, Nothing, UnconstraintExact] = {
      pers.modify(modify(ue)).map(s => ue.copy(nv = s))
    }
  }

  val persistenceZIO: ZIO[Any, Nothing, Persistence] = ZRef.make(Map.empty[String, String]).map(MapPersistence)
  val obfuscatorZIO: ZIO[Any, Nothing, Obfuscator]  = persistenceZIO.map(PersObfuscator)
  val obfuscatorLayer:ZLayer[Any, Nothing, Has[Obfuscator]] = obfuscatorZIO.toLayer

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
  //  val srcFilePath = "/Users/nschelle/work/gitrepos/zaphod66/learnZIO/unconstraintExact_0000010.csv"
    val srcFilePath = "/Users/nscheller/work/gitrepos/projects/zaphod66/learnZIO/unconstraintExact_0000010.csv"
    val dstFilePath = srcFilePath + "_obf.csv"

    val managedFile = ZManaged.make(ZIO(new BufferedWriter(new FileWriter(dstFilePath, false))))(f => ZIO(f.close()).orDie)

    val fileStream  = ZStream.fromFile(Paths.get(srcFilePath))
      .transduce(ZTransducer.utf8Decode >>> ZTransducer.splitLines)

    val transformZIO = managedFile.use { bw =>
      for {
        ob <- ZIO.service[Obfuscator]
        no <- fileStream
          .map(UnconstraintExact.make)
          .mapM(ob.obfuscateUE)
          .tap(ue => ZIO(bw.write(s"${ue.toString}\n")))
          .runCount
      } yield no
    }

    import zio.console.Console
    import zio.blocking.Blocking
    val obfEnv  = Console.live ++ Blocking.live ++ obfuscatorLayer
    val program = transformZIO.provideLayer(obfEnv)

    program.tap(l => putStrLn(s"Ref processed $l lines.")).exitCode
  }
}
