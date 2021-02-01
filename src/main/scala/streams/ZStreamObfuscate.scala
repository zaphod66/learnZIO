package streams

import zio._
import zio.blocking.Blocking
import zio.console.putStrLn
import zio.stream._

import java.io.{BufferedWriter, FileWriter, Writer}
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

  import scala.collection.mutable
  val alMap = mutable.Map.empty[String, String]

  def mod(m: Map[String, String], k: String, obf: String => String): Map[String, String] = {
    m.get(k).fold(m + (k -> obf(k)))(_ => m)
  }

  def obfuscateNV(s: String, f: String => String, m: mutable.Map[String, String]): String = {
    m.get(s).fold {
      val o = f(s)
      m.put(s, o)

      o
    }(identity)
  }

  def obfuscateEmail(s: String): String = {
    def f(e: String): String = {
      val se = e.split('@')
      val na = alphanumericString(se(0).length)
      s"$na@${se(1)}"
    }

    obfuscateNV(s, f, alMap)
  }
  def obfuscatePhone(s: String): String  = obfuscateNV(s, s => numericString(s.length), alMap)
  def obfuscateCustNo(s: String): String = obfuscateNV(s, s => numericString(s.length), alMap)
  def obfuscateExact(s: String): String  = obfuscateNV(s, s => alphanumericString(s.length), alMap)
  def obfuscateSmart(s: String): String  = obfuscateNV(s, s => alphanumericString(s.length), alMap)
  def obfuscateAddr(a: String): String = {
    val sa = a.split(';')
    val na = obfuscateNV(sa(0), s => alphanumericString(s.length), alMap)
    s"$na;${sa(1)};${sa(2)}"
  }

  trait Persistence {
    def update(f: Map[String, String] => Map[String, String]): ZIO[Any, Nothing, Unit]
    def modify(f: Map[String, String] => (String, Map[String, String])): ZIO[Any, Nothing, String]
    def get: ZIO[Any, Nothing, Map[String, String]]
  }

  case class MapPersistence(rm: Ref[Map[String, String]]) extends Persistence {
    override def update(f: Map[String, String] => Map[String, String]): ZIO[Any, Nothing, Unit] = rm.update(f)
    override def modify(f: Map[String, String] => (String, Map[String, String])): ZIO[Any, Nothing, String] = rm.modify(f)
    override def get: ZIO[Any, Nothing, Map[String, String]] = rm.get
  }

  trait Obfuscator {
    def tObfuscateUE(ue: UnconstraintExact): ZIO[Any, Nothing, UnconstraintExact]
  }

  case class PersObfuscator(pers: Persistence) extends Obfuscator {
    def modifyMap(s: String, e: String => String, f: String => String, m: Map[String, String]): (String, Map[String, String]) = {
      val ext = e(s)
      m.get(ext).fold {
        val v = f(s)
        (v, m + (ext -> v))
      } (v => (v, m))
    }

    def createNumericString(s: String): String = numericString(s.length)
    def createAddr(a: String): String = {
      val sa = a.split(';')
      val na = alphanumericString(sa(0).length)
      s"$na;${sa(1)};${sa(2)}"
    }
    def createEmail(e: String): String = {
      val se = e.split('@')
      val na = alphanumericString(se(0).length)
      s"$na@${se(1)}"
    }
    def createAlphanumericString(s: String): String = alphanumericString(s.length)

    def modifyRef(ue: UnconstraintExact)(m: Map[String, String]): (String, Map[String, String]) = {
      ue.mt match {
        case "SameCustomerNumberMatcher$"  => modifyMap(ue.nv, identity, createNumericString, m)
        case "SamePhoneNumberMatcher$"     => modifyMap(ue.nv, identity, createNumericString, m)
        case "SameShipmentAddressMatcher$" => modifyMap(ue.nv, s => s.split(';')(0), createAddr  , m)
        case "SameBillingAddressMatcher$"  => modifyMap(ue.nv, s => s.split(';')(0), createAddr  , m)
        case "SameEmailMatcher$"           => modifyMap(ue.nv, s => s.split('@')(0), createEmail , m)
        case "SameDIExactIdMatcher$"       => modifyMap(ue.nv, identity, createAlphanumericString, m)
        case "SameDISmartIdMatcher$"       => modifyMap(ue.nv, identity, createAlphanumericString, m)
      }
    }

    override def tObfuscateUE(ue: UnconstraintExact): ZIO[Any, Nothing, UnconstraintExact] = {
      pers.modify(modifyRef(ue)).map(s => ue.copy(nv = s))
    }

    def mutableObfuscateUE(ue: UnconstraintExact): ZIO[Any, Nothing, UnconstraintExact] = {
      val obfNv = ue.mt match {
        case "SameCustomerNumberMatcher$"  => obfuscateCustNo(ue.nv)
        case "SamePhoneNumberMatcher$"     => obfuscatePhone(ue.nv)
        case "SameShipmentAddressMatcher$" => obfuscateAddr(ue.nv)
        case "SameBillingAddressMatcher$"  => obfuscateAddr(ue.nv)
        case "SameEmailMatcher$"           => obfuscateEmail(ue.nv)
        case "SameDIExactIdMatcher$"       => obfuscateExact(ue.nv)
        case "SameDISmartIdMatcher$"       => obfuscateSmart(ue.nv)
      }
      UIO(ue.copy(nv = obfNv))
    }
  }

  val mapZIO: ZIO[Any, Nothing, MapPersistence] = ZRef.make(Map.empty[String, String]).map(MapPersistence)
  val perZIO: ZIO[Any, Nothing, Obfuscator] = mapZIO.map(PersObfuscator)
  val perLayer:ZLayer[Any, Nothing, Has[Obfuscator]] = perZIO.toLayer

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
  //  val srcFilePath = "/Users/nschelle/work/gitrepos/zaphod66/learnZIO/unconstraintExact_0000010.csv"
    val srcFilePath = "/Users/nscheller/work/gitrepos/projects/zaphod66/learnZIO/unconstraintExact_0000010.csv"
    val dstFilePath = srcFilePath + "_obf.csv"

    val managedFile = ZManaged.make(ZIO(new BufferedWriter(new FileWriter(dstFilePath, false))))(f => ZIO(f.close()).orDie)

    val fileStream  = ZStream.fromFile(Paths.get(srcFilePath))
      .transduce(ZTransducer.utf8Decode >>> ZTransducer.splitLines)

    val obfPro = managedFile.use { bw =>
      for {
        per <- ZIO.service[Obfuscator]
        no  <- fileStream
          .map(UnconstraintExact.make)
          .mapM(per.tObfuscateUE)
          .tap(ue => ZIO(bw.write(s"${ue.toString}\n")))
          .runCount
      } yield no
    }

    import zio.console.Console
    import zio.blocking.Blocking
    val obfEnv  = Console.live ++ Blocking.live ++ perLayer
    val program = obfPro.provideLayer(obfEnv)

    program.tap(l => putStrLn(s"Ref processed $l lines.")).exitCode
  }
}
