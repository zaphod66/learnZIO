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

  import scala.collection.mutable
  val alMap = mutable.Map.empty[String, String]

  def mod(m: Map[String, String], k: String, obf: String => String): Map[String, String] = {
    m.get(k).fold(m + (k -> obf(k)))(_ => m)
  }

  def mod2(m: Map[String, String], k: String, obf: String => String): (String, Map[String, String]) = {
    m.get(k).fold {
      val v = obf(k)
      (v, m + (k -> v))
    }(v => (v, m))
  }

  def obfuscateNVR(s: String, f: String => String, urm: UIO[Ref[Map[String, String]]]) = {
    val fff = for {
      rm <- urm
      nv <- rm.modify(m => mod2(m, s, f))
    } yield (nv, urm)
    fff
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

  def obfuscateUE(in: UnconstraintExact): UnconstraintExact = {
    val nvNew = in.mt match {
      case "SameCustomerNumberMatcher$"  => obfuscateCustNo(in.nv)
      case "SamePhoneNumberMatcher$"     => obfuscatePhone(in.nv)
      case "SameShipmentAddressMatcher$" => obfuscateAddr(in.nv)
      case "SameBillingAddressMatcher$"  => obfuscateAddr(in.nv)
      case "SameEmailMatcher$"           => obfuscateEmail(in.nv)
      case "SameDIExactIdMatcher$"       => obfuscateExact(in.nv)
      case "SameDISmartIdMatcher$"       => obfuscateSmart(in.nv)
    }

    in.copy(nv = nvNew)
  }

  trait Persistence {
    def update(f: Map[String, String] => Map[String, String]): ZIO[Any, Nothing, Unit]
    def get: ZIO[Any, Nothing, Map[String, String]]
  }

  case class MapPersistence(rm: Ref[Map[String, String]]) extends Persistence {
    override def update(f: Map[String, String] => Map[String, String]): ZIO[Any, Nothing, Unit] = rm.update(f)
    override def get: ZIO[Any, Nothing, Map[String, String]] = rm.get
  }

  val mapZIO: ZIO[Any, Nothing, MapPersistence] = ZRef.make(Map.empty[String, String]).map(MapPersistence)
  val mapLayer: ZLayer[Any, Nothing, Has[Persistence]] = mapZIO.toLayer

  trait Obfuscator {
    def tObfuscateUE(ue: UnconstraintExact): ZIO[Any, Nothing, UnconstraintExact]
  }

  case class PersObfuscator(pers: Persistence) extends Obfuscator {
    override def tObfuscateUE(ue: UnconstraintExact): ZIO[Any, Nothing, UnconstraintExact] = UIO(ue.copy(nv = "v11"))
  }

  val perZIO: ZIO[Any, Nothing, Obfuscator] = mapZIO.map(PersObfuscator)
  val perLayer:ZLayer[Any, Nothing, Has[Obfuscator]] = perZIO.toLayer

  val ralMap = ZRef.make(Map.empty[String, String])
  val zalMap = ralMap.toLayer

  val zalLay = for {
//    pers <- ZIO.service[Persistence]
    obfu <- ZIO.service[Obfuscator]
//    _  <- pers.update(_ + ("k11" -> "v11"))
//    _  <- pers.update(_ + ("k12" -> "v12"))
    u1 <- obfu.tObfuscateUE(UnconstraintExact("mt", "k11", "tid1", Instant.now()))
    u2 <- obfu.tObfuscateUE(UnconstraintExact("mt", "k11", "tid2", Instant.now()))
//    s  <- pers.get
//    v  = s("k12")
  } yield (u1, u2)

  val zalPro = for {
    per <- zalLay
//    str <- per._2.get.map(_.toString)
//    _   <- putStrLn(per._1 + ", " + str)
    _ <- putStrLn(s"${per._1} = ${per._2}")
  } yield ()

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
  //  val srcFilePath = "/Users/nschelle/work/gitrepos/zaphod66/learnZIO/unconstraintExact_0000010.csv"
    val srcFilePath = "/Users/nscheller/work/gitrepos/projects/zaphod66/learnZIO/unconstraintExact_0000010.csv"
    val dstFilePath = srcFilePath + "_obf.csv"

    val managedFile = ZManaged.make(ZIO(new BufferedWriter(new FileWriter(dstFilePath, false))))(f => ZIO(f.close()).orDie)

    val fileStream  = ZStream.fromFile(Paths.get(srcFilePath))
      .transduce(ZTransducer.utf8Decode >>> ZTransducer.splitLines)

    val stream = ZStream.managed(managedFile)
      .flatMap{ bw =>
        fileStream
          .map(UnconstraintExact.make)
          .map(obfuscateUE)
          .tap(ue => ZIO(bw.write(s"${ue.toString}\n")))
      }.runCount

    val obfPr1 = for {
      per <- ZIO.service[Obfuscator]
      no  <- ZStream.managed(managedFile).flatMap { bw =>
        fileStream
          .map(UnconstraintExact.make)
          .mapM(per.tObfuscateUE)
          .tap(ue => ZIO(bw.write(s"${ue.toString}\n")))
      }.runCount
    } yield no

    val obfPr2 = for {
      per <- ZIO.service[Obfuscator]
      no  <- (for {
                bw <- ZStream.managed(managedFile)
              } yield fileStream
                      .map(UnconstraintExact.make)
                      .mapM(per.tObfuscateUE)
                      .tap(ue => ZIO(bw.write(s"${ue.toString}\n")))
             ).runCount
    } yield no

    import zio.console.Console
    import zio.blocking.Blocking
    val obfEnv = Console.live ++ perLayer ++ Blocking.live
    val obfCo1 = obfPr1.provideLayer(obfEnv)
    val obfCo2 = obfPr2.provideLayer(obfEnv)

//    val zalEnv = Console.live ++ perLayer // ++ mapLayer
//    val zalCom = zalPro.provideLayer(zalEnv)
//    val program = stream.tap(c => putStrLn(s"processed $c lines.")) *>
//      putStrLn("alMap: " + alMap.size.toString) *> zalCom *> obfHoo *> obfHo2
    val program = obfCo2
    program.tap(l => putStrLn(s"Ref processed $l lines.")).exitCode
  }
}
