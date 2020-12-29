package streams

import zio._
import zio.console.{getStrLn, putStrLn}
import zio.stream._

import java.io.{BufferedWriter, FileWriter}
import java.nio.file.Paths
import java.time.Instant

object ZStreamObfuscate extends App {
  val srcFilePath = "/Users/nschelle/work/gitrepos/zaphod66/learnZIO/unconstraintExact_0001000.csv"
  val dstFilePath = srcFilePath + "_obf.csv"

  val fileStream  = ZStream.fromFile(Paths.get(srcFilePath))
    .transduce(ZTransducer.utf8Decode >>> ZTransducer.splitLines)

  case class UnconstraintExact(mt: String, nv: String, tid: String, instant: Instant) {
    override def toString: String = {
      s"$mt,$nv,$tid,${instant.toString}"
    }
  }

  def toUnconstraintExact(s: String): UnconstraintExact = {
    val sl = s.split(',')

    UnconstraintExact(sl(0), sl(1), sl(2), Instant.parse(sl(3)))
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

  def alphanumericString(length: Int): String = Random.alphanumeric.take(length).mkString("")

  import scala.collection.mutable
  val emMap = mutable.Map.empty[String, String]
  val pnMap = mutable.Map.empty[String, String]
  val cnMap = mutable.Map.empty[String, String]
  val eiMap = mutable.Map.empty[String, String]
  val siMap = mutable.Map.empty[String, String]
  val adMap = mutable.Map.empty[String, String]

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

    obfuscateNV(s, f, emMap)
  }
  def obfuscatePhone(s: String): String  = obfuscateNV(s, s => numericString(s.length), pnMap)
  def obfuscateCustNo(s: String): String = obfuscateNV(s, s => numericString(s.length), cnMap)
  def obfuscateExact(s: String): String  = obfuscateNV(s, s => alphanumericString(s.length), eiMap)
  def obfuscateSmart(s: String): String  = obfuscateNV(s, s => alphanumericString(s.length), siMap)

  def obfuscateAddr(a: String): String = {
    val sa = a.split(';')
    val na = obfuscateNV(sa(0), s => alphanumericString(s.length), adMap)
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

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    val logStream =
      ZStream(emMap).tap(m => putStrLn("emMap: " + m.size.toString)) ++
      ZStream(pnMap).tap(m => putStrLn("pnMap: " + m.size.toString)) ++
      ZStream(cnMap).tap(m => putStrLn("cnMap: " + m.size.toString)) ++
      ZStream(eiMap).tap(m => putStrLn("eiMap: " + m.size.toString)) ++
      ZStream(siMap).tap(m => putStrLn("siMap: " + m.size.toString)) ++
      ZStream(adMap).tap(m => putStrLn("adMap: " + m.size.toString))

    val managedFile = ZManaged.make(ZIO(new BufferedWriter(new FileWriter(dstFilePath, false))))(f => ZIO(f.close()).orDie)
    val program = managedFile.use { fw =>
      (fileStream
        .map(toUnconstraintExact)
        .map(obfuscateUE)
        .tap(ue => ZIO(fw.write(s"${ue.toString}\n"))) ++
        logStream).runDrain
    }

    program.exitCode
  }
}
