package happyEyeballs

import zio.Exit.{Failure, Success}
import zio._
import zio.clock.Clock
import zio.duration._

object HappyEyeballs {
  def apply[R, T](tasks: List[ZIO[R, Throwable, T]], delay: Duration): ZIO[R with Clock, Throwable, T] = {
    tasks match {
      case Nil => IO.fail(new IllegalArgumentException("no tasks"))
      case task :: Nil => task
      case task :: otherTasks =>
        Queue.bounded[Unit](1).flatMap { failedTask =>
          val taskWithSignalOnFail = task.onError(_ => failedTask.offer(()))
          val sleepOrFailed = ZIO.sleep(delay).race(failedTask.take)

          taskWithSignalOnFail.race(sleepOrFailed *> apply(otherTasks, delay))
        }
    }
  }
}

object ReleasableHappyEyeballs {
  def apply[R, T](tasks: List[ZIO[R, Throwable, T]],
                  delay: Duration,
                  release: T => ZIO[R, Nothing, Unit]
                 ): ZIO[R with Clock, Throwable, T] = {
    for {
      successful <- Queue.bounded[T](tasks.size)
      enqueingTasks = tasks.map { task =>
        task.onExit {
          case Success(value) => successful.offer(value)
          case Failure(_) => ZIO.unit
        }
      }
      _ <- HappyEyeballs(enqueingTasks, delay)
      h :: t <- successful.takeAll
      _ <- ZIO.foreach(t)(release)
    } yield h
  }
}

object TestPrintln extends App {
  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, ExitCode] = {
    val start = System.currentTimeMillis()
    def log(msg: String): UIO[Unit] = UIO {
      val now = System.currentTimeMillis()
      val sec = (now - start) / 1000L

      println(s"after: $sec s: $msg")
    }

    def printSleepPrint(sleep: Duration, name: String): ZIO[Clock, Nothing, String] =
      log(s"START: $name") *> URIO.sleep(sleep) *> log(s"DONE: $name") *> UIO(name)

    def printSleepFail(sleep: Duration, name: String): ZIO[Clock, Throwable, String] =
      log(s"START: $name") *> URIO.sleep(sleep) *> log(s"FAIL: $name") *> IO.fail(new RuntimeException(s"FAIL: name"))

    val res = HappyEyeballs(
      List(
        printSleepPrint(10.seconds, "task1"),
        printSleepFail(1.seconds, "task2"),
        printSleepPrint(3.seconds, "task3"),
        printSleepPrint(2.seconds, "task4"),
        printSleepPrint(2.seconds, "task5")
      ),
      2.seconds
    )

    res.fold(_ => ExitCode.failure, s => { println(s"WON: $s"); ExitCode.success } )
      .untraced
  }
}

object TestSocket extends App{
  import zio.blocking._
  import zio.console._

  import java.net.{InetAddress, Socket}

  def closeSocket(s: Socket): ZIO[Blocking with Console, Nothing, Unit] =
    putStrLn(s"closing: ${s.getInetAddress}") *> effectBlocking(s.close()).catchAll(_ => ZIO.unit)

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, ExitCode] = {
    effectBlocking(InetAddress.getAllByName("debian.org").toList)
      .map { addresses =>
        addresses.map { address =>
          def eff = {
            println(s"trying : $address of ${addresses.size}")
            Thread.sleep(300)
            println(s"over   : $address of ${addresses.size}")
            val socket = new Socket(address, 443)
            println(s"success: $address of ${addresses.size}")
            socket
          }
          effectBlocking(eff)
        }
      }
      .flatMap(tasks => ReleasableHappyEyeballs(tasks, 250.milliseconds, closeSocket))
      .tap(v => putStrLn(s"Connected: ${v.getInetAddress}"))
      .fold(_ => ExitCode.failure, _ => ExitCode.success)
  }
}
