// https://blog.softwaremill.com/managing-dependencies-using-zio-8acc1539e276

package AdamWarskiExample

import zio.{Task, ZIO, ZLayer}

object DB {
  // 1. Service
  trait Service {
    def execute(sql: String): Task[Unit]
  }

  // rest omitted here
}

object UserModel {
  type User = String

  // 1. Service
  trait Service {
    def insert(u: User): Task[Unit]
  }

  // 2. Service impl
  val live: ZLayer[DB, Nothing, UserModel] = ZLayer.fromService { db =>
    (u: User) => db.execute("INSERT INTO user VALUES ('$u')")
  }

  // 3. accessor method (optional)
  def insert(u: User): ZIO[UserModel, Throwable, Unit] = ZIO.accessM(um => um.get.insert(u))
}
