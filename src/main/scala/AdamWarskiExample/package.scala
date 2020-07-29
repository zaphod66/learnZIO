import zio.Has

package object AdamWarskiExample {
  type DB = Has[DB.Service]
  type UserModel = Has[UserModel.Service]
}
