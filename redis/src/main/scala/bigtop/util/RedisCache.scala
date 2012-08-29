package bigtop.util

import com.redis._
import com.redis.serialization.{
  Format => RedisFormat,
  Parse  => RedisParse
}

trait RedisCache[Key, Value] {
  def pool: RedisClientPool

  def encodeKey(key: Key): String

  def ttl: Option[Int]

  implicit def valueFormat: RedisFormat
  implicit def valueParse: RedisParse[Value]

  def set(key: Key, value: Value, ttl: Option[Int] = this.ttl): Unit = {
    pool.withClient { redis =>
      ttl match {
        case Some(ttl) => redis.setex(encodeKey(key), ttl, value)
        case None      => redis.set(encodeKey(key), value)
      }
    }
  }

  def get(key: Key, ttl: Option[Int] = this.ttl): Option[Value] = {
    pool.withClient { redis =>
      val k = encodeKey(key)
      val v = redis.get[Value](k)
      ttl.foreach(redis.expire(k, _))
      v
    }
  }

  def delete(key: Key): Unit = {
    pool.withClient { redis =>
      redis.del(encodeKey(key))
    }
  }
}

trait SimpleRedisCache[Value] extends RedisCache[String, Value] {
  def prefix: String

  final def encodeKey(key: String): String = {
    prefix + key
  }
}
