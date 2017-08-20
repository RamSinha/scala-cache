package examples

import scala.util.{Failure, Try}

trait CacheBackend[K, V] {
  val size: Int

  def get(k: K): Try[V]
  def put(k: K, v: V): Try[Unit]
  def delete(k: K): Try[Unit]
}

final class CleanUpCache[K, V]
    extends Function4[Int,
                      InvalidationStrategy[K, V],
                      CacheReplacementStrategy[K, V],
                      CacheBackend[K, V],
                      CacheBackend[K, V]] {

  override def apply(v1: Int,
                     v2: InvalidationStrategy[K, V],
                     v3: CacheReplacementStrategy[K, V],
                     v4: CacheBackend[K, V]): CacheBackend[K, V] = {
    val cleaned = v2.invalidate(v4)

    if (cleaned.size >= v1) {
      v3.onCacheFull(cleaned)
    } else
      cleaned
  }
}
