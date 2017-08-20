package examples

trait InvalidationStrategy[K, V] {
  def invalidate(cacheBackend: CacheBackend[K, V]): CacheBackend[K, V]
}

class AlwaysValid[K, V] extends InvalidationStrategy[K, V] {
  override def invalidate(cacheBackend: CacheBackend[K, V]) = cacheBackend
}
