package examples

trait CacheReplacementStrategy[K, V] {
  def onCacheFull(cacheBackend: CacheBackend[K, V]): CacheBackend[K, V]
}
