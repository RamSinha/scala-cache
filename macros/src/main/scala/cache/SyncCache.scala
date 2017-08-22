package cache

trait SyncCache[K, V] {
  def get(k: K): Option[V]
  def put(k: K, v: V): Unit
}
