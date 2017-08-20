package examples

import scala.util.Try

object MyApp extends App {

  val alwaysValid = new AlwaysValid[String, String]
  val neverReplace = new CacheReplacementStrategy[String, String] {
    override def onCacheFull(cacheBackend: CacheBackend[String, String]) =
      cacheBackend
  }

  val simpleMapCache = new CacheBackend[String, String] {
    var map = Map.empty[String, String]
    override def get(k: String) = Try(map(k))

    override def put(k: String, v: String) = Try {
      println(s"Put $k -> $v")
      map = map.updated(k, v)
    }

    override def delete(k: String) = Try {
      map = map - k
    }

    override val size = 100
  }

  @cache[String, String](100, alwaysValid, neverReplace, simpleMapCache)
  def test(x: String): String = x

  test("200")
  test("200")
  println("Hello Scala.meta macros!")
}
