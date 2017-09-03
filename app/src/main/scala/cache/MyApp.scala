package cache

object MyApp extends App {
  class InMemCache[A, B] extends SyncCache[A, B] {
    var map = Map.empty[A, B]
    override def get(k: A) = {
      println("Called GET")
      map.get(k)
    }

    override def put(k: A, v: B) = {
      println("Called SET")
      map = map.updated(k, v)
    }
  }
  val simpleMapCache = new InMemCache[String, String]

  val simpleMapCache2 = new InMemCache[(Int, Int), Int]


  @cache[String, String](simpleMapCache)
  def test(x: String): String = x

  @cache[(Int, Int), Int](simpleMapCache2)
  def testMultipleArg(x: Int, y: Int): Int = x + y

  test("200")
  test("200")

  testMultipleArg(2, 3)
  testMultipleArg(2, 3)
  testMultipleArg(2, 3)
}
