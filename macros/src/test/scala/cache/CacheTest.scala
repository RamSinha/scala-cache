package cache

import org.scalatest._
import scala.meta._
import scala.meta.testkit._

class CacheTest extends WordSpec with MustMatchers {
  "@cache" should {
    "expand to" in {
      val appliedDef = q"def pass(i: Int): Int = i"

      val applyMacro = q"met[Int, Int](1)"

      applyMacro match {
        case q"new $_[..$tps]($arg)" =>
          val actual = CacheMacroImpl.expand(tps, arg, appliedDef)
          q"""
           def pass(i: Int): Int = {
             val cleanFn = new cache.CleanUpCache[Int, Int]
             val cache = cleanFn()

           }
           """
          assertStructurallyEqual(actual, ???)
      }
    }
  }

  def assertStructurallyEqual(obtained: Tree, expected: Tree): Unit = {
    StructurallyEqual(obtained, expected) match {
      case Left(AnyDiff(x, y)) =>
        fail(s"""Not Structurally equal!:
                |obtained: $x
                |expected: $y
             """.stripMargin)
      case _ =>
    }
  }
}
