package examples

import scala.collection.immutable
import scala.meta._



class cache[K, V](maxSize: Int,
                  invalidationStrategy: InvalidationStrategy[K, V],
                  replacementStrategy: CacheReplacementStrategy[K, V],
                  backend: CacheBackend[K, V])
    extends scala.annotation.StaticAnnotation {

  inline def apply(defn: Any): Any = meta {

    defn match {
      case defn: Defn.Def =>
        val params: immutable.Seq[immutable.Seq[Term.Name]] = defn.paramss.map(_.map(p => Term.Name(p.name.value)))



        this match {
          case q"new $_[..$tpr](...${argList})" =>
            val body: Term =
              q"""

              val cleanFn = new examples.CleanUpCache[${tpr.head}, ${tpr(1)}]
              val cache = cleanFn(...${argList})

              val result: ${tpr(1)} = cache.get(...${params}) match {
                case scala.util.Success(r) =>
                  println("Cache hit!")
                  r
                case scala.util.Failure(_) =>
                 println("Cache missed !!")
                 val r = ${defn.body}
                 cache.put(${params.head.head}, r)
                 r
              }

              result
              """
            defn.copy(body = body)
          case x =>
            abort(s"Unrecognized pattern $x")
        }


      case _ =>
        abort("Only def is supported")
    }
  }
}

object Helper {
  type Params = immutable.Seq[immutable.Seq[Term.Name]]
  def extend1stArgList(params: Params, a: Term.Name): Params = {
    params match {
      case head :: rest =>
        (head :+ a) :: rest
    }
  }
}

//class sample(maxSize: Int) extends scala.annotation.StaticAnnotation {
//  val x = maxSize * 10              // <---------- some derived val
//
//  inline def apply(defn: Any): Any = meta {
//
//    defn match {
//      case defn: Defn.Def =>
//
//        val body: Term =
//          q"""
//          println($x)                 // <----------- trying to use it
//          val result = ${defn.body}
//          result
//          """
//        defn.copy(body = body)
//      case _ =>
//        abort("Only def is supported")
//    }
//  }
//}

