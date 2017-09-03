package cache

import scala.collection.immutable.Seq
import scala.meta._


class cache[K, V](backend: SyncCache[K, V])
    extends scala.annotation.StaticAnnotation {

  inline def apply(defn: Any): Any = meta {

    defn match {
      case defn: Defn.Def =>
        this match {
          case q"new $_[..$tpr]($backendParam)" =>
            val body: Term = CacheMacroImpl.expand(tpr, backendParam, defn)
            defn.copy(body = body)
          case x =>
            abort(s"Unrecognized pattern $x")
        }

      case _ =>
        abort("This annotation only works on `def`")
    }
  }
}

object CacheMacroImpl {

  def expand(fnTypeParams: Seq[Type], cacheExpr: Term.Arg, annotatedDef: Defn.Def): Term = {
    val cache: Term.Name = Term.Name(cacheExpr.syntax)
    annotatedDef match {
      case q"..$_ def $methodName[..$tps](..$nonCurriedParams): $rtType = $expr" =>

        if (nonCurriedParams.size == 1) {
          val paramAsArg = Term.Name(nonCurriedParams.head.name.value)
          q"""
            val result: ${rtType} = $cache.get($paramAsArg) match {
              case Some(v) => v
              case None =>
                val value = ${expr}
                $cache.put($paramAsArg, value)
                value
            }
            result
           """
        } else {
          val paramAsArg = nonCurriedParams.map(p => Term.Name(p.name.value))
          q"""
            val result: ${rtType} = $cache.get((..$paramAsArg)) match {
              case Some(v) => v
              case None =>
                val value = ${expr}
                $cache.put((..$paramAsArg), value)
                value
            }
            result
           """
        }
      case other => abort(s"Expected non-curried method, got $other")
    }
  }
}

