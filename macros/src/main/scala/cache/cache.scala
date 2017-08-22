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
    val cache = Term.Name(cacheExpr.syntax)
    annotatedDef match {
      case q"..$_ def $methodName[..$tps]($nonCurriedParam): $rtType = $expr" =>

        val paramAsArg = Term.Name(nonCurriedParam.name.value)
        q"""
        val result: ${fnTypeParams(1)} = $cache.get($paramAsArg) match {
          case Some(r) =>
            r
          case None =>
           val r = ${annotatedDef.body}
           $cache.put($paramAsArg, r)
           r
        }

        result
        """
      case q"..$_ def $methodName[..$tps](..$nonCurriedParams): $rtType = $expr" =>
        val paramAsArg = nonCurriedParams.map(p => Term.Name(p.name.value))
        q"""
        val result: ${fnTypeParams(1)} = $cache.get(..$paramAsArg) match {
          case Some(r) =>
            r
          case None =>
           val r = ${annotatedDef.body}
           $cache.put((..$paramAsArg), r)
           r
        }

        result
        """
      case other => abort(s"Expected non-curried method, got $other")
    }
  }
}

