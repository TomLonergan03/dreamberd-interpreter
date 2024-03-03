import DreamBerd.Syntax.Syntax._
import DreamBerd.Eval.Eval
import scala.collection.mutable.ListMap

object Exec {

  def exec(
      env: Env[Value],
      deletedKeywords: Set[String],
      s: Stmt
  ): (Env[Value], Set[String]) =
    s match {
      case Delete(k) => (env, deletedKeywords + k)
      case Skip      => (env, deletedKeywords)
      case Seq(s1, s2) =>
        val (env1, deletedKeywords1) = exec(env, deletedKeywords, s1)
        exec(env1, deletedKeywords1, s2)
      case IfThenElseS(e, s1, s2) =>
        if (deletedKeywords.contains("if"))
          sys.error("This expression has been deleted")
        val v = Eval.eval(env, deletedKeywords, e)
        v match {
          case BoolV(BoolOptions.True)  => exec(env, deletedKeywords, s1)
          case BoolV(BoolOptions.False) => exec(env, deletedKeywords, s2)
          case BoolV(BoolOptions.Maybe) => exec(env, deletedKeywords, s2)
        }
      case Assign(x, e) =>
        val v = Eval.eval(env, deletedKeywords, e)
        (env + (x -> v), deletedKeywords)
      case Reverse() => (env, deletedKeywords)
    }

}
