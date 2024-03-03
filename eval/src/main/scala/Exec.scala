import DreamBerd.Syntax.Syntax._
import DreamBerd.Eval.Eval

object Exec {

  def exec(env: Env[Value], deletedKeywords: Set[String], when: Set[(String, Value, Expr)], s: Stmt): (Env[Value], Set[String], Set[(String, Value, Expr)]) =
    s match {
      case Delete(k) => (env, deletedKeywords + k, when)
      case Skip      => (env, deletedKeywords, when)
      case Seq(s1, s2) =>
        val (env1, deletedKeywords1, when1) = exec(env, deletedKeywords, when, s1)
        exec(env1, deletedKeywords1, when1, s2)
      case IfThenElseS(e, s1, s2) =>
        if (deletedKeywords.contains("if"))
          sys.error("This expression has been deleted")
        val v = Eval.eval(env, deletedKeywords, when, e)
        v match {
          case BoolV(BoolOptions.True)  => exec(env, deletedKeywords, when, s1)
          case BoolV(BoolOptions.False) => exec(env, deletedKeywords, when, s2)
          case BoolV(BoolOptions.Maybe) => exec(env, deletedKeywords, when, s2)
        }
      case Assign(x, e) =>
        val v = Eval.eval(env, deletedKeywords, when, e)
        val existsTuple = when.map {
          case (first, second, third) if first == x && second == v => third
          case _ => Unit
        }
        if (existsTuple.nonEmpty) {
          Eval.eval(env, deletedKeywords, when, existsTuple.head)
        }
        (env + (x -> v), deletedKeywords, when)
    }

}
