import DreamBerd.Syntax.Syntax._
import DreamBerd.Eval.Eval

object Exec {

  def exec(env: Env[Value], s: Stmt): Env[Value] =
    s match {
      case Skip => env
      case Seq(s1, s2) =>
        val env1 = exec(env, s1)
        exec(env1, s2)
      case IfThenElseS(e, s1, s2) =>
        val v = Eval.eval(env, e)
        v match {
          case BoolV(BoolOptions.True)  => exec(env, s1)
          case BoolV(BoolOptions.False) => exec(env, s2)
          case BoolV(BoolOptions.Maybe) => exec(env, s2)
        }
    }

}
