package DreamBerd.Eval

import DreamBerd.Syntax.Syntax._
import scala.collection.immutable.ListMap

object Eval {

  // ======================================================================
  // Capture-avoiding substitution
  // ======================================================================

  val generator = SymGenerator()

  object SubstExpr extends Substitutable[Expr] {
    // swap y and z in e
    def swap(e: Expr, y: Variable, z: Variable): Expr =
      def go(e: Expr): Expr = e match {
        // Value must be closed
        case v: Value => v

        case Unit => Unit

        case Num(n)           => Num(n)
        case Plus(e1, e2)     => Plus(go(e1), go(e2))
        case Minus(e1, e2)    => Minus(go(e1), go(e2))
        case Times(e1, e2)    => Times(go(e1), go(e2))
        case Divide(e1, e2)   => Divide(go(e1), go(e2))
        case Exponent(e1, e2) => Exponent(go(e1), go(e2))

        case Bool(b)               => Bool(b)
        case OneEquals(e1, e2)     => OneEquals(go(e1), go(e2))
        case TwoEquals(e1, e2)     => TwoEquals(go(e1), go(e2))
        case ThreeEquals(e1, e2)   => ThreeEquals(go(e1), go(e2))
        case FourEquals(e1, e2)    => FourEquals(go(e1), go(e2))
        case IfThenElse(e, e1, e2) => IfThenElse(go(e), go(e1), go(e2))

        case Str(s)         => Str(s)
        case Length(e)      => Length(go(e))
        case Index(e1, e2)  => Index(go(e1), go(e2))
        case Concat(e1, e2) => Concat(go(e1), go(e2))

        case Var(x)         => Var(swapVar(x, y, z))
        case Let(x, e1, e2) => Let(swapVar(x, y, z), go(e1), go(e2))

        case Pair(e1, e2) => Pair(go(e1), go(e2))
        case First(e)     => First(go(e))
        case Second(e)    => Second(go(e))

        case Lambda(x, e)  => Lambda(swapVar(x, y, z), go(e))
        case Apply(e1, e2) => Apply(go(e1), go(e2))
        case Rec(f, x, e)  => Rec(swapVar(f, y, z), swapVar(x, y, z), go(e))

        case Record(es) => Record(es.map((x, e) => (x, go(e))))
        case Proj(e, l) => Proj(go(e), l)

        // case Ref(e)         => Ref(go(e))
        // case Deref(e)       => Deref(go(e))
        // case Assign(e1, e2) => Assign(go(e1), go(e2))

        case Sequ(e1, e2) => Sequ(go(e1), go(e2))
        case LetPair(x1, x2, e1, e2) =>
          LetPair(swapVar(x1, y, z), swapVar(x2, y, z), go(e1), go(e2))
        case LetFun(f, x, e1, e2) =>
          LetFun(swapVar(f, y, z), swapVar(x, y, z), go(e1), go(e2))
        case LetRec(f, x, e1, e2) =>
          LetRec(swapVar(f, y, z), swapVar(x, y, z), go(e1), go(e2))
        case LetRecord(xs, e1, e2) =>
          LetRecord(xs.map((l, x) => (l, swapVar(x, y, z))), go(e1), go(e2))
      }
      go(e)

    def apply(theta: Subst[Expr], e: Expr): Expr =
      // BEGIN ANSWER
      e match {
        case Num(e)           => Num(e)
        case Plus(t1, t2)     => Plus(apply(theta, t1), apply(theta, t2))
        case Minus(t1, t2)    => Minus(apply(theta, t1), apply(theta, t2))
        case Times(t1, t2)    => Times(apply(theta, t1), apply(theta, t2))
        case Divide(t1, t2)   => Divide(apply(theta, t1), apply(theta, t2))
        case Exponent(t1, t2) => Exponent(apply(theta, t1), apply(theta, t2))

        // Booleans
        case Bool(b)           => Bool(b)
        case OneEquals(t1, t2) => OneEquals(apply(theta, t1), apply(theta, t2))
        case TwoEquals(t1, t2) => TwoEquals(apply(theta, t1), apply(theta, t2))
        case ThreeEquals(t1, t2) =>
          ThreeEquals(apply(theta, t1), apply(theta, t2))
        case FourEquals(t1, t2) =>
          FourEquals(apply(theta, t1), apply(theta, t2))
        case IfThenElse(t0, t1, t2) =>
          IfThenElse(apply(theta, t0), apply(theta, t1), apply(theta, t2))

        // Strings
        case Str(s)         => Str(s)
        case Length(t0)     => Length(apply(theta, t0))
        case Index(t1, t2)  => Index(apply(theta, t1), apply(theta, t2))
        case Concat(t1, t2) => Concat(apply(theta, t1), apply(theta, t2))

        case Let(y, t1, t2) => {
          val z = generator.genVar(y);
          Let(z, apply(theta, t1), apply(theta, swap(t2, y, z)))
        }

        // Pairs
        case Pair(t1, t2) => Pair(apply(theta, t1), apply(theta, t2))
        case First(t0)    => First(apply(theta, t0))
        case Second(t0)   => Second(apply(theta, t0))

        // Functions
        case Lambda(x, e) => {
          val y = generator.genVar(x);
          Lambda(y, apply(theta, swap(e, x, y)))
        }
        case Apply(t1, t2) => Apply(apply(theta, t1), apply(theta, t2))
        case Rec(f, x, e) => {
          val g = generator.genVar(f);
          val y = generator.genVar(x);
          Rec(g, y, apply(theta, swap(swap(e, f, g), x, y)))
        }

        // Syntactic sugar
        case LetPair(y1, y2, t1, t2) => {
          val y1z = generator.genVar(y1);
          val y2z = generator.genVar(y2);
          LetPair(
            y1z,
            y2z,
            apply(theta, t1),
            apply(theta, swap(swap(t2, y1z, y1), y2z, y2))
          )
        }

        case LetFun(f, x, e1, e2) => {
          val g = generator.genVar(f);
          val y = generator.genVar(x);
          LetFun(
            g,
            y,
            apply(theta, swap(swap(e1, f, g), x, y)),
            apply(theta, swap(e2, f, g))
          )
        }

        case LetRec(f, x, e1, e2) => {
          val g = generator.genVar(f);
          val y = generator.genVar(x);
          LetRec(
            g,
            y,
            apply(theta, swap(swap(e1, f, g), x, y)),
            apply(theta, swap(e2, f, g))
          )
        }

        case Var(y) => theta.getOrElse(y, Var(y))

        case Record(es) => Record(es.map((l, t) => (l, apply(theta, t))))
        case Proj(t, l) => Proj(apply(theta, t), l)
        // case Ref(t)     => Ref(apply(theta, t))
        // case Deref(t)   => Deref(apply(theta, t))
        // case Assign(t1, t2) =>
        //   Assign(apply(theta, t1), apply(theta, t2))
        case Sequ(t1, t2) =>
          Sequ(apply(theta, t1), apply(theta, t2))
        case LetRecord(xs, t1, t2) => {
          val ys = xs.map((l, x) => (l, generator.genVar(x)));
          val theta1 = theta ++ xs.zip(ys).map((x, y) => (x._2, Var(y._2)));
          LetRecord(
            ys,
            apply(theta, t1),
            apply(theta1, t2)
          )
        }
        case v: Value => v
        case Unit     => Unit
        // END ANSWER
      }
  }
  import SubstExpr.{subst, ra2AppAssoc, ra2CompAssoc}

  object Value {
    // utility methods for operating on values
    def add(v1: Value, v2: Value): Value = (v1, v2) match
      case (NumV(v1), NumV(v2)) => NumV(v1 + v2)
      case _ =>
        sys.error("arguments to addition are non-numeric " + v1 + " " + v2)

    def subtract(v1: Value, v2: Value): Value = (v1, v2) match
      case (NumV(v1), NumV(v2)) => NumV(v1 - v2)
      case _ => sys.error("arguments to subtraction are non-numeric")

    def multiply(v1: Value, v2: Value): Value = (v1, v2) match
      case (NumV(v1), NumV(v2)) => NumV(v1 * v2)
      case _ => sys.error("arguments to multiplication are non-numeric")

    def exponent(v1: Value, v2: Value): Value = (v1, v2) match
      case (NumV(v1), NumV(v2)) => NumV(Math.pow(v1, v2).toFloat)
      case _ => sys.error("arguments to multiplication are non-numeric")

    def divide(v1: Value, v2: Value): Value = (v1, v2) match
      case (NumV(v1), NumV(0))  => sys.error("Denominator cannot be 0")
      case (NumV(v1), NumV(v2)) => NumV(v1 / v2)
      case _ => sys.error("arguments to multiplication are non-numeric")

    def oneEquals(v1: Value, v2: Value): Value = (v1, v2) match
      case (NumV(v1), NumV(v2)) =>
        if (Math.round(v1) == Math.round(v2))
          BoolV(BoolOptions.True)
        else
          BoolV(BoolOptions.False)
      case (BoolV(BoolOptions.Maybe), BoolV(v2)) => BoolV(BoolOptions.True)
      case (BoolV(v1), BoolV(BoolOptions.Maybe)) => BoolV(BoolOptions.True)
      case (BoolV(v1), BoolV(v2)) =>
        if (v1 == v2)
          BoolV(BoolOptions.True)
        else
          BoolV(BoolOptions.False)
      case (StringV(v1), StringV(v2)) =>
        if (v1.charAt(0).equals(v2.charAt(0)))
          BoolV(BoolOptions.True)
        else
          BoolV(BoolOptions.False)
      case (NumV(v1), StringV(v2)) =>
        if (v1.toString().charAt(0).equals(v2.charAt(0)))
          BoolV(BoolOptions.True)
        else
          BoolV(BoolOptions.False)
      case (StringV(v1), NumV(v2)) =>
        if (v2.toString().charAt(0).equals(v1.charAt(0)))
          BoolV(BoolOptions.True)
        else
          BoolV(BoolOptions.False)
      case _ => sys.error("arguments to = are not base types " + v1 + " " + v2)

    def twoEquals(v1: Value, v2: Value): Value = (v1, v2) match
      case (NumV(v1), NumV(v2)) =>
        if (v1 == v2)
          BoolV(BoolOptions.True)
        else
          BoolV(BoolOptions.False)
      case (BoolV(BoolOptions.Maybe), BoolV(v2)) =>
        val rand = new scala.util.Random
        if (rand.nextFloat() > 0.5)
          BoolV(BoolOptions.True)
        else
          BoolV(BoolOptions.False)
      case (BoolV(v1), BoolV(BoolOptions.Maybe)) =>
        val rand = new scala.util.Random
        if (rand.nextFloat() > 0.5)
          BoolV(BoolOptions.True)
        else
          BoolV(BoolOptions.False)
      case (BoolV(v1), BoolV(v2)) =>
        if (v1 == v2)
          BoolV(BoolOptions.True)
        else
          BoolV(BoolOptions.False)
      case (StringV(v1), StringV(v2)) =>
        if (v1.equals(v2))
          BoolV(BoolOptions.True)
        else
          BoolV(BoolOptions.False)
      case (NumV(v1), StringV(v2)) =>
        if (v1.toString().equals(v2))
          BoolV(BoolOptions.True)
        else
          BoolV(BoolOptions.False)
      case (StringV(v1), NumV(v2)) =>
        if (v2.toString().equals(v1))
          BoolV(BoolOptions.True)
        else
          BoolV(BoolOptions.False)
      case _ => sys.error("arguments to == are not base types " + v1 + " " + v2)

    def threeEquals(v1: Value, v2: Value): Value = (v1, v2) match
      case (NumV(v1), NumV(v2)) =>
        if (v1 == v2)
          BoolV(BoolOptions.True)
        else
          BoolV(BoolOptions.False)
      case (BoolV(v1), BoolV(v2)) =>
        if (v1 == v2)
          BoolV(BoolOptions.True)
        else
          BoolV(BoolOptions.False)
      case (StringV(v1), StringV(v2)) =>
        if (v1 == v2)
          BoolV(BoolOptions.True)
        else
          BoolV(BoolOptions.False)
      case _ =>
        sys.error("arguments to === are not base types " + v1 + " " + v2)

    def fourEquals(v1: Value, v2: Value): Value = (v1, v2) match
      case _ => BoolV(BoolOptions.False)

    def length(v: Value): Value = v match
      case StringV(v1) => NumV(v1.length)
      case _           => sys.error("argument to length is not a string")

    def index(v1: Value, v2: Value): Value = (v1, v2) match
      case (StringV(v1), NumV(v2)) =>
        StringV(v1.charAt(Math.round(v2)).toString)
      case _ => sys.error("arguments to index are not valid")

    def concat(v1: Value, v2: Value): Value = (v1, v2) match
      case (StringV(v1), StringV(v2)) => StringV(v1 ++ v2)
      case _ => sys.error("arguments to concat are not strings")
  }

  // ======================================================================
  // Evaluation
  // ======================================================================

  def eval(env: Env[Value], e: Expr): Value = e match {
    // Value
    case v: Value => v

    // BEGIN ANSWER
    case Plus(e1, e2)     => Value.add(eval(env, e1), eval(env, e2))
    case Minus(e1, e2)    => Value.subtract(eval(env, e1), eval(env, e2))
    case Times(e1, e2)    => Value.multiply(eval(env, e1), eval(env, e2))
    case Divide(e1, e2)   => Value.divide(eval(env, e1), eval(env, e2))
    case Exponent(e1, e2) => Value.exponent(eval(env, e1), eval(env, e2))
    case IfThenElse(e, e1, e2) =>
      if eval(env, e) == BoolV(BoolOptions.True) then eval(env, e1)
      else eval(env, e2)
    case OneEquals(e1, e2)   => Value.oneEquals(eval(env, e1), eval(env, e2))
    case TwoEquals(e1, e2)   => Value.twoEquals(eval(env, e1), eval(env, e2))
    case ThreeEquals(e1, e2) => Value.threeEquals(eval(env, e1), eval(env, e2))
    case FourEquals(e1, e2)  => Value.fourEquals(eval(env, e1), eval(env, e2))
    case Length(e)           => Value.length(eval(env, e))
    case Index(e1, e2)       => Value.index(eval(env, e1), eval(env, e2))
    case Concat(e1, e2)      => Value.concat(eval(env, e1), eval(env, e2))
    case Apply(e1, e2) =>
      val v1 = eval(env, e1)
      val v2 = eval(env, e2)
      v1 match {
        case FunV(x, e)    => eval(env, subst(e, v2, x))
        case RecV(f, x, e) => eval(env, subst(subst(e, v1, f), v2, x))
        case _             => sys.error("eval: apply e1 not a function")
      }
    case Let(x, e1, e2) => eval(env, subst(e2, eval(env, e1), x))
    case Pair(e1, e2)   => PairV(eval(env, e1), eval(env, e2))
    case First(e) =>
      eval(env, e) match {
        case PairV(v1, v2) => v1
        case _             => sys.error("eval: First e not a pair")
      }
    case Second(e) =>
      eval(env, e) match {
        case PairV(v1, v2) => v2
        case _             => sys.error("eval: Second e not a pair")
      }
    case Record(es) => RecordV(es.map((l, e) => (l, eval(env, e))))
    case Proj(e, l) =>
      eval(env, e) match {
        case RecordV(es) => es(l)
        case _           => sys.error("eval: Proj e not a record")
      }
    // case Ref(e) => Cell(RefCell(eval(e)))
    // case Deref(e) =>
    //   eval(e) match {
    //     case Cell(r) => r.get
    //     case _       => sys.error("eval: deref not a cell")
    //   }
    // case Assign(e1, e2) =>
    //   eval(e1) match {
    //     case Cell(r) => r.set(eval(e2)); UnitV
    //     case _       => sys.error("eval: assign not a cell")
    //   }
    case Num(n)       => NumV(n)
    case Bool(b)      => BoolV(b)
    case Str(s)       => StringV(s)
    case Var(x)       => sys.error("eval: free variable: " + x)
    case Lambda(x, e) => FunV(x, e)
    case Rec(f, x, e) => RecV(f, x, e)
    case Unit         => UnitV
    case _ =>
      sys.error("eval: should have been desugared " + e)
  }

  // END ANSWER

  type Subst[A] = ListMap[Variable, A]
  object Subst {
    def empty[A]: Subst[A] = ListMap.empty[Variable, A]
    def apply[A](pairs: (Variable, A)*): Subst[A] = ListMap(pairs: _*)
  }
  // the empty substitution ι
  def iota[A]: Subst[A] = Subst.empty

  trait Applicable[A, B] {
    // apply the substitution theta to t
    def apply(theta: Subst[A], t: B): B

    // Scala magic to define the right-associative infix operator +:
    class AppAssoc(x: B) {
      def +:(theta: Subst[A]): B = apply(theta, x)
    }
    implicit def ra2AppAssoc(x: B): AppAssoc = new AppAssoc(x)
  }

  // a trait for substitutable things, e.g., expressions and types
  trait Substitutable[A] extends Applicable[A, A] {
    // swap y and z in t
    def swap(t: A, y: Variable, z: Variable): A

    // subst x in t1 with t2, i.e., t1[t2/x]
    def subst(t1: A, t2: A, x: Variable): A =
      apply(ListMap(x -> t2), t1)

    // compose two subsitutions, i.e., s1 ∘ s2
    def comp(s1: Subst[A], s2: Subst[A]): Subst[A] =
      s1 ++ s2.map((x: Variable, t: A) => (x, apply(s1, t)))

    // Scala magic to define the right-associative infix operator *:
    class CompAssoc(y: Subst[A]) {
      def *:(x: Subst[A]): Subst[A] = comp(x, y)
    }
    implicit def ra2CompAssoc(x: Subst[A]): CompAssoc = new CompAssoc(x)
  }
}

@main def test() =
  var env = ListMap.empty[Variable, Value]
  println(Eval.eval(env, OneEquals(NumV(2.4), NumV(3))))
  println(
    Eval.eval(env, TwoEquals(BoolV(BoolOptions.Maybe), BoolV(BoolOptions.True)))
  )
  println(Eval.eval(env, Let("😀😀", Num(3), Num(4))))
