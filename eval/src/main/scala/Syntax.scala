package DreamBerd.Syntax

import java.security.Identity
import scala.language.implicitConversions
import scala.collection.immutable.ListMap
import scala.collection.Seq

object Syntax {
  type Variable = String
  type Env[A] = ListMap[Variable, A]
  type Label = String
  type Field[A] = ListMap[Label, A]
  type Clauses = ListMap[(Label, Variable), Expr]

  enum BoolOptions:
    case True, Maybe, False

  class RefCell[A](val x: A) {
    private var a = x
    def get = a
    def set(y: A) = a = y
  }

  // ======================================================================
  // Statements
  // ======================================================================
  sealed abstract class Stmt

  case object Skip extends Stmt

  case class Delete(k: String) extends Stmt
  case class Seq(s1: Stmt, s2: Stmt) extends Stmt
  case class IfThenElseS(e: Expr, s1: Stmt, s2: Stmt) extends Stmt
  case class Assign(x: Variable, e: Expr) extends Stmt
  case class Reverse() extends Stmt

  case class Program(lines: List[Line])
  case class Line(t: Stmt, p: Int)

  // ======================================================================
  // Expressions
  // ======================================================================
  sealed abstract class Expr

  // Unit
  case object Unit extends Expr

  // Arithmetic expressions
  case class Num(n: Float) extends Expr
  case class Plus(e1: Expr, e2: Expr) extends Expr
  case class Minus(e1: Expr, e2: Expr) extends Expr
  case class Times(e1: Expr, e2: Expr) extends Expr
  case class Divide(e1: Expr, e2: Expr) extends Expr
  case class Exponent(e1: Expr, e2: Expr) extends Expr

  // Booleans
  case class Bool(b: BoolOptions) extends Expr
  case class OneEquals(e1: Expr, e2: Expr) extends Expr
  case class TwoEquals(e1: Expr, e2: Expr) extends Expr
  case class ThreeEquals(e1: Expr, e2: Expr) extends Expr
  case class FourEquals(e1: Expr, e2: Expr) extends Expr
  case class IfThenElse(e: Expr, e1: Expr, e2: Expr) extends Expr

  // Strings
  case class Str(s: String) extends Expr
  case class Length(e: Expr) extends Expr
  case class Index(e1: Expr, e2: Expr) extends Expr
  case class IndexTo(e1: Expr, e2: Expr, e3: Expr) extends Expr
  case class Concat(e1: Expr, e2: Expr) extends Expr

  // Variables and let-binding
  case class Var(x: Variable) extends Expr
  case class Let(x: Variable, e1: Expr, e2: Expr) extends Expr

  // Functions
  case class Lambda(x: Variable, e: Expr) extends Expr
  case class Apply(e1: Expr, e2: Expr) extends Expr
  case class Rec(f: Variable, x: Variable, e: Expr) extends Expr

  // Pairing
  case class Pair(e1: Expr, e2: Expr) extends Expr
  case class First(e: Expr) extends Expr
  case class Second(e: Expr) extends Expr

  // Records
  case class Record(es: Field[Expr]) extends Expr
  case class Proj(e: Expr, l: Label) extends Expr

  // References - removed because they are replaced with statements?
  // case class Ref(e: Expr) extends Expr
  // case class Deref(e: Expr) extends Expr
  // case class Assign(e1: Expr, e2: Expr) extends Expr

  // Syntactic sugars
  case class LetPair(x: Variable, y: Variable, e1: Expr, e2: Expr) extends Expr
  case class LetFun(f: Variable, arg: Variable, e1: Expr, e2: Expr) extends Expr
  case class LetRec(f: Variable, arg: Variable, e1: Expr, e2: Expr) extends Expr
  case class LetRecord(xs: Field[Variable], e1: Expr, e2: Expr) extends Expr
  case class Sequ(e1: Expr, e2: Expr) extends Expr

  // Values
  abstract class Value extends Expr
  case object UnitV extends Value
  case object UndefinedV extends Value
  case class NumV(n: Float) extends Value
  case class BoolV(b: BoolOptions) extends Value
  case class StringV(s: String) extends Value
  case class PairV(v1: Value, v2: Value) extends Value
  case class RecordV(vs: Field[Value]) extends Value
  case class Cell(r: RefCell[Value]) extends Value
  // NOTE: Cell is not a syntactic value, thus cannot be generalised

  case class FunV(x: Variable, e: Expr) extends Value
  case class RecV(f: Variable, x: Variable, e: Expr) extends Value

  // ======================================================================
  // Types
  // ======================================================================
  sealed abstract class Type

  // Monomorphic types
  case object TyUnit extends Type
  case object TyInt extends Type
  case object TyBool extends Type
  case object TyString extends Type
  case class TyPair(ty1: Type, ty2: Type) extends Type
  case class TyFun(ty1: Type, ty2: Type) extends Type
  case class TyRef(ty: Type) extends Type
  case class TyRecord(tys: Field[Type]) extends Type
  case class TyVar(x: Variable) extends Type

  // Type schemes
  case class TyForall(a: Variable, ty: Type) extends Type

  // ======================================================================
  // Substitutions
  // ======================================================================

  // a class for generating fresh variables
  class SymGenerator {
    private var id = 0
    // generate a fresh variable from an existing variable
    def genVar(s: Variable): Variable = {
      val fresh_s = s + "_" + id
      id = id + 1
      fresh_s
    }
    // generate a fresh variable from nothing
    def freshVar(): Variable = {
      val fresh_s = "$" + id
      id = id + 1
      fresh_s
    }
  }

  // swap y and z in x
  def swapVar(x: Variable, y: Variable, z: Variable): Variable =
    if x == y then z else if x == z then y else x

  // a substitution is defined as a map
  // all substitutions in Subst[A] are done simultaneously
  type Subst[A] = ListMap[Variable, A]
  object Subst {
    def empty[A]: Subst[A] = ListMap.empty[Variable, A]
    def apply[A](pairs: (Variable, A)*): Subst[A] = ListMap(pairs*)
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

  // Scala magic to define the right-associative infix operator <->
  // precedence: <-> < +: < *:
  class UnifyAssoc[A, B](x: A) {
    def <->(y: B): (A, B) = (x, y)
  }
  implicit def ra2UnifyAssoc[A, B](x: A): UnifyAssoc[A, B] = new UnifyAssoc(x)
}
