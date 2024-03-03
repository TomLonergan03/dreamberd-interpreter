package DreamBerd.Import

import io.circe._
import io.circe.parser._
import io.circe.generic.semiauto._

import DreamBerd.Syntax.Syntax._

var example = """
{
  type: prog,
  lines: [
    {
      tree: {
        type: Add,
        left: {
          type: If,
          condition: {
            type: Boolean,
            value: Maybe
          },
          then: {
            type: Num,
            value: 1
          },
          else: {
            type: Num,
            value: 2
          }
        },
        right: {
          type: Num,
          value: 3
        }
      },
      priority: 2
    }
  ]
}
"""

implicit val decodeExpr: Decoder[Expr] = new Decoder[Expr] {
  final def apply(c: HCursor): Decoder.Result[Expr] =
    for {
      t <- c.downField("type").as[String]
      e <- t match {
        case "Unit"        => Right(Unit)
        case "Num"         => c.as[Num]
        case "Plus"        => c.as[Plus]
        case "Minus"       => c.as[Minus]
        case "Times"       => c.as[Times]
        case "Divide"      => c.as[Divide]
        case "Exponent"    => c.as[Exponent]
        case "Bool"        => c.as[Bool]
        case "OneEquals"   => c.as[OneEquals]
        case "TwoEquals"   => c.as[TwoEquals]
        case "ThreeEquals" => c.as[ThreeEquals]
        case "FourEquals"  => c.as[FourEquals]
        case "Str"         => c.as[Str]
        case "Length"      => c.as[Length]
        case "Index"       => c.as[Index]
        case "Concat"      => c.as[Concat]
        case "Var"         => c.as[Var]
        case "Lambda"      => c.as[Lambda]
        case "Rec"         => c.as[Rec]
        case "Pair"        => c.as[Pair]
        case "First"       => c.as[First]
        case "Second"      => c.as[Second]
        case "Record"      => c.as[Record]
        case "Proj"        => c.as[Proj]
        case _             => Left(DecodingFailure("Expr", c.history))
      }
    } yield e
}

implicit val decodeStmt: Decoder[Stmt] = new Decoder[Stmt] {
  final def apply(c: HCursor): Decoder.Result[Stmt] =
    for {
      t <- c.downField("type").as[String]
      s <- t match {
        case "Skip"        => Right(Skip)
        case "Seq"         => c.as[Seq]
        case "IfThenElseS" => c.as[IfThenElseS]
        case "Assign"      => c.as[Assign]
        case _             => Left(DecodingFailure("Stmt", c.history))
      }
    } yield s
}

implicit val decodeSeq: Decoder[Seq] = new Decoder[Seq] {
  final def apply(c: HCursor): Decoder.Result[Seq] =
    for {
      s1 <- c.downField("s1").as[Stmt]
      s2 <- c.downField("s2").as[Stmt]
    } yield {
      new Seq(s1, s2)
    }
}

implicit val decodeIfThenElseS: Decoder[IfThenElseS] =
  new Decoder[IfThenElseS] {
    final def apply(c: HCursor): Decoder.Result[IfThenElseS] =
      for {
        e <- c.downField("e").as[Expr]
        s1 <- c.downField("s1").as[Stmt]
        s2 <- c.downField("s2").as[Stmt]
      } yield {
        new IfThenElseS(e, s1, s2)
      }
  }

implicit val decodeAssign: Decoder[Assign] = new Decoder[Assign] {
  final def apply(c: HCursor): Decoder.Result[Assign] =
    for {
      x <- c.downField("x").as[Variable]
      e <- c.downField("e").as[Expr]
    } yield {
      new Assign(x, e)
    }
}

implicit val decodeProgram: Decoder[Program] = new Decoder[Program] {
  final def apply(c: HCursor): Decoder.Result[Program] =
    for {
      lines <- c.downField("lines").as[List[Stmt]]
    } yield {
      lines
    }
}

implicit val decodeNum: Decoder[Num] = new Decoder[Num] {
  final def apply(c: HCursor): Decoder.Result[Num] =
    for {
      n <- c.downField("value").as[Float]
    } yield {
      new Num(n)
    }
}

implicit val decodePlus: Decoder[Plus] = new Decoder[Plus] {
  final def apply(c: HCursor): Decoder.Result[Plus] =
    for {
      left <- c.downField("left").as[Expr]
      right <- c.downField("right").as[Expr]
    } yield {
      new Plus(left, right)
    }
}

implicit val decodeMinus: Decoder[Minus] = new Decoder[Minus] {
  final def apply(c: HCursor): Decoder.Result[Minus] =
    for {
      left <- c.downField("left").as[Expr]
      right <- c.downField("right").as[Expr]
    } yield {
      new Minus(left, right)
    }
}

implicit val decodeTimes: Decoder[Times] = new Decoder[Times] {
  final def apply(c: HCursor): Decoder.Result[Times] =
    for {
      left <- c.downField("left").as[Expr]
      right <- c.downField("right").as[Expr]
    } yield {
      new Times(left, right)
    }
}

implicit val decodeDivide: Decoder[Divide] = new Decoder[Divide] {
  final def apply(c: HCursor): Decoder.Result[Divide] =
    for {
      left <- c.downField("left").as[Expr]
      right <- c.downField("right").as[Expr]
    } yield {
      new Divide(left, right)
    }
}

implicit val decodeExponent: Decoder[Exponent] = new Decoder[Exponent] {
  final def apply(c: HCursor): Decoder.Result[Exponent] =
    for {
      left <- c.downField("left").as[Expr]
      right <- c.downField("right").as[Expr]
    } yield {
      new Exponent(left, right)
    }
}

implicit val decodeBool: Decoder[Bool] = new Decoder[Bool] {
  final def apply(c: HCursor): Decoder.Result[Bool] =
    for {
      b <- c.downField("value").as[BoolOptions]
    } yield {
      new Bool(b)
    }
}

implicit val decodeBoolOptions: Decoder[BoolOptions] =
  new Decoder[BoolOptions] {
    final def apply(c: HCursor): Decoder.Result[BoolOptions] =
      for {
        s <- c.as[String]
      } yield {
        s match {
          case "True"  => BoolOptions.True
          case "Maybe" => BoolOptions.Maybe
          case "False" => BoolOptions.False
        }
      }
  }

implicit val decodeOneEquals: Decoder[OneEquals] =
  new Decoder[OneEquals] {
    final def apply(c: HCursor): Decoder.Result[OneEquals] =
      for {
        left <- c.downField("left").as[Expr]
        right <- c.downField("right").as[Expr]
      } yield {
        new OneEquals(left, right)
      }
  }

implicit val decodeTwoEquals: Decoder[TwoEquals] =
  new Decoder[TwoEquals] {
    final def apply(c: HCursor): Decoder.Result[TwoEquals] =
      for {
        left <- c.downField("left").as[Expr]
        right <- c.downField("right").as[Expr]
      } yield {
        new TwoEquals(left, right)
      }
  }

implicit val decodeThreeEquals: Decoder[ThreeEquals] =
  new Decoder[ThreeEquals] {
    final def apply(c: HCursor): Decoder.Result[ThreeEquals] =
      for {
        left <- c.downField("left").as[Expr]
        right <- c.downField("right").as[Expr]
      } yield {
        new ThreeEquals(left, right)
      }
  }

implicit val decodeFourEquals: Decoder[FourEquals] =
  new Decoder[FourEquals] {
    final def apply(c: HCursor): Decoder.Result[FourEquals] =
      for {
        left <- c.downField("left").as[Expr]
        right <- c.downField("right").as[Expr]
      } yield {
        new FourEquals(left, right)
      }
  }

implicit val decodeStr: Decoder[Str] = new Decoder[Str] {
  final def apply(c: HCursor): Decoder.Result[Str] =
    for {
      s <- c.downField("value").as[String]
    } yield {
      new Str(s)
    }
}

implicit val decodeLength: Decoder[Length] = new Decoder[Length] {
  final def apply(c: HCursor): Decoder.Result[Length] =
    for {
      e <- c.downField("value").as[Expr]
    } yield {
      new Length(e)
    }
}

implicit val decodeIndex: Decoder[Index] = new Decoder[Index] {
  final def apply(c: HCursor): Decoder.Result[Index] =
    for {
      e1 <- c.downField("left").as[Expr]
      e2 <- c.downField("right").as[Expr]
    } yield {
      new Index(e1, e2)
    }
}

implicit val decodeConcat: Decoder[Concat] = new Decoder[Concat] {
  final def apply(c: HCursor): Decoder.Result[Concat] =
    for {
      e1 <- c.downField("left").as[Expr]
      e2 <- c.downField("right").as[Expr]
    } yield {
      new Concat(e1, e2)
    }
}

implicit val decodeVar: Decoder[Var] = new Decoder[Var] {
  final def apply(c: HCursor): Decoder.Result[Var] =
    for {
      x <- c.downField("value").as[String]
    } yield {
      new Var(x)
    }
}

implicit val decodeLambda: Decoder[Lambda] = new Decoder[Lambda] {
  final def apply(c: HCursor): Decoder.Result[Lambda] =
    for {
      x <- c.downField("variable").as[String]
      e <- c.downField("body").as[Expr]
    } yield {
      new Lambda(x, e)
    }
}

implicit val decodeApply: Decoder[Apply] = new Decoder[Apply] {
  final def apply(c: HCursor): Decoder.Result[Apply] =
    for {
      e1 <- c.downField("function").as[Expr]
      e2 <- c.downField("argument").as[Expr]
    } yield {
      new Apply(e1, e2)
    }
}

implicit val decodeRec: Decoder[Rec] = new Decoder[Rec] {
  final def apply(c: HCursor): Decoder.Result[Rec] =
    for {
      f <- c.downField("function").as[String]
      x <- c.downField("variable").as[String]
      e <- c.downField("body").as[Expr]
    } yield {
      new Rec(f, x, e)
    }
}

implicit val decodePair: Decoder[Pair] = new Decoder[Pair] {
  final def apply(c: HCursor): Decoder.Result[Pair] =
    for {
      e1 <- c.downField("left").as[Expr]
      e2 <- c.downField("right").as[Expr]
    } yield {
      new Pair(e1, e2)
    }
}

implicit val decodeFirst: Decoder[First] = new Decoder[First] {
  final def apply(c: HCursor): Decoder.Result[First] =
    for {
      e <- c.downField("value").as[Expr]
    } yield {
      new First(e)
    }
}

implicit val decodeSecond: Decoder[Second] = new Decoder[Second] {
  final def apply(c: HCursor): Decoder.Result[Second] =
    for {
      e <- c.downField("value").as[Expr]
    } yield {
      new Second(e)
    }
}

implicit val decodeRecord: Decoder[Record] = new Decoder[Record] {
  final def apply(c: HCursor): Decoder.Result[Record] =
    for {
      es <- c.downField("fields").as[Field[Expr]]
    } yield {
      new Record(es)
    }
}

implicit val decodeProj: Decoder[Proj] = new Decoder[Proj] {
  final def apply(c: HCursor): Decoder.Result[Proj] =
    for {
      e <- c.downField("record").as[Expr]
      l <- c.downField("label").as[String]
    } yield {
      new Proj(e, l)
    }
}

object Import {

  def parse(json: String): Either[Error, Program] = {
    decode[Program](json)
  }

}

object Main extends App {
  val parsed = Import.parse(example)
  println(parsed)
}
