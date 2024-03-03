package DreamBerd.ConstConstConst

import requests.Response
import upickle.default._
import DreamBerd.Syntax.Syntax._
import scala.collection.immutable.ListMap

var constConstConstServerAddress = "http://localhost:8080"

object ConstConstConst {

  // { "vars" : [ { "var": "example", "value": 5 } ] }
  type ConstConstConstMap = ListMap[String, Value]

  /** Get all const const consts from the server
    *
    * @return
    *   a map of all const const consts
    */
  def getAllConstConstConsts: ConstConstConstMap = {
    // get all const const consts from the server
    val r: Response = requests.get(constConstConstServerAddress + "/get-all")
    val json = ujson.read(r.text())
    val vars = json("vars").arr
    val constConstConsts = vars.map { v =>
      val varName = v("var").str
      val value = v("value")
      (varName, value)
    }
    var constConstConstMap: ConstConstConstMap = ListMap[String, Value]()
    constConstConsts.foreach { case (varName, value) =>
      // convert the values to their Value types
      value match {
        case ujson.Num(x) =>
          constConstConstMap = constConstConstMap + (varName -> NumV(x.toFloat))
        case ujson.Str(x) =>
          constConstConstMap = constConstConstMap + (varName -> StringV(x))
        case ujson.Bool(x) =>
          constConstConstMap = constConstConstMap + (varName -> BoolV(
            if (x) BoolOptions.True else BoolOptions.False
          ))
        case _ => throw new Exception("Invalid value type")
      }
    }
    constConstConstMap
  }

  /** Get a specific const const const from the server
    *
    * @param varName
    *   the name of the const const const
    * @return
    *   the value of the const const const
    */
  def getConstConstConst(varName: String): Value = {
    // get a specific const const const from the server
    val r: Response =
      requests.get(constConstConstServerAddress + "/get/" + varName)
    val json = ujson.read(r.text())
    val value = json("value")
    value match {
      case ujson.Num(x) =>
        NumV(x.toFloat)
      case ujson.Str(x) =>
        StringV(x)
      case ujson.Bool(x) =>
        BoolV(if (x) BoolOptions.True else BoolOptions.False)
      case _ => throw new Exception("Invalid value type")
    }
  }

  /** Set a specific const const const on the server
    *
    * @param varName
    *   the name of the const const const
    * @param value
    *   the value of the const const const
    * @return
    *   true if the const const const was set successfully, false otherwise
    */
  def setConstConstConst(varName: String, value: Value): Boolean = {
    var json = value match {
      case NumV(x) =>
        sys.error(
          "const const const only works for strings because David is lazy"
        )
      case StringV(x) => write(Map("var" -> varName, "value" -> x))
      case BoolV(x) =>
        sys.error(
          "const const const only works for strings because David is lazy"
        )
    }

    val r: Response = requests.post(
      constConstConstServerAddress + "/set",
      data = json,
      headers = Map("Content-Type" -> "application/json")
    )
    r.statusCode == 200
  }

}

@main def runConstConstConst(): Unit = {
  val test =
    ConstConstConst.setConstConstConst("example", StringV("hello there"))
}
