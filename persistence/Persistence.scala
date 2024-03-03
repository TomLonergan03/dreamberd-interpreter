package HTB.Persistance

import scala.jdk.CollectionConverters._
import java.io._
import com.github.tototoshi.csv._ // libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.3.10",
import com.github.dwickern.macros.NameOf._ // libraryDependencies += "com.github.dwickern" %% "scala-nameof" % "4.0.0" % "provided"
import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer
import Assign3.Syntax.Syntax._
import scala.collection.Seq
import scala.collection.mutable._

object persistent_handler {
    // data will be of form: name, value, type
    val filepath = "./output.csv"// "/~/.dreamberd/persistent.csv"

    // write data to file
    def save(data: Env[Value], file: String) = {
        var listOfStrings = Array[List[String]]()

        data.foreach {(name, value) =>
            val array = List(name, value.toString(), value.getClass().getName())
            listOfStrings = listOfStrings :+ array
        }

        val f = new File(file)
        val writer = CSVWriter.open(f)
        writer.writeAll(listOfStrings.toList)
        writer.close()        
    }

    // load data in from file
    def load(path: String): Env[Value] = {
        val reader = CSVReader.open(new File(path))
        var data = Array[Array[String]]()
        var env = ListMap[String, Value]()
        var resultingMap = Map[String, Value]()


        reader.foreach {fields =>
            var vals = fields.toArray
            // println(fields.mkString(","))
            // println(vals(0))
            // resultingMap = resultingMap + (vals(0) -> vals(1).asInstanceOf[Value])
            data = data :+ vals
        }

        reader.close()

        env
    }
}