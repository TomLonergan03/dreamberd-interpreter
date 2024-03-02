// TODO: add opencsv and nameof to build.sbt
import com.opencsv._ // libraryDependencies += "com.opencsv" % "opencsv" % "5.9"
import java.io._
import com.github.dwickern.macros.NameOf._ // libraryDependencies += "com.github.dwickern" %% "scala-nameof" % "4.0.0" % "provided"

object persistent_handler {
    // data will be of form: name, value, type

    // write data to file
    def save(data: Array[Array[String]], file: String) = {
        val writer = new FileWriter(file)
        val csvWriter = new CSVWriter(writer)
        csvWriter.writeAll(data.toList)
        csvWriter.close()        
    }

    // load data in from file
    def load(path: String): Array[Array[String]] = {
        val reader = new FileReader(path)
        val csvReader = new CSVReader(reader)

        var data = Array[Array[String]]()

        var record : Array[String] = csvReader.readNext()
        data = data :+ record

        while (record != null) {
            record = csvReader.readNext()
            if (record != null) {
                data = data :+ record
            }
        }

        data
    }

    // add a new persistent
    def add(persistent: Any) = {
        val name = nameOf(persistent)
        val value = persistent.toString()
        val type_ = persistent.getClass().getName()

        persistents = persistents :+ Array(name, value, type_)
    }

    // storage of persistents
    private var persistents = Array[Array[String]]()
}