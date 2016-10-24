import scala.collection.mutable.ArrayBuffer
import scala.io.Source

trait Output {
  def println(x:Any) = Console.println(x)
}
trait TimeSeriesAnalyzer extends Output{
  case class MeasurementsWindow(measurements: Array[Measurement]) {
    val T = measurements.last.timestamp
    val V = measurements.last.measurement.formatted("%.5f").replaceAll("0*$", "").replaceAll("\\.$", "")
    val N = measurements.length
    val RS = measurements.map(_.measurement).sum.formatted("%.5f").replaceAll("0*$", "").replaceAll("\\.$", "")
    val MinV = measurements.minBy(_.measurement).measurement.formatted("%.5f").replaceAll("0*$", "").replaceAll("\\.$", "")
    val MaxV = measurements.maxBy(_.measurement).measurement.formatted("%.5f").replaceAll("0*$", "").replaceAll("\\.$", "")
  }

  case class Measurement(timestamp: Int, measurement: Double)

  object Measurement {
    def apply(line: String): Measurement = line.replace('\t', ' ').split(" ") match {
      case Array(s1, s2) => Measurement(s1.toInt, s2.toDouble)
    }
  }


  val windowSize = 60

  def process(file: Iterator[String]): Unit = {
    println("T          V       N RS      MinV    MaxV")
    println("--------------------------------------------")
    var mesBuffer = ArrayBuffer[Measurement]()
    while (file.hasNext) {
      val mes = Measurement(file.next())
      mesBuffer = mesBuffer.filter(x => x.timestamp >= (mes.timestamp - windowSize))
      mesBuffer += mes
      val window = MeasurementsWindow(mesBuffer.toArray)
      import window._
      println(s"$T $V $N $RS $MinV $MaxV")
    }
  }
}
object Main extends Output{



  def main(args: Array[String]): Unit = {
    val fileName = args(0)
    val file = Source.fromFile(fileName).getLines()
    val analyzer = new TimeSeriesAnalyzer {}
    analyzer.process(file)
  }

}
