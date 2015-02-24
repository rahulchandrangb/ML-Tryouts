package rahul.misc
import scala.math._
import breeze.linalg._
import breeze.plot.Figure
import breeze.plot.plot

object MLETest extends App {
  calculateByMLE(probDetect, 10, 4,true)

  def calculateByMLE(f: (Double, Int, Int) => Double, n: Int, r: Int, plotFlag: Boolean = false) = {
    val probs = for {
      i <- 0.0 to 1.0 by 0.01
      bigP = f(i, n, r)
    } yield (bigP, i)
    probs.foreach(x => println(s"(${x._1},${x._2})"))
    if (plotFlag) {
      val f = Figure("MLE")
      val p = f.subplot(0)
      val x = linspace(0.0, 1.0)
      val px = new DenseVector(probs.map(_._2).toArray)
      val py = new DenseVector(probs.map(_._1).toArray)
      p += plot(px,py)
      val xxx = plot(px,py)
      p.xlabel = "p"
      p.ylabel = "P"
      p.title = "Maximum Likelihood probability estimation of H4T6"
      p.chart
      p.setXAxisDecimalTickUnits
      p.setYAxisDecimalTickUnits
      f.saveas("/tmp/test.png")
    }
    println("Best Value By Maximum Likelihood estimation:" + probs.max)
  }

  def splitCalc(str: String): (Int, Int) = str.split("").foldLeft(0, 0) {
    (a, b) =>
      if (b.toUpperCase.equals("H"))
        (a._1 + 1, a._2)
      else {
        (a._1, a._2 + 1)
      }
  }
  def factorial(n: Int): Int = {
    if (n == 0) 1
    else n * factorial(n - 1)
  }
  def probDetect(p: Double, n: Int, r: Int): Double = {
    (factorial(n) / (factorial(n - r) * factorial(r))) * pow(p, r) * pow(1 - p, n - r)
  }
}