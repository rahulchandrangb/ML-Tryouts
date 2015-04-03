package rahul.misc
import scala.math._
import breeze.linalg._
import breeze.plot.Figure
import breeze.plot.plot
import breeze.stats.distributions.Beta
import breeze.stats.distributions.Gamma

object MLEMCTest extends App {
  calculateByMLE(probDetect, 10, 4, true) //Binomial prob  dist

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
      p += plot(px, py)
      val xxx = plot(px, py)
      p.xlabel = "p"
      p.ylabel = "P"
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

object BetaPlot extends App {
  val f = Figure("MLE")
  val p = f.subplot(0)
  val x = linspace(0.1,0.9)
  val alpha = 0.5
  val beta = 1.0
  val betaDist = new Beta(alpha, beta)
  val beta2 = new Gamma(1.0,beta)
  val beta3 = new Gamma(0.3,beta)
  val beta4  = new Gamma(2.0,beta)
  
  val betaVals = x.map(betaDist(_))
  val betaVals2 = x.map(beta2(_))
  val betaVals3 = x.map(beta3(_))
  val betaVals4 = x.map(beta4(_))
  
  println(betaVals)
  p += plot( betaVals,x)
  
  p += plot(betaVals2,x)
  p += plot(betaVals3,x)
  p += plot(betaVals4,x)
  
  p.xlabel = "x"
  
  p.ylabel = s"Gamma(x,alpha= $alpha ,beta= $beta )"
  p.setXAxisDecimalTickUnits
  p.setYAxisDecimalTickUnits
  f.saveas("/tmp/testGamma.png")
}