package rahul.montecarlo
import breeze.stats.distributions.Uniform
import scala.math._
object TestMonte extends App{
  println("Finding Sinx integral from 0 to Pi/2 using Monte Carlo...\n-----------------------------------------------------------")
  def radianVal(x:Double) = 0.0174532925 * x //degree to radian
  def degreeVal(x:Double) = 57.2957795 * x //radian to degree
  val rng = new Uniform(0,1)
  val testOut =   cos(0) - cos(Pi/4)
  val epochs  = 10000000
  val monteOut = (Iterator.iterate(0.0){
    case (s:Double) =>
      val newNum = sin(rng.draw * Pi/4)
      (s + newNum)
  }.drop(epochs).next) * Pi * 0.25 / epochs 
  println(s"\nDirectly calculated Integral Output : $testOut \nMonte-Carlo Calculated Output: $monteOut")
}