package rahul.misc
import rahul.NLP.samplevals.SampleVal
import java.util.StringTokenizer
import scala.collection.JavaConversions._
import breeze.stats.distributions.Multinomial
import breeze.stats.distributions.Bernoulli
import breeze.stats.distributions.Beta


object GNBConsts {
  //Classes
  val POSITIVE = 1
  val NEGATIVE = 0
  // Hyper params of beta prior ..Here we give uniform distrib or equally likely
  val alpha = 1.0
  val beta = 1.0
  //
}


object GibbsOnNB {
  def main(args: Array[String]): Unit = {
    val corpusLoc: String = SampleVal.NBCORPUSLOC
    
  }
  def tokenize(str: String) = {
    new StringTokenizer(str).toList.map(_.toString.replaceAll("\\W+", ""))
  }
}
class GibbsOnNB(val corpusLoc:String,
    k:Int, //State space dimension
    priorPiAlpha:Double=GNBConsts.alpha, //Alpha for beta distrib prior
    priorPiBeta:Double = GNBConsts.beta 
) {
  
  
  
  
}

