package rahul.recommend
import scala.math._

object PCCTest {

}
/*
 * This code contains methods to calculate Pearson Correlation Coefficient, Cosine Similiarity and k-nearest neighbour for collaborative filtering
 *  
 * 
 * Note: (1) Pearson Correlation Coefficient (r) is generally used when the data is subjected to grade-inflation (ie. different users may be using different scales)
 * 			It ranges between -1 and 1 inclusive. 1 indicates perfect agreement. -1 indicates perfect disagreement.
 *    
 *       (2) Minkowski's methods like manhattan distance and euclidean distance are used when the data is dense
 *       
 *       (3) Cosine similiarity is used when data given is sparse
 */
class Recommenders {
  /*
   * Pearson Correlation Coefficient , r is given by
   *   
   *                      (Sum(xi*yi)) - [(sum(xi) * sum(yi))/n]
   *   r =    __________________________________________________________________
   *            
   *            Sqrt{ sum(xi^2) - (sum(xi)^2)/n} * Sqrt{ sum(yi^2) - (sum(yi)^2)/n} 
   *              
   *              
   */
  def getPCCValue(dataX: List[Double], dataY: List[Double]): Double = {
    val xiYiSum = dataX.zip(dataY).foldLeft(0.0)((x, y) => (y._1 * y._2) + x)
    val xiSum = dataX.sum
    val yiSum = dataY.sum

    val sumXiSqr = dataX.map(pow(_, 2)).sum
    val sumXiWholeSquare = pow(dataX.sum, 2)

    val sumYiSqr = dataY.map(pow(_, 2)).sum
    val sumYiWholeSquare = pow(dataY.sum, 2)
    val n = dataX.size
    val pearsonCorCoeff = (xiYiSum - ((xiSum * yiSum) / n)) / pow((sumXiSqr - (sumXiWholeSquare / n)), 0.5) * pow((sumYiSqr - (sumYiWholeSquare / n)), 0.5)
    pearsonCorCoeff
  }
  def recommendNearestNeighBourByPCC(curUser: (String, List[Double]), allUsers: Map[String, List[Double]]) = {
    println("Calculating nearest neighbour using Pearson Correlation Coefficient")
    //Note that pearson correlation co-efficient extends from 1 to -1 [more positive means more close]
    val pccList = allUsers.filter(!_._1.equals(curUser._1)) map {
      case (usr: String, dataList: List[Double]) =>
        val pccVal = getPCCValue(curUser._2, dataList)
        (usr, pccVal)
    }
    val bestNeighBour = pccList.foldLeft(("", -2.0)) {
      (x, y) =>
        if (y._2 > x._2) y
        else x
    }
    println(s"The best nearest neighbour is ${bestNeighBour._1} with pearson correlation coefficient as ${bestNeighBour._2}")
  }
}