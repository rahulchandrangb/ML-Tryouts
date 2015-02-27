package rahul.recommend

object PCCTest {

}
/*
 * This code contains methods to calculate Pearson Correlation Coefficient, Cosine Similiarity and k-nearest neighbour for collaborative filtering
 * 
 * Note: (1) Pearson Coefficient is generally used when the data is subjected to grade-inflation (ie. different users may be using different scales) 
 *       (2) Minkowski's methods like manhattan distance and euclidean distance are used when the data is dense
 *       (3) Cosine similiarity is used when data given is sparse
 */
class Recommenders{
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
  def getPCCValue(dataX:List[Double], dataY:List[Double])={
    
       
    
  }
}