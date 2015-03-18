package rahul.pgm

case class FactorTable(rows: Int, cols: Int,
  colLabels: List[(String, List[String])], // Should be of the form: [A,List("a1","a2","a3")]
  values: (List[String], Double), //Careful with this.. both the markov and bayes represented in Double 
  factorType: FactorType) /* represents markov or bayesian */ {
  /*
   * Operations:
   * 
   * 1) Factor marginalization - normalized,unnormalized
   * 2) Factor Reduction
   * 3) Factor Product
   * 
   */
  def factorMarginalize(shouldNormalize: Boolean = false) = {

  }
  def factorReduce(filterFactorList: List[(String, String)]) = {

  }
}

object FactorTable {
  def factorProduct(factorTables: List[FactorTable]) {
    
  }
}