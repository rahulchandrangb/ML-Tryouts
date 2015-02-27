package rahul.recommend
import scala.math._

object MinkowskiTest {
  def main(args: Array[String]): Unit = {
    val lst = new Minkowski(2).getMinkowskiList(Map("Hello" -> List(1, 2, 3), "World" -> List(1, 2, 1), "test" -> List(2, 3, 4)), "Hello")
    println(lst)
  }
}
class Minkowski(normOrder: Int) {
  def getMinkowskiList(data: Map[String, List[Int]], user: String): List[(String, Double)] = {
    val usrList = data.getOrElse(user, List())
    val distanceList = data.map {
      case (nm: String, vl: List[Int]) =>
        val posElems = vl.zipWithIndex.map {
          elem =>
            if (elem._1 != -1 && usrList.size - 1 >= elem._2 && usrList(elem._2) != -1) {
              pow(abs(usrList(elem._2) - elem._1), normOrder)
            } else
              -1
        }.filter(_ > 0).sum
        val nthRoot = pow(posElems, (1.0 / normOrder))
        (nm, nthRoot)
    }
    println(distanceList)
    distanceList.toList.filter(! _._1.equals(user)).sortBy(_._2)
  }
}