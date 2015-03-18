package rahul.markov
import rahul.pgm.FactorTable

import scala.collection.mutable.Map

class MarkovNode(val nodeName:String,val factorTable:FactorTable) {
  val connectedNodes = Map[String,MarkovNode]()
  
  def insertConnectedNodes(nodeName:String,node:MarkovNode)={
    connectedNodes += (nodeName -> node)
  }
  def insertConnectedNodes(nodeMap:Map[String,MarkovNode]) ={
    connectedNodes ++= nodeMap
  }
  override def toString={
   nodeName + "-> { "+   connectedNodes.map(_._1).toList.mkString(",") +" }"
  }
}