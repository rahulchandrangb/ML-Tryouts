package rahul.beliefnets

sealed trait MessageDirection
case class ForwardMessage extends MessageDirection
case class ReverseMessage extends MessageDirection 

object TestBP extends App {
  
}

case class TNode(nodName:String,message:Double=1.0){ 
  var connectedNodes:Array[TNode] = null
  var delta:Double=1.0 //message val 
  
  
  def calcMessage(destNode:TNode,msgDir:MessageDirection = ForwardMessage())={  //Double
	//delta * curNode.connectedNodes.
    
  }
  
  def calcLogLkRatio={
    
  }
}