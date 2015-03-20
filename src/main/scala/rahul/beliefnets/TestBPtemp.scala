package rahul.beliefnets

trait MessageDirection
case class ForwardMessage extends MessageDirection
case class ReverseMessage extends MessageDirection 

object TestBP extends App {
  
}

case class TNode(nodName:String,message:Double=1.0){
  var connectedNodes:Array[TNode] = null
  def calcMessage(destNode:TNode,msgDir:MessageDirection = ForwardMessage())={  //Double
    
  }
  
  def calcLogLkRatio={
    
  }
}