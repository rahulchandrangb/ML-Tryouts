package rahul.NeuralNetwork



// 2. Strategy
// 2.1 Training strategy
trait TrainStrategy
case class TrainByEpoch(val numEpoch:Int) extends TrainStrategy
case class TrainByError(val errorThresholds:Double) extends TrainStrategy
case class BatchTrainByEpoch extends TrainStrategy
case class BatchTrainByError extends TrainStrategy


// RNN parent selection strategy.
trait PSelectStrategy
case class GreedySelect() extends PSelectStrategy
case class BruteforceBest() extends  PSelectStrategy