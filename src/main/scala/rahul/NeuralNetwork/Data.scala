package rahul.NeuralNetwork
import breeze.linalg.DenseVector

object Data {
  val SAMPLE_INPUT = DenseVector(1.0, 1.0, 0.2, 0.8, 0.6)
  val SAMPLE_OUTPUT = DenseVector(1.0, 1.0, 0.0, 1.0, 1.0)
  val DEFAULT_TRAIN_EPOCHS = 30000
  val DEFAULT_MINIMUM_ERROR = 0.0003
}