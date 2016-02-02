package common.math.matrix

class DoubleMatrix(back: IndexedSeq[IndexedSeq[Double]]) extends NumericMatrix[Double, DoubleMatrix] {
override val backing: IndexedSeq[IndexedSeq[Double]] = back

  def this(x: Int, y: Int) = this(Array.ofDim[Double](x, y).map(_.toIndexedSeq).toIndexedSeq)

  override def cloneWith(backing: IndexedSeq[IndexedSeq[Double]]):DoubleMatrix = new DoubleMatrix(backing)
}
