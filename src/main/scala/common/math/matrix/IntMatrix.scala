package common.math.matrix

class IntMatrix(back: IndexedSeq[IndexedSeq[Int]]) extends NumericMatrix[Int, IntMatrix] {
  override val backing: IndexedSeq[IndexedSeq[Int]] = back

  def this(x: Int, y: Int) = this(Array.ofDim[Int](x, y).map(_.toIndexedSeq).toIndexedSeq)

  override def cloneWith(backing: IndexedSeq[IndexedSeq[Int]]): IntMatrix = new IntMatrix(backing)
}