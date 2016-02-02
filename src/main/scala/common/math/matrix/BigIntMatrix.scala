package common.math.matrix


class BigIntMatrix(back: IndexedSeq[IndexedSeq[BigInt]]) extends NumericMatrix[BigInt, BigIntMatrix] {
  override val backing: IndexedSeq[IndexedSeq[BigInt]] = back

  def this(x: Int, y: Int) = this(Array.ofDim[BigInt](x, y).map(_.toIndexedSeq).toIndexedSeq)

  override def cloneWith(backing: IndexedSeq[IndexedSeq[BigInt]]): BigIntMatrix = new BigIntMatrix(backing)
}

