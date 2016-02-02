package common.math.matrix

class VectorMatrix[T:Numeric](back: IndexedSeq[IndexedSeq[T]]) extends NumericMatrix[T, VectorMatrix[T]]{

  override val backing: IndexedSeq[IndexedSeq[T]] = back

  //  def this(x: Int, y: Int) = this(Vector.fill[T](x,y)(0.asInstanceOf[T]))//this(Array.ofDim[T](x, y)(ev).map(_.toIndexedSeq).toIndexedSeq)

  def this(subs:IndexedSeq[T]*) = this(subs.toIndexedSeq)

  override def cloneWith(backing: IndexedSeq[IndexedSeq[T]]): VectorMatrix[T] = new VectorMatrix[T](backing)
}
