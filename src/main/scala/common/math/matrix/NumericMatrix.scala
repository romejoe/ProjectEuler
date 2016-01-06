package common.math.matrix

trait NumericMatrix[@specialized T] extends Matrix[T] {
  implicit val num: Numeric[T]

  def cloneWith(backing: IndexedSeq[IndexedSeq[T]]): NumericMatrix[T]

  def +(m: NumericMatrix[T]): NumericMatrix[T] = {
    require(dimensions == m.dimensions)
    val back: IndexedSeq[IndexedSeq[T]] = backing.zip(m.backing).map(p => p._1.zip(p._2).map(p2 => num.plus(p2._1, p2._2)))
    cloneWith(back)
  }

  def -(m: NumericMatrix[T]): NumericMatrix[T] = {
    require(dimensions == m.dimensions)
    val back: IndexedSeq[IndexedSeq[T]] = backing.zip(m.backing).map(p => p._1.zip(p._2).map(p2 => num.minus(p2._1, p2._2)))
    cloneWith(back)
  }

  def *(m: NumericMatrix[T]): NumericMatrix[T] = {
    val result: IndexedSeq[IndexedSeq[T]] =
      (for (row <- rows) yield {
        val tmp = for (col <- m.cols) yield {
          val tmp2 = row.zip(col).map(p => num.times(p._1, p._2)).reduce(num.plus)
          tmp2
        }

        tmp.toIndexedSeq
      }
        ).toIndexedSeq

    val ret = cloneWith(result)
    ret
  }

  def ^(i: Int): NumericMatrix[T] = {
    if (i == 1)
      this
    else if (i == 2)
      this * this
    else if (i % 2 == 1)
      this * (this ^ 2) ^ ((i - 1) / 2)
    else
      (this ^ 2) ^ (i / 2)
  }

}

class DoubleMatrix(back: IndexedSeq[IndexedSeq[Double]]) extends NumericMatrix[Double] {
  override val num = implicitly[Numeric[Double]]
  override val backing: IndexedSeq[IndexedSeq[Double]] = back

  def this(x: Int, y: Int) = this(Array.ofDim[Double](x, y).map(_.toIndexedSeq).toIndexedSeq)

  override def cloneWith(backing: IndexedSeq[IndexedSeq[Double]]): NumericMatrix[Double] = new DoubleMatrix(backing)
}

class IntMatrix(back: IndexedSeq[IndexedSeq[Int]]) extends NumericMatrix[Int] {
  override val num = implicitly[Numeric[Int]]
  override val backing: IndexedSeq[IndexedSeq[Int]] = back

  def this(x: Int, y: Int) = this(Array.ofDim[Int](x, y).map(_.toIndexedSeq).toIndexedSeq)

  override def cloneWith(backing: IndexedSeq[IndexedSeq[Int]]): NumericMatrix[Int] = new IntMatrix(backing)
}

class BigIntMatrix(back: IndexedSeq[IndexedSeq[BigInt]]) extends NumericMatrix[BigInt] {
  override val num = implicitly[Numeric[BigInt]]
  override val backing: IndexedSeq[IndexedSeq[BigInt]] = back

  def this(x: Int, y: Int) = this(Array.ofDim[BigInt](x, y).map(_.toIndexedSeq).toIndexedSeq)

  override def cloneWith(backing: IndexedSeq[IndexedSeq[BigInt]]): NumericMatrix[BigInt] = new BigIntMatrix(backing)
}
