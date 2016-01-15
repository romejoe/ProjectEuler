package common.math.matrix

abstract class NumericMatrix[@specialized T:Numeric, S <: NumericMatrix[T,S]] extends Matrix[T, S] { self: S =>

  //implicit val num: Numeric[T]
  //type U <: NumericMatrix[T]
  //self: U =>

  private def plus(x:T, y:T) = implicitly[Numeric[T]] plus(x,y)
  private def minus(x:T, y:T) = implicitly[Numeric[T]] minus(x,y)
  private def times(x:T, y:T) = implicitly[Numeric[T]] times(x,y)

  //def cloneWith(backing: IndexedSeq[IndexedSeq[T]]):

  def +(m: S): S = {
    require(dimensions == m.dimensions)
    val back: IndexedSeq[IndexedSeq[T]] = backing.zip(m.backing).map(p => p._1.zip(p._2).map(p2 => plus(p2._1, p2._2)))
    cloneWith(back)
  }

  def -(m: S): S = {
    require(dimensions == m.dimensions)
    val back: IndexedSeq[IndexedSeq[T]] = backing.zip(m.backing).map(p => p._1.zip(p._2).map(p2 => minus(p2._1, p2._2)))
    cloneWith(back)
  }

  def *(m: S): S = {
    val result: IndexedSeq[IndexedSeq[T]] =
      (for (row <- rows) yield {
        val tmp = for (col <- m.cols) yield {
          val tmp2 = row.zip(col).map(p => times(p._1, p._2)).reduce(plus)
          tmp2
        }

        tmp.toIndexedSeq
      }
        ).toIndexedSeq

    val ret = cloneWith(result)
    ret
  }

  def ^(i: Int): S = {
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

class DoubleMatrix(back: IndexedSeq[IndexedSeq[Double]]) extends NumericMatrix[Double, DoubleMatrix] { //self: S =>
  override val backing: IndexedSeq[IndexedSeq[Double]] = back

  def this(x: Int, y: Int) = this(Array.ofDim[Double](x, y).map(_.toIndexedSeq).toIndexedSeq)

  override def cloneWith(backing: IndexedSeq[IndexedSeq[Double]]):DoubleMatrix = new DoubleMatrix(backing)
}

class IntMatrix(back: IndexedSeq[IndexedSeq[Int]]) extends NumericMatrix[Int, IntMatrix] {
  override val backing: IndexedSeq[IndexedSeq[Int]] = back

  def this(x: Int, y: Int) = this(Array.ofDim[Int](x, y).map(_.toIndexedSeq).toIndexedSeq)

  override def cloneWith(backing: IndexedSeq[IndexedSeq[Int]]): IntMatrix = new IntMatrix(backing)
}

class BigIntMatrix(back: IndexedSeq[IndexedSeq[BigInt]]) extends NumericMatrix[BigInt, BigIntMatrix] {
  override val backing: IndexedSeq[IndexedSeq[BigInt]] = back

  def this(x: Int, y: Int) = this(Array.ofDim[BigInt](x, y).map(_.toIndexedSeq).toIndexedSeq)

  override def cloneWith(backing: IndexedSeq[IndexedSeq[BigInt]]): BigIntMatrix = new BigIntMatrix(backing)
}

class VectorMatrix[T:Numeric](back: IndexedSeq[IndexedSeq[T]]) extends NumericMatrix[T, VectorMatrix[T]]{
  override val backing: IndexedSeq[IndexedSeq[T]] = back

//  def this(x: Int, y: Int) = this(Vector.fill[T](x,y)(0.asInstanceOf[T]))//this(Array.ofDim[T](x, y)(ev).map(_.toIndexedSeq).toIndexedSeq)

  def this(subs:IndexedSeq[T]*) = this(subs.toIndexedSeq)

  override def cloneWith(backing: IndexedSeq[IndexedSeq[T]]): VectorMatrix[T] = new VectorMatrix[T](backing)
}

