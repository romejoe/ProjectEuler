package common.math.matrix

trait Matrix[T, S <: Matrix[T,S]] { self: S =>

  val backing: IndexedSeq[IndexedSeq[T]]

  def cloneWith(backing: IndexedSeq[IndexedSeq[T]]): S

  def apply(x: Int, y: Int) = backing(x)(y)

  def row(x: Int): IndexedSeq[T] = backing(x)

  def rows: Iterator[IndexedSeq[T]] = backing.iterator

  def cols: Iterator[IndexedSeq[T]] = (0 until dimensions._2).iterator.map(col)

  def dimensions: (Int, Int) = (backing.length, backing(0).length)

  def col(y: Int): IndexedSeq[T] = backing.map(_ (y))

  def transpose: S = cloneWith(backing.transpose)

  def updated(x: Int, y: Int, v: T): S = updated((x, y, v))

  def updated(v: (Int, Int, T)*): S = cloneWith(v.foldLeft(backing)((m, p) => m.updated(p._1, m(p._1).updated(p._2, p._3))))

  override def toString(): String = backing.map(_.mkString(",")).mkString(System.lineSeparator())

  def canEqual(a:Any) = a.isInstanceOf[S]

  override def equals(a:Any):Boolean = {
    val m = a.asInstanceOf[S]
    if(dimensions != m.dimensions)
      false
    else{
      val (xlim, ylim) = dimensions

      (for(
        x <- 0 until xlim;
        y <- 0 until ylim
        ) yield apply(x,y) == m(x,y))
        .forall(b => b)
    }

  }
}