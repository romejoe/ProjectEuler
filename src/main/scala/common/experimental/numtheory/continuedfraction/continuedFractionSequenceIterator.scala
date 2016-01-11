package common.experimental.numtheory.continuedfraction

class continuedFractionSequenceIterator(n:Double, m:Double=1.0) extends Iterator[Int] {
  var a = n.toInt
  var r = n - a

  override def hasNext: Boolean = true

  override def next(): Int = {
    val A:Int = a
    val t:Double = m / r
    a = t.toInt
    r = t - a
    A
  }
}
