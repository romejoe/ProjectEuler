package common.experimental.numtheory.continuedfraction

import common.math.Rational

class continuedFractionIterator(iter:Iterator[Int]) extends Iterator[Rational]{
  def this(n:Double) = this(new continuedFractionSequenceIterator(n))
  var sequence:List[Int] = Nil

  override def hasNext: Boolean = iter.hasNext

  override def next(): Rational = {
    sequence = iter.next :: sequence
    val base = sequence.head
    sequence.tail.foldLeft(Rational(base,1))((s,a) => s.flip + a)
  }
}
