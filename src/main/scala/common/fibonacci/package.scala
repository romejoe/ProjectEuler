package common

package object fibonacci {
  class fibIterator extends Iterator[BigInt] {
    var state = (BigInt(1), BigInt(0))

    override def hasNext: Boolean = true

    override def next(): BigInt = {
      val ret = state._2
      state = (state._1 + state._2, state._1)
      ret
    }
  }
}
