package common.math

package object matrix {
  implicit def ArrToIndexSeq[T](arr:Array[Array[T]]):IndexedSeq[IndexedSeq[T]] = {
    arr.map(_.toIndexedSeq).toIndexedSeq
  }
}
