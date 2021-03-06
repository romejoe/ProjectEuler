package common.fibonacci

import common.math.matrix.BigIntMatrix

import scala.collection.immutable.BitSet.BitSet1

object MatrixFibonacci{
  val BigFib = new BigIntMatrix(Array(Array(BigInt(1), BigInt(1)).toIndexedSeq, Array(BigInt(1), BigInt(0)).toIndexedSeq).toIndexedSeq)
  val Identity:BigIntMatrix = new BigIntMatrix(Array(Array(BigInt(1), BigInt(0)).toIndexedSeq, Array(BigInt(1), BigInt(0)).toIndexedSeq).toIndexedSeq)
}

class MatrixFibonacci {
    import MatrixFibonacci._
    var cache:Map[Int, BigIntMatrix] = Map(1->(BigFib^1))
    def apply(n:Int):BigInt = {
      val elems = new BitSet1(n.toLong)
      val newMax = elems.map(1<<_).foldLeft(Identity)((r, i) =>{
        if(!cache.contains(i)){
          cache += (i -> (BigFib^i))
        }
        r * cache(i)
      })

      cache += (n -> newMax)
      newMax(0,1)
    }
}
