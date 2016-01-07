package p68

import common.math.matrix.IntMatrix

object Main extends App{
  /**
    * If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
    * Find the sum of all the multiples of 3 or 5 below 1000.
  */

  def makeN[T](N:Int)(f: =>T):List[T] = {
    var value:List[T] = Nil
    var i = 0
    while (i < N){
      value = f :: value
      i += 1
    }

    value
  }

  abstract class GonNode
  case class InnerGon(var exterior:ExteriorGon, var right:InnerGon, var value:Int) extends GonNode
  case class ExteriorGon(var anchor:InnerGon, var value:Int) extends GonNode

  class NgonBuilder(N:Int){
    val exteriorNodeList = makeN(N)(ExteriorGon(null, 0))
    val innerNodeList = makeN(N)(InnerGon(null, null, 0))
    exteriorNodeList.zip(innerNodeList)
        .foreach({case (exterior, inner) =>
          exterior.anchor = inner
          inner.exterior = exterior
        })
  }

  val A = new IntMatrix(Vector(Vector(3,2,1,4,6,5)))
  val old_connected = new IntMatrix(Vector(
    Vector(0, 1, 1, 1, 0, 0),
    Vector(1, 0, 1, 0, 1, 0),
    Vector(1, 1, 0, 0, 0, 1),
    Vector(1, 0, 0, 0, 0, 0),
    Vector(0, 1, 0, 0, 0, 0),
    Vector(0, 0, 1, 0, 0, 0)
  ))

  val connected = new IntMatrix(Vector(
    Vector(1, 0, 1),
    Vector(1, 1, 0),
    Vector(0, 1, 1),
    Vector(1, 0, 0),
    Vector(0, 1, 0),
    Vector(0, 0, 1)
  ))

  println(s"A:\n$A")
  println(s"connected:\n$connected")
  println(s"A*connected:\n${A*connected}")

  val int = new IntMatrix(Vector(Vector(3,2,1)))
  val ext = new IntMatrix(Vector(Vector(4,6,5)))

  implicit class RichIndexedSeq[T](i:IndexedSeq[T]){
    def *(n:Int):IndexedSeq[T]={
      (0 until n).foldLeft(Vector[T]())((a,b) => a++i)
    }
  }

  def makeConnectedMatrix(N:Int) = {
    val ZERO = Vector(0) * N
    new IntMatrix(Vector() ++
      (0 until N).map(i => ZERO.updated(i, 1).updated(if((i-1)<0) N-1 else i-1, 1)) ++
      (0 until N).map(i => ZERO.updated(i, 1)))
  }

  println(makeConnectedMatrix(3))

  println(connected == makeConnectedMatrix(3))
  println()
  println(makeConnectedMatrix(5))

}
