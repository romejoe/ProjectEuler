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

  implicit class RichIndexedSeq[T](i:IndexedSeq[T]){
    def *(n:Int):IndexedSeq[T]={
      (0 until n).foldLeft(Vector[T]())((a,b) => a++i)
    }
  }

  object Ngon{
    var cache:Map[Int, IntMatrix] = Map()
    def makeConnectedMatrix(N:Int) = {
      if(!cache.contains(N)){
        val ZERO = Vector(0) * N
        cache += (N -> new IntMatrix(Vector() ++
          (0 until N).map(i => ZERO.updated(i, 1).updated(if ((i - 1) < 0) N - 1 else i - 1, 1)) ++
          (0 until N).map(i => ZERO.updated(i, 1)))
          )
      }
      cache(N)
    }

    private def parseNgon(n:String):IndexedSeq[Int] = {
      val parts:Array[Array[Int]] = n.split(";").map(_.split(",").map(_.trim.toInt))
      val ext:IndexedSeq[Int] = parts.map(_(0))
      val int:IndexedSeq[Int] = parts.map(_(1))
      int ++ ext
    }

  }


  class Ngon(config:IndexedSeq[Int]) extends IntMatrix(Vector(config)){
    val N = config.length / 2

    def this(n:Int) = this(Vector(0)*2*n)
    def this(n:String) = this(Ngon.parseNgon(n))

    val connectionMatrix = Ngon.makeConnectedMatrix(N)

    def isValid:Boolean = {
      val tmp = this * connectionMatrix
      val res = tmp.row(0).sliding(2).forall(seq => seq(0) == seq(1))
      res
    }

    def sum:Int = {
      (this*connectionMatrix)(0,0)
    }

    def toEmptyString:String = toString.replace(";","").replace(",","")


    override def toString():String = {
      val ext = config.drop(N)
      val start = ext.zipWithIndex.minBy(_._1)._2
      val pairs = (0 until N).map(i => {
        val idx = (start + i) % N
        Seq(config(N + idx), config(idx), config((idx + 1) % N))
      })

      pairs.map(p => p.mkString(",")).mkString(";")
    }
  }

  val m = (1 to 10).toIndexedSeq.permutations.map(new Ngon(_))
    .filter(ngon => ngon.isValid && ngon.toEmptyString.length == 16).maxBy(_.toEmptyString.toLong)

  println(m)
  println(m.toEmptyString)


}
