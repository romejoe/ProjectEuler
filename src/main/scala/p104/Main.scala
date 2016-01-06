package p104

import scala.collection.immutable.BitSet.BitSet1

/*
The Fibonacci sequence is defined by the recurrence relation:

Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.
It turns out that F541, which contains 113 digits, is the first Fibonacci number for which the last nine digits are 1-9 pandigital (contain all the digits 1 to 9, but not necessarily in order). And F2749, which contains 575 digits, is the first Fibonacci number for which the first nine digits are 1-9 pandigital.

Given that Fk is the first Fibonacci number for which the first nine digits AND the last nine digits are 1-9 pandigital, find k.
 */
object Main extends App {
  val DIGITS = Set('1', '2', '3', '4', '5', '6', '7', '8', '9')
  val iter = new fibIterator()

  var m: Map[Int, BigInt] = Map[Int, BigInt](1 -> BigInt(1), 2 -> BigInt(1))
  var cache: Map[Int, BigInt] = Map()

  def isAcceptable(l: BigInt): Boolean = {
    val str = l.toString
    isFrontAcceptable(str) && isBackAcceptable(str)
  }

  def isFrontAcceptable(str: String): Boolean = {
    str.take(9).toSet == DIGITS
  }

  //def ArraysToIndexedSeqs[T](arrs:Array[Array[T]]):IndexedSeq[IndexedSeq[T]]
  //  = arrs.map(_.toIndexedSeq)

  def isBackAcceptable(str: String): Boolean = {
    str.takeRight(9).toSet == DIGITS
  }

  def f(i: Int): BigInt = {
    /*var state = (1,1)
    var I = 0
    if(i != 0)
      while(I < i){
        state = (state._1 + state._2, state._1)
        I = I + 1
      }
    state._1*/
    if (!cache.contains(i)) {
      cache ++= iter.take(i + 1 - cache.size).zipWithIndex.map(p => (p._2 + cache.size, p._1))
    }

    cache(i)

  }

  /* abstract class ArrayNumericMatrix[T](back:IndexedSeq[IndexedSeq[T]]) extends NumericMatrix[T]{
     override val backing: IndexedSeq[IndexedSeq[T]] = back
   }*/

  trait Matrix[T] {
    val backing: IndexedSeq[IndexedSeq[T]]

    def cloneWith(backing: IndexedSeq[IndexedSeq[T]]): Matrix[T]

    def apply(x: Int, y: Int) = backing(x)(y)

    def row(x: Int): IndexedSeq[T] = backing(x)

    def rows: Iterator[IndexedSeq[T]] = backing.iterator

    def cols: Iterator[IndexedSeq[T]] = (0 until dimensions._2).iterator.map(col)

    def dimensions: (Int, Int) = (backing.length, backing(0).length)

    def col(y: Int): IndexedSeq[T] = backing.map(_ (y))

    def transpose: Matrix[T] = cloneWith(backing.transpose)

    def updated(x: Int, y: Int, v: T): Matrix[T] = updated((x, y, v))

    def updated(v: (Int, Int, T)*): Matrix[T] = cloneWith(v.foldLeft(backing)((m, p) => m.updated(p._1, m(p._1).updated(p._2, p._3))))

    override def toString(): String = backing.map(_.mkString(",")).mkString(System.lineSeparator())

  }

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

  class IntMatrix(back: IndexedSeq[IndexedSeq[Int]]) extends NumericMatrix[Int] {
    override val num = implicitly[Numeric[Int]]
    override val backing: IndexedSeq[IndexedSeq[Int]] = back

    def this(x: Int, y: Int) = this(Array.ofDim[Int](x, y).map(_.toIndexedSeq).toIndexedSeq)

    override def cloneWith(backing: IndexedSeq[IndexedSeq[Int]]): NumericMatrix[Int] = new IntMatrix(backing)

    override def toString: String = super.toString
  }

  class BigIntMatrix(back: IndexedSeq[IndexedSeq[BigInt]]) extends NumericMatrix[BigInt] {
    override val num = implicitly[Numeric[BigInt]]
    override val backing: IndexedSeq[IndexedSeq[BigInt]] = back

    def this(x: Int, y: Int) = this(Array.ofDim[BigInt](x, y).map(_.toIndexedSeq).toIndexedSeq)

    override def cloneWith(backing: IndexedSeq[IndexedSeq[BigInt]]): NumericMatrix[BigInt] = new BigIntMatrix(backing)

    override def toString: String = super.toString
  }


  val Identity = new IntMatrix(Array(Array(1, 0).toIndexedSeq, Array(0, 1).toIndexedSeq).toIndexedSeq)
  val fib = new IntMatrix(Array(Array(1, 1).toIndexedSeq, Array(1, 0).toIndexedSeq).toIndexedSeq)
  val BigFib = new BigIntMatrix(Array(Array(BigInt(1), BigInt(1)).toIndexedSeq, Array(BigInt(1), BigInt(0)).toIndexedSeq).toIndexedSeq)


  //println(Identity)
  //println("---")



  //println(fib)
  //println("---")



  //println(fib^15)
  //val BigFib = new BigIntMatrix(Array(Array(BigInt(1), BigInt(1)).toIndexedSeq, Array(BigInt(1), BigInt(0)).toIndexedSeq).toIndexedSeq)
  def fastFib(n:Int):BigInt = {
    (BigFib^n-1)(0,0)
  }

  var fastFibCache:Map[Int, NumericMatrix[BigInt]] = Map(1->(BigFib^1))

  def fastFib2(n:Int):BigInt = {
    val maxCached:Int = fastFibCache.keySet.max
    val toCompute:Int = n - maxCached

    if(!fastFibCache.contains(toCompute)) {
      fastFibCache += (toCompute -> (BigFib^toCompute))
    }

    val newMax = fastFibCache(maxCached) * fastFibCache(toCompute)
    fastFibCache += (n -> newMax)
    newMax(0,1)
  }

  def fastFib3(n:Int):BigInt = {
    val maxCached:Int = fastFibCache.keySet.max
    val toCompute:Int = n - maxCached
    val elems = new BitSet1(toCompute.toLong)
    val newMax = elems.map(1<<_).foldLeft(fastFibCache(maxCached))((r, i) =>{
        if(!fastFibCache.contains(i)){
          fastFibCache += (i -> (BigFib^i))
        }
        r * fastFibCache(i)
      })

    fastFibCache += (n -> newMax)
    newMax(0,1)
  }

  class fibIterator extends Iterator[BigInt] {
    var state = (BigInt(1), BigInt(0))

    override def hasNext: Boolean = true

    override def next(): BigInt = {
      val ret = state._2
      state = (state._1 + state._2, state._1)
      ret
    }
  }
  //def fastFib(n:Int):(BigInt, BigInt) = {

  //}

  //println(iter.take(5).mkString(", "))
  val n = math.pow(10, 9).toInt
  //f(n)
  //println(f(100000))
  //println((1 to 5).map(f))
  //println(isAcceptable(BigInt(123456789)))
  //println(isAcceptable(fastFib(541*2749)))

  case class TruncInt(value:Int, trunc:Int) {
    def +(i:Int):TruncInt = TruncInt((value+i)%trunc, trunc)
    def +(i:TruncInt):TruncInt = TruncInt((value+i.value)%trunc, trunc)
  }

  class truncIterator extends Iterator[Int] {
    var state = (TruncInt(1, 1000000000), TruncInt(0, 1000000000))

    override def hasNext: Boolean = true

    override def next(): Int = {
      val ret = state._2
      state = (state._1 + state._2, state._1)
      ret.value
    }
  }

  //println(fastFib2(3))
//System.exit(0)
  val truncIter = new truncIterator

  val value = truncIter.zipWithIndex.withFilter(p =>{
    val str = p._1.toString
    isBackAcceptable(str)
  }).find(p =>{
    val n = p._2
    println(s"n:$n")
    val Fn = fastFib3(n)
    val str = Fn.toString
    val result = isFrontAcceptable(str)
    if(result)
      println(s"$n")
    result
  })
  println(value)
/*
  iter.take(n).zipWithIndex.foreach(p => {
    val str = p._1.toString
    if(isFrontAcceptable(str))
      println(s"${p._2} => f, ${str.take(9)}")
    if(isBackAcceptable(str))
      println(s"${p._2} => b, ${str.takeRight(9)}")
  })*/


  //val opt = (1 to n).par.find(i => isAcceptable(f(i)))
  //println(opt)

}
