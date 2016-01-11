package p66

import common.numtheory
import common.numtheory.{PellsEquation, Prime}


object Main extends App{
  /*
Consider quadratic Diophantine equations of the form:

x2 – Dy2 = 1

For example, when D=13, the minimal solution in x is 6492 – 13×1802 = 1.

It can be assumed that there are no solutions in positive integers when D is square.

By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, we obtain the following:

32 – 2×22 = 1
22 – 3×12 = 1
92 – 5×42 = 1
52 – 6×22 = 1
82 – 7×32 = 1

Hence, by considering minimal solutions in x for D ≤ 7, the largest x is obtained when D=5.

Find the value of D ≤ 1000 in minimal solutions of x for which the largest value of x is obtained.
  */
  /**
    * See Pell's Equations
    */

  class Rational(val n:Long, val d:Long){
    def +(i:Long):Rational = this + Rational(i,1)
    def -(i:Long):Rational = this - Rational(i,1)
    def *(i:Long):Rational = this * Rational(i,1)
    def /(i:Long):Rational = this / Rational(i,1)

    def +(r:Rational):Rational = Rational(n*r.d + r.n*d,d*r.d)
    def -(r:Rational):Rational = Rational(n*r.d - r.n*d,d*r.d)
    def *(r:Rational):Rational = Rational(n*r.n, d*r.d)
    def /(r:Rational):Rational = Rational(n*r.d, d*r.n)

    def minimize:Rational = Rational.minimize(n,d)
    def flip:Rational = Rational(d,n)

    override def toString:String = s"$n/$d"
  }

  object Rational {
    def apply(n:Long):Rational = new Rational(n,1)
    def apply(n:Long,d:Long):Rational = new Rational(n,d)
    def minimize(n: Long, d: Long): Rational = {
      val i = Prime.gcd(n,d)
      Rational(n/i, d/i)
    }
  }

  /*def continuedFraction(f:(Double)=>Double):Seq[Int] = {

  }*/

  /*def continuedRoot(n:Long,percision:Long):Seq = {
    var base = Rational(0,n)
    var i = 0
    while(i < percision){
      base = Rational(1,1)/(base + n)
      i = i + 1
    }

    base + 1
  }*/

  /*
  @ val a_0 = N.toInt
a_0: Int = 2
@ val r_0 = N - a_0
r_0: Double = 0.2360679774997898
@ val t_2 = 1.0 / r_0
t_2: Double = 4.236067977499788
@ val t_1 = 1.0 / r_0
t_1: Double = 4.236067977499788
@ val a_1 = t_1.toInt
a_1: Int = 4
@ val r_1 = t_1 - a_1
r_1: Double = 0.23606797749978803
@ val t_2 = 1.0 / r_1
t_2: Double = 4.23606797749982
@ val a_2 = t_2.toInt

   */

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

  class continuedRootSequenceIterator(n:Long) extends Iterator[Int] {
    val iter = new continuedFractionSequenceIterator(math.sqrt(n))
    val a_0 = iter.next()
    var a_n = Vector(iter.next())
    var done = false
    while(!done){
      val tmp = iter.next()
      if(tmp == a_n.head)
        done = true
      else
        a_n = a_n.:+(tmp)
    }
    var i = 0

    override def hasNext: Boolean = true

    override def next(): Int = {
      val A = if(i == 0)
        a_0
      else{
        a_n((i-1)%a_n.length )
      }
      i += 1
      A
    }
  }

  def continuedFractionToRational(seq:List[Int]):Rational = {
    val base = seq.head
    seq.tail.foldLeft(Rational(base))((s,a) => s.flip + a)
  }

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


  class PairIterator(withZero:Boolean = true) extends Iterator[(Int,Int)]{
    val queue:scala.collection.mutable.Queue[(Int,Int,Boolean)] = new scala.collection.mutable.Queue[(Int,Int,Boolean)]()
    queue.enqueue(if(withZero) (0,0,true) else (1,1,true))

    override def hasNext: Boolean = true

    override def next(): (Int, Int) ={
      val item = queue.dequeue()
      if(item._3){
        queue.enqueue(item.copy(_1 = item._1 + 1))
      }
      queue.enqueue(item.copy(_2 = item._2 + 1, _3 = false))

      (item._1, item._2)
    }
  }



  def findMinValueOfX(D:Long):Long = {
    val x = new PairIterator(false).find({case (x,y) =>
        x*x == 1+D*y*y
    }).get._1
    println(s"$D => $x")
    x
  }

  def hasYforX2(X2:Long, D:Long):Boolean = {
    //x^2 – Dy^2 = 1
    //1-x^2 = -Dy^2
    //(x^2-1)/D = y^2

    val tmp:Long = X2 - 1

    if(tmp % D == 0){
      val div = tmp / D
      val Y:Long = math.sqrt(div).toLong
      X2-D*(Y*Y) == 1
      //Prime.fastIsSquare(div)
    }
    else{
      false
    }

  }

  def test(X:Int, D:(Long, BigDecimal)):Boolean = {

    val tmp = X * X - 1
    if(tmp % D._1 == 0){
      val tmp2 = BigDecimal(tmp) / D._2
      numtheory.isInteger(tmp2) && Prime.isSquare(tmp2.toLong)
    }else{
      false
    }

  }

  val Ds = (2 to 1000).filterNot(i => Prime.fastIsSquare(i))//.map(d => (d,BigDecimal(d)))

  var toTest = Ds


  var XtoD:Map[Long, Set[Long]] = Map()


  def Work(D:Int):Long = {
    val bound = math.ceil(math.sqrt(D)).toLong
   // val test = math.floor(math.sqrt(D)).toLong

    val d = D.toDouble

    println(s"Evaluating $D")
    //println(s"Bound: $bound")
    (bound.to(Int.MaxValue,1)).find(x =>{
      val tmp = x*x
      //val lbound = math.max(x-bound-1, 1)
      val y:Double = math.sqrt((tmp - 1) / d)
      //!(lbound to x).exists(y =>{
        //println(s"($D,$x,$y)")
      //val lhs = tmp - D * math.floor(y)*math.floor(y)
      //println((x,y, lhs))
      //lhs == 1
      math.floor(y) == y
      //})
    }).get
  }

  def test(X:BigInt,Y:BigInt,D:BigInt):Boolean={

    X*X-D*Y*Y == BigInt(1)
  }


val runTime = common.util.timeIt({
  val answer =
    Ds.map(D =>{
      //println(s"Evaluating $D")
    /*var seq:List[Int] = Nil

    val iter = new continuedRootSequenceIterator(D)

    val n = iter.a_n.size
    if(n % 2 == 1){
      seq = iter.take(n).toList.reverse ++ seq
    }
    else{
      seq = iter.take(2*n+1).toList.reverse ++ seq
    }*/
      val (x,y) = PellsEquation.solvePellsEquation(D)

      //println((x,y))
      //println(test(x, y, D))
      //println()
    /*val root = iter.map(a => {
      seq = a :: seq
      continuedFractionToRational(seq)
    }).find(tmp =>
          test(tmp.n, tmp.d, D)
       ).get.minimize



    //val root = continuedRoot(D, N)
    //val x = (2L to Int.MaxValue).find(X => hasYforX(X,D)).get
    println((D,root, test(root.n, root.d, D)))*/
      //require(test(x, y, D))
    (D,x)
  }).maxBy(_._2)._1
  println(answer)
})
  println(s"That took $runTime ms")



  /*(2L to Int.MaxValue).dropWhile(x => {
    val X = x*x
    val tmp = toTest.par.filterNot(d => hasYforX2(X, d)).toSet
    val amountRemoved = toTest.size - tmp.size
    if(amountRemoved > 0) {
      println(s"$x =>(removed: $amountRemoved, remainingToCheck: ${tmp.size}, removed: ${toTest.diff(tmp)})")
      XtoD += (x -> toTest.diff(tmp).toIterator.toSet)
      toTest = tmp
    }


    toTest.nonEmpty
  })

  val k = XtoD.keySet.max
  println(s"Answer = ${XtoD(k)}")
*/
  /*println(Ds.map(D => {
    val x = Work(D)
    println((D,x))
    //println()
    (D,x)
  }).maxBy(_._2))*/
/*
  println(continuedRoot(2,0))
  println(continuedRoot(2,1))
  println(continuedRoot(2,2))
  println(continuedRoot(2,3))
*/
//  new continuedFractionIterator(math.sqrt(5)).take(5).foreach(println)

  //println(s"Answer = $answer")
  //println(Ds.map(D => (D,findMinValueOfX(D))).maxBy(_._2)._1)


}
