package p110

import common.{RicherLong, TimedApp}
import common.util.{Collection, Memonize}
import org.primesieve.PrimeSieve
/*
In the following equation x, y, and n are positive integers.

1
x
+
1
y
=
1
n
It can be verified that when n = 1260 there are 113 distinct solutions and this is the least value of n for which the total number of distinct solutions exceeds one hundred.

What is the least value of n for which the number of distinct solutions exceeds four million?

NOTE: This problem is a much more difficult version of Problem 108 and as it is well beyond the limitations of a brute force approach it requires a clever implementation.

 */
object Main extends TimedApp {

  def countNumberOfSolutions(n:Long):Long = {
    val xRange = n+1 to 2*n
    xRange.count(x => {
      val rem = (x*n)%(x-n)
      if(rem == 0){
        println((x, (x*n)/(x-n)))
        true
      }
      else
        false
    })
  }

  def findExactSolutions(n:Long):Seq[Long] = {
    val xRange = n+1 to 2*n

    xRange.filter(x => {
      val rem = (x*n)%(x-n)
      if(rem == 0){
        //println((x, (x*n)/(x-n)))
        true
      }
      else
        false
    }).toList
  }

  def test(n:Long, k:Long):Boolean = {
    //println(s"Testing $n")
    //val xRange = (n+1 to 2*n).iterator
    var x = n+1
    val end = 2*n
    var found = 0
    var remToCheck = 2*n - (n+1)
    //xRange.takeWhile(i => )
    while(x < end && found < k && found + remToCheck >= k){
      //val x = xRange.next()
      if((x*n)%(x-n) == 0){
        found += 1
      }
      remToCheck -= 1
      x += 1
    }
    //println(s"$n\t$found")
    found >= k
    //xRange.count(x => (x*n)%(x-n) == 0)
  }

  def findAtleastSolutions(k:Long):Option[Long] = {
    val start = k-1
    //(start to start + 100000).find(i => countNumberOfSolutions(i) >= k)
    var j = 0
    (start to start + 100000).find(i => {
      if(j > 100){
        println(i)
        j = 0
      }
      else
        j += 1
      test(i, k)
    })
  }

  object Factorize{
    var primes = Array[Long]()
    var max = 0L
    def apply(l:Long):Map[Long,Long] = {
      if(l == 1)
        Map()
      else {
        val tmp = math.sqrt(l)
        if (tmp > max) {
          primes ++= PrimeSieve.SINGLETON.generatePrimes(max, math.ceil(tmp).toLong)
          max = math.ceil(tmp).toLong
        }
        val p = primes.find(l % _ == 0)
        if (p.isDefined) {
          val t = p.get
          val m = Factorize(l / t)
          m.updated(t, m.getOrElse(t, 0L) + 1L)
        } else {
          Map(l -> 1)
        }
      }
//      primes.filter(l%_==0)
    }
  }

  def sortTuple(p:(Long,Long)) = if(p._2 < p._1) p.swap else p

  val findSolutions:Long =>Seq[(Long,Long)] = Memonize(
    (n:Long) =>{
      val factors = Factorize(n)
      val factor_list = factors.flatMap(p => Collection.constSeq(p._2,p._1)).toArray
      /*val tmp = factors.keys.map(p =>(p**factors(p))*((n/p)+p)).filter(_ != n)
      //println(tmp)

      val tmp2 = Seq((2*n,2*n), (n+1, n * n + n)) ++ factors.keys.flatMap(i => findSolutions(n/i).map(t =>(t._1*i, t._2*i)))// ++ tmp.filter(p => ((p*n)%(p-n)) == 0)
      val ret = tmp2 ++  tmp.filter(p => ((p*n)%(p-n)) == 0).map(i => (i, (i*n)/(i-n)))
      val tret = ret.map(sortTuple).distinct
      //(tret ++ tret.flatMap(p => (factors.keys ++ factors.keys.map(_ * -1)).map(p._1 + _).filter(_ > n).filter(p => ((p*n)%(p-n)) == 0).map(i => (i, (i*n)/(i-n))))).map(sortTuple).distinct
      tmp2.map(sortTuple).distinct*/
      val ubound = 2L ** factor_list.length
      (1L until ubound)
        .flatMap(d => (1L until ubound).filter(t => (t & d) == 0)
          .flatMap(n => {
            val tmp = n.elems.map(l => factor_list(l.toInt)).toSet//foldLeft(1L)((i,j) => i*factor_list(j))

            (1L until ubound)
              .filter(t => (t & (d | n)) == 0)
              .filter(t => d + n + t == ubound - 1)
              .filter(t =>  t.elems.map(l => factor_list(l.toInt)).toSet.intersect(tmp).isEmpty)
              .map(m => (
                d.elems.map(l => factor_list(l.toInt)).foldLeft(1L)(_*_),
                tmp.foldLeft(1L)(_*_),
                m.elems.map(l => factor_list(l.toInt)).foldLeft(1L)(_*_))
              )
          }
          )
        ).map({case (k,nt,m) => (k*m*(nt+m), k*nt*(nt+m))}).map(sortTuple).distinct.toSeq

    }
  )
  /*println(Factorize(7))
  println(Factorize(8))
  println(Factorize(9))
  println(countNumberOfSolutions(10))
  println(countNumberOfSolutions(6))
  println(countNumberOfSolutions(12))
*/
  (2 to 300).foreach(i =>{
    println(s"$i =>")
    val control = findExactSolutions(i)
    println(findExactSolutions(i))
    val testy = findSolutions(i)
    //println(findSolutions(i).map(_._1).sorted)
    println(testy)
    println(testy.map(_._1).sorted)
    println(s"$i => ${control == testy.map(_._1).sorted}")
    //println(s"phi =>${Prime.EulerTotient(i)}")
    //println(i-Prime.EulerTotient(i))
    println()
  })

  /*println("6 =>")
  println(findSolutions(6))
  println("10 =>")
  println(findSolutions(10))
  println("12 =>")
  println(findSolutions(12))
  *///println(findAtleastSolutions(1000))

  //val tmp = new Prime(30)
  /*(2 to 30).foreach(i => {
    val y = countNumberOfSolutions(i)
    println(s"$i => $y,\t ${Rational(y,i)}, \t ${tmp.primes.contains(i)}")
    //println()
  })*/

  //val tmp = (1 to 30)//PrimeSieve.SINGLETON.generatePrimes(1, 20)
  //println("Have Primes")
  //println(tmp.primes.toArray.dropWhile(i => i < 1000).par.find(i => test(i-1,1000) || test(i+1,1000)))
  //tmp//.toArray.dropWhile(i => i < 4000000L).par
    //.find(i => test(i-1,4000000L) || test(i+1,4000000L)))
    //.find(i => test(i-1,1000) || test(i+1,1000)))
    //.foreach(i => {
    //println(s"${i-1} => ${countNumberOfSolutions(i-1)}")
    //println(s"${i} => ${countNumberOfSolutions(i)}")
  //})
  //println(tmp//.toArray.dropWhile(i => i < 4000000L).par
  //.find(i => test(i-1,4000000L) || test(i+1,4000000L)))
  //.find(i => test(i-1,1000) || test(i+1,1000)))
}
