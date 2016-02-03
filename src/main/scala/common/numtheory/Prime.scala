package common.numtheory

import common.util.Memonize

import scala.collection.mutable
import common.RicherBigInt

class Prime(N:Int) {
  val primes = {

    val foundPrimes = mutable.BitSet()

    val is_prime:mutable.BitSet = mutable.BitSet(N) ++ (2 to N)

    def findNextN(i:Int):Int = {
      var tmp = i+1
      while(tmp < N && !is_prime.contains(tmp)){
        tmp += 1
      }
      tmp
    }

    var p = findNextN(1)
    while(p < N){

      var i = p
      while(i < N){
        is_prime -= i
        i += p
      }
      foundPrimes += p
      p = findNextN(p)
    }

    foundPrimes
  }

  /**
    * Taken from https://gist.github.com/cslarsen/1635288
    */
  val EulerTotient:Int=>Int = Memonize((N:Int) => {

    if(primes.contains(N))
      N-1
    else {
      if ((N & 1) == 0) {
        val m = N >> 1
        if ((m & 1) == 1)
          EulerTotient(m)
        else
          EulerTotient(m) << 1
      }
      else {
        //(primes.filter(p => p < N && (N%p == 0)).map(1 - 1.0 / _).product*N).toInt

        val m = primes.find(p => N % p == 0).get

        val o = N / m
        val d = Prime.binaryGcd(m, o).toInt

        if (d == 1)
          EulerTotient(m) * EulerTotient(o)
        else
          EulerTotient(m) * EulerTotient(o) * d / EulerTotient(d)
      }
    }

  })

}

object Prime {
  val lessThan:Long => List[Long] = Memonize((i: Long) => {
    i match {

      case _ if i <= 1000 => primes.toSeq.sorted.toList.takeWhile(_ < i)
      case _ =>
        if (isPrime(i - 1))
          (i - 1) :: lessThan(i - 1)
        else
          lessThan(i - 1)
    }
  })

  def lessThanEqual(i: Long): List[Long] = {
    i match {
      case 1 => List()
      case _ if i <= 1000 => primes.toSeq.sorted.toList.takeWhile(_ <= i)
      case _ =>
        if (isPrime(i))
          i :: lessThanEqual(i - 1)
        else
          lessThan(i)
    }
  }

  var primes: Set[Long] = Set(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607, 613, 617, 619, 631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719, 727, 733, 739, 743, 751, 757, 761, 769, 773, 787, 797, 809, 811, 821, 823, 827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997)
  var notPrimes: Set[Long] = Set()


  def isPrime(i: Long): Boolean = {
    require(i > 0)
    if (i == 1)
      true
    else if (primes.contains(i) || notPrimes.contains(i)) {
      primes.contains(i)
    } else {
      val sqrt = math.sqrt(i).toLong
      if (primes.max < sqrt) {
        (primes.max to sqrt).filterNot(notPrimes.contains).foreach(isPrime)
      }

      val toTest = primes.filter(_ <= sqrt)
      val result = toTest.forall(i % _ != 0)
      if (result) {
        primes = primes + i
      } else {
        notPrimes = notPrimes + i
      }
      result
    }
  }

  def isSquare(l: Long): Boolean = naiveIsSquare(l)

  def naiveIsSquare(l: Long): Boolean = {
    val tmp: Long = Math.sqrt(l).toLong
    tmp * tmp == l
  }

  def naiveIsSquare(l: BigInt): Boolean = {
    val tmp: BigInt = l.sqrt
    tmp * tmp == l
  }

  @Experimental
  def digitalRoot(l: Long): Int = {
    if (l < 10)
      l.toInt
    else {
      digitalRoot(l.toString.toList.map(_.toString.toInt).sum)
    }
  }
  @Experimental
  def digitalRoot(l: BigInt): Int = {
    if (l < 10)
      l.toInt
    else {
      digitalRoot(l.toString.toList.map(_.toString.toInt).sum)
    }
  }

  @Experimental
  def fastIsSquare(l: Long): Boolean =
    l & 11 match {
      case 2 | 3 | 7 | 8 => false
      case _ => {
        digitalRoot(l) match {
          case 0 | 1 | 4 | 7 | 9 => {
            naiveIsSquare(l)
          }
          case _ => false
        }
      }
    }

  @Experimental
  def fastIsSquare(bi: BigInt): Boolean =
    (bi & 11).toInt match {
      case 2 | 3 | 7 | 8 => false
      case _ => {
        digitalRoot(bi) match {
          case 0 | 1 | 4 | 7 | 9 => {
            naiveIsSquare(bi)
          }
          case _ => false
        }
      }
    }

  def gcd(a: Long, b: Long): Long = euclideanGCD(a, b)

  def euclideanGCD(a: Long, b: Long): Long = {
    val r_0 = math.max(a, b)
    val r_1 = math.min(a, b)
    val r_2 = r_0 % r_1
    if (r_2 == 0)
      r_0 / (r_0 / r_1)
    else
      euclideanGCD(r_1, r_2)
  }

  /**
    * Taken from: https://en.wikipedia.org/wiki/Binary_GCD_algorithm
    */
  def binaryGcd(a:Long, b:Long): Long = {
    require(a>=0 && b >= 0)
    val ret = if(a == 0)
      b
    else if(b == 0)
      a
    else {
      var (u, v) = (a, b)
      var shift = 0
      while (((u | v) & 1) == 0) {
        u >>= 1
        v >>= 1
        shift += 1
      }

      while ((u & 1) == 0) {
        u >>= 1
      }
      do {
        while ((v & 1) == 0)
          v >>= 1
        if (u > v) {
          val t = v
          v = u
          u = t
        }
        v -= u
      } while (v != 0)


      u << shift
    }

    ret
  }

  val EulerTotient: Int => Int = Memonize((N:Int) => {

    if(isPrime(N))
      N
    else{
      if((N & 1) == 0) {
        EulerTotient(N >> 1) << 1
      }
      else {
        val factors = Factorize.naiveFactor(N).keySet

        //factors.map(p => EulerTotient(math.pow(p._1, p._2).toInt)).product
        (N * factors.map(1 - 1.0 / _).product).toInt
      }
    }

  })

}

