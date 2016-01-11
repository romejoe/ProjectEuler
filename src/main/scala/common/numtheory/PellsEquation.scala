package common.numtheory

import scala.collection.mutable.ArrayBuffer

object PellsEquation {
  def solvePellsEquation(D:Long):(BigInt,BigInt) = {
    implicit def BDtoD(bd:BigDecimal):Double = bd.toDouble

    val a_0 = math.floor(math.sqrt(D)).toLong

    val P: ArrayBuffer[BigInt] = ArrayBuffer()
    P += 0 // 0
    P += a_0 // 1

    val Q: ArrayBuffer[BigInt] = ArrayBuffer()
    Q += 1 // 0
    Q += D - a_0*a_0 // 1

    val a:ArrayBuffer[BigInt] = ArrayBuffer()
    a += a_0 // 0
    a +=  ((a_0 + P(1).toLong)/Q(1)) // 1

    val p: ArrayBuffer[BigInt] = ArrayBuffer()
    p += a_0 // 0
    p += a(0)*a(1) + 1 // 1

    val q: ArrayBuffer[BigInt] = ArrayBuffer()
    q += 1 // 0
    q += a(1) // 1


    var n = 1

    while(a(n) != a_0*2){
      //while(!test(p(n).toBigInt(), q(n).toBigInt, BigInt(D.toLong))){
      n += 1
      P += a(n-1)*Q(n-1) - P(n-1) // (n)
      Q += (D - P(n)*P(n))/Q(n-1) // (n)
      a += ((a_0 + P(n))/Q(n)) // (n)

      p += a(n)*p(n-1) + p(n-2) // (n)
      q += a(n)*q(n-1) + q(n-2) // (n)
    }
    //(p(n).toBigInt(), q(n).toBigInt())

    val r = n-1

    require(a(r+1) == a(0)*2)
    if(r%2 == 1){
      (p(r), q(r))
    }else{
      var i = n
      val a_sub = a.tail

      while(i < 2*r + 1){
        i += 1
        //P += a(i-1)*Q(i-1) - P(i-1) // (n)
        //Q += (D - P(i)*P(i))/Q(i-1) // (n)
        a += a(i % a_sub.size)//math.floor((a_0 + P(i))/Q(i)) // (n)

        p += a(i)*p(i-1) + p(i-2) // (n)
        q += a(i)*q(i-1) + q(i-2) // (n)
      }
      //val size = a.length - 1

      //val r = 1+((2*n+1)% size)
      (p(2*r+1), q(2*r+1))
    }

  }
}
