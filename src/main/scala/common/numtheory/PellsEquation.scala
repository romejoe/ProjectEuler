package common.numtheory

import scala.collection.mutable.ArrayBuffer

object PellsEquation {
  def solvePellsEquation(D: Long): (BigInt, BigInt) = {


    val a_0 = BigInt(math.sqrt(D).toLong)

    var P_n1: BigInt = 0
    var P_n: BigInt = a_0

    var Q_n1: BigInt = 1
    var Q_n: BigInt = D - a_0 * a_0

    val a: ArrayBuffer[BigInt] = ArrayBuffer()
    a += a_0 // 0
    a += ((a_0 + P_n.toLong) / Q_n) // 1


    var p_n: BigInt = a(0) * a(1) + 1
    var p_n1: BigInt = a_0
    var p_n2: BigInt = 0

    var q_n: BigInt = a(1)
    var q_n1: BigInt = 1
    var q_n2: BigInt = 0

    var n = 1

    while (a(n) != a_0 * 2) {
      n += 1
      val tmp = (P_n, Q_n)
      P_n1 = tmp._1
      Q_n1 = tmp._2
      P_n = a(n - 1) * Q_n1 - P_n1
      Q_n = (D - P_n * P_n) / Q_n1 // (n)
      a += ((a_0 + P_n) / Q_n) // (n)

      p_n2 = p_n1
      p_n1 = p_n
      p_n = a(n) * p_n1 + p_n2

      q_n2 = q_n1
      q_n1 = q_n
      q_n = a(n) * q_n1 + q_n2

    }

    val r = n - 1

    require(a(r + 1) == a(0) * 2)
    if (r % 2 == 1) {
      (p_n1, q_n1)
    } else {
      var i = n
      val a_sub = a.tail

      while (i < 2 * r + 1) {
        i += 1

        val a_i = a(i % a_sub.size)

        p_n2 = p_n1
        p_n1 = p_n
        p_n = a_i * p_n1 + p_n2

        q_n2 = q_n1
        q_n1 = q_n
        q_n = a_i * q_n1 + q_n2
      }

      (p_n, q_n)
    }

  }
}
