package p103

import common.util.Memonize
import common.{RicherInt, TimedApp}

/*
Let S(A) represent the sum of elements in set A of size n. We shall call it a special sum set if for any two non-empty disjoint subsets, B and C, the following properties are true:

S(B) ≠ S(C); that is, sums of subsets cannot be equal.
If B contains more elements than C then S(B) > S(C).
If S(A) is minimised for a given n, we shall call it an optimum special sum set. The first five optimum special sum sets are given below.

n = 1: {1}
n = 2: {1, 2}
n = 3: {2, 3, 4}
n = 4: {3, 5, 6, 7}
n = 5: {6, 9, 11, 12, 13}

It seems that for a given optimum set, A = {a1, a2, ... , an}, the next optimum set is of the form B = {b, a1+b, a2+b, ... ,an+b}, where b is the "middle" element on the previous row.

By applying this "rule" we would expect the optimum set for n = 6 to be A = {11, 17, 20, 22, 23, 24}, with S(A) = 117. However, this is not the optimum set, as we have merely applied an algorithm to provide a near optimum set. The optimum set for n = 6 is A = {11, 18, 19, 20, 22, 25}, with S(A) = 115 and corresponding set string: 111819202225.

Given that A is an optimum special sum set for n = 7, find its set string.

NOTE: This problem is related to Problem 105 and Problem 106.
 */

object Main extends TimedApp {



  val S:(Array[Int], Int) => Int = Memonize((s, i) =>{
      val tmp = i.elems.map(s(_)).sum
    tmp
  })

  def isSCC(s:Array[Int], lbound:Int = 1):Boolean = {
    val m = s.length
    val limit = 1 << m
    (lbound until limit)
        .forall(c =>{
          val S_c = S(s,c)
          val len_c = c.elems.length
          (1 until limit)
            .filter(t => (t & c) == 0)
            .forall(t => {
              val S_t = S(s, t)
              val len_t = t.elems.length
              val ret = S_c != S_t
              if(ret) {
                if (len_t > len_c) {
                  S_t > S_c
                }
                else if (len_t < len_c) {
                  S_t < S_c
                }
                else {
                  true
                }
              }
              else false
            })
        })

  }

  def buildOptimumSS(base:Seq[Int], remainder:Int, remainingSize:Int, partLbound:Int):Option[Array[Int]] = {
    if(isSCC(base.toArray, base.length)){
      if(remainingSize == 0) {
        if(remainder == 0)
          Some(base.toArray)
        else
          None
      }
      else {
        val tmp = (partLbound until remainingSize).view.map(i => {
          buildOptimumSS(base :+ i, remainder - i, remainingSize - 1, i+1)
        }).find(_.isDefined)

        if(tmp.isDefined)
          tmp.get
        else
          None
      }
    }
    else {
      None
    }
  }

  def findOptimumSS(n:Int):Seq[Int] = {
    n match {
      case 1 => Seq(1)
      case 2 => Seq(1, 2)
      case 3 => Seq(2, 3, 4)
      case 4 => Seq(3, 5, 6, 7)
      case 5 => Seq(6, 9, 11, 12, 13)
      case 6 => Seq(11, 18, 19, 20, 22, 25)
      case _ => {
        var optimal = {
          val tmp = findOptimumSS(n-1)
          val seed = tmp.drop(tmp.size /2).head
          seed +: tmp.map(_ + seed)
        }
        val ubound = optimal.sum - 1
        val lbound = findOptimumSS(n-1).sum
        val base = Seq(optimal.head)
        for( i <- ubound.to(lbound, -1)){
          val tmp = buildOptimumSS(base, i-base.head,n-1, base.head+1)
          if(tmp.isDefined)
            optimal = tmp.get
          println(s"Optimal ==> ${optimal.mkString("")}")
        }
        optimal
      }
    }
  }
  (1 to 6).foreach(i => println(findOptimumSS(i).sum))

  val opt = findOptimumSS(7)
  println(s"Optimum => $opt")

}
