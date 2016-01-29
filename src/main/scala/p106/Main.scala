package p106

import common.TimedApp
import common.RicherInt
/*
Let S(A) represent the sum of elements in set A of size n. We shall call it a special sum set if for any two non-empty disjoint subsets, B and C, the following properties are true:

S(B) ≠ S(C); that is, sums of subsets cannot be equal.
If B contains more elements than C then S(B) > S(C).
For this problem we shall assume that a given set contains n strictly increasing elements and it already satisfies the second rule.

Surprisingly, out of the 25 possible subset pairs that can be obtained from a set for which n = 4, only 1 of these pairs need to be tested for equality (first rule). Similarly, when n = 7, only 70 out of the 966 subset pairs need to be tested.

For n = 12, how many of the 261625 subset pairs that can be obtained need to be tested for equality?

NOTE: This problem is related to Problem 103 and Problem 105.


 */
object Main extends TimedApp {

  def doRangesIntersect(a:Seq[Int], b:Seq[Int]):Boolean = {
   val range_a = (a.min, a.max)
   val range_b = (b.min, b.max)
   range_a._1 <= range_b._2 && range_b._1 <= range_a._2
  }

  def ElementwiseGreater(a:Seq[Int], b:Seq[Int]):Boolean = {
    a.zip(b).forall(p => p._1 < p._2) || a.zip(b).forall(p => p._1 > p._2)

  }

  def count(m:Int):Seq[(Int,Int)] = {
    val limit = 1 << m
    (1 until limit)
        .flatMap(c =>{
          (1 until limit)
            .filter(t => (t & c) == 0)
              .filter(t => t.elems.length == c.elems.length)
                .filter(_.elems.length != 1)
              .filter(t => doRangesIntersect(t.elems, c.elems))
              .filterNot(t => ElementwiseGreater(t.elems, c.elems))
              .map(t =>{
                val k = math.min(t,c)
                val v = math.max(t,c)
                k -> v
              })
        }).distinct
  }

  def format(i:Int):String = i.elems.sorted.map(_+1).mkString(",")

  val toTest = count(12)
  //toTest.foreach(p => println(s"[${format(p._1)}] != [${format(p._2)}]"))
  println(toTest.size)



}
