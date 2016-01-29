package p105

import common.util.Memonize
import common.{RicherInt, TimedApp}

/*
Let S(A) represent the sum of elements in set A of size n. We shall call it a special sum set if for any two non-empty disjoint subsets, B and C, the following properties are true:

S(B) ≠ S(C); that is, sums of subsets cannot be equal.
If B contains more elements than C then S(B) > S(C).
For example, {81, 88, 75, 42, 87, 84, 86, 65} is not a special sum set because 65 + 87 + 88 = 75 + 81 + 84, whereas {157, 150, 164, 119, 79, 159, 161, 139, 158} satisfies both rules for all possible subset pair combinations and S(A) = 1286.

Using sets.txt (right click and "Save Link/Target As..."), a 4K text file with one-hundred sets containing seven to twelve elements (the two examples given above are the first two sets in the file), identify all the special sum sets, A1, A2, ..., Ak, and find the value of S(A1) + S(A2) + ... + S(Ak).

NOTE: This problem is related to Problem 103 and Problem 106.


 */
object Main extends TimedApp {

  val S:(Array[Int], Int) => Int = Memonize((s, i) =>{
      val tmp = i.elems.map(s(_)).sum
    tmp
  })


  def isSCC(s:Array[Int]):Boolean = {
    val m = s.length
    val limit = 1 << m
    (1 until limit)
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

  val stream = getClass.getResourceAsStream("/p105/p105_sets.txt")
  val lines = scala.io.Source.fromInputStream( stream ).getLines
  val rows = lines.map(_.split(",").map(_.toInt)).toSeq

  val sum = rows.filter(isSCC).map(_.sum).sum
  println(s"Sum => $sum")

}
