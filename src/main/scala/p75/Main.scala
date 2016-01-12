package p75

import scala.collection.mutable

object Main extends App {
  /*
It turns out that 12 cm is the smallest length of wire that can be bent to form an integer sided right angle triangle in exactly one way, but there are many more examples.

12 cm: (3,4,5)
24 cm: (6,8,10)
30 cm: (5,12,13)
36 cm: (9,12,15)
40 cm: (8,15,17)
48 cm: (12,16,20)

In contrast, some lengths of wire, like 20 cm, cannot be bent to form an integer sided right angle triangle, and other lengths allow more than one solution to be found; for example, using 120 cm it is possible to form exactly three different integer sided right angle triangles.

120 cm: (30,40,50), (20,48,52), (24,45,51)

Given that L is the length of the wire, for how many values of L ≤ 1,500,000 can exactly one integer sided right angle triangle be formed?
  */


  class PythagoreanTriple private(thing: (Long, Long, Long)) {
    //|c+b=G     a |
    //| a     c-b=H|

    def a = thing._1
    def b = (G - H) >> 1
    def c = (G + H) >> 1

    val G = thing._2
    val H = thing._3

    def this(a: Long, b: Long, c: Long) = this((a, c + b, c - b))

    def this(seq: IndexedSeq[Long]) = this(seq(0), seq(1), seq(2))

    def T1: PythagoreanTriple =
      new PythagoreanTriple((a + (H << 1), (a << 2) + G + (H << 2), H))

    def T2: PythagoreanTriple =
      new PythagoreanTriple((a + (G << 1), (a << 2) + (G << 2) + H, G))

    def T3: PythagoreanTriple =
      new PythagoreanTriple((-a + (G << 1), -(a << 2) + (G << 2) + H, G))

    override def toString: String = s"($a,$b,$c)"

    def toSeq: IndexedSeq[Long] = Vector(a, b, c)

    def toPair: (Long, Long) = (math.min(a, b), math.max(a, b))

    def sum = a + G

    def canEqual(A: Any): Boolean = A.isInstanceOf[PythagoreanTriple]

    def equals(p: PythagoreanTriple): Boolean = {
      toSeq.sorted == p.toSeq.sorted
    }

  }

  val runTime = common.util.timeIt({
    val base = new PythagoreanTriple(3, 4, 5)
    val toVisit = mutable.Queue(base)
    val counts: mutable.Map[Long, Long] = mutable.Map()

    while (toVisit.nonEmpty) {
      val cur = toVisit.dequeue()
      val sum = cur.sum

      //get multiples
      counts ++= sum.to(1500000, sum).map(s => s -> (1L + counts.getOrElse(s, 0L)))

      val seq = Seq(cur.T1, cur.T2, cur.T3)
      toVisit.enqueue(seq.filter(p => p.sum < 1500000): _*)
    }
    println(counts.count(_._2 == 1))
  })
  println(s"That took $runTime ms")
}
