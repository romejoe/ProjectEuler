package common.numtheory

class PrimitivePythagoreanTriple private(thing: (Long, Long, Long)) {
  //|c+b=G     a |
  //| a     c-b=H|

  def a = thing._1
  def b = (G - H) >> 1
  def c = (G + H) >> 1

  val G = thing._2
  val H = thing._3

  def this(a: Long, b: Long, c: Long) = this((a, c + b, c - b))

  def this(seq: IndexedSeq[Long]) = this(seq(0), seq(1), seq(2))

  def T1: PrimitivePythagoreanTriple =
    new PrimitivePythagoreanTriple((a + (H << 1), (a << 2) + G + (H << 2), H))

  def T2: PrimitivePythagoreanTriple =
    new PrimitivePythagoreanTriple((a + (G << 1), (a << 2) + (G << 2) + H, G))

  def T3: PrimitivePythagoreanTriple =
    new PrimitivePythagoreanTriple((-a + (G << 1), -(a << 2) + (G << 2) + H, G))

  override def toString: String = s"($a,$b,$c)"

  def toSeq: IndexedSeq[Long] = Vector(a, b, c)

  def toPair: (Long, Long) = (math.min(a, b), math.max(a, b))

  def sum = a + G

  def canEqual(A: Any): Boolean = A.isInstanceOf[PrimitivePythagoreanTriple]

  def equals(p: PrimitivePythagoreanTriple): Boolean = {
    toSeq.sorted == p.toSeq.sorted
  }

}
