package p201

import scala.collection.immutable.TreeSet

object Main extends App{

  class RangedView[T](seq:IndexedSeq[T], r:Range) extends IndexedSeq[T]{
    override def length: Int = math.max(seq.length - r.size, 0)

    override def apply(idx: Int): T = if(idx < r.start){
      seq(idx)
    }
    else{
      seq(r.end + (idx - r.start))
    }
  }

  implicit class RichIndexedSeq[T](seq:IndexedSeq[T]){
    def withOutRange(r:Range):IndexedSeq[T] = new RangedView(seq,r)
  }

  def IsSummable(S:IndexedSeq[Int], Amount:Int, remainingSums:Int):Boolean = {
    (S, Amount, remainingSums) match {
      case (_, _, _) if Amount == 0 && remainingSums == 0 => true
      case (_, _, _) if Amount < 0 || (Amount != 0 && remainingSums == 0) => false
      case _ =>
        //S.exists(i => IsSummable(S.withOutRange(,S.length), Amount - i, remainingSums - 1))
        false//S.exists(i => IsSummable(S.))

    }
  }

  def U(S:Set[Int], N:Int):Set[Int] = {
    (S,N) match{
      case (_,1) => S
    }
  }

  val decreasingOrdering = new Ordering[Int] {
    override def compare(x: Int, y: Int): Int = x.compareTo(y) * -1
  }

  val t = TreeSet.empty[Int](decreasingOrdering)

  val S_seq = (1 to 100).map(i=> i*i).toArray
  val S = t ++ S_seq.toArray
  val S_h = S_seq.drop(50)



  val ubound = S_h.sum
  val lbound = S_seq.take(50).sum

  println(s"lbound:$lbound")
  println(s"ubound:$ubound")
  println(S_seq.mkString("\n"))

  val valid = (lbound to lbound + 5).filter(IsSummable(S_seq.view.reverse, _, 50))
  println(s"Set Size: ${valid.size}")
  println(s"Set Sum: ${valid.sum}")
}
