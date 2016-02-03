package common.util

object Collection {
  def constSeq(n:Int, v:Int):Seq[Int] = {
    (0 until n).foldLeft(Seq[Int]())((s,i) => {s :+ v})
  }

  def constSeq(n:Long, v:Long):Seq[Long] = {
    (0L until n).foldLeft(Seq[Long]())((s,i) => {s :+ v})
  }
}
