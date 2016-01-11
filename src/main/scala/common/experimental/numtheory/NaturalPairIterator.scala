package common.experimental.numtheory

class NaturalPairIterator(withZero:Boolean = true) extends Iterator[(Int,Int)]{
  val queue:scala.collection.mutable.Queue[(Int,Int,Boolean)] = new scala.collection.mutable.Queue[(Int,Int,Boolean)]()
  queue.enqueue(if(withZero) (0,0,true) else (1,1,true))

  override def hasNext: Boolean = true

  override def next(): (Int, Int) ={
    val item = queue.dequeue()
    if(item._3){
      queue.enqueue(item.copy(_1 = item._1 + 1))
    }
    queue.enqueue(item.copy(_2 = item._2 + 1, _3 = false))

    (item._1, item._2)
  }
}