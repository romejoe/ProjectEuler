package common.experimental.numtheory.continuedfraction

class continuedRootSequenceIterator(n:Long) extends Iterator[Int] {
  val iter = new continuedFractionSequenceIterator(math.sqrt(n))
  val a_0 = iter.next()
  var a_n = Vector(iter.next())
  var done = false
  while(!done){
    val tmp = iter.next()
    if(tmp == a_n.head)
      done = true
    else
      a_n = a_n.:+(tmp)
  }
  var i = 0

  override def hasNext: Boolean = true

  override def next(): Int = {
    val A = if(i == 0)
      a_0
    else{
      a_n((i-1)%a_n.length )
    }
    i += 1
    A
  }
}