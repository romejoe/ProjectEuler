package common.math

class RootIterator(N:Int) extends Iterator[Int]{
  val raw_str = N.toString
  val base_str = if(raw_str.length % 2 == 1) "0" + raw_str else raw_str

  val base_iter = base_str.sliding(2,2)

  var c = BigInt(0)
  var p = BigInt(0)

  val digits = (1 to 10).toList

  override def hasNext: Boolean = true

  override def next(): Int = {
     c = 100 * c + (if(base_iter.hasNext) base_iter.next.toInt else 0)

    /*var (lbound, ubound, mid) = (0,9,5)
    var done = false
    while(lbound <= ubound && !done){
      var x = mid
      var y = x*(20*p + x)
      if(y == c){
        done = true
      }
      else if (y = )
    }*/
    val tmp = digits.find(x => (x*(20*p + x)) > c)
    val x = tmp.get - 1
    val y = x * (20 * p + x)
    p = 10 * p + x
    c -= y
    x
  }
}
