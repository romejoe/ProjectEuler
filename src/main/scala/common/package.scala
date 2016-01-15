import scala.collection.mutable

package object common {

  object RicherInt{
    val factorialCache:scala.collection.mutable.Map[Int,Int] = mutable.Map()
  }

  implicit class RicherInt(i:Int){
    import RicherInt._
    def ! :Int = {
      if (!factorialCache.contains(i))
        factorialCache += (i -> (i match {
          case 0 | 1 => 1
          case _ => i * ((i - 1)!)
        }))
      factorialCache(i)
    }

    def toDigits:Seq[Int] = toDigits(10)
    def toHexDigits:Seq[Int] = {
      var ret:Seq[Int] = Seq()
      var j = i
      while (j > 0){
        ret = ret.+:(j&0xF)
        j = j >> 4
      }
      ret
    }

    def toDigits(radix:Int):Seq[Int] = {
      var ret:Seq[Int] = Seq()
      var j = i
      while (j > 0){
        ret = ret.+:(j%radix)
        j = j / radix
      }
      ret
    }

  }
}
