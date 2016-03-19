import scala.collection.mutable

package object common {

  object RicherInt{
    val factorialCache:scala.collection.mutable.Map[Int,Int] = mutable.Map()
    val elemCache:scala.collection.mutable.Map[Int, Array[Int]] = mutable.Map()
  }

  implicit class RicherInt(i:Int){
    import RicherInt._

    def **(e:Int):Int = {
      var t = 0
      var ret:Int = 1
      while(t < e){
        ret *= i
        t += 1
      }
      ret
    }

    def elems: Array[Int] = {
      if(!elemCache.contains(i)){
        var j = 0
        var tmp = i
        var ret = List[Int]()

        while(tmp != 0){
          if((tmp & 1) != 0){
            ret =  j :: ret
          }
          tmp >>= 1
          j += 1
        }

        elemCache +=(i -> ret.toArray)
      }
      elemCache(i)
    }

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

    def isEven = (i & 1) == 0
    def isOdd = !isEven
  }


  object RicherLong{

    val elemCache:scala.collection.mutable.Map[Long, Array[Long]] = mutable.Map()
  }
  implicit class RicherLong(l:Long){
    import RicherLong._
    def **(e:Long):Long = {
      var t = 0
      var ret:Long = 1
      while(t < e){
        ret *= l
        t += 1
      }
      ret
    }

    def elems: Array[Long] = {
      if(!elemCache.contains(l)){
        var j = 0
        var tmp = l
        var ret = List[Long]()

        while(tmp != 0){
          if((tmp & 1) != 0){
            ret =  j :: ret
          }
          tmp >>= 1
          j += 1
        }

        elemCache += (l -> ret.toArray)
      }
      elemCache(l)
    }

    def isEven = (l & 1) == 0
    def isOdd = !isEven
  }

  object RicherBigInt{
    val sqrtCache:scala.collection.mutable.Map[BigInt,BigInt] = mutable.Map()
  }
  implicit class RicherBigInt(bi:BigInt){
    //var tmpSqrt:Option[BigInt] = None
    // http://www.codecodex.com/wiki/Calculate_an_integer_square_root
    def sqrt:BigInt = {
     // if(!sqrtCache.contains(bi)) {
      //if(tmpSqrt.isEmpty) {
        def next(n: BigInt, i: BigInt): BigInt = (n + i / n) >> 1

        val one = BigInt(1)

        var n = one
        var n1 = next(n, bi)

        while ((n1 - n).abs > one) {
          n = n1
          n1 = next(n, bi)
        }

        while (n1 * n1 > bi) {
          n1 -= one
        }

      //  sqrtCache.put(bi,n1)
        //tmpSqrt = Some(n1)
      //}
      //sqrtCache(bi)
      //tmpSqrt.get
      n1
    }
  }
}
