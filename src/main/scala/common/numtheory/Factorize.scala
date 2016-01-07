package common.numtheory

import Prime.isPrime
object Factorize {
  def naiveFactor(i:Long):Map[Long, Long] = {
    i match{
      case 0 => Map()
      case 1 => Map(1L->1L)
      case _ =>
        var divisor:Option[Long] = None
        if(i % 2 == 1) {
          var j = 3L
          while (j < i) {
            if (isPrime(j) && i % j == 0) {
              divisor = Some(j)
              j = i
            }
            j += 2
          }
        }
        else{
          divisor = Some(2L)
        }

        if(divisor.isDefined) {
          val div = divisor.get
          var e = 1
          var tmp = div
          while(i % (tmp*div) == 0){
            e += 1
            tmp  *= div
          }

          val factors = naiveFactor(i/tmp)
          factors + (div -> e)
        }
        else
          Map(i->1)
    }
  }

}
