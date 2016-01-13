package p76

import common.TimedApp
import common.util.Memonize

object Main extends TimedApp {
  /*
It is possible to write five as a sum in exactly six different ways:

4 + 1
3 + 2
3 + 1 + 1
2 + 2 + 1
2 + 1 + 1 + 1
1 + 1 + 1 + 1 + 1

How many different ways can one hundred be written as a sum of at least two positive integers?
  */
   val F:(Long,Long) => Long = Memonize((i:Long, max:Long) => {

     val domain = math.min(i,max).to(1L, -1)
      domain.filter(i-_ >= 0).map(s => {
       if (i - s == 0)
         1
       else {
         F(i - s, s)
       }
     }).sum

   })

  val f = (N:Long) => F(N, N)
  val i = f(100) - 1
  println(s"i => $i")

}
