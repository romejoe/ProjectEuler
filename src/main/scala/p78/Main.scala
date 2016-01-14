package p78

import common.TimedApp
import common.util.Memonize

import scala.math.BigInt

object Main extends TimedApp {
  /*
Let p(n) represent the number of different ways in which n coins can be separated into piles. For example, five coins can be separated into piles in exactly seven different ways, so p(5)=7.

OOOOO
OOOO   O
OOO   OO
OOO   O   O
OO   OO   O
OO   O   O   O
O   O   O   O   O
Find the least value of n for which p(n) is divisible by one million.
  */
  /**
    * See Partition counting function, p(n)
    */

  val p:Int=> BigInt = Memonize((n:Int) =>{
    //require(n >= 0)
    //println(s"n: $n")
    if(n < 0) 0
    else if(n < 2) 1
    else{
      (1 until n).takeWhile(k => {
        n - ((k * (k + (k << 1) - 1)) >> 1) >= 0
      }).map(k =>{
        val tmp = k + (k << 1)
        val v1: Int = n - ((k * (tmp - 1)) >> 1)
        val v2: Int = n - ((k * (tmp + 1)) >> 1)
        val tmp2 = p(v1) + p(v2)
        if((k&1) == 0){
          tmp2 * -1
        }  else{
          tmp2
        }
      }).sum
    }
  })


  val i = (1 to Int.MaxValue).find(i =>{
    p(i) % 1000000 == 0
  }).get
  println(s"$i")
}
