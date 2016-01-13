package p78

import common.TimedApp
import common.util.Memonize

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

  implicit class RichString(str:String){
    def repeat(i:Int):String = {
      val builder:StringBuilder= new StringBuilder
      (0 until i).foreach(t => builder.append(str))
      builder.toString()
    }
  }

  val p:Int=> Set[Map[Int, Int]] = Memonize((n:Int) =>{
    if(n == 1) {
      Set(Map(1->1))
    }else{
      val subs = p(n-1)

      val tmp = subs.flatMap(m =>{
        m.keySet.map(k =>{
          val newValues = (m(k) - 1, m.getOrElse(k+1,0)+1)
          val ret = m.updated(k+1,newValues._2)
          if(newValues._1 == 0)
            ret - k
          else
            ret.updated(k, newValues._1)
        })
      })

      Set(Map(1 -> n)) ++ tmp
    }
  })
  def mapToString(m:Map[Int,Int]):String = {
    m.keySet.withFilter(m(_) > 0).map(s=>{
      val pile:String = "0".repeat(s)
      val r = 0 until m(s)
        r.map(t =>{
          pile
        }).mkString(" ")
    }).mkString(" ")
  }

 /* println(p(1).mkString("\n"))
  println()
  println(p(2).mkString("\n"))
  println()
  println(p(5).map(mapToString).mkString("\n"))*/
  val i = (2 to Int.MaxValue).find(i =>{
    //println(s"$i => ${p(i).size}")
   println(s"$i, ${p(i).size}, ${p(i).size - p(i-1).size}")
    p(i).size % 1000000 == 0
  }).get
  println(s"$i")
}
