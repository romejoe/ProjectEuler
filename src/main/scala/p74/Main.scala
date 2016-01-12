package p74

import common.TimedApp

import scala.collection.mutable

object Main extends TimedApp {
  /*
The number 145 is well known for the property that the sum of the factorial of its digits is equal to 145:

1! + 4! + 5! = 1 + 24 + 120 = 145

Perhaps less well known is 169, in that it produces the longest chain of numbers that link back to 169; it turns out that there are only three such loops that exist:

169 → 363601 → 1454 → 169
871 → 45361 → 871
872 → 45362 → 872

It is not difficult to prove that EVERY starting number will eventually get stuck in a loop. For example,

69 → 363600 → 1454 → 169 → 363601 (→ 1454)
78 → 45360 → 871 → 45361 (→ 871)
540 → 145 (→ 145)

Starting with 69 produces a chain of five non-repeating terms, but the longest non-repeating chain with a starting number below one million is sixty terms.

How many chains, with a starting number below one million, contain exactly sixty non-repeating terms?
  */
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

  def F(i:Int):Int = i.toDigits.map(_!).sum

  //fill factorial cache
  (1 to 9).map(_!)

  def ComputeChain(i:Int):mutable.Map[Int, Int] = {
    val ret:mutable.Map[Int, Int] = mutable.Map()
    var cur = i
    var idx = 0
    while(!ret.contains(cur)){
      ret += (cur -> idx)
      cur += F(cur)
      idx += 1
    }

    ret
  }

  val chains:mutable.Map[Int,List[Int]] = mutable.Map()
  chains ++= 0.to(9,1).map(i => i -> List(i))
/*
  abstract class ChainTarget{
    def target:ChainTarget
    def value:Int
  }
  case class FutureTarget(i:Int) extends ChainTarget{
    def target = RawTarget(i)

  }
  case class RawTarget(i:Int) extends ChainTarget{
    def value = chains(i)
  }



  def ComputeChain2(i:Int):List[Int] = {
    if(chains.contains(i))
      if(chains(i) == Nil)
        FutureTarget(i)
      else

      chains(i)
    else{
      chains += (i -> Nil)
      val chain = i :: ComputeChain2(F(i))
      chains += (i -> chain)
      chain
    }
  }
*/
  def printChain(chain:mutable.Map[Int,Int]):Unit = {
    println(chain.toArray.sortBy(_._2).map(_._1).mkString(" -> "))
  }

  def printChain2(chain:List[Int]):Unit = {
    println(chain.mkString(" -> "))
  }

  //val chainCount = (0 until 1000000).map(ComputeChain2).count(_.length == 59)
  //println(s"Chaincount => $chainCount")
  val chainBlocks:mutable.Map[Int, Int] = mutable.Map()

  /*(0 until 1000000).foreach(i => {
    chainBlocks.+= (i ->F(i))
  })*/

  def computeChainBlocks():Unit = {
    val queue = mutable.Queue[Int]()
    queue ++= (0 until 1000000)
    while(queue.nonEmpty){
      val cur = queue.dequeue()
      if(!chainBlocks.contains(cur)) {
        val v = F(cur)
        chainBlocks += (cur -> v)
        queue += v
      }
    }
  }

  def computeChainSetFromBlocks(start:Int):mutable.Set[Int] = {
    val ret = mutable.Set[Int]()
    var cur = start
    while(!ret.contains(cur)){
      ret += cur
      cur = chainBlocks(cur)
    }
    ret
  }
  computeChainBlocks()
  val chainCount = (0 until 1000000).par.map(computeChainSetFromBlocks).count(_.size == 60)
  println(s"Chaincount => $chainCount")
}
