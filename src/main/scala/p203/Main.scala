package p203.Main

import scala.math.sqrt

object Main extends App {
  class PascalsTriangleRow(arr:IndexedSeq[Long]) extends IndexedSeq[Long]{
    override def length: Int = arr.length
    override def apply(idx: Int): Long = arr(idx)
    override def toString() = mkString(", ")
  }
  class BasePascalsTriangleRow(base:Long) extends IndexedSeq[Long]{
    override def length: Int = 1
    override def apply(idx: Int): Long = base
    override def toString() = mkString(", ")
  }



  def isSquareFree(l:Long):Boolean = {
    l
    false
  }


  object PascalsTriangle{
    var rowMap:Map[Long, IndexedSeq[Long]] = Map()
    def row(i:Long):IndexedSeq[Long]={
      if(!rowMap.contains(i))
        rowMap = rowMap + (i -> computeRow(i))
      rowMap(i)
    }

    def computeRow(i:Long):IndexedSeq[Long] = {
      i match {
        case 0 => new BasePascalsTriangleRow(1)
      //  case 1 => new PascalsTriangleRow(Array(1L, 1L))
        case _ => {
          val parentRow = row(i-1)
          val arr = Array(1L) ++ parentRow.sliding(2).map(_.sum) ++ Array(1L)
          new PascalsTriangleRow(arr)
        }
      }
    }

  }

  object Prime{
    def isPrime(l:Long):Boolean = {
      l match{
        case 1 => true
        case _ if (l & 1L) == 0L => false
        case _ => {
          3L.to(sqrt(l).toLong, 2L).forall(l % _ != 0)
        }
      }
    }
  }

  class PrimeContainer(m:Long) extends IndexedSeq[Long]{
    val backing:Array[Long] = Array(1L, 2L) ++ 3L.to(sqrt(m).toLong, 2L).filter(Prime.isPrime)

    override def length: Int = backing.length

    override def apply(idx: Int): Long = backing(idx)
  }



  val items = (for(i <- 0 to 51) yield PascalsTriangle.row(i)).foldLeft(Set[Long]())(_ ++ _)
  items.max

  items.par.filter(isSquareFree)

}
