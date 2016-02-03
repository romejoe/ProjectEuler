package p107

import common.TimedApp

/*
The following undirected network consists of seven vertices and twelve edges with a total weight of 243.


The same network can be represented by the matrix below.

    	A	B	C	D	E	F	G
A	-	16	12	21	-	-	-
B	16	-	-	17	20	-	-
C	12	-	-	28	-	31	-
D	21	17	28	-	18	19	23
E	-	20	-	18	-	-	11
F	-	-	31	19	-	-	27
G	-	-	-	23	11	27	-
However, it is possible to optimise the network by removing some edges and still ensure that all points on the network remain connected. The network which achieves the maximum saving is shown below. It has a weight of 93, representing a saving of 243 − 93 = 150 from the original network.


Using network.txt (right click and 'Save Link/Target As...'), a 6K text file containing a network with forty vertices, and given in matrix form, find the maximum saving which can be achieved by removing redundant edges whilst ensuring that the network remains connected.

 */
object Main extends TimedApp {
  val stream = getClass.getResourceAsStream("/p107/p107_network.txt")
  val lines = scala.io.Source.fromInputStream( stream ).getLines
  val edges = lines.zipWithIndex.flatMap({case (line, i) => {
    line.split(",").zipWithIndex.filter(_._1 != "-").map(p => {
      val a = math.min(i,p._2)
      val b = math.max(i,p._2)
      (a, b, p._1.toInt)
    })
  }}).toArray.distinct

  val n = edges.flatMap(p => Array(p._1, p._2)).max

  val OriginalWeight = edges.map(_._3).sum

  case class State(Sets:Set[Set[Int]], tree:List[(Int,Int)], weight:Int)
  val tmpSets = (0 to n).foldLeft(Set[Set[Int]]())((s,i) => s + Set(i))

  val result = edges.sortBy(_._3).toIterator.scanLeft(State(tmpSets, Nil, 0))({ case (s,p) =>
    val (a,b) = (p._1, p._2)
    val s1 = s.Sets.find(_.contains(a)).get
    val s2 = s.Sets.find(_.contains(b)).get
    if(s1 != s2)
      State(s.Sets - s1 - s2 + (s1++s2), (a,b) :: s.tree, s.weight + p._3)
    else
      s

  }).find(_.Sets.size == 1)

  println(s"Original Weight => $OriginalWeight")
  println(s"Weight => ${result.get.weight}")
  println(s"Savings => ${OriginalWeight - result.get.weight}")

}
