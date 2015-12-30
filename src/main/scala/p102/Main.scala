package p102
import math._

object Main extends App{
  case class Point(x:Double, y:Double)

  case class Line(a:Point, b:Point){

    val M = (b.y-a.y)/(b.x-a.x)
    val B = a.y - a.x*M

    def isDefinedAt(x:Double):Boolean = x <= max(a.x, b.x) && x >= min(a.x,b.x)

    val asFn = (x:Double) => {
      M*x + B
    }
  }

  object Triangle{
    def apply(arr:Array[Double]):Triangle = Triangle(Point(arr(0), arr(1)),Point(arr(2), arr(3)),Point(arr(4), arr(5)))
  }

  def isBetween(a:Double, b:Double, y:Double):Boolean = {
    y <= max(a,b) && y >= min(a,b)
  }

  case class Triangle(a:Point, b:Point, c:Point){
    def centroid:Point = Point((a.x + b.x + c.x)/3.0, (a.y + b.y + c.y)/3.0)
    def AB = Line(a,b)
    def AC = Line(a,c)
    def BC = Line(b,c)

    def containsPoint(p:Point):Boolean = {
      val (x,y) = (p.x,p.y)
      Seq(AB,AC,BC)
          .filter(_.isDefinedAt(x))
        .combinations(2)
            .map(_.map(_.asFn(x)))
          .exists(seq =>{
            isBetween(seq.head, seq(1),y)
          })
    }

  }

  val ORIGIN = Point(0.0, 0.0)

  val stream = getClass.getResourceAsStream("/p102/p102_triangles.txt")
  val lines = scala.io.Source.fromInputStream( stream ).getLines

  val triangles = lines
    .map(_.trim.split(",").map(_.toDouble))
    .map(Triangle(_))

  println(triangles.count(_.containsPoint(ORIGIN)))
}
