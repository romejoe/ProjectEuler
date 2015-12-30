package p102

object Main extends App{
  case class Point(x:Double, y:Double){
    def -(p:Point):Point = Point(x-p.x, y-p.y)
    def *(p:Point):Double = x*p.x + y*p.y

    def x(p:Point):Double = (x*p.y)-(y*p.x)
  }

  case class Line(a:Point, b:Point){

    def asPointSlope = Point(b.x - a.x, b.y - a.y)

    def doesIntersect(l:Line):Boolean = {
      val (c, d) = l




    }
  }

  object Triangle{
    def apply(arr:Array[Double]):Triangle = Triangle(Point(arr(0), arr(1)),Point(arr(2), arr(3)),Point(arr(4), arr(5)))
  }

  case class Triangle(a:Point, b:Point, c:Point){
    def centroid:Point = Point((a.x + b.x + c.x)/3.0, (a.y + b.y + c.y)/3.0)
    def AB = Line(a,b)
    def AC = Line(a,c)
    def BC = Line(b,c)
  }

  val ORIGIN = Point(0.0, 0.0)

  val stream = getClass.getResourceAsStream("/p102/p102_triangles.txt")
  val lines = scala.io.Source.fromInputStream( stream ).getLines

  val triangles = lines
    .map(_.trim.split(",").map(_.toDouble))
    .map(Triangle(_))

  println(triangles.count(t => {
    val testLine = Line(ORIGIN, t.centroid)
    !(testLine.doesIntersect(t.AB) ||
      testLine.doesIntersect(t.AC) ||
      testLine.doesIntersect(t.BC))
  }))

}
