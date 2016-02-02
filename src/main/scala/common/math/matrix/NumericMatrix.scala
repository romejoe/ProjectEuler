package common.math.matrix

abstract class NumericMatrix[@specialized T:Numeric, S <: NumericMatrix[T,S]] extends Matrix[T, S] { self: S =>

  private implicit class Tp(t:T){
    def +(s:T):T = implicitly[Numeric[T]] plus(t,s)
    def -(s:T):T = implicitly[Numeric[T]] minus(t,s)
    def *(s:T):T = implicitly[Numeric[T]] times(t,s)
    def toDouble:Double = implicitly[Numeric[T]] toDouble(t)

  }

  private def +(x:T, y:T) = implicitly[Numeric[T]] plus(x,y)
  private def plus(x:T, y:T) = implicitly[Numeric[T]] plus(x,y)
  private def -(x:T, y:T) = implicitly[Numeric[T]] minus(x,y)
  private def minus(x:T, y:T) = implicitly[Numeric[T]] minus(x,y)
  private def *(x:T, y:T) = implicitly[Numeric[T]] times(x,y)
  private def times(x:T, y:T) = implicitly[Numeric[T]] times(x,y)

  def +(m: S): S = {
    require(dimensions == m.dimensions)
    val back: IndexedSeq[IndexedSeq[T]] = backing.zip(m.backing).map(p => p._1.zip(p._2).map(p2 => plus(p2._1, p2._2)))
    cloneWith(back)
  }

  def -(m: S): S = {
    require(dimensions == m.dimensions)
    val back: IndexedSeq[IndexedSeq[T]] = backing.zip(m.backing).map(p => p._1.zip(p._2).map(p2 => minus(p2._1, p2._2)))
    cloneWith(back)
  }

  def *(m: S): S = {
    val result: IndexedSeq[IndexedSeq[T]] =
      (for (row <- rows) yield {
        val tmp = for (col <- m.cols) yield {
          val tmp2 = row.zip(col).map(p => times(p._1, p._2)).reduce(plus)
          tmp2
        }

        tmp.toIndexedSeq
      }
        ).toIndexedSeq

    val ret = cloneWith(result)
    ret
  }

  def ^(i: Int): S = {
    if (i == 1)
      this
    else if (i == 2)
      this * this
    else if (i % 2 == 1)
      this * (this ^ 2) ^ ((i - 1) / 2)
    else
      (this ^ 2) ^ (i / 2)
  }

  def det: T = {
    require(dimensions._1 == dimensions._2)
    dimensions match {
      case (2,2) => plus(times(this(0,0),this(1,1)), times(this(0,1), this(1,0)))
      case (3,3) => {
        val
        (a,b,c,
         d,e,f,
         g,h,i)
        = (
          this(0,0), this(0,1), this(0,2),
          this(1,0), this(1,1), this(1,2),
          this(2,0), this(2,1), this(2,2)
          )

        (a*e*i) + (b*f*g) + (c*d*h) - (c*e*g) - (a*f*h) - (b*d*i)
      }
      case _ => {
        val tmp = removeRow(0)

        row(0).zipWithIndex.map(p => {
          (tmp.removeCol(p._2).det * p._1, p._2)
        }).reduceLeft((a,b) => {
          (
            if((b._2 & 1) == 0)
            a._1 + b._1
          else
            a._1 - b._1
            , 0)
        })._1
      }
    }
  }



  /*def inv:VectorMatrix[Double] = {
    require(det != 0)

    val Identity = new VectorMatrix((0 until dimensions._1).map(i => {
      ((0 until i).toIndexedSeq.map(t => 0.0) :+ 1.0) ++ ((i + 1) until dimensions._1).map(t => 0.0)
    }):_*)
    //println(Identity)

    var mat = new VectorMatrix(backing.map(r => r.map(c => c.toDouble)):_*)
    mat = mat.appendColumns(Identity)


    val (m,n) = mat.dimensions
    for(k <- 0 until math.min(m,n)){
      //val i_max = (k until m).map(i => (math.abs(mat(i,k)), i)).maxBy(_._1)._2
      if(mat(k.toInt,k.toInt) == 0.0)
        return null
      //mat = mat.swapRows(i_max.toInt, k)
      for(i <- (k+1) until m){
        val t = mat(i,k) / mat(k,k)
        for(j <- (k+1) until n){
          mat = mat.updated(i,j, mat(i,j) - (mat(k,j) * t))
        }
        mat = mat.updated(i,k, 0.0)
      }



      //val tmp = mat(k,k)
      //if(tmp != 0.0)
      //  for(i <- k until n){
      //    mat = mat.updated(k, i, mat(k,i) / tmp)
      //  }

    }
    println("****")
    println(mat)
    println("****")
    mat.subMatrix((0,m), (m,m))
  }*/
}