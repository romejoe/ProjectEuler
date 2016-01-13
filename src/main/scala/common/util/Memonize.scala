package common.util

trait MemFunctionN{
  val cache:scala.collection.mutable.Map[Any,Any] = scala.collection.mutable.Map[Any,Any]()
  def apply[T,R](tuple:T, f:T=>R)={
    if(!cache.contains(tuple)){
      cache += (tuple -> f(tuple))
    }
    cache(tuple).asInstanceOf[R]
  }
}

class MemFuncion1[T1, R](f:(T1) => R) extends ((T1) => R) with MemFunctionN{
  def apply(v1:T1):R = apply(v1,f)
}

class MemFuncion2[T1,T2, R](f:(T1,T2) => R) extends ((T1,T2) => R) with MemFunctionN{
  def apply(v1:T1, v2:T2):R = apply((v1,v2),f.tupled)
}

object Memonize{

  def apply[T1,R](f: (T1) => R) = {
    new MemFuncion1(f)
  }
  def apply[T1,T2,R](f: (T1,T2) => R) = {
    new MemFuncion2(f)
  }
}
