package common.math

import common.numtheory.Prime

class BigRational(val n:BigInt, val d:BigInt) extends Ordered[BigRational]{
  def +(i:BigInt):BigRational = this + BigRational(i,1)
  def -(i:BigInt):BigRational = this - BigRational(i,1)
  def *(i:BigInt):BigRational = this * BigRational(i,1)
  def /(i:BigInt):BigRational = this / BigRational(i,1)

  def +(r:BigRational):BigRational = BigRational(n*r.d + r.n*d,d*r.d)
  def -(r:BigRational):BigRational = BigRational(n*r.d - r.n*d,d*r.d)
  def *(r:BigRational):BigRational = BigRational(n*r.n, d*r.d)
  def /(r:BigRational):BigRational = BigRational(n*r.d, d*r.n)

  def minimize:BigRational = BigRational.minimize(n,d)
  def flip:BigRational = BigRational(d,n)

  override def toString:String = s"$n/$d"

  override def compare(that: BigRational): Int = {
    BigRational.compare(this, that)
  }
}

object BigRational extends Ordering[BigRational]{
  def apply(n:BigInt):BigRational = new BigRational(n,1)
  def apply(n:BigInt,d:BigInt):BigRational = new BigRational(n,d)
  def minimize(n: BigInt, d: BigInt): BigRational = {
    val i = Prime.gcd(n.toLong,d.toLong)
    BigRational(n/i, d/i)
  }

  override def compare(x: BigRational, y: BigRational): Int = {
    (x*y.d).n.compare((y*x.d).n)
  }
}