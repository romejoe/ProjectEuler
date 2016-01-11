package common.math

import common.numtheory.Prime

class Rational(val n:Long, val d:Long){
  def +(i:Long):Rational = this + Rational(i,1)
  def -(i:Long):Rational = this - Rational(i,1)
  def *(i:Long):Rational = this * Rational(i,1)
  def /(i:Long):Rational = this / Rational(i,1)

  def +(r:Rational):Rational = Rational(n*r.d + r.n*d,d*r.d)
  def -(r:Rational):Rational = Rational(n*r.d - r.n*d,d*r.d)
  def *(r:Rational):Rational = Rational(n*r.n, d*r.d)
  def /(r:Rational):Rational = Rational(n*r.d, d*r.n)

  def minimize:Rational = Rational.minimize(n,d)
  def flip:Rational = Rational(d,n)

  override def toString:String = s"$n/$d"
}

object Rational {
  def apply(n:Long):Rational = new Rational(n,1)
  def apply(n:Long,d:Long):Rational = new Rational(n,d)
  def minimize(n: Long, d: Long): Rational = {
    val i = Prime.gcd(n,d)
    Rational(n/i, d/i)
  }
}