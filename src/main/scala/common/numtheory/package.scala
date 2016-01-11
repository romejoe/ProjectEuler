package common

package object numtheory {
  implicit def ModIntToInt(modInt: ModInt):Int = modInt.value

  def isInteger(d:Double):Boolean = d.toLong.toDouble == d
  def isInteger(d:BigDecimal):Boolean = BigDecimal(d.toLong) == d
}
