package common

package object pandigital {
  val digits = Set('1','2','3','4','5','6','7','8','9')

  def isPandigital(str:String):Boolean = {
    str.toSet == digits
  }
}
