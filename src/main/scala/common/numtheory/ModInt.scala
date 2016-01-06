package common.numtheory


case class ModInt(value:Int, modulus:Int) {
  def +(i:Int):ModInt = ModInt((value+i)%modulus, modulus)
  def -(i:Int):ModInt = ModInt((value-i)%modulus, modulus)
  def *(i:Int):ModInt = ModInt((value*i)%modulus, modulus)
}
