package common

package object numtheory {
  implicit def ModIntToInt(modInt: ModInt):Int = modInt.value
}
