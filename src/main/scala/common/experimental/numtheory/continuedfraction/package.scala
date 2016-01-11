package common.experimental.numtheory

import common.math.Rational

package object continuedfraction {
  def continuedFractionToRational(seq:List[Int]):Rational = {
    val base = seq.head
    seq.tail.foldLeft(Rational(base))((s,a) => s.flip + a)
  }
}
