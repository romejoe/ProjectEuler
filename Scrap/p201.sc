val r = (1 to 100).map(i => i*i)

def FindBounds(S:Seq[Int], size:Int) = {
  val s = S.reverse
  val seqSize = S.length
  s
    .zipWithIndex
    .take(seqSize - size + 1)
      .map({case (i, idx) => (i, i + s.takeRight(size-1).sum, s.slice(idx, idx + size).sum)})
        .foreach({case(i, lbound, ubound) => println(s"${math.sqrt(i).toInt} => ($lbound, $ubound)")})
}
FindBounds(r, 50)