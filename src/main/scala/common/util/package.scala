package common

package object util {
  def timeIt(f: =>Unit):Long = {
    val start = System.currentTimeMillis()
    f
    System.currentTimeMillis() - start
  }
}
