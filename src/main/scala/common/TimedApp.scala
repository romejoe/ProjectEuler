package common

trait TimedApp extends App{
  override def main(args:Array[String]): Unit = {
    val start = System.currentTimeMillis()
    super.main(args)
    println(s"Executed in ${System.currentTimeMillis() - start} ms")
  }
}
