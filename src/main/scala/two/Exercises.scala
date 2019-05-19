package two

import org.slf4j.LoggerFactory

import scala.annotation.tailrec

object Exercises {

  val logger = LoggerFactory.getLogger(getClass.getName)

  def fibonacci(n: Int):Int = {
    if (n <= 1) n else fibonacci(n - 1) + fibonacci( n - 2)
  }



  def isSorted[A](array: Array[A], isSorted: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(i: Int): Boolean = {
      if (array.length < 2) return true

      //end of loop
      if (i >= array.length - 1) return true
      else
        if (! isSorted(array(i), array(i + 1)))
          return false
        else
          loop(i + 1)
    }
    loop(0)
  }

  def main(args: Array[String]): Unit = {
    logger.debug("" + fibonacci(4))
  }
}
