package two

import org.slf4j.LoggerFactory

import scala.annotation.tailrec

object Exercises {

  val logger = LoggerFactory.getLogger(getClass.getName)

  def fibonacci(n: Int):Int = {
    if (n <= 1) n else fibonacci(n - 1) + fibonacci( n - 2)
  }


  def main(args: Array[String]): Unit = {
    logger.debug("" + fibonacci(4))
  }
}
