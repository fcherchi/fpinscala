package two

import org.slf4j.LoggerFactory

object MathModule {

  val logger = LoggerFactory.getLogger(getClass.getName)

  def abs(n: Int) = if (n < 0) -n else n

  private def formatAbs(n: Int) = {
    val msg = "The abs value of %d is %d".format(n, abs(n))
    msg
  }



  def main(args: Array[String]): Unit = {
    logger.debug(formatAbs(-42))
  }
}
