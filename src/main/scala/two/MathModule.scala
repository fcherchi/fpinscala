package two

import org.slf4j.LoggerFactory

import scala.annotation.tailrec

object MathModule {

  val logger = LoggerFactory.getLogger(getClass.getName)

  def abs(n: Int) = if (n < 0) -n else n

  private def formatAbs(n: Int) = "The abs value of %d is %d".format(n, abs(n))

  private def formatFactorial(n: Int) = "Factorial of %d is %d".format(n, factorial(n))

  private def factorial(n: Int):Int = {
    //when all recursive calls inside a function are in tail position, we talk about tail recursion
    //if we would do something like 1 + loop(whatever) is not in tail position as loop has to evaluate
    //before the addition
    @tailrec
    def loop(n: Int, acc: Int) : Int  = {
      if (n > 0)
        loop(n - 1, n * acc) //iterate
      else
        acc //exit loop with the result
    }
    loop(n, 1)
  }


  //Now Higher-order functions
  //Make format generic
  private def formatGeneric(name: String, n: Int, f: (Int => Int)) = {
    "The %s of %d is %d".format(name, n, f(n))
  }



  def main(args: Array[String]): Unit = {
//    logger.debug(formatAbs(-42))
//    logger.debug(formatFactorial(5))
    logger.debug(formatGeneric("Abs", -42, abs))
    logger.debug(formatGeneric("Factorial", -42, factorial))
  }
}
