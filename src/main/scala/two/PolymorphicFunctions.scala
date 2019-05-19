package two

import org.slf4j.LoggerFactory

import scala.annotation.tailrec

object PolymorphicFunctions {

  val logger = LoggerFactory.getLogger(getClass.getName)

  //initial version. Monomorphic
  /**
    * Returns the index of the given key in the array or -1
    * @param strs
    * @param key
    * @return
    */
  def findFirstArr(strs: Array[String], key: String): Int = {
    // another recursion yah
    @tailrec
    def loop(n: Int): Int =
      if (n >= strs.length)-1 else {
        //equals is the same
        if (strs(n) == key) n else loop(n + 1)
      }

    loop(0)
  }


  //second version, recursive
  def findFirst[A](strs: Array[A], key: A): Int = {
    // another recursion yah
    @tailrec
    def loop(n: Int): Int =
      if (n >= strs.length)-1 else {
        if (strs(n) == key) n else loop(n + 1)
      }

    loop(0)
  }

  //even more generic when receiving the comparision instead of the key
  def findFirstComparing[A](strs: Array[A], comparator: A => Boolean): Int = {
    // another recursion yah
    @tailrec
    def loop(n: Int): Int =
      if (n >= strs.length)-1 else {
        if (comparator(strs(n))) n else loop(n + 1)
      }

    loop(0)
  }


  // Partial Application (reducing arity / aridad of a function)

  /**
    * Example of this could be Receiving a product and returning a double
    * @param fixTerm In the double example this would be 2
    * @param f In the example the product function
    * @tparam A
    * @tparam B
    * @tparam C
    * @return
    */
  def partial1[A, B, C](fixTerm: A, f: (A, B) => C): B => C = {
    (b: B) => f(fixTerm, b)
  }


  //Currying

  //from a function that receives 2 args, create a function that receive 1 and return a function that receives the other
  def curry[A, B, C](f: (A, B) => C) : A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  //reverse
  def uncurry[A, B, C](f: (A => B => C)) : (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }


  //and compose

  def compose[A, B, C](f1: B => C, f2: A => B): A => C = {
    (a: A) => f1(f2(a))
  }

  def main(args: Array[String]): Unit = {
//    val strs = Array("a", "b", "c")
//    logger.debug("Found %d".format(findFirstArr(strs, "b")))
//    logger.debug("Found %d".format(findFirstArr(strs, "x")))
//    logger.debug("Found %d".format(findFirst(strs, "b")))
//    logger.debug("Found %d".format(findFirst(strs, "x")))
//
//    logger.debug("Found %d".format(findFirstComparing(strs, (s: String) => s.equals("b"))))
//    logger.debug("Found %d".format(findFirstComparing(strs, (s: String) => s.equals("x"))))
//
//    logger.debug("Found %d".format(findFirstComparing(Array(1, 2, 4), (i: Int) => i.equals(1))))

    val doble = partial1(2, (a: Int, b: Int) => a * b)

    val curr = curry((a: Int, b: Int) => a + b)

    val sixteen = doble(8)

    logger.debug("El doble de %d es: %d".format(8, sixteen))

    //testing curry
    val five = curr(2)(3)
    logger.debug("Suma en 2 pasos de %d + %d = %d".format(2, 3, five))

    //testing uncurry
    val add = uncurry(curr)

    val eight = add(5, 3)
    logger.debug("Suma en 1 paso con uncurry. %d + %d = %d". format(5, 3, eight))

    //testing compose
    val cuadradoYDivide = compose((a: Int) => a / 2, (a: Int) => a * a)
    val result = cuadradoYDivide(6)
    logger.debug("(6^2) / 2 = %d".format(result))

  }
}
