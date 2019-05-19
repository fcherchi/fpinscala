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


  def main(args: Array[String]): Unit = {
    val strs = Array("a", "b", "c")
    logger.debug("Found %d".format(findFirstArr(strs, "b")))
    logger.debug("Found %d".format(findFirstArr(strs, "x")))
    logger.debug("Found %d".format(findFirst(strs, "b")))
    logger.debug("Found %d".format(findFirst(strs, "x")))

    logger.debug("Found %d".format(findFirstComparing(strs, (s: String) => s.equals("b"))))
    logger.debug("Found %d".format(findFirstComparing(strs, (s: String) => s.equals("x"))))

    logger.debug("Found %d".format(findFirstComparing(Array(1, 2, 4), (i: Int) => i.equals(1))))

  }
}
