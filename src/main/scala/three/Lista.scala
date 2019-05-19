package three

//import org.slf4j.LoggerFactory

//sealed means all implementations of the trait must be in this class
//+A is covariant
sealed trait Lista[+A]

//nothing is a subtype of all types in Scala
case object Nil extends Lista[Nothing]

case class NonEmpty[+A](head: A, tail: Lista[A]) extends Lista[A]

//companion object
object Lista {

  //val logger = LoggerFactory.getLogger(getClass.getSimpleName)

  //adds up all elements in a list of integer
  def sum(ints: Lista[Int]): Int = {
    //pattern matching
    ints match {
        //stop iteration if list is nil (last element or empty)
      case Nil => 0
        // if is not null, iterate from head to bottom
      case NonEmpty(head, tail) => head + sum(tail)
    }
  }

  def product(ints: Lista[Double]): Double = {
    //pattern matching
    ints match {
      case Nil => 1.0

      // if head is 0, the result is 0
      case NonEmpty(0.0, _) => 0.0

      // if it is not null, iterate from head to bottom
      case NonEmpty(head, tail) => head * product(tail)
    }
  }

  def apply[A](as: A*): Lista[A] = {
    if (as.isEmpty) Nil
    else NonEmpty(as.head, apply(as.tail: _*))
  }

  def main(args: Array[String]): Unit = {
    val l: Lista[Double] = NonEmpty(2.0, Nil)

   // logger.debug("" + Lista.product(l))
  }
}
