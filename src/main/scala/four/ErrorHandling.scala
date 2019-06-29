//package four

object ErrorHandling {

  /**
    * Here with side effect
    * @param list
    * @return
    */
  def avg(list: Seq[Double]): Double = {

    if (list.isEmpty) throw new ArithmeticException("Average of an empty list")

    list.sum / list.length

  }

  /**
    * Here without side effect.
    * But callers need to know how to handle empty lists behavior
    * @param list
    * @param onEmpty
    * @return
    */
  def avgNoSideEffect(list: Seq[Double], onEmpty: Double): Double = {
    if (list.isEmpty) onEmpty else list.sum / list.length
  }

  //Option (like optional in java)

  //def avgWithOption(list: Seq[Double], on)


}
