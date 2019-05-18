package one


case class Charge (creditCard: CreditCard, amount: Double){


  /**
    * Combines (adds) two charges
    * @param other
    * @return
    */
  def combine(other: Charge): Charge =

    // returns a new charge (immutable) combining the amount of this plus the other
    // but hey! both cards should be the same! So we add the if sentence
    // Charge(creditCard, this.amount + other.amount)

   // if (this.creditCard == other.creditCard) {
      Charge(creditCard, this.amount + other.amount)
//    } else {
//      //geee this is not functional
//      throw new Exception("Cannot combine charges from different cards!")
//    }



  override def toString: String = {
    "Card: " + creditCard.name + " :: Amount: " + amount
  }
}
