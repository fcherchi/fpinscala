package one

import org.slf4j.LoggerFactory

class CreditCard {

  val logger = LoggerFactory.getLogger(getClass.getName)

  val name = "ING Debit"

  def charge(amount: Double) = {
    logger.debug("Charged {}", amount)
  }
}
