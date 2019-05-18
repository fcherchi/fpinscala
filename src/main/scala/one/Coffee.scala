package one

import org.slf4j.LoggerFactory

class Coffee {
  val logger = LoggerFactory.getLogger(getClass.getName)
  val price = 3.0

  override def toString: String = {
    "\n   ( (\n    ) )\n  ........\n  |      |]\n  \\      / \n   `----'"
  }

}
