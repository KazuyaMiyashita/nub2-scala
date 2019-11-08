package nub2

object Collections {
  def listOf[T](elements: T*): List = {
    elements.toList
  }
}
