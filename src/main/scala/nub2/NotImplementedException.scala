package nub2

class NotImplementedException(feature: String)
  extends RuntimeException("Feature `" + feature + "` is not implemented yet")
