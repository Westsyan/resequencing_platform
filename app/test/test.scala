package test

object test {

  def main(args: Array[String]): Unit = {

    val x =
      """
        |a
        """stripMargin
    val y =
        """a
        |a
      """.stripMargin

    println(x+y)
  }
}
