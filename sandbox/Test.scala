object Test  {
  def main(args: Array[String]): Unit = {
    def nya = throw new Exception("Hi!")

    println(nya)
  }

}
