package test

object gbpy {
  //gpby iterates over every element and adds it to a group
  def main(args: Array[String]): Unit = {
    val inputrdd = Seq(("key1", 1), ("key2", 2), ("key1", 3))
    val s = inputrdd.groupBy ( x =>
      if ((x._2 % 2) == 0) {
        0
      } else {
        1
      }
    )
    print(s)
  }
}
