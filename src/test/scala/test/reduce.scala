package test

object reduce {
  def main(args: Array[String]): Unit = {
    val collection = List(1, 5, 7, 8)
    val new_collection = collection.map(x => (x, 1))
    print(new_collection)
    val result = new_collection.reduce( (a,b) => ( a._1 + b._1, a._2 + b._2));
    println()
    print(result._1/result._2.toFloat)
    print(result)
    val answer = collection.reduce( (a, b) => a+b)/collection.size.toFloat
    println("haha")
    print(answer)
  }
}
