package test

object test {

  def main(args: Array[String]): Unit = {
    def f(a: List[String]): Boolean = {
      a match {
        case Nil => true
        case i :: tail => {
          if (!i.endsWith(".js"))
            false
          else
            f(tail)
        }

      }
    }

  }
}
