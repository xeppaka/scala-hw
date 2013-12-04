package funsets

object Main extends App {
  def ff(e: Int): Boolean = {
    e == 0
  }
  
  import FunSets._
  //println(contains(singletonSet(1), 1))
  println(exists(union(singletonSet(1), singletonSet(2)), ff))
}
