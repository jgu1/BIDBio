package sandbox

/*
abstract class SemiGroup[A] {
    def add(x: A, y: A): A
}
abstract class Monoid[A] extends SemiGroup[A] {
    def unit: A
}
* */

abstract class Addable[T]{
  def +(x: T, y: T): T
}


object ImplicitTest {
      def add[T](x: T, y: T)(implicit addy: Addable[T]): T = 
          addy.+(x, y)
          
      implicit object IntAddable extends Addable[Int]{
          def +(x: Int, y: Int): Int = x + y
      }    
      implicit object DoubleAddable extends Addable[Double]{
         def +(x: Double, y: Double): Double = x + y
      }
      implicit object StringAddable extends Addable[String]{
         def +(x: String, y: String): String = x concat y
      }
      /*
  
      implicit object StringMonoid extends Monoid[String] {
        def add(x: String, y: String): String = x concat y
        def unit: String = ""
      }
      implicit object IntMonoid extends Monoid[Int] {
        def add(x: Int, y: Int): Int = x + y
        def unit: Int = 0
      }
      def sum[A](xs: List[A])(implicit m: Monoid[A]): A =
        if (xs.isEmpty) m.unit
        else m.add(xs.head, sum(xs.tail))
        
      def main(args: Array[String]){
        println(sum(List(1, 2, 3)))
        println(sum(List("a", "b", "c")))
      }
    */
      
    def main(args: Array[String]){
      println( add(1,2))
      println( add(2.3,5.6))
    }
}