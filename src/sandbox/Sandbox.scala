package sandbox

import ImplicitTest._
import sandbox.Addable
import sandbox.ImplicitTest
object Sandbox {
   def main(args: Array[String]){
     /*
     val int : Int = Int.MaxValue
     println(Utils.toInt(int))
     
     val neg1 : Int = -1
     
     val long : Long = neg1.toLong + 1
     println(Utils.toInt(long))
     
     val double : Double = 10.99
     println(Utils.toInt(double))
     
     */
     implicit val addy :Addable[Int] = ImplicitTest.IntAddable
     println(ImplicitTest.add(1, 2))
     
          println(ImplicitTest.add(3, 4))
     println(ImplicitTest.add("aaa", "bbb"))
     println(ImplicitTest.add(3.5,6.7))
     
   }
}