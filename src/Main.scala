import scala.util.control.Breaks._
import BIDMat.IMat;
import BIDBio.B2Mat
object Main {
    
  def main(args: Array[String])= {
    println("Scala rocks - by Jialiang Gu")
    /*
    var a1:Array[Int] = new Array[Int](10)
    for (i <- 0 to 9){
      a1(i) = i;
    }
    var iMat: IMat = new IMat(2,5,a1)
    println("iMat.nnz=" + iMat.nnz)
    println("iMat(2,1)=" + iMat(1,3))
    var myInt : Int= 5;
    println("myInt's hex is " + myInt.toBinaryString)
    val someDNA : DNAEnum = A;
    println(someDNA)
   
    
    val firstInt : Int = 0x1b1b1b1b
    val secondInt : Int = 0xe4e4e4e4
    var B2Mat : B2Mat = new B2Mat(4,8,Array(firstInt,secondInt))
    
    println(B2Mat(0,7))
    println(B2Mat(2,3))
    
    B2Mat._update(2, 3, 3)
    println(B2Mat(2,3))
    
    */
    var arr:Array[Int] = Array(0,1,0,0,1,2)
    var iMat: IMat = new IMat(2,3,arr)
    //var ind : IMat
    //var ind2 : IMat
    var (ind, ind2) = iMat.find2
    print("ind = " + ind)
    print("ind2 = " + ind2)
    

    val firstInt : Int = 0x1b1b1b1b
    val secondInt : Int = 0xe4e4e4e4
    var B2Mat : B2Mat = new B2Mat(4,8,Array(firstInt,secondInt))
    /*
   * at this time the array will logically look like
   * 00 00 00 00 11 11 11 11*
   * 01 01 01 01 10 10 10 10
   * 10 10 10 10*01 01 01 01
   * 11 11 11 11 00 00 00 00
   */  
    
    B2Mat._update(3,0,2)
    B2Mat._update(3,1,2)
    B2Mat._update(3,2,2)
    B2Mat._update(3,3,2)
    
    /*
    B2Mat._update(2,0,3)
    B2Mat._update(2,1,3)
    B2Mat._update(2,2,3)
    B2Mat._update(2,3,3)
    
    B2Mat._update(1,4,3)
    B2Mat._update(1,5,3)
    B2Mat._update(1,6,3)
    //B2Mat._update(1,7,3)
    
    */
    
    println("B2Mat,indexOf2(2) = " + B2Mat.indexOf2(3))
    
    
    var imat: IMat = new IMat(2,3,arr)
    println(imat(4))
    
  }

}