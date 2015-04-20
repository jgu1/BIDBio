package BIDBioTest

import BIDBio.B2Mat
import BIDMat.IMat
import BIDMat.LMat

object Main{
  def main(args: Array[String]){
    val firstInt : Int = 0x1b1b1b1b
    val secondInt : Int = 0xe4e4e4e4
    var B2Mat : B2Mat = new B2Mat(4,8,Array(firstInt,secondInt))
    /* this is to test horizontal concatenation
    var a : B2Mat = new B2Mat(4,4,Array(firstInt))
    var b : B2Mat = new B2Mat(4,4,Array(secondInt))
    var B2Mat: B2Mat = a.ghorzcat(b)
    */
    
    /*
   * at this time the array will logically look like
   * 00 00 00 00 11 11 11 11*
   * 01 01 01 01 10 10 10 10
   * 10 10 10 10*01 01 01 01
   * 11 11 11 11 00 00 00 00
   */  
     
     
    println("B2Mat(0,7) = " + B2Mat(0,7))      //expect 3
    println("B2Mat(2,3) = " + B2Mat(2,3))      //expect 2
    
    B2Mat._update(2, 3, 3)
    println("Now B2Mat(2,3) = " + B2Mat(2,3)) //expect 3
    B2Mat._update(2,3,2)  //recover
    
 ///////////////////////////////////////////////////////
    
    B2Mat._update(3,0,2)
    B2Mat._update(3,1,2)
    B2Mat._update(3,2,2)
    B2Mat._update(3,3,2)
    
  
  /* at this time the array will logically look like
   * 00 00 00 00 11 11 11 11*
   * 01 01 01 01 10 10 10 10
   * 10 10 10 10*01 01 01 01
   * 10 10 10 10 00 00 00 00
   */  
   println("B2Mat,indexOf(3) = " + B2Mat.indexOf(3))   //expect 16
   println("B2Mat,indexOf2(3) = " + B2Mat.indexOf2(3)) //expect (0,4)
   
   B2Mat  = new B2Mat(4,8,Array(firstInt,secondInt))  //recover
/////////////////////////////////////////////////////////////    
    B2Mat._update(2,0,3)
    B2Mat._update(2,1,3)
    B2Mat._update(2,2,3)
    B2Mat._update(2,3,3)
    
    B2Mat._update(1,4,3)
    B2Mat._update(1,5,3)
    B2Mat._update(1,6,3)
    //B2Mat._update(1,7,3)
    
  /* at this time the array will logically look like
   * 00 00 00 00 11 11 11 11*
   * 01 01 01 01 11 11 11 10
   * 11 11 11 11 01 01 01 01
   * 11 11 11 11 00 00 00 00
   */ 
   println("B2Mat,indexOf(2) = " + B2Mat.indexOf(2))   //expect 29
   println("B2Mat,indexOf2(2) = " + B2Mat.indexOf2(2)) //expect (1,7) 
    
    B2Mat._update(1,7,3)
    
   println("B2Mat,indexOf(2) = " + B2Mat.indexOf(2))   //expect -1
   println("B2Mat,indexOf2(2) = " + B2Mat.indexOf2(2)) //expect (-1 -1) 
   
   B2Mat  = new B2Mat(4,8,Array(firstInt,secondInt))  //recover
    
   println("B2Mat.nnz = " + B2Mat.nnz)  // expect 24
   B2Mat._update(1,6,0)
   println("B2Mat.nnz = " + B2Mat.nnz)  //expect 23
   
   B2Mat  = new B2Mat(4,8,Array(firstInt,secondInt))  //recover
   
   var a : B2Mat = new B2Mat(4,4,Array(firstInt))
   var b : B2Mat = new B2Mat(4,4,Array(secondInt))
   var vert_concat : B2Mat = a.gvertcat(b)
   
    /*
   * at this time the array will logically look like
   * 00 00 00 00 
   * 01 01 01 01 
   * 10 10 10 10*
   * 11 11 11 11 
   * 11 11 11 11*
   * 10 10 10 10
   * 01 01 01 01
   * 00 00 00 00
   */  
   
    println("vert_concat(0,3) = " + vert_concat(0,3))      //expect 0
    println("vert_concat(7,3) = " + vert_concat(7,3))      //expect 0
    println("vert_concat(6,2) = " + vert_concat(6,2))      //expect 1
    println("vert_concat(5,2) = " + vert_concat(5,2))      //expect 2
    println("vert_concat(4,2) = " + vert_concat(4,2))      //expect 3
    
    var transposed : B2Mat = B2Mat.gt
    
  /*
   * at this time the array will logically look like
   * 00 00 00 00 11 11 11 11*
   * 01 01 01 01 10 10 10 10
   * 10 10 10 10*01 01 01 01
   * 11 11 11 11 00 00 00 00
   */  
    
    println("transposed(7,0) = " + transposed(7,0))      //expect 3
    println("transposed(3,2) = " + transposed(3,2))      //expect 2
    println("transposed(7,1) = " + transposed(7,1))      //expect 2
    println("transposed(4,3) = " + transposed(4,3))      //expect 0
    
    
    B2Mat = new B2Mat(4,8,Array(firstInt,secondInt))
    /*
   * at this time the array will logically look like
   * 00 00 00 00 11 11 11 11*
   * 01 01 01 01 10 10 10 10
   * 10 10 10 10*01 01 01 01
   * 11 11 11 11 00 00 00 00
   */  

   B2Mat  = new B2Mat(4,8,Array(firstInt,secondInt))  //recover
    
    
   var newFirstInt : Int = 0xaaaaaa00
   var newSecondInt : Int = 0xffffffff
   var newFirstIntMat : B2Mat = new B2Mat(4,3,Array(newFirstInt))
   var newSecondIntMat : B2Mat = new B2Mat(4,4,Array(newSecondInt))
   var concatMatHorizontally : B2Mat = newFirstIntMat.ghorzcat(newSecondIntMat)
   
   println(" concatMatHorizontally(3,1) = "+  concatMatHorizontally(3,1))  //expect 2
   println(" concatMatHorizontally(1,2) = "+  concatMatHorizontally(1,2))  //expect 2
   println(" concatMatHorizontally(2,2) = "+  concatMatHorizontally(2,2))  //expect 2
   println(" concatMatHorizontally(0,2) = "+  concatMatHorizontally(0,2))  //expect 2
   println(" concatMatHorizontally(3,4) = "+  concatMatHorizontally(3,4))  //expect 3
   println(" concatMatHorizontally(0,3) = "+  concatMatHorizontally(0,3))  //expect 3
    
  /*
   * at this time the array will logically look like
   * 10 10 10 11 11 11 11
   * 10 10 10 11 11 11 11
   * 10 10 10 11 11 11 11
   * 10 10 10 11 11 11 11
   */  
   
   newFirstInt  = 0xaaaaaa00
   newSecondInt  = 0xffffffff
   val newThirdInt : Int = 0x55555555
   newFirstIntMat  = new B2Mat(4,3,Array(newFirstInt))
   newSecondIntMat  = new B2Mat(4,8,Array(newSecondInt,newThirdInt))
   concatMatHorizontally  =  newFirstIntMat.ghorzcat(newSecondIntMat)
   
   println(" concat 3 ints")
   println(" concatMatHorizontally(0,2) = "+  concatMatHorizontally(0,2))  //expect 2
   println(" concatMatHorizontally(0,3) = "+  concatMatHorizontally(0,3))  //expect 3
   println(" concatMatHorizontally(0,6) = "+  concatMatHorizontally(0,6))  //expect 3
   println(" concatMatHorizontally(0,7) = "+  concatMatHorizontally(0,7))  //expect 1
   println(" concatMatHorizontally(0,10) = "+  concatMatHorizontally(0,10))  //expect 1
   
   println(concatMatHorizontally(8))                                  //expect 2
   println(concatMatHorizontally(12))                                 //expect 3
   println(concatMatHorizontally(30))                                 //expect 1
 
   println(concatMatHorizontally.extract_seed(1,2))
   
   
/*
   * at this time the array will logically look like
   * 10 10 10 11 11 11 11 01 01 01 01
   * 10 10 10 11 11 11 11 01 01 01 01
   * 10 10 10 11 11 11 11 01 01 01 01
   * 10 10 10 11 11 11 11 01 01 01 01
   */  
 
   B2Mat = new B2Mat(4,8,Array(firstInt,secondInt))
    /*
   * at this time the array will logically look like
   * 00 00 00 00 11 11 11 11*
   * 01 01 01 01 10 10 10 10
   * 10 10 10 10*01 01 01 01
   * 11 11 11 11 00 00 00 00
   */  
   var nonZero : LMat = B2Mat.find
   //print(nonZero)
    
   B2Mat = new B2Mat(4,6,Array(firstInt,secondInt))
  /*
   * at this time the array will logically look like
   * 00 00 00 00 11 11 00 00*
   * 01 01 01 01 10 10 00 00
   * 10 10 10 10*01 01 00 00
   * 11 11 11 11 00 00 00 00
   */
   nonZero = B2Mat.find
   print(nonZero)
   
    
    
   B2Mat = new B2Mat(4,8,Array(firstInt,secondInt))
   println(B2Mat.extract_seed(0))
    
  }
}