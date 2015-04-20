package BIDBioTest

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter

import BIDMat.LMat
import BIDBio.B2Mat
/**
 * Requires ScalaTest and JUnit4.
 */
@RunWith(classOf[JUnitRunner])
class B2MatSuite extends FunSuite with BeforeAndAfter {

  val firstInt : Int = 0x1b1b1b1b
  val secondInt : Int = 0xe4e4e4e4
  
  test("B2Mat apply: B2Mat(i,j) should return the correct item") {
    var b2Mat : B2Mat = new B2Mat(4,8,Array(firstInt,secondInt))
  /*
   * at this time the array will logically look like
   * 00 00 00 00 11 11 11 11*
   * 01 01 01 01 10 10 10 10
   * 10 10 10 10*01 01 01 01
   * 11 11 11 11 00 00 00 00
   */  
    val b2Mat_0_7 = b2Mat(0,7)
    val b2Mat_2_3 = b2Mat(2,3)
    assert(b2Mat_0_7.==(3))
    assert(b2Mat_2_3.==(2))
  }
  
  test("B2Mat update"){
    var b2Mat : B2Mat = new B2Mat(4,8,Array(firstInt,secondInt))
  /*
   * at this time the array will logically look like
   * 00 00 00 00 11 11 11 11*
   * 01 01 01 01 10 10 10 10
   * 10 10 10 10*01 01 01 01
   * 11 11 11 11 00 00 00 00
   */  
    b2Mat._update(2, 3, 3)
    assert(b2Mat(2,3).==(3))
  }
  
  test("B2Mat indexOf"){
    var b2Mat : B2Mat = new B2Mat(4,8,Array(firstInt,secondInt))
    b2Mat._update(3,0,2)
    b2Mat._update(3,1,2)
    b2Mat._update(3,2,2)
    b2Mat._update(3,3,2)
  /* at this time the array will logically look like
   * 00 00 00 00 11 11 11 11*
   * 01 01 01 01 10 10 10 10
   * 10 10 10 10*01 01 01 01
   * 10 10 10 10 00 00 00 00
   */  
    assert(b2Mat.indexOf(3).==(16))
    assert(b2Mat.indexOf2(3).==((0,4)))
    
    b2Mat  = new B2Mat(4,8,Array(firstInt,secondInt))  //recover
    b2Mat._update(2,0,3)
    b2Mat._update(2,1,3)
    b2Mat._update(2,2,3)
    b2Mat._update(2,3,3)
    
    b2Mat._update(1,4,3)
    b2Mat._update(1,5,3)
    b2Mat._update(1,6,3)
  /* at this time the array will logically look like
   * 00 00 00 00 11 11 11 11*
   * 01 01 01 01 11 11 11 10
   * 11 11 11 11 01 01 01 01
   * 11 11 11 11 00 00 00 00
   */ 
    assert(b2Mat.indexOf(2).==(29))
    assert(b2Mat.indexOf2(2).==((1,7)))
    
    b2Mat._update(1,7,3)
    assert(b2Mat.indexOf(2).==(-1))
    assert(b2Mat.indexOf2(2).==((-1,-1)))
  }
  
  test("B2Mat nnz, number of non-zero elements"){
   var b2Mat  = new B2Mat(4,8,Array(firstInt,secondInt)) 
  /*
   * at this time the array will logically look like
   * 00 00 00 00 11 11 11 11*
   * 01 01 01 01 10 10 10 10
   * 10 10 10 10*01 01 01 01
   * 11 11 11 11 00 00 00 00
   */  
   assert(b2Mat.nnz.==(24))
   b2Mat._update(1,6,0)
  /*
   * at this time the array will logically look like
   * 00 00 00 00 11 11 11 11*
   * 01 01 01 01 10 10 00 10
   * 10 10 10 10*01 01 01 01
   * 11 11 11 11 00 00 00 00
   */  
   assert(b2Mat.nnz.==(23))
  }
  
  test("B2Mat.gvertcat, concatenate two B2Mat Vertically"){
   var a : B2Mat = new B2Mat(4,4,Array(firstInt))
   var b : B2Mat = new B2Mat(4,4,Array(secondInt))
   var vert_concat : B2Mat = a.gvertcat(b)    
  /*
   * at this time the matrix will logically look like
   * 00 00 00 00 
   * 01 01 01 01 
   * 10 10 10 10*
   * 11 11 11 11 
   * 11 11 11 11*
   * 10 10 10 10
   * 01 01 01 01
   * 00 00 00 00
   */  
   assert(vert_concat(0,3).==(0))
   assert(vert_concat(7,3).==(0))
   assert(vert_concat(6,2).==(1))
   assert(vert_concat(5,2).==(2))
   assert(vert_concat(4,2).==(3))
  }
  
  test("B2Mat.gt, transpose"){
    var b2Mat = new B2Mat(4,8,Array(firstInt,secondInt))  //recover
    var transposed : B2Mat = b2Mat.gt
    assert(transposed(7,0).==(3))      //expect 3
    assert(transposed(3,2).==(2))      //expect 2
    assert(transposed(7,1).==(2))      //expect 2
    assert(transposed(4,3).==(0))      //expect 0
  }
  
  test("B2Mat.ghorzcat, Concatenamte two B2Mat Horizontally, squeeze out padding zeros"){
   var newFirstInt : Int = 0xaaaaaa00
   var newSecondInt : Int = 0xffffffff
   var newFirstIntMat : B2Mat = new B2Mat(4,3,Array(newFirstInt))
   var newSecondIntMat : B2Mat = new B2Mat(4,4,Array(newSecondInt))
   var concatMatHorizontally : B2Mat = newFirstIntMat.ghorzcat(newSecondIntMat)
  /*
   * at this time the array will logically look like
   * 10 10 10 11 11 11 11
   * 10 10 10 11 11 11 11
   * 10 10 10 11 11 11 11
   * 10 10 10 11 11 11 11
   */  
   assert(concatMatHorizontally(3,1).==(2))  //expect 2
   assert(concatMatHorizontally(1,2).==(2))  //expect 2
   assert(concatMatHorizontally(2,2).==(2))  //expect 2
   assert(concatMatHorizontally(0,2).==(2))  //expect 2
   assert(concatMatHorizontally(3,4).==(3))  //expect 3
   assert(concatMatHorizontally(0,3).==(3))  //expect 3
   
   newFirstInt  = 0xaaaaaa00
   newSecondInt  = 0xffffffff
   val newThirdInt : Int = 0x55555555
   newFirstIntMat  = new B2Mat(4,3,Array(newFirstInt))
   newSecondIntMat  = new B2Mat(4,8,Array(newSecondInt,newThirdInt))
   concatMatHorizontally  =  newFirstIntMat.ghorzcat(newSecondIntMat)
  /*
   * at this time the array will logically look like
   * 10 10 10 11 11 11 11 01 01 01 01
   * 10 10 10 11 11 11 11 01 01 01 01
   * 10 10 10 11 11 11 11 01 01 01 01
   * 10 10 10 11 11 11 11 01 01 01 01
   */ 
   assert(concatMatHorizontally(0,2).==(2))  //expect 2
   assert(concatMatHorizontally(0,3).==(3))  //expect 3
   assert(concatMatHorizontally(0,6).==(3))  //expect 3
   assert(concatMatHorizontally(0,7).==(1))  //expect 1
   assert(concatMatHorizontally(0,10).==(1))  //expect 1
   
   assert(concatMatHorizontally(8).==(2))                                  //expect 2
   assert(concatMatHorizontally(12).==(3))                                 //expect 3
   assert(concatMatHorizontally(30).==(1))                                 //expect 1
   
  }
  
  test("B2Mat.extractSeed, extract 16base pases staring from the designate position"){
   val newFirstInt  = 0xaaaaaa00
   val newSecondInt  = 0xffffffff
   val newThirdInt : Int = 0x55555555
   val newFirstIntMat  = new B2Mat(4,3,Array(newFirstInt))
   val newSecondIntMat  = new B2Mat(4,8,Array(newSecondInt,newThirdInt))
   val concatMatHorizontally  =  newFirstIntMat.ghorzcat(newSecondIntMat)
  /*
   * at this time the array will logically look like
   * 10 10 10 11 11 11 11 01~01 01 01
   * 10 10 10*11 11#11 11 01 01 01 01
   * 10 10 10 11 11 11 11 01 01 01 01
   * 10 10 10 11 11 11 11 01 01 01 01
   */ 
   val seed_starting_at_star : Int = concatMatHorizontally.extract_seed(1,2)
   assert(seed_starting_at_star.toBinaryString.== ("10101011111111111111111111111111"))
   
   val seed_starting_at_pound : Int = concatMatHorizontally.extract_seed(17)
   assert(seed_starting_at_pound.toBinaryString.==("11111111111111111111110101010101"))
   
   val seed_starting_at_tidle : Int = concatMatHorizontally.extract_seed(0,7)
   assert(seed_starting_at_tidle.toBinaryString.==("1010101010101010101010101010101"))    //this string is 31bits because a leading 0 is not included
  }
  
  test("B2Mat.find, find position of all non-zero elements"){
   var b2Mat = new B2Mat(4,8,Array(firstInt,secondInt))
    /*
   * at this time the array will logically look like
   * 00 00 00 00 11 11 11 11*
   * 01 01 01 01 10 10 10 10
   * 10 10 10 10*01 01 01 01
   * 11 11 11 11 00 00 00 00
   */  
   var nonZeroPos : LMat = b2Mat.find
   var expectedPos : LMat = new LMat(1,24,Array[Long](1,2,3,5,6,7,9,10,11,13,14,15,16,17,18,20,21,22,24,25,26,28,29,30))
   assert(nonZeroPos.toString.==(expectedPos.toString))  //FIX ME
   
   b2Mat = new B2Mat(4,6,Array(firstInt,secondInt))
  /*
   * at this time the array will logically look like
   * 00 00 00 00 11 11 00 00*
   * 01 01 01 01 10 10 00 00
   * 10 10 10 10*01 01 00 00
   * 11 11 11 11 00 00 00 00
   */
   nonZeroPos = b2Mat.find
   expectedPos = new LMat(1,18,Array[Long](1,2,3,5,6,7,9,10,11,13,14,15,16,17,18,20,21,22))
   assert(nonZeroPos.toString.==(expectedPos.toString))  //FIX ME
  }
}