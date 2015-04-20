package BIDBioTest

//import BIDBio.DNAMat

import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class Tester extends FunSuite {//with Checkers {
  val firstInt : Int = 0x1b1b1b1b
  val secondInt : Int = 0xe4e4e4e4
  /*
  var dnaMat : DNAMat = new DNAMat(4,8,Array(firstInt,secondInt))
  
    println("dnaMat(0,7) = " + dnaMat(0,7))      //expect 3
    println("dnaMat(2,3) = " + dnaMat(2,3))      //expect 2
    
    
    test("DNAMat apply") {
    assert(dnaMat(0,7) == 3);
    assert(dnaMat(2,3) == 2);
  }
  */
  test("1+1"){
    val a : Int = 1
    val b : Int = 2
    assert(a+ b ==2);
  }
}