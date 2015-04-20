package BIDBio

import Numeric._

object Utils {
  
  def mode[T](in: T, mode_num:Int)(implicit bManip: BitManipulator[T]): Int ={
     bManip.mode(in, mode_num)
  }
  def divide[T](in:T, divide_num:Int)(implicit bManip: BitManipulator[T]):Int={
    bManip.divide(in, divide_num)
  }
  def maskLeadingOnes(numOnes: Int):Int={
    1<<31>>(numOnes-1)
  }
  def maskTailingOnes(numOnes: Int):Int={
    ~(1<<31>>(31-numOnes))
  }
  def maskTailingZeros(numZeros:Int):Int={
    ~maskTailingOnes(numZeros)
  }
  
  
  
  implicit object IntBitManipulator extends BitManipulator[Int]{
    def mode(in:Int, mode_num:Int):Int = {
      if (mode_num == 32){
        in & 0x1f
      }else if (mode_num == 16){
        in & 0xf
      }else{
         throw new RuntimeException("Mode_num not supported")
      }
    }
    
    def divide(in:Int,divide_num:Int)={
      if (divide_num == 32){
        in>>5 & maskTailingOnes(27)
      }else if (divide_num == 16){
        in>>4 & maskTailingOnes(28)
      }else{
         throw new RuntimeException("Divide_num not supported")
      }
    }
  }
  
  implicit object LongBitManipulator extends BitManipulator[Long]{
    def mode(in:Long, mode_num:Int):Int = {
      if (mode_num == 32){
        (in & 0x1f).toInt
      }else if (mode_num == 16){
        (in & 0xf).toInt
      }else{
         throw new RuntimeException("Mode_num not supported")
      }
    }
    
    def divide(in:Long,divide_num:Int)={
      if (divide_num == 32){
        (in>>5 & maskTailingOnes(27)).toInt
      }else if (divide_num == 16){
        (in>>4 & maskTailingOnes(28)).toInt
      }else{
         throw new RuntimeException("Divide_num not supported")
      }
    }
  }
  
  
  

  
  /*
  
  def longDivide16(long:Long):Int = {
    (long>>4 & ~(1<<31>>3)).toInt  //the truncation is done because required return type is Int
  }
  
  def longDivide32(long:Long):Int = {
     (long>>5 & ~(1<<31>>4)).toInt  //the truncation is done because required return type is Int
  }
  
  def longMode16(long:Long):Int = {
    (long & 0xf).toInt
  }
  
  def longMode32(long:Long):Int = {
    (long & 0x1f).toInt
  }
    
  def IntDivide16(int:Int):Int = {
     (int>>4 & ~(1<<31>>3)).toInt
  }
  
  def IntMode32(int:Int):Int = {
    (int & 0x1f).toInt
  }
  
  def mode32[T](in:T)(implicit numeric: Numeric[T]):Int={
      (numeric.toInt(in) & 0x1f).toInt
  }
  
  */

}


abstract class BitManipulator[T]{
    def divide(in:T,divide_num:Int):Int
    def mode(in:T,mode_num:Int):Int
}