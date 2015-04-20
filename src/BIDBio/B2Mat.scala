package BIDBio

import scala.util.control.Breaks._
import BIDMat.DenseMat
import BIDMat.IMat
import BIDMat.LMat
import Utils._
class B2Mat (nr:Long, nc:Long, data0:Array[Int]){
  def mytype = "B2Mat"

  var nextReal2BitsPos : Long = 0
  var nInt : Int = 0
  var data = if (data0 != null)copyData else Array[Int]()
  val nrows : Long = nr
  val ncols : Long = nc
  def length : Long = nr * nc // FIX ME, assuming an Int takes 32 bits
  /*
   * Return the (0,0) value as a Int
   */
  
  def copyData : Array[Int] = {
    var data = data0
    val numIntNeeded = Utils.divide(length + 15,16)
    var numPaddingBits : Int = 0
    if (data0.length >= numIntNeeded){              // if more data is feed in than nc*nr can access
      data = Array.fill[Int](numIntNeeded){0}
      System.arraycopy(data0, 0, data, 0, numIntNeeded)
      if(Utils.mode(length,16) != 0){                //  strip data in the last Int that cannot be accessed in nc*nr, padding with 0
        numPaddingBits = ((numIntNeeded.toLong << 5) - length*2).toInt 
        //val mask = 1<<31>>(31-numPaddingBits)
        data(numIntNeeded-1) = data(numIntNeeded-1) & Utils.maskTailingZeros(numPaddingBits)
      }
    }

    val dataLen = data.length
    val dataLenLong = data.length.toLong
    val totBits = data.length.toLong << 5
    val numP = numPaddingBits
    
    nextReal2BitsPos = ((data.length.toLong << 5) - numPaddingBits) / 2
    this.nInt = data.length
    data
  }
  
  def v:Int = 
    if(nrows > 1 || ncols > 1) {
      throw new RuntimeException("Matrix should be 1x1 to extract value")
    } else {
      (data(0) >> 30) & 3 //first two bits
    }
  /*
   * Test if this matrix is a row or column vector
   */
  def isvector(): Boolean = {
    if (nrows == 1 || ncols == 1) {
      true
    } else {
      false
    }
  }
  /*
   * Bounds-checked matrix access, 0- or 1-based 
   */ 
  def apply(r0:Long, c0:Long):Int = {
    val off = 0 //FIX ME
    val r = r0 - off
    val c = c0 - off
    if (r < 0 || r >= nrows || c < 0 || c >= ncols) {
      throw new IndexOutOfBoundsException("("+(r+off)+","+(c+off)+") vs ("+nrows+","+ncols+")");
    } else {
      get_(r,c)
    }
    
  }
  
  /*
   * Bounds-checked linear access, 0- or 1-based 
   */ 
  def apply(i0:Long):Int = {
    val off = 0 //FIX ME
    val i = i0 - off
    if (i < 0 || i >= length) {
      throw new IndexOutOfBoundsException(""+(i+off)+" >= ("+length+")");
    } else {
      get_(i0)
    }
  } 
    /*
   * Bounds-checked matrix access, 0- or 1-based 
   */ 
  def extract_seed(r0:Long, c0:Long):Int = {
    extract_seed(r0 + c0*nrows)
  }
  
  /*
   * Bounds-checked linear access, 0- or 1-based 
   */ 
  def extract_seed(i0:Long):Int = {
    val off = 0 //FIX ME
    val i = i0 - off
    if (i < 0 || i + 15 >= length) {
      throw new IndexOutOfBoundsException(""+(i+off + 15)+" >= ("+length+")");
    } else {
      val bitsPos = (i0 << 1)  //bits left in current Int in terms of raw bits
      val currInt : Int = Utils.divide(bitsPos+32,32) - 1
      var ret = data(currInt)      
      if (Utils.mode(bitsPos,32) != 0){       
        val numBitsInCurrInt : Int = 32 - Utils.mode(bitsPos,32)
        //val nextMask = 1<<31>>(31-numBitsInCurrInt)  //in the format of 11100       
        //val thisMask = ~nextMask                     //in the format of 00011
        val thisMask = Utils.maskTailingOnes(numBitsInCurrInt)
        val nextMask = ~thisMask

        var thisIntPart = data(currInt) & thisMask
        thisIntPart = thisIntPart << (32 - numBitsInCurrInt)
        var nextIntPart = data(currInt + 1) & nextMask
        //nextIntPart = (nextIntPart >> numBitsInCurrInt) & ~(1<<31>>(numBitsInCurrInt - 1))
        nextIntPart = (nextIntPart >> numBitsInCurrInt) & Utils.maskTailingOnes(32-numBitsInCurrInt)
        ret = thisIntPart | nextIntPart
      }
      ret
    }
  } 
  
  /*
   * Unchecked 0-based matrix access
   */ 
  def get_(r:Long, c:Long):Int = {
      val bitIdx = r+c*nrows
      get_(bitIdx)
  }
  
  /*
   * Unchecked 0-based matrix access
   */ 
  def get_(bitIdx:Long):Int = {
      val nIntCurr : Int = Utils.divide(bitIdx,16)
      val pos : Int = Utils.mode(bitIdx,16)
      extract2BitsAtPos(pos,data(nIntCurr))
  }  
  /*
   * Helper method for apply method
   * First move left to eliminate tuples in the front
   * Then move right to set lowest 2 bits
   * 'bit and' with 0x11 to erase higher 30 bits
   */
  def extract2BitsAtPos(pos:Int,data:Int):Int ={
    ((data << pos * 2) >> 30) & 3 
  }
  
    /*
   * Update a matrix value, m(r,c) = v, 0- or 1-based 
   */
  def _update(r0:Long, c0:Long, v:Int):Int = {
    val off = 0  //FIX ME
    val r = r0 - off
    val c = c0 - off
    if (r < 0 || r >= nrows || c < 0 || c >= ncols) {
      throw new IndexOutOfBoundsException("("+(r+off)+","+(c+off)+") vs ("+nrows+","+ncols+")");
    } else {
      set_(r,c,v)
     }
    
    v
  }
  /*
   * Update a matrix value with linear access, m(i) = v
   */
  def _update(i0:Long, v:Int):Int = {
    val off = 0  //FIX ME
    val i = i0 - off
    if (i < 0 || i >= length) {
      throw new IndexOutOfBoundsException(""+(i+off)+" vs ("+length+")");
    } else {
      set_(i,v)
    }
    v
  }
  
  def set_(r:Long, c:Long, v:Int):Int = {
    set_(r+c*nrows, v)
    v
  } 
  
  def set_(i:Long, v:Int):Int ={
    val bitIdx = i
    val nIntCurr : Int = Utils.divide(bitIdx,16)
    val pos : Int = Utils.mode(bitIdx,16)
    data(nIntCurr) = update2BitsAtPos(pos,data(nIntCurr),v)
    v
  }
  
    /*
   * Helper method for update method
   * First move v to Pos
   * Then make a mask that erase value at Pos
   * erase value at Pos and then set it to v
   */
  def update2BitsAtPos(pos:Int,data:Int,v:Int):Int ={
    if ((v & ~3) >0){
       throw new RuntimeException("update value must be 2-bits")
    }
    val vAtPos = v << (32 - pos*2 - 2)
    val eraser = ~(3 << (32 - pos*2 - 2))
    data & eraser | vAtPos
  }
  
  def indexOf2(query:Int):(Long, Long) = {
    val off = 0  //FIX ME
    val oneDIdx : Long = indexOf(query)
    if (oneDIdx < 0){
      (-1,-1)
    }else{
      (oneDIdx % nrows + off, oneDIdx / nrows + off)
    }
  }
  
  def indexOf(query:Int):Long = {
    if ((query & ~3)> 0){
      throw new RuntimeException("cannot look for a value longer than 2 bits")
    }
    val off = 0  //FIX ME
    var nIntCurr : Int = 0
    var pos : Int = -1
    
    breakable{
      while(nIntCurr < data.length){  //iterate through all integers in data Array, break if a match is found inside of current element
        val currInt : Int = data(nIntCurr)
        pos = indexOfInInteger(query, currInt)
        if (pos >=0){
          break
        }
        nIntCurr += 1
      }
    }
    if (nIntCurr == data.length){  //not found
      -1
    }else{
       16*nIntCurr + pos 
    }
  }
  
  def indexOfInInteger(query:Int, data:Int):Int ={
    var maskCutTail : Int = 3 << 30
    var i : Int = 0
    breakable{
      while(i<16){
        val dataValue : Int = (data << i*2) & maskCutTail  // move interesting 2 bits to the very front and cut tail
        if (query << 30 == dataValue) {             // move query 2 bits to the very front to compare
          break
        }
        i = i+1
      }
    }
    if (i == 16){  //not found
      -1
    }else{
      i
    }
  }
  
 /*
  * Transpose
  */
  def gt:B2Mat  = {
    var out:B2Mat = new B2Mat(ncols,nrows, new Array[Int](nInt))
    var row_idx : Long= 0
    var col_idx : Long= 0
    while(col_idx < ncols){
      row_idx = 0
      while(row_idx < nrows){
        out._update(col_idx, row_idx, this(row_idx, col_idx))
        row_idx += 1
      }
      col_idx += 1
    }
    out
  }
  
  
  
  
  
 /*
  * Stack matrices vertically
  */
  def gvertcat(a:B2Mat):B2Mat = 
    if (ncols != a.ncols) {
      throw new RuntimeException("ncols must match")
    } else {
      val newLength = ((length + a.length)/16).toInt
      var out = new B2Mat(nrows + a.nrows, ncols, new Array[Int](newLength))
      var col_idx : Long= 0
      var row_idx : Long= 0
      while(col_idx < ncols){
        row_idx = 0;
        while(row_idx < nrows){  //copy the original mat above
          out._update(row_idx, col_idx, this(row_idx,col_idx))
          row_idx = row_idx + 1;
        }
        while(row_idx < nrows + a.nrows){  //copy the other mat below
          out._update(row_idx, col_idx, a(row_idx - nrows, col_idx))
          row_idx = row_idx + 1;
        }
        col_idx = col_idx + 1;
      }
      out.nextReal2BitsPos = this.nextReal2BitsPos + a.nextReal2BitsPos ;  //there're two elements at position 0 in each B2Mat 
      out
    }
  
  
  
 /*
  * Stack matrices horizontally
  */ 
  def ghorzcat(a:B2Mat):B2Mat= 
    if (nrows != a.nrows) {
      throw new RuntimeException("nrows must match")
    } else {
      val newLength = nInt + a.nInt
      //var out = new B2Mat(nrows,ncols+a.ncols, new Array[Int](newLength))
      var out = new B2Mat(nrows,ncols+a.ncols, Array.fill[Int](newLength){0})
      
      if (nrows*ncols != nInt << 4){  //is multiple of 16
        val num_padding_bits : Int = (((nInt << 4) - nrows * ncols) * 2).toInt
        this.data(nInt-1) = squeezeBitsForward(this.data(nInt-1),a.data(0),num_padding_bits)
        for (i <- 0 to a.nInt - 2){
          a.data(i) = squeezeBitsForward(a.data(i),a.data(i+1),num_padding_bits)
        }
        //a.data(a.nInt-1) = a.data(a.nInt - 1) & ((1<<31)>>(31-num_padding_bits))
        a.data(a.nInt-1) = a.data(a.nInt - 1) & (Utils.maskTailingZeros(num_padding_bits))
        val s : String = a.data(nInt-1).toBinaryString
        val s1 : String = a.data(nInt-1).toBinaryString
      }
      System.arraycopy(data, 0, out.data, 0, nInt)
      System.arraycopy(a.data, 0, out.data, nInt, a.nInt)
      out.nextReal2BitsPos = this.nextReal2BitsPos + a.nextReal2BitsPos ;  //there're two elements at position 0 in each B2Mat 
      out
    }
  
  def squeezeBitsForward(thisInt:Int, nextInt: Int, num_padding_bits:Int):Int={
    //val tail_mask : Int = ~((1<<31) >>(31 - num_padding_bits))  //in the format of leading 0, tailing 1. "00011
    val tail_mask : Int = Utils.maskTailingOnes(num_padding_bits)  //in the format of leading 0, tailing 1. "00011
    val tail_maskString : String = tail_mask.toBinaryString
    val tail : Int = (nextInt >> (32-num_padding_bits)) & tail_mask;
    val ret = (thisInt>>num_padding_bits<<num_padding_bits) | tail;  //erase the tailing bits, set them to tail
    val ret_String : String = ret.toBinaryString
    ret
  }
  
  /*
  * Count number of non-zero entries
  */
  def nnz:Long = {
    var count:Long = 0
    var i = 0
    var j = 0
    var currInt = 0
    var mask : Int = 3 
    for (i <- 0 to nInt -1) {
      currInt = data(i)
      mask = 3
      for(j <- 0 to 15 ){
        val curr2Bits : Int = currInt & mask
        if (curr2Bits != 0){
          count += 1
        }
        mask = mask << 2
      }
    }
    count
  }
  
  /*
  * Helper function for find functions
  */ 
  def findInds(out:LMat, off:Int):LMat = {
    var count = 0
    //var i = off
    var i = 0  //index over each int
    var j = 0  //index in each int over each 2bits
    var currInt = 0
    var mask : Int = 3
    var currIdx : Long = 0
    for (i <- 0 to nInt -1) {
      currInt = data(i)
      if (currInt > (1<<29) || currInt < 0){  // if the first 2bits are not zero
        currIdx = (i<<4) + j
        out.data(count) = currIdx
        count += 1
      }
      mask = 3<<28
      for(j <- 1 to 15 ){
        val curr2Bits : Int = currInt & mask
        if (curr2Bits != 0){
          currIdx = (i<<4) + j
          
          var currCount : Long = count
          val curr2BitsStr : String = curr2Bits.toBinaryString
          val data : Array[Long] = out.data
          val outDataLen : Int= out.data.length

          val local_i = i
          val local_count = count
          val local_data = out.data
          out.data(count) = currIdx
          count += 1
        }
        mask = mask >> 2
      }
    }
    out
  }
  
 /*
  * Find indices (linear) for all non-zeros elements
  */
  def find:LMat = {
    if (nnz > Int.MaxValue){
      throw new RuntimeException("the number of non-zero elements is more than Int.MaxValue")
    }
    val num_nnz : Int = nnz.toInt
    var out = new LMat(1,num_nnz, new Array[Long](num_nnz))
    findInds(out,0)  //FIX ME, the second argument can be 1 when 1-based
  }    
  
  def printOne(i:Long):String = {
    val v = apply(i)
    "%d" format v
  }
  
  override def toString:String = {
    val sb:StringBuilder = new StringBuilder
    if (nrows == 1) {
      if (ncols > 0) sb.append(printOne(0))
      var i = 1
      while (i < math.min(20000, ncols)) {
      sb.append(",")
      sb.append(printOne(i))
      i += 1
      }
    } else {
      val nChars = 80-4  //FIX ME
      val maxRows = 640/nChars
      var maxCols = nChars
      var fieldWidth = 4
      var icols = 0
      while (icols < math.min(ncols, maxCols)) {
        var newWidth = fieldWidth
        for (j <- 0 until math.min(math.min(Int.MaxValue.toLong,nrows).toInt,maxRows)) newWidth = math.max(newWidth, 2+(printOne(j+nrows*icols).length))
        if ((icols+1)*newWidth < nChars) {
          fieldWidth = newWidth
          icols += 1
        } else {
          maxCols = icols
        }
      }     
      val somespaces = "                                             "
        for (i <- 0 until math.min(math.min(Int.MaxValue.toLong,nrows).toInt, maxRows)) {
          for (j <- 0 until math.min(math.min(Int.MaxValue.toLong,ncols).toInt, icols)) {
            val str = printOne(i+j*nrows)
            sb.append(somespaces.substring(0,fieldWidth-str.length)+str)
          }
          if (ncols > icols) {
            sb.append("...")
          }
          sb.append("\n")
        }
      if (nrows > maxRows) {
        for (j <- 0 until math.min(math.min(Int.MaxValue.toLong,ncols).toInt, maxCols)) {
          sb.append(somespaces.substring(0, fieldWidth-2)+"..")
        }
        sb.append("\n")
      }
    }
  sb.toString()
  }
  
}


object DNAEnum extends Enumeration{
  type DNAEnum = Value
  val A,T,C,G = Value
  //A-00 T-01 C-10 G-11
}
import DNAEnum._



