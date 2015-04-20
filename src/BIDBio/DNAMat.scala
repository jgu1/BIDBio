package BIDBio
import scala.io.Source
import scala.collection.mutable.Set
import scala.collection.mutable.Map
import Utils._
case class DNAMat(nr : Long, nc : Long, data0 : Array[Int]) extends B2Mat(nr, nc, data0) {
  
  val FASTAIdentifier : Char = '>'
  val identifierSet = Set[Char]()
  identifierSet += FASTAIdentifier
  
  val DNACharIntMap = Map[Char,Int]()
  DNACharIntMap += ('A' -> 0)
  DNACharIntMap += ('C' -> 1)
  DNACharIntMap += ('G' -> 2)
  DNACharIntMap += ('T' -> 3)
  
  val DNAIntCharMap = Map[Int,Char]()
  DNAIntCharMap += (0 -> 'A')
  DNAIntCharMap += (1 -> 'C')
  DNAIntCharMap += (2 -> 'G')
  DNAIntCharMap += (3 -> 'T')
  
  def loadFile(fileName:String,append:Boolean = false) : B2Mat = {
 
   var numPaddingZeroBit : Int = 0
   if (append == false){
      this.data = Array[Int](0)  //clear current data
      this.nextReal2BitsPos = 0
      numPaddingZeroBit = 32
   }else{
      numPaddingZeroBit = ((nInt.toLong <<5) - nextReal2BitsPos<<1).toInt
      numPaddingZeroBit = Utils.mode(numPaddingZeroBit,32)
   }
   for (line <- Source.fromFile(fileName).getLines()){
      val firstChar = line.charAt(0)
      if (identifierSet.contains(firstChar) == false){  //this is not an identifier line
        val nChar = line.length()
        var iChar : Int = 0
        var currChar : Char = '.'
        while(iChar < nChar && nextReal2BitsPos < length ){  //while this line has stuff and B2Mat limit not reached
          currChar = line.charAt(iChar)
          if(numPaddingZeroBit == 0){
            this.data ++= Array[Int](0)
            nInt += 1
            numPaddingZeroBit = 32
          }
          this._update(nextReal2BitsPos, mapCharToInt(currChar))
          this.nextReal2BitsPos += 1
          numPaddingZeroBit -= 2
          iChar += 1
        }
      }
   }
   this.nInt = this.data.length
   this
  }

  def getNumEmptyBits(curr2BitsPos: Long) : Long = {
    (length - curr2BitsPos)<<1
  }
  
  def mapCharToInt(char:Char):Int={
    DNACharIntMap(char)
  }
  
  override def printOne(i:Long):String = {
    val v = apply(i)
    DNAIntCharMap(v).toString
  }

}