package BIDBioTest

import BIDBio.DNAMat

object DNAMatTest {
  def main(args: Array[String]){
    val DNAMat = new DNAMat(3,4,null)
    val fileName = "/home/jgu1/WorkSpace/eclipseWorkSpace/BIDMatDataTypeTest/src/BIDBioTest/testFile"
    /*
     * $cat /home/jgu1/WorkSpace/eclipseWorkSpace/BIDMatDataTypeTest/src/BIDBioTest/testFile
     * >First Line
        CGCAACGT
        TGCA
        TTTTGTTTTTTTTTTT
     */
    DNAMat.loadFile(fileName)
    println(DNAMat(0))
    println(DNAMat(1))
    println(DNAMat(2))
    println(DNAMat(3))
    println(DNAMat(4))
    println(DNAMat(5))
    println(DNAMat(6))
    println(DNAMat(7))
    println(DNAMat(8))
    println(DNAMat(9))
    println(DNAMat(10))
    println(DNAMat(11))
    println(DNAMat)
    println("haha")
    DNAMat.loadFile(fileName, true)
   // DNAMat.loadFile(fileName,true)
  //  println("###"+DNAMat(12))
                
  }
}