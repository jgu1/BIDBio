package BIDBioTest

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter

import BIDMat.LMat
import BIDBio.B2Mat
import BIDBio.DNAMat
/**
 * Requires ScalaTest and JUnit4.
 */
@RunWith(classOf[JUnitRunner])
class DNAMatSuite extends FunSuite with BeforeAndAfter {
  test("test loading file from disk"){
    val dNAMat = new DNAMat(4,3,null)
    val fileName = "/home/jgu1/WorkSpace/eclipseWorkSpace/BIDBio/src/BIDBioTest/testFile"
    dNAMat.loadFile(fileName)
    /*
     * $cat /home/jgu1/WorkSpace/eclipseWorkSpace/BIDMatDataTypeTest/src/BIDBioTest/testFile
     * >First Line
        CGCAACGT
        TGCA
        TTTTGTTTTTTTTTTT
     */
    val intValue : Int = 0x641be400
    val expected = new DNAMat(4,3,Array[Int](intValue))
    /*    print(expected)
     *    C   A   T
          G   C   G
          C   G   C
          A   T   A
     */
    assert(dNAMat.toString.==(expected.toString))
  }

  test("test append file to existing DNAMat"){
    val dNAMat = new DNAMat(4,7,null)
    val fileName = "/home/jgu1/WorkSpace/eclipseWorkSpace/BIDBio/src/BIDBioTest/testFile"
    val appendFileName = "/home/jgu1/WorkSpace/eclipseWorkSpace/BIDBio/src/BIDBioTest/appendFile"
    /*
     * $cat /home/jgu1/WorkSpace/eclipseWorkSpace/BIDMatDataTypeTest/src/BIDBioTest/appendFile
     * >Append File
        TTTTGTTTTTTTTTTT
     */
    dNAMat.loadFile(fileName)
    dNAMat.loadFile(appendFileName,true)
    //print(dNAMat)
    val intValue : Int = 0x641be4ff
    val appendIntValue : Int = 0xbfffff00
    val expected = new DNAMat(4,7,Array[Int](intValue, appendIntValue))
    //val expected = new DNAMat(4,3,Array[Int](intValue)).ghorzcat(new DNAMat(4,4,Array[Int](appendIntValue)))
       /*    print(expected)
             C   A   T   T   G   T   T
             G   C   G   T   T   T   T
             C   G   C   T   T   T   T
             A   T   A   T   T   T   T
        */
    assert(dNAMat.toString.==(expected.toString))
  }
}