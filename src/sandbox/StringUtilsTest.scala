package BIDBioTest

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
 
/**
 * Requires ScalaTest and JUnit4.
 */
@RunWith(classOf[JUnitRunner])
class StringUtilsTest extends FunSuite with BeforeAndAfter {
   
  test("splitCamelCase works on FooBarBaz") {
    val s = StringUtils.splitCamelCase("FooBarBaz")
    assert(s.equals("Foo Bar Baz"))
  }
 
  test("splitCamelCase works on a blank string") {
    val s = StringUtils.splitCamelCase("")
    assert(s.equals(""))
  }
 
  test("splitCamelCase fails with a null") {
    val e = intercept[NullPointerException] {
      val s = StringUtils.splitCamelCase(null)
    }
    assert(e != null)
  }
 
}