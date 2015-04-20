package BIDBioTest

object StringUtils {
   
  def splitCamelCase(s: String): String = {
   return s.replaceAll(
      String.format("%s|%s|%s",
         "(?<=[A-Z])(?=[A-Z][a-z])",
         "(?<=[^A-Z])(?=[A-Z])",
         "(?<=[A-Za-z])(?=[^A-Za-z])"
      ),
      " "
   ).replaceAll("  ", " ")
  }
 
}