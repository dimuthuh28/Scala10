class Rational(val numerator: Int, val denominator: Int) {
  // Ensure the denominator is positive and reduce the fraction
  require(denominator != 0, "Denominator cannot be zero")
  private val gcd = greatestCommonDivisor(numerator, denominator)
  val num = numerator / gcd
  val denom = denominator / gcd
  
  // Method to negate the rational number
  def neg: Rational = new Rational(-num, denom)
  
  // Helper method to find greatest common divisor
  private def greatestCommonDivisor(a: Int, b: Int): Int = {
    if (b == 0) a else greatestCommonDivisor(b, a % b)
  }
  
  override def toString: String = s"$num/$denom"
}
//example
object RationalTest extends App {
  val x = new Rational(3, 4)
  println(x)          
  println(x.neg)      
}
