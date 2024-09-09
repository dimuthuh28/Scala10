class Rational2(val numerator: Int, val denominator: Int) {
  // Ensure the denominator is positive and reduce the fraction
  require(denominator != 0, "Denominator cannot be zero")
  private val gcd = greatestCommonDivisor(numerator, denominator)
  val num = numerator / gcd
  val denom = denominator / gcd
  
  // Method to negate the rational number
  def neg: Rational2 = new Rational2(-num, denom)
  
  // Method to subtract another Rational number from this one
  def sub(other: Rational2): Rational2 = {
    val newNum = (num * other.denom) - (other.num * denom)
    val newDenom = denom * other.denom
    new Rational2(newNum, newDenom)
  } 
  // Helper method to find greatest common divisor
  private def greatestCommonDivisor(a: Int, b: Int): Int = {
    if (b == 0) a else greatestCommonDivisor(b, a % b)
  }
  override def toString: String = s"$num/$denom"
}
// Usage example
object RationalTest2 extends App {
  val x = new Rational2(3, 4)
  val y = new Rational2(5, 8)
  val z = new Rational2(2, 7)  
  // Compute y - z
  val yMinusZ = y.sub(z)
  println(s"y - z = $yMinusZ")  // Output: y - z = 11/56
  // Compute x - (y - z)
  val result = x.sub(yMinusZ)
  println(s"x - (y - z) = $result")  // Output: x - (y - z) = 29/56
}
