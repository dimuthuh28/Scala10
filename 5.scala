object LetterCount {
  // Function to count the total number of letter occurrences
  def countLetterOccurrences(words: List[String]): Int = {
    // Map each word to its length
    val lengths = words.map(word => word.length)
    
    // Reduce to sum up all lengths
    val totalOccurrences = lengths.reduce((acc, length) => acc + length)
    
    totalOccurrences
  }

  def main(args: Array[String]): Unit = {
    // Example usage
    val words = List("apple", "banana", "cherry", "date")
    val total = countLetterOccurrences(words)
    println(s"Total count of letter occurrences: $total")
  }
}
