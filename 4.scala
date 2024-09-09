class Account2(private var balance: Double) {
  // Deposit money into the Account2
  def deposit(amount: Double): Unit = {
    require(amount > 0, "Deposit amount must be positive")
    balance += amount
  }
  // Withdraw money from the Account2
  def withdraw(amount: Double): Unit = {
    require(amount > 0, "Withdrawal amount must be positive")
    require(amount <= balance, "Insufficient funds")
    balance -= amount
  }
  // Transfer money from this Account2 to another Account2
  def transfer(amount: Double, targetAccount2: Account2): Unit = {
    require(amount > 0, "Transfer amount must be positive")
    require(amount <= balance, "Insufficient funds")
    this.withdraw(amount)
    targetAccount2.deposit(amount)
  }
  // Get the current balance
  def getBalance: Double = balance 
  // Print Account2 details
  override def toString: String = f"Account2 balance: $$${balance}%.2f"
}
// Define the Bank class
class Bank(private val Account2s: List[Account2]) {
  // List of Account2s with negative balances
  def Account2sWithNegativeBalances: List[Account2] = {
    Account2s.filter(Account2 => Account2.getBalance < 0)
  }
  // Calculate the sum of all Account2 balances
  def totalBalance: Double = {
    Account2s.map(_.getBalance).sum
  }
  // Apply interest to all Account2s based on their balance
  def applyInterest(): Unit = {
    Account2s.foreach { Account2 =>
      val balance = Account2.getBalance
      if (balance > 0) {
        Account2.deposit(balance * 0.05)  // Apply deposit interest
      } else {
        Account2.withdraw(balance * 0.1)  // Apply overdraft interest
      }
    }
  }
}
// Usage example
object BankTest extends App {
  val Account21 = new Account2(500.0)
  val Account22 = new Account2(-150.0)
  val Account23 = new Account2(300.0)
  val Account24 = new Account2(-50.0)

  val bank = new Bank(List(Account21, Account22, Account23, Account24))

  // List of Account2s with negative balances
  println("Account2s with negative balances:")
  bank.Account2sWithNegativeBalances.foreach(println)

  // Calculate the sum of all Account2 balances
  println(s"Total balance: $$${bank.totalBalance}")

  // Apply interest and print final balances
  bank.applyInterest()
  println("Final Account2 balances after interest:")
  bank.Account2sWithNegativeBalances.foreach(println)  // Print final balances
}
