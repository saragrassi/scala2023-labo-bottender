package Chat

/** This sealed trait represents a node of the tree.
  */
sealed trait ExprTree

/** Declarations of the nodes' types.
  */
object ExprTree:
  // TODO - Part 2 Step 3
  // Example cases
  case object Thirsty extends ExprTree
  case object Hungry extends ExprTree
  case object Greeting extends ExprTree
  case object Quel extends ExprTree
  case object Combien extends ExprTree
  case object Je extends ExprTree
  case object Pseudo extends ExprTree
  case object Commander extends ExprTree
  case object Connaitre extends ExprTree
  case object parseProduct extends ExprTree
  case class Identification(user: String) extends ExprTree
  case class And(left: ExprTree, right: ExprTree) extends ExprTree
  case class Or(left: ExprTree, right: ExprTree) extends ExprTree
  case class Product(num: Int, brand: String) extends ExprTree
  case class Order(num: Int, brand: String) extends ExprTree
  case class Price(num: Int, brand: String) extends ExprTree
  case class Payment(user: String, amount: Int) extends ExprTree
