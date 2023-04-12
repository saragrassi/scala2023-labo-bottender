package Chat

/** This sealed trait represents a node of the tree.
  */
sealed trait ExprTree

/** Declarations of the nodes' types.
  */
object ExprTree:
  // TODO - Part 2 Step 3
  case object Greeting extends ExprTree
  case object Thirsty extends ExprTree
  case object Hungry extends ExprTree
  case class Price(product: ExprTree) extends ExprTree
  case class Order(Products: ExprTree) extends ExprTree
  case object GetBalance extends ExprTree
  case class Identification(user: String) extends ExprTree
  case class Products(num: Int, brand: String) extends ExprTree
  case class Product(num: Int, productType: String, brand: Option[String])
      extends ExprTree
  case class And(left: ExprTree, right: ExprTree) extends ExprTree
  case class Or(left: ExprTree, right: ExprTree) extends ExprTree
