package Chat

/** This sealed trait represents a node of the tree.
  */
sealed trait ExprTree:
  def show: String = this match
    case ExprTree.Greeting             => "Greeting"
    case ExprTree.Thirsty              => "Thirsty"
    case ExprTree.Hungry               => "Hungry"
    case ExprTree.Price(product)       => s"Price(${product.show})"
    case ExprTree.Order(products)      => s"Order(${products.show})"
    case ExprTree.GetBalance           => "GetBalance"
    case ExprTree.Identification(user) => s"Identification($user)"
    case ExprTree.Products(brand, num) => s"Products($num, $brand)"
    case ExprTree.Product(num, productType, brand) =>
      s"Product($num, $productType, ${brand.getOrElse("None")})"
    case ExprTree.And(left, right) => s"And(${left.show}, ${right.show})"
    case ExprTree.Or(left, right)  => s"Or(${left.show}, ${right.show})"

/** Declarations of the nodes' types.
  */
object ExprTree:
  // TODO - Part 2 Step 3
  case object Greeting extends ExprTree
  case object Thirsty extends ExprTree
  case object Hungry extends ExprTree
  case class Price(products: ExprTree) extends ExprTree
  case class Order(products: ExprTree) extends ExprTree
  case object GetBalance extends ExprTree
  case class Identification(user: String) extends ExprTree
  case class Products(num: Int, brand: String) extends ExprTree
  case class Product(num: Int, productType: String, brand: Option[String])
      extends ExprTree
  case class And(left: ExprTree, right: ExprTree) extends ExprTree
  case class Or(left: ExprTree, right: ExprTree) extends ExprTree
