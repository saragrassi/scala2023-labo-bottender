package Chat
import Data.{AccountService, ProductService, Session}

class AnalyzerService(productSvc: ProductService, accountSvc: AccountService):
  import ExprTree._

  /** Computes the price of the given expression tree `t`.
    *
    * If the node is a `Product`, the method returns the price of the product.
    * If the node is an `And` node, the method recursively computes the price of
    * its two children and returns their sum. If the node is an `Or` node, the
    * method recursively computes the price of its two children and returns the
    * lowest price. If the node is not a computational node, the method returns
    * 0.0.
    *
    * @param t
    *   the expression tree to compute the price of
    * @return
    *   the price of the expression tree
    */
  // TODO - Part 2 Step 3
  def computePrice(t: ExprTree): (Double, ExprTree) = t match
    case Product(num, productType, brand) =>
      val price = num * productSvc.getPrice(
        productType,
        brand.getOrElse(productSvc.getDefaultBrand(productType))
      )
      (price, t)
    case And(left, right) =>
      val leftResult = computePrice(left)
      val rightResult = computePrice(right)
      (leftResult._1 + rightResult._1, t)
    case Or(left, right) =>
      val leftResult = computePrice(left)
      val rightResult = computePrice(right)
      if (leftResult._1 < rightResult._1) (leftResult._1, left)
      else (rightResult._1, right)
    case _ => (0.0, t)

  /** Helper to transform an order in a string for feedback to the user.
    *
    * @param e
    *   the expression tree to transform
    * @return
    *   the string representation of the order
    */
  def showExprTree(e: ExprTree): String = e match {
    case Product(num, "biere", brand) =>
      s"$num ${brand.getOrElse(productSvc.getDefaultBrand("biere"))}"
    case Product(num, productType, brand) =>
      s"$num $productType ${brand.getOrElse(productSvc.getDefaultBrand(productType))} "
    case And(left, right) =>
      s"${showExprTree(left)} et ${showExprTree(right)}"
    case _ => e.show
  }

  /** Return the output text of the current node, in order to write it in
    * console.
    * @param session
    *   the current session
    * @param t
    *   the current node
    * @return
    *   the output text of the current node
    */
  def reply(session: Session)(t: ExprTree): String =
    // you can use this to avoid having to pass the session when doing recursion
    val inner: ExprTree => String = reply(session)
    t match
      // TODO - Part 2 Step 3
      // Example cases
      case Greeting => "Bonjour, que puis-je faire pour vous ?"
      case Thirsty =>
        "Eh bien, la chance est de votre côté, car nous offrons les meilleures bières de la région !"
      case Hungry =>
        "Pas de soucis, nous pouvons notamment vous offrir des croissants faits maisons !"
      // case Price => "Price"
      // case Order => "Order"

      case GetBalance =>
        if (session.getCurrentUser.isEmpty)
          return "Veuillez d'abord vous identifier."
        else
          val user = session.getCurrentUser.getOrElse("inconnu")
          val balance = accountSvc.getAccountBalance(user)
          s"Le montant actuel de votre solde est de CHF ${balance}."

      case Identification(user) =>
        if (!accountSvc.isAccountExisting(user))
          accountSvc.addAccount(user, 30.0)
        session.setCurrentUser(user)
        "Bonjour, " + user + " !"

      case Price(product) =>
        val price = computePrice(product)._1
        s"Cela coûte CHF $price."

      case Order(product) =>
        if (session.getCurrentUser.isEmpty)
          return "Veuillez d'abord vous identifier."
        else
          val price = computePrice(product)._1;
          val order = computePrice(product)._2;
          val user = session.getCurrentUser.getOrElse("inconnu")
          val balance = accountSvc.getAccountBalance(user)
          if (balance < price)
            return "Vous n'avez pas assez d'argent sur votre compte."
          else
            val newbalance = accountSvc.purchase(user, price)
            s"Voici donc ${showExprTree(order)} ! Cela coûte CHF $price et votre nouveau solde est de CHF $newbalance."

      case _ => "Expression non reconnue."

end AnalyzerService
