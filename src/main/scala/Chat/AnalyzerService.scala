package Chat
import Data.{AccountService, ProductService, Session}

class AnalyzerService(productSvc: ProductService, accountSvc: AccountService):
  import ExprTree._

  /** Compute the price of the current node, then returns it. If the node is not
    * a computational node, the method returns 0.0. For example if we had a "+"
    * node, we would add the values of its two children, then return the result.
    * @return
    *   the result of the computation
    */
  // TODO - Part 2 Step 3
  def computePrice(t: ExprTree): Double = t match
    case Price(product)       => computePrice(product)
    case Products(num, brand) => num * productSvc.getPrice(brand)
    case Product(num, productType, brand) =>
      num * productSvc.getPrice(
        productType,
        brand.getOrElse(productSvc.getDefaultBrand(productType))
      )
    case _ => 0.0

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
      case Order => "Order"

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
        val price = computePrice(product);
        s"Cela coûte CHF $price."

end AnalyzerService
