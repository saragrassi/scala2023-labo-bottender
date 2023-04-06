package Data

trait ProductService:
  type BrandName = String
  type ProductName = String

  /** Get the price of a product for a given brand
    *
    * @param product
    *   the product name
    * @param brand
    *   the brand name
    * @return
    *   the price of the product for the given brand
    */
  def getPrice(product: ProductName, brand: BrandName): Double

  /** Get the default price of a product, using its default brand
    *
    * @param product
    *   the product name
    * @return
    *   the price of the product for its default brand
    */
  def getPrice(product: ProductName): Double

  /** Get the default brand of a product
    *
    * @param product
    *   the product name
    * @return
    *   the default brand of the product
    */
  def getDefaultBrand(product: ProductName): BrandName

class ProductImpl extends ProductService:
  // TODO - Part 2 Step 2
  private val products: Map[ProductName, Map[BrandName, Double]] = Map(
    "biere" -> Map(
      "boxer" -> 1.0,
      "farmer" -> 1.0,
      "wittekop" -> 2.0,
      "punkipa" -> 3.0,
      "jackhammer" -> 3.0,
      "tenebreuse" -> 4.0
    ),
    "croissant" -> Map(
      "maison" -> 2.0,
      "cailler" -> 2.0
    )
  )

  def getPrice(product: ProductName, brand: String): Double =
    products
      .getOrElse(
        product,
        throw new IllegalArgumentException(s"Unknown product $product")
      )
      .getOrElse(
        brand,
        throw new IllegalArgumentException(s"Unknown brand $brand")
      )
  def getPrice(product: ProductName): Double =
    products
      .getOrElse(
        product,
        throw new IllegalArgumentException(s"Unknown product $product")
      )
      .getOrElse(getDefaultBrand(product), 0.0)

  def getDefaultBrand(product: ProductName): BrandName = product match
    case "biere"     => "boxer"
    case "croissant" => "maison"
    case _ => throw new IllegalArgumentException(s"Unknown product $product")

end ProductImpl
