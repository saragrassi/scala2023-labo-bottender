package Data

trait ProductService:
  type BrandName = String
  type ProductName = String

  def getPrice(product: ProductName, brand: BrandName): Double
  def getDefaultBrand(product: ProductName): BrandName

class ProductImpl extends ProductService:
  def getPrice(product: ProductName, brand: String): Double = ???
  def getDefaultBrand(product: ProductName): BrandName = ???
end ProductImpl
