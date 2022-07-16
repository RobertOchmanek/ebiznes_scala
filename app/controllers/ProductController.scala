package controllers

import models.{Product, ProductDto}
import play.api.libs.json.{Json, OFormat}
import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents}

import javax.inject.Inject
import scala.collection.mutable

class ProductController @Inject()(val controllerComponents: ControllerComponents)
  extends BaseController {

  private var productsList = new mutable.ListBuffer[Product]()
  productsList += Product(1, "iPhone", "Description 1", 9.99)
  productsList += Product(2, "iPad", "Description 2", 19.99)

  implicit val productFormatter: OFormat[Product] = Json.format[Product]
  implicit val productDtoFormatter: OFormat[ProductDto] = Json.format[ProductDto]

  def getAll: Action[AnyContent] = Action {
    if (productsList.isEmpty) {
      NoContent
    } else {
      Ok(Json.toJson(productsList))
    }
  }

  def getById(productId: Long): Action[AnyContent] = Action {
    val foundProduct = productsList.find(_.id == productId)
    foundProduct match {
      case Some(product) => Ok(Json.toJson(product))
      case None => NotFound
    }
  }

  def add(): Action[AnyContent] = Action { implicit request =>
    val requestBody = request.body
    val jsonObject = requestBody.asJson
    val newProduct: Option[ProductDto] =
      jsonObject.flatMap(
        Json.fromJson[ProductDto](_).asOpt
      )

    newProduct match {
      case Some(productDto) =>
        val nextId = productsList.map(_.id).max + 1
        val newProduct = Product(nextId, productDto.name, productDto.description, productDto.price)
        productsList += newProduct
        Created(Json.toJson(newProduct))
      case None =>
        BadRequest
    }
  }

  def updateById(productId: Long): Action[AnyContent] = Action { implicit request =>
    val requestBody = request.body
    val jsonObject = requestBody.asJson
    val updatedProduct: Option[ProductDto] =
      jsonObject.flatMap(
        Json.fromJson[ProductDto](_).asOpt
      )

    updatedProduct match {
      case Some(productDto) =>
        val existingProduct = productsList.find(_.id == productId)
        existingProduct match {
          case Some(product) =>
            val updatedProduct = product.copy(name = productDto.name, description = productDto.description, price = productDto.price)
            productsList = productsList.filterNot(_.id == productId)
            productsList += updatedProduct
            Accepted(Json.toJson(updatedProduct))
          case None => NotFound
        }
      case None =>
        BadRequest
    }
  }

  def deleteById(productId: Long): Action[AnyContent] = Action {
    productsList.filterInPlace(_.id != productId)
    Accepted
  }
}
