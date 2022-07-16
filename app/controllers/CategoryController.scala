package controllers

import models.{Category, CategoryDto}
import play.api.libs.json.{Json, OFormat}
import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents}

import javax.inject.Inject
import scala.collection.mutable

class CategoryController @Inject()(val controllerComponents: ControllerComponents)
  extends BaseController {

  private var categoriesList = new mutable.ListBuffer[Category]()
  categoriesList += Category(1, "Phone")
  categoriesList += Category(2, "Tablet")

  implicit val categoryFormatter: OFormat[Category] = Json.format[Category]
  implicit val categoryDtoFormatter: OFormat[CategoryDto] = Json.format[CategoryDto]

  def getAll: Action[AnyContent] = Action {
    if (categoriesList.isEmpty) {
      NoContent
    } else {
      Ok(Json.toJson(categoriesList))
    }
  }

  def getById(categoryId: Long): Action[AnyContent] = Action {
    val foundCategory = categoriesList.find(_.id == categoryId)
    foundCategory match {
      case Some(category) => Ok(Json.toJson(category))
      case None => NotFound
    }
  }

  def add(): Action[AnyContent] = Action { implicit request =>
    val requestBody = request.body
    val jsonObject = requestBody.asJson
    val newCategory: Option[CategoryDto] =
      jsonObject.flatMap(
        Json.fromJson[CategoryDto](_).asOpt
      )

    newCategory match {
      case Some(categoryDto) =>
        val nextId = categoriesList.map(_.id).max + 1
        val newCategory = Category(nextId, categoryDto.name)
        categoriesList += newCategory
        Created(Json.toJson(newCategory))
      case None =>
        BadRequest
    }
  }

  def updateById(categoryId: Long): Action[AnyContent] = Action { implicit request =>
    val requestBody = request.body
    val jsonObject = requestBody.asJson
    val updatedCategory: Option[CategoryDto] =
      jsonObject.flatMap(
        Json.fromJson[CategoryDto](_).asOpt
      )

    updatedCategory match {
      case Some(categoryDto) =>
        val existingCategory = categoriesList.find(_.id == categoryId)
        existingCategory match {
          case Some(category) =>
            val updatedCategory = category.copy(name = categoryDto.name)
            categoriesList = categoriesList.filterNot(_.id == categoryId)
            categoriesList += updatedCategory
            Accepted(Json.toJson(updatedCategory))
          case None => NotFound
        }
      case None =>
        BadRequest
    }
  }

  def deleteById(categoryId: Long): Action[AnyContent] = Action {
    categoriesList.filterInPlace(_.id != categoryId)
    Accepted
  }
}
