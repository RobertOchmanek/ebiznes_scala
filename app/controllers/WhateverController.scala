package controllers

import models.{WhateverItem, WhateverItemDto}
import play.api.libs.json.Json
import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents}

import javax.inject.Inject
import scala.collection.mutable

//@Singleton
class WhateverController @Inject()(val controllerComponents: ControllerComponents)
  extends BaseController {

  private val whateverList = new mutable.ListBuffer[WhateverItem]()
  whateverList += WhateverItem(1, "Description 1")
  whateverList += WhateverItem(2, "Description 2")

  implicit val whateverItemFormatter = Json.format[WhateverItem]
  implicit val whateverItemDtoFormatter = Json.format[WhateverItemDto]

  def getAll(): Action[AnyContent] = Action {
    if (whateverList.isEmpty) {
      NoContent
    } else {
      Ok(Json.toJson(whateverList))
    }
  }

  def getById(itemId: Long) = Action {
    val foundItem = whateverList.find(_.id == itemId)
    foundItem match {
      case Some(whateverItem) => Ok(Json.toJson(whateverItem))
      case None => NotFound
    }
  }

  def updateById(itemId: Long) = Action { implicit request =>
    val content = request.body
    val jsonObject = content.asJson
    val updatedItem: Option[WhateverItemDto] =
      jsonObject.flatMap(
        Json.fromJson[WhateverItemDto](_).asOpt
      )

    updatedItem match {
      case Some(itemDto) =>
        val existingItem = whateverList.find(_.id == itemId)
        existingItem match {
          case Some(existingItem) =>
            val updatedItem = existingItem.copy(description = itemDto.description)
            whateverList.dropWhileInPlace(_.id == itemId)
            whateverList += updatedItem
            Accepted(Json.toJson(updatedItem))
          case None => NotFound
        }
      case None =>
        BadRequest
    }
  }

  def add() = Action { implicit request =>
    val content = request.body
    val jsonObject = content.asJson
    val newItem: Option[WhateverItemDto] =
      jsonObject.flatMap(
        Json.fromJson[WhateverItemDto](_).asOpt
      )

    newItem match {
      case Some(itemDto) =>
        val nextId = whateverList.map(_.id).max + 1
        val newItem = WhateverItem(nextId, itemDto.description)
        whateverList += newItem
        Created(Json.toJson(newItem))
      case None =>
        BadRequest
    }
  }

  def deleteById(itemId: Long) = Action {
    whateverList.filterInPlace(_.id != itemId)
    Accepted
  }
}
