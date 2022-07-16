package controllers

import models.{User, UserDto}
import play.api.libs.json.{Json, OFormat}
import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents}

import javax.inject.Inject
import scala.collection.mutable

class UserController @Inject()(val controllerComponents: ControllerComponents)
  extends BaseController {

  private var usersList = new mutable.ListBuffer[User]()
  usersList += User(1, "Robert", "robert@domain.com")
  usersList += User(2, "Tomasz", "tomasz@domain.com")

  implicit val userFormatter: OFormat[User] = Json.format[User]
  implicit val userDtoFormatter: OFormat[UserDto] = Json.format[UserDto]

  def getAll: Action[AnyContent] = Action {
    if (usersList.isEmpty) {
      NoContent
    } else {
      Ok(Json.toJson(usersList))
    }
  }

  def getById(userId: Long): Action[AnyContent] = Action {
    val foundUser = usersList.find(_.id == userId)
    foundUser match {
      case Some(user) => Ok(Json.toJson(user))
      case None => NotFound
    }
  }

  def add(): Action[AnyContent] = Action { implicit request =>
    val requestBody = request.body
    val jsonObject = requestBody.asJson
    val newUser: Option[UserDto] =
      jsonObject.flatMap(
        Json.fromJson[UserDto](_).asOpt
      )

    newUser match {
      case Some(userDto) =>
        val nextId = usersList.map(_.id).max + 1
        val newUser = User(nextId, userDto.username, userDto.email)
        usersList += newUser
        Created(Json.toJson(newUser))
      case None =>
        BadRequest
    }
  }

  def updateById(userId: Long): Action[AnyContent] = Action { implicit request =>
    val requestBody = request.body
    val jsonObject = requestBody.asJson
    val updatedUser: Option[UserDto] =
      jsonObject.flatMap(
        Json.fromJson[UserDto](_).asOpt
      )

    updatedUser match {
      case Some(userDto: UserDto) =>
        val existingUser = usersList.find(_.id == userId)
        existingUser match {
          case Some(user) =>
            val updatedUser = user.copy(username = userDto.username, email = userDto.email)
            usersList = usersList.filterNot(_.id == userId)
            usersList += updatedUser
            Accepted(Json.toJson(updatedUser))
          case None => NotFound
        }
      case None =>
        BadRequest
    }
  }

  def deleteById(userId: Long): Action[AnyContent] = Action {
    usersList.filterInPlace(_.id != userId)
    Accepted
  }
}
