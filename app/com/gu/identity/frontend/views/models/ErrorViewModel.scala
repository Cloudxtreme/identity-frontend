package com.gu.identity.frontend.views.models

import com.gu.identity.frontend.errors.{ErrorID, ErrorIDs}
import play.api.i18n.Messages

case class ErrorViewModel(id: String, message: String) extends ViewModel

object ErrorViewModel {

  import ErrorIDs._

  def apply(id: String): ErrorViewModel = {
    ErrorViewModel(id, getErrorMessage(id))
  }

  val errorMessages: Map[ErrorID, String] = Map(
    SignInGatewayErrorID -> "There was a problem signing in; please try again.",
    SignInBadRequestErrorID -> "There was a problem signing in; please try again.",
    SignInInvalidCredentialsErrorID -> "Incorrect email or password; please try again.",
    SignInInvalidCaptchaErrorID -> "Problem with the captcha process; please try again.",
    SignInActionBadRequestErrorID -> "There was a problem with your request; please try again.",

    RegisterGatewayErrorID -> "There was a problem creating your account; please try again.",
    RegisterBadRequestErrorID -> "One or more inputs was not valid; please try again.",
    RegisterActionBadRequestErrorID -> "One or more inputs was not valid; please try again.",
    RegisterUsernameConflictErrorID -> "The username you selected is already in use; please try another one.",
    RegisterEmailConflictErrorID -> "This email is already in use; please check you do not already have an account.",
    RegisterActionInvalidFirstNameErrorID -> nonEmptyField("first name"),
    RegisterActionInvalidLastNameErrorID -> nonEmptyField("last name"),
    RegisterActionInvalidEmailErrorID -> "Invalid email address; please try again.",
    RegisterActionInvalidUsernameErrorID -> "Invalid username; your username must be between 6 and 20 characters long and contain only letters and numbers.",
    RegisterActionInvalidPasswordErrorID -> "Invalid password; your password must be between 6 and 20 characters long.",

    ResetPasswordGatewayErrorID -> "There was a problem resetting your password; please try again",
    ResetPasswordBadRequestErrorID -> "There was a problem resetting your password; please try again.",
    ResetPasswordInvalidEmailErrorID -> nonEmptyField("email"),

    ForgeryTokenErrorID -> "Forgery token invalid; please try again.",
    UnexpectedErrorID -> "There was an unexpected problem; please try again."
  )

  lazy val errorMessagesByKey: Map[String, String] =
    errorMessages.map {
      case (k, v) => k.key -> v
    }

  private def nonEmptyField(fieldName: String) = s"${fieldName.capitalize} field must not be blank."

  val default: String = "There was an unexpected problem; please try again."

  private def getErrorMessage(id: String) = errorMessagesByKey.getOrElse(id, default)
}
