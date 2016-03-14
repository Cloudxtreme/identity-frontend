package com.gu.identity.frontend.errors

import com.gu.identity.service.client.{ClientInvalidCredentialsError, ClientBadRequestError, ClientGatewayError, IdentityClientError}

import scala.util.control.NoStackTrace


sealed trait ErrorID {
  val key: String
}

case object SignInGatewayErrorID extends ErrorID { val key = "signin-error-gateway" }
case object SignInBadRequestErrorID extends ErrorID { val key = "signin-error-bad-request" }
case object SignInInvalidCredentialsErrorID extends ErrorID { val key = "signin-error-credentials" }

case object RegisterGatewayErrorID extends ErrorID { val key = "register-error-gateway" }
case object RegisterBadRequestErrorID extends ErrorID { val key = "register-error-bad-request" }

case object ForgeryTokenErrorID extends ErrorID { val key = "error-forgery-token" }

case object SignInActionBadRequestErrorID extends ErrorID { val key = "signin-error-bad-request" }

case object UnexpectedErrorID extends ErrorID { val key = "error-unexpected" }



/**
 * Application error.
 */
sealed trait AppException extends Throwable {
  val id: ErrorID
}

case class SeqAppExceptions(errors: Seq[AppException]) extends AppException {
  val id = errors.headOption.map(_.id).getOrElse(UnexpectedErrorID)
}

sealed abstract class AbstractAppException(
    message: String,
    cause: Option[Throwable] = None)
  extends RuntimeException(message, cause.orNull)
  with AppException {
  def this(clientError: IdentityClientError) =
    this(clientError.message, clientError.cause)
}

sealed trait BadRequestAppException extends AppException

// 4xx
sealed abstract class ServiceBadRequestAppException(
    clientError: IdentityClientError)
  extends AbstractAppException(clientError)
  with BadRequestAppException
  with NoStackTrace

// 503
sealed abstract class ServiceGatewayAppException(
    clientError: IdentityClientError)
  extends AbstractAppException(clientError)
  with NoStackTrace


// 500
case class UnexpectedAppException(
    message: String,
    cause: Option[Throwable] = None)
  extends AbstractAppException(message, cause) {
  val id = UnexpectedErrorID
}




// Errors when Signing In
sealed trait SignInServiceAppException extends AppException

object SignInServiceAppException {
  def apply(clientError: IdentityClientError): SignInServiceAppException =
    clientError match {
      case ClientInvalidCredentialsError => SignInInvalidCredentialsAppException
      case err: ClientBadRequestError => SignInServiceBadRequestException(clientError)
      case err: ClientGatewayError => SignInServiceGatewayAppException(clientError)
    }
}

case class SignInServiceGatewayAppException(
    clientError: IdentityClientError)
  extends ServiceGatewayAppException(clientError)
  with SignInServiceAppException {

  val id = SignInGatewayErrorID
}

case class SignInServiceBadRequestException(
    clientError: IdentityClientError)
  extends ServiceBadRequestAppException(clientError)
  with SignInServiceAppException {

  val id = SignInBadRequestErrorID
}

case object SignInInvalidCredentialsAppException
  extends ServiceBadRequestAppException(ClientInvalidCredentialsError)
  with SignInServiceAppException {

  val id = SignInInvalidCredentialsErrorID
}


// Errors when Registering
sealed trait RegisterServiceAppException extends AppException

object RegisterServiceAppException {
  def apply(clientError: IdentityClientError): RegisterServiceAppException =
    clientError match {
      case err: ClientBadRequestError => RegisterServiceBadRequestException(clientError)
      case err: ClientGatewayError => RegisterServiceGatewayAppException(clientError)
    }
}

case class RegisterServiceGatewayAppException(
    clientError: IdentityClientError)
  extends ServiceGatewayAppException(clientError)
  with RegisterServiceAppException {

  val id = RegisterGatewayErrorID
}

case class RegisterServiceBadRequestException(
    clientError: IdentityClientError)
  extends ServiceBadRequestAppException(clientError)
  with RegisterServiceAppException {

  val id = RegisterBadRequestErrorID
}


// Forgery token errors (CSRF checks)
case class ForgeryTokenAppException(message: String) extends AbstractAppException(message) {
  val id = ForgeryTokenErrorID
}

case class SignInActionBadRequestAppException(message: String)
  extends AbstractAppException(message)
  with BadRequestAppException {
  val id = SignInActionBadRequestErrorID
}

