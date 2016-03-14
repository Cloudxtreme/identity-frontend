package com.gu.identity.frontend.errors

import com.gu.identity.service.client.IdentityClientError

import scala.util.control.NoStackTrace


/**
 * Application error.
 */
trait AppException extends Throwable {
  val id: ErrorID
}

case class SeqAppExceptions(errors: Seq[AppException])
  extends AbstractAppException(
    message = errors.map(_.getMessage).mkString(", "),
    cause = errors.headOption)
  with NoStackTrace {
  val id = errors.headOption.map(_.id).getOrElse(UnexpectedErrorID)
}

private[errors] abstract class AbstractAppException(
    message: String,
    cause: Option[Throwable] = None)
  extends RuntimeException(message, cause.orNull)
  with AppException {
  def this(clientError: IdentityClientError) =
    this(clientError.message, clientError.cause)
}


trait BadRequestAppException extends AppException

// 4xx
private[errors] abstract class ServiceBadRequestAppException(
    clientError: IdentityClientError)
  extends AbstractAppException(clientError)
  with BadRequestAppException
  with NoStackTrace

// 503
private[errors] abstract class ServiceGatewayAppException(
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


// Forgery token errors (CSRF checks)
case class ForgeryTokenAppException(message: String)
  extends AbstractAppException(message)
  with BadRequestAppException {
  val id = ForgeryTokenErrorID
}
