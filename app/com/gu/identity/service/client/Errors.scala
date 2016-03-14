package com.gu.identity.service.client

import scala.util.control.NoStackTrace

sealed trait IdentityClientError extends Throwable {
  val message: String
  val description: Option[String]
  val context: Option[String]
  val cause: Option[Throwable]
}

sealed abstract class AbstractIdentityClientError(
    val message: String,
    val description: Option[String] = None,
    val context: Option[String] = None,
    val cause: Option[Throwable] = None)

  extends RuntimeException(
    IdentityClientError.makeExceptionMessage(message, description, context),
    cause.orNull
  )
  with IdentityClientError

object IdentityClientError {
  def makeExceptionMessage(
      message: String,
      description: Option[String],
      context: Option[String]): String =
    Seq(
      context.map(c => s"[$c]"),
      Some(message),
      description.map(d => s"- $d")
    ).flatten.mkString(" ")

  def apply(statusCode: Int, message: String): IdentityClientError =
    apply(statusCode, message, None, None)

  def apply(statusCode: Int, message: String, description: Option[String], context: Option[String] = None): IdentityClientError =
    if (isBadRequestError(statusCode)) ClientBadRequestError(message, description, context)
    else ClientGatewayError(message, description, context)


  private def isBadRequestError(statusCode: Int) =
    statusCode >= 400 && statusCode < 500
}

case class ClientGatewayError(
    override val message: String,
    override val description: Option[String] = None,
    override val context: Option[String] = None,
    override val cause: Option[Throwable] = None)
  extends AbstractIdentityClientError(message, description, context, cause)


// Bad request errors
trait ClientBadRequestError extends IdentityClientError

// Known bad request errors captured as constants
case object ClientInvalidCredentialsError
  extends AbstractIdentityClientError("Invalid email or password")
  with ClientBadRequestError
  with NoStackTrace

case object ClientRegistrationUsernameConflictError
  extends AbstractIdentityClientError("Username in use", context = Some("user.publicFields.username"))
  with ClientBadRequestError
  with NoStackTrace

case object ClientRegistrationEmailConflictError
  extends AbstractIdentityClientError("Email in use", context = Some("user.primaryEmailAddress"))
  with ClientBadRequestError
  with NoStackTrace


case class OtherClientBadRequestError(
    override val message: String,
    override val description: Option[String] = None,
    override val context: Option[String] = None)
  extends AbstractIdentityClientError(message, description, context)
  with ClientBadRequestError


object ClientBadRequestError {
  def apply(message: String): ClientBadRequestError =
    apply(message, None, None)

  def apply(message: String, description: Option[String], context: Option[String]): ClientBadRequestError =
    message match {
      case ClientInvalidCredentialsError.message => ClientInvalidCredentialsError
      case ClientRegistrationUsernameConflictError.message => ClientRegistrationUsernameConflictError
      case ClientRegistrationEmailConflictError.message => ClientRegistrationEmailConflictError
      case _ => OtherClientBadRequestError(message, description, context)
    }
}
