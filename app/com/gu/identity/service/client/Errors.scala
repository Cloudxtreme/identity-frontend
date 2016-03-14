package com.gu.identity.service.client

sealed trait IdentityClientError extends Throwable {
  val message: String
  val description: Option[String]
  val context: Option[String]
  val cause: Option[Throwable]
}

sealed abstract class AbstractIdentityClientError(
    message: String,
    description: Option[String],
    context: Option[String],
    cause: Option[Throwable])

  extends RuntimeException(
    IdentityClientError.makeExceptionMessage(message, description, context),
    cause.orNull
  )
  with IdentityClientError

private object IdentityClientError {
  def makeExceptionMessage(
      message: String,
      description: Option[String],
      context: Option[String]): String =
    Seq(
      context.map(c => s"[$c]"),
      Some(message),
      description.map(d => s"- $d")
    ).flatten.mkString(" ")
}

case class ClientGatewayError(
    message: String,
    description: Option[String] = None,
    context: Option[String] = None,
    cause: Option[Throwable] = None)
  extends AbstractIdentityClientError(message, description, context, cause)

case class ClientBadRequestError(
    message: String,
    description: Option[String] = None,
    context: Option[String] = None,
    cause: Option[Throwable] = None)
  extends AbstractIdentityClientError(message, description, context, cause)
