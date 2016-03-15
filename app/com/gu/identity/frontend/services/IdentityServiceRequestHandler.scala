package com.gu.identity.frontend.services

import com.gu.identity.frontend.logging.{Logging => ApplicationLogging}
import com.gu.identity.service.client._
import com.gu.identity.service.client.models._
import com.gu.identity.service.client.request._
import play.api.libs.json.Json
import play.api.libs.json.Reads.jodaDateReads
import play.api.libs.ws.{WSResponse, WSClient}

import scala.concurrent.Future
import scala.util.control.NonFatal

import play.api.libs.concurrent.Execution.Implicits.defaultContext

class IdentityServiceRequestHandler (ws: WSClient) extends IdentityClientRequestHandler with ApplicationLogging {

  implicit val dateReads = jodaDateReads("yyyy-MM-dd'T'HH:mm:ssZ")

  implicit val apiErrorResponseErrorReads = Json.format[ApiErrorResponseError]
  implicit val apiErrorResponseReads = Json.format[ApiErrorResponse]

  implicit val authenticationCookieReads = Json.format[AuthenticationCookiesResponseCookie]
  implicit val authenticationCookiesListReads = Json.format[AuthenticationCookiesResponseCookieList]
  implicit val authenticationReads = Json.format[AuthenticationCookiesResponse]

  implicit val deauthenticationCookieReads = Json.format[DeauthenticationCookiesResponseCookie]
  implicit val deauthenticationCookiesListReads = Json.format[DeauthenticationCookiesResponseCookieList]
  implicit val deauthenticationReads = Json.format[DeauthenticationCookiesResponse]

  implicit val registerRequestBodyPublicFieldsFormat = Json.format[RegisterRequestBodyPublicFields]
  implicit val registerRequestBodyPrivateFieldsFormat = Json.format[RegisterRequestBodyPrivateFields]
  implicit val registerRequestBodyStatusFieldsFormat = Json.format[RegisterRequestBodyStatusFields]
  implicit val registerRequestBodyFormat = Json.format[RegisterRequestBody]

  implicit val sendResetPasswordEmailRequestBody = Json.format[SendResetPasswordEmailRequestBody]

  implicit val registerResponseUserGroupsFormat = Json.format[RegisterResponseUserGroups]
  implicit val registerResponseUserFormat = Json.format[RegisterResponseUser]
  implicit val registerResponseFormat = Json.format[RegisterResponse]

  implicit val userResponseGroupFormat = Json.format[UserGroup]
  implicit val userResponseStatusFieldsFormat = Json.format[StatusFields]
  implicit val userResponsePublicFieldsFormat = Json.format[PublicFields]
  implicit val userResponseLastActiveLocationFormat = Json.format[LastActiveLocation]
  implicit val userResponsePrivateFieldsFormat = Json.format[PrivateFields]
  implicit val userResponseCreationDateFormat = Json.format[CreationDate]
  implicit val userResponseUserFormat = Json.format[User]
  implicit val userResponseFormat = Json.format[UserResponse]

  implicit val assignGroupResponseFormat = Json.format[AssignGroupResponse]

  def handleRequest(request: ApiRequest): Future[Either[IdentityClientErrors, ApiResponse]] =
    ws.url(request.url)
      .withHeaders(request.headers.toSeq: _*)
      .withQueryString(request.parameters.toSeq: _*)
      .withRequestTimeout(10000)
      .withBody(request.body.map(handleRequestBody).getOrElse(""))
      .execute(request.method.toString)
        .map(handleResponse(request))
        .recoverWith {
          case NonFatal(err) => Future.failed {
            GatewayError(
              "Request Error",
              Some(s"Error executing ${request.method} request to: ${request.url} - ${err.getMessage}"),
              cause = Some(err)
            )
          }
        }

  def handleRequestBody(body: ApiRequestBody): String = body match {
    case b: RegisterRequestBody => Json.stringify(Json.toJson(b))
    case AuthenticateCookiesApiRequestBody(email, password) => encodeBody("email" -> email, "password" -> password)
    case b: SendResetPasswordEmailRequestBody => Json.stringify(Json.toJson(b))
  }

  private def encodeBody(params: (String, String)*) = {
    def encode = java.net.URLEncoder.encode(_: String, "UTF8")

    params.map(p => s"${p._1}=${encode(p._2)}").mkString("&")
  }

  def handleResponse(request: ApiRequest)(response: WSResponse): Either[IdentityClientErrors, ApiResponse] = request match {
    case r if isErrorResponse(response) => Left {
      handleErrorResponse(response)
    }

    case r: AuthenticateCookiesApiRequest =>
      response.json.asOpt[AuthenticationCookiesResponse]
        .map(Right.apply)
        .getOrElse(handleUnexpectedResponse(response))

    case r: RegisterApiRequest =>
      response.json.asOpt[RegisterResponse]
        .map(Right.apply)
        .getOrElse(handleUnexpectedResponse(response))

    case r: UserApiRequest =>
      response.json.asOpt[UserResponse]
        .map(Right.apply)
        .getOrElse(handleUnexpectedResponse(response))

    case r: AssignGroupApiRequest =>
      response.json.asOpt[AssignGroupResponse]
        .map(Right.apply)
        .getOrElse(handleUnexpectedResponse(response))

    case r: DeauthenticateApiRequest =>
      response.json.asOpt[DeauthenticationCookiesResponse]
        .map(Right.apply)
        .getOrElse(handleUnexpectedResponse(response))

    case r: SendResetPasswordEmailApiRequest =>
      if (response.status == 200) {
        Right(SendResetPasswordEmailResponse())
      }
      else {
        logger.warn(s"Unexpected response from server: ${response.status} ${response.statusText} ${response.body}")
        Left(Seq(GatewayError("Unexpected response from server")))
      }

    case _ => Left(Seq(GatewayError("Unsupported request")))
  }

  def isErrorResponse(response: WSResponse) =
    response.status >= 400 && response.status < 600

  def isBadRequestError(response: WSResponse) =
    response.status >= 400 && response.status < 500

  def handleErrorResponse(response: WSResponse): IdentityClientErrors =
    response.json.asOpt[ApiErrorResponse]
      .map(_.errors.map {
        case e if isBadRequestError(response) => BadRequest(e.message, e.description, e.context)
        case e => GatewayError(e.message, e.description, e.context)
      })
      .getOrElse {
        logger.warn(s"Unexpected error response: ${response.status} ${response.statusText} ${response.body}")

        Seq(
          if (isBadRequestError(response)) {
            BadRequest(s"Bad request: ${response.status} ${response.statusText}")

          } else {
            GatewayError(s"Unknown error: ${response.status} ${response.statusText}")
          }
        )
      }

  def handleUnexpectedResponse(response: WSResponse): Either[IdentityClientErrors, ApiResponse] = {
    logger.warn(s"Unexpected response from server: ${response.status} ${response.statusText} ${response.body}")
    Left(Seq(GatewayError("Unexpected response from server")))
  }
}
