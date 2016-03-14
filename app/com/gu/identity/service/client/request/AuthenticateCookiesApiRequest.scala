package com.gu.identity.service.client.request

import com.gu.identity.frontend.models.TrackingData
import com.gu.identity.service.client._


case class AuthenticateCookiesApiRequest private(
    url: String,
    override val body: Option[AuthenticateCookiesApiRequestBody],
    private val extraHeaders: HttpParameters = Nil,
    private val extraParameters: HttpParameters = Nil)
  extends ApiRequest {

  override val method = POST
  override val headers = Seq("Content-Type" -> "application/x-www-form-urlencoded") ++ extraHeaders
  override val parameters = Seq("format" -> "cookies") ++ extraParameters
}

case class AuthenticateCookiesApiRequestBody(email: String, password: String) extends ApiRequestBody

object AuthenticateCookiesApiRequest {
  private val emailRegex = "^.+@.+$".r

  private def isValidEmail(email: String): Boolean =
    emailRegex.findFirstIn(email).isDefined

  private def isValidPassword(password: String): Boolean =
    password.nonEmpty


  def endpoint(implicit configuration: IdentityClientConfiguration) =
    ApiRequest.apiEndpoint("auth")


  def apply(email: Option[String], password: Option[String], rememberMe: Boolean, trackingData: TrackingData)(implicit configuration: IdentityClientConfiguration): Either[ClientBadRequestError, AuthenticateCookiesApiRequest] =
    (email, password) match {
      case (Some(e), Some(p)) if isValidEmail(e) && isValidPassword(p) => Right {
        apply(AuthenticateCookiesApiRequestBody(e, p), rememberMe, trackingData)
      }
      case _ => Left {
        ClientBadRequestError("Invalid request")
      }
    }


  def apply(body: AuthenticateCookiesApiRequestBody, rememberMe: Boolean, trackingData: TrackingData)(implicit configuration: IdentityClientConfiguration): AuthenticateCookiesApiRequest = {
    lazy val extraHeaders = ApiRequest.commonApiHeaders(trackingData)
    lazy val extraParams = Seq(
      "persistent" -> rememberMe.toString
    ) ++ trackingData.parameters

    AuthenticateCookiesApiRequest(endpoint, Some(body), extraHeaders, extraParams)
  }

}
