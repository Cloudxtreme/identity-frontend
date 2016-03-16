package com.gu.identity.frontend.models

import com.gu.identity.frontend.errors.{SeqAppExceptions, AppException}
import play.api.mvc.Call

object UrlBuilder {

  type UrlParameter = (String, String)
  type UrlParameters = Seq[UrlParameter]

  private def encode = java.net.URLEncoder.encode(_: String, "UTF8")

  def apply(baseUrl: String, params: UrlParameters): String =
    params.headOption match {
      case None => baseUrl
      case _ => {
        val paramString = params.map {
          case (key, value) => s"$key=${encode(value)}"
        }.mkString("&")

        s"$baseUrl?$paramString"
      }
    }

  def apply(baseUrl: String, returnUrl: ReturnUrl): String =
    apply(baseUrl, returnUrl, skipConfirmation = None, clientId = None, group = None)

  def apply(baseUrl: String, returnUrl: ReturnUrl, clientId: Option[ClientID]): String =
    apply(baseUrl, returnUrl, skipConfirmation = None, clientId, group = None)

  def apply(baseUrl: String, returnUrl: ReturnUrl, skipConfirmation: Option[Boolean], clientId: Option[ClientID], group: Option[String]): String =
    apply(baseUrl, buildParams(Some(returnUrl), skipConfirmation, clientId, group))

  def apply(baseUrl: String, returnUrl: Option[ReturnUrl], skipConfirmation: Option[Boolean], clientId: Option[ClientID], group: Option[String], error: AppException): String =
    apply(baseUrl, buildParams(returnUrl, skipConfirmation, clientId, group, Some(error)))

  def apply(baseUrl: String, error: AppException): String =
    apply(baseUrl, buildParams(error = Some(error)))

  def apply(call: Call, params: UrlParameters): String =
    apply(call.url, params)

  def apply(call: Call, returnUrl: ReturnUrl, clientId: Option[ClientID]): String =
    apply(call, returnUrl, None, clientId, None)

  def apply(call: Call, returnUrl: ReturnUrl, skipConfirmation: Option[Boolean], clientId: Option[ClientID], group: Option[String]): String =
    apply(call.url, returnUrl, skipConfirmation, clientId, group)


  private def buildParams(
      returnUrl: Option[ReturnUrl] = None,
      skipConfirmation: Option[Boolean] = None,
      clientId: Option[ClientID] = None,
      group: Option[String] = None,
      error: Option[AppException] = None): UrlParameters = {
    Seq(
      returnUrl.flatMap(_.toStringOpt).map("returnUrl" -> _),
      skipConfirmation.map("skipConfirmation" -> _.toString),
      clientId.map("clientId" -> _.id),
      group.map("group" -> _)
    ).flatten ++ error.map(errorToUrlParameters).getOrElse(Seq.empty)
  }

  private def errorToUrlParameters(error: AppException): UrlParameters =
    error match {
      case SeqAppExceptions(errors) => errors.flatMap(errorToUrlParameters)
      case e => Seq("error" -> e.id.key)
    }
}
