package com.gu.identity.frontend.models

import java.net.URI
import com.gu.identity.frontend.configuration.Configuration

import scala.util.Try

case class ReturnUrl private[models](
    uri: URI,
    isDefault: Boolean = false) {

  lazy val url: String = uri.toString

  lazy val toStringOpt: Option[String] =
    Some(url).filter(_ => isDefault)
}

object ReturnUrl {

  val domains = List(".theguardian.com", ".code.dev-theguardian.com", ".thegulocal.com")
  val invalidUrlPaths = List("/signin", "/register", "/register/confirm")

  def apply(returnUrlParam: Option[String], configuration: Configuration): ReturnUrl =
    apply(returnUrlParam, refererHeader = None, configuration, clientId = None)

  def apply(returnUrlParam: Option[String], refererHeader: Option[String], configuration: Configuration, clientId: Option[ClientID]): ReturnUrl =
    returnUrlParam
      .flatMap(uriOpt)
      .orElse(refererHeader.flatMap(uriOpt))
      .filter(validDomain)
      .filter(validUrlPath)
      .map(uri => ReturnUrl(uri))
      .getOrElse {
        default(configuration, clientId)
      }

  def default(configuration: Configuration, clientId: Option[ClientID]) = {
    val url = clientId match {
      case Some(GuardianMembersClientID) => configuration.membershipBaseUrl
      case _ => configuration.dotcomBaseUrl
    }

    val defaultUri = uri(url).getOrElse {
      sys.error("Invalid defaultReturnUrl specified in configuration")
    }

    ReturnUrl(defaultUri, isDefault = true)
  }

  /**
   * Parse a URI from a url string, ensure URISyntaxException is caught
   * by wrapping in a Try.
   */
  def uri(url: String): Try[URI] =
    Try(new URI(url))

  def uriOpt(url: String): Option[URI] =
    uri(url).toOption

  private[models] def validDomain(uri: URI): Boolean = {
    val hostname = uri.getHost
    domains.exists(s".$hostname".endsWith(_))
  }

  private[models] def validUrlPath(uri: URI): Boolean = {
    val urlPath = uri.getPath
    !invalidUrlPaths.contains(urlPath)
  }


  object FormMapping {
    import play.api.data.Mapping
    import play.api.data.Forms.{optional, text}

    def returnUrl(refererHeader: Option[String], configuration: Configuration): Mapping[ReturnUrl] =
      optional(text).transform(ReturnUrl.apply(_: Option[String], refererHeader, configuration), _.toStringOpt)
  }

}
