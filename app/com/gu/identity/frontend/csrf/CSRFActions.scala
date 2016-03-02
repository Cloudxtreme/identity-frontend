package com.gu.identity.frontend.csrf

import com.gu.identity.frontend.controllers.NoCache
import com.gu.identity.frontend.utils.ComposableActionBuilder
import play.api.mvc._
import play.filters.csrf.CSRF.ErrorHandler
import play.filters.csrf.{CSRFAddToken => PlayCSRFAddToken, CSRFCheck => PlayCSRFCheck}

import scala.concurrent.Future


sealed trait CSRFActions

case class CSRFAddToken(config: CSRFConfig) extends ComposableActionBuilder[Request] with CSRFActions {
  def invokeBlock[A](request: Request[A], block: (Request[A]) => Future[Result]): Future[Result] =
    NoCache(block(request))

  override def composeAction[A](action: Action[A]) =
    if (config.enabled)
      PlayCSRFAddToken(action, config.underlying)

    else
      action

}

case class CSRFCheck(config: CSRFConfig, errorHandler: CSRFErrorHandler = defaultErrorHandler) extends ComposableActionBuilder[Request] with CSRFActions {

  def invokeBlock[A](request: Request[A], block: (Request[A]) => Future[Result]): Future[Result] =
    NoCache(block(request))

  override def composeAction[A](action: Action[A]) =
    if (config.enabled)
      new CSRFCheckWrapperAction(action)

    else
      action

  protected def playCSRFCheckAction[A](wrapAction: Action[A]) =
    PlayCSRFCheck(wrapAction, playErrorHandler, config.underlying)

  lazy val playErrorHandler = new ErrorHandler {
    def handle(req: RequestHeader, msg: String): Future[Result] = errorHandler(req, msg)
  }


  /**
   * Adapts Plays CSRF Check Action to always use a UrlFormEncoded request.
   * Required to support other request body parsers, as we're using with
   * Sign in and Register. To use CSRF Check with a custom request body type,
   * mix-in the `RequestBodyWithCSRFToken` type.
   */
  private class CSRFCheckWrapperAction[A](action: Action[A]) extends Action[A] {

    type RequestBodyUrlFormEncoded = Map[String, Seq[String]]

    def parser: BodyParser[A] = action.parser

    def apply(request: Request[A]): Future[Result] = {
      val augmentedRequest = transformRequestBody(request)
      val proxyAction = new CSRFProxyAction(request)

      playCSRFCheckAction(proxyAction)
        .apply(augmentedRequest)
    }


    private class CSRFProxyAction(request: Request[A]) extends Action[RequestBodyUrlFormEncoded] {
      def parser = BodyParsers.parse.urlFormEncoded

      def apply(r: Request[RequestBodyUrlFormEncoded]): Future[Result] =
        action.apply(request)
    }


    private def transformRequestBody(request: Request[_]): Request[RequestBodyUrlFormEncoded] =
      request.map {
        case body: RequestBodyWithCSRFToken =>
          Map(config.tokenName -> Seq(body.csrfToken))

        case body: AnyContent if body.asFormUrlEncoded.isDefined => body.asFormUrlEncoded.get
        case body: AnyContent if body.asMultipartFormData.isDefined => body.asMultipartFormData.get.asFormUrlEncoded
        case body: Map[_, _] => body.asInstanceOf[RequestBodyUrlFormEncoded]
        case body: MultipartFormData[_] => body.asFormUrlEncoded
        case _ => Map.empty
      }
  }
}
