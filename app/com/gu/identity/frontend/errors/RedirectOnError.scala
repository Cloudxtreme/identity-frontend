package com.gu.identity.frontend.errors

import com.gu.identity.frontend.models.UrlBuilder
import com.gu.identity.frontend.request.RequestParameters.CoreSessionParameters
import com.gu.identity.frontend.utils.ComposableActionBuilder
import play.api.mvc._
import play.api.mvc.Results.SeeOther

import scala.concurrent.Future
import scala.util.control.NonFatal


/**
 * Recover from application errors. Other errors will be caught by the global
 * error handler, eg: 500s.
 */
case class RedirectOnError(route: String) extends ComposableActionBuilder[Request] {

  def invokeBlock[A](request: Request[A], block: (Request[A]) => Future[Result]): Future[Result] =
    block(request)
      .recover(recoverExceptions(request))(executionContext)


  /**
   * Catch known AppExceptions when the Action is composed and applied.
   * Play's CSRF checks when Action is applied.
   */
  override def composeAction[A](other: Action[A]): Action[A] =
    new Action[A] {
      def parser: BodyParser[A] = other.parser

      def apply(request: Request[A]): Future[Result] =
        other.apply(request).recover(recoverExceptions(request))(executionContext)
    }


  override def composeParser[A](other: BodyParser[A]): BodyParser[A] =
    BodyParser { requestHeader =>
      other.apply(requestHeader).recover {
        case ex: AppException => Left {
          redirectResultFromAppException(request = Request(requestHeader, ()), ex)
        }
      }(executionContext)
    }


  def recoverExceptions[A](request: Request[A]) = PartialFunction[Throwable, Result] {
    case ex: AppException => redirectResultFromAppException(request, ex)
    case NonFatal(ex) =>
      redirectResultFromAppException(request, UnexpectedAppException(s"Unexpected error: ${ex.getMessage}", Some(ex)))
  }


  def redirectResultFromAppException[A](request: Request[A], error: AppException): Result = {
    val url = request.body match {
      case CoreSessionParameters(returnUrl, skipConfirmation, clientId) =>
        UrlBuilder(route, returnUrl, skipConfirmation, clientId, error)

      case _ =>
        UrlBuilder(route, error)
    }

    SeeOther(url)
  }


}
