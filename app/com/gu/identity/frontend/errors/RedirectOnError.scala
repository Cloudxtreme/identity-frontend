package com.gu.identity.frontend.errors

import com.gu.identity.frontend.utils.ComposableActionBuilder
import play.api.mvc._
import play.api.mvc.Results.SeeOther

import scala.concurrent.Future


/**
 * Recover from application errors. Other errors will be caught by the global
 * error handler, eg: 500s.
 */
case class RedirectOnError(route: String) extends ComposableActionBuilder[Request] {

  def invokeBlock[A](request: Request[A], block: (Request[A]) => Future[Result]): Future[Result] =
    block(request)
      .recover(recoverKnownAppExceptions(request))(executionContext)


  /**
   * Catch known AppExceptions when the Action is composed and applied.
   * Play's CSRF checks when Action is applied.
   */
  override def composeAction[A](other: Action[A]): Action[A] =
    new Action[A] {
      def parser: BodyParser[A] = other.parser

      def apply(request: Request[A]): Future[Result] =
        other.apply(request).recover(recoverKnownAppExceptions(request))(executionContext)
    }


  def recoverKnownAppExceptions[A](request: Request[A]) = PartialFunction[Throwable, Result] {
    case SeqAppExceptions(errors) => SeeOther(route + "?error=multi")
    case ex: AppException => redirectResultFromAppException(request, ex)
  }


  // TODO: use URL builder to build error params
  def redirectResultFromAppException[A](request: Request[A], error: AppException): Result =
    SeeOther(route + s"?error=${error.id.key}")

}
