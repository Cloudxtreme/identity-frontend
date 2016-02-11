package com.gu.identity.frontend.errors

import play.api.mvc._
import play.api.mvc.Results.SeeOther

import scala.concurrent.Future


/**
 * Recover from application errors. Other errors will be caught by the global
 * error handler, eg: 500s.
 */
case class RedirectOnError(route: String) extends ActionBuilder[Request] {
  def invokeBlock[A](request: Request[A], block: (Request[A]) => Future[Result]): Future[Result] = {
    implicit val exc = executionContext

    block(request)
      .recover {
        case SeqAppExceptions(errors) => SeeOther(route + "?error=multi")
        case ex: AppException => redirectResultFromAppException(request, ex)
      }

  }

  // TODO: use URL builder to build error params
  def redirectResultFromAppException[A](request: Request[A], error: AppException): Result =
    SeeOther(route + s"?error=${error.id.key}")

}
