package com.gu.identity.frontend.errors

import play.api.mvc._
import play.api.mvc.Results.SeeOther

import scala.concurrent.Future
import scala.util.control.NonFatal


case class RedirectOnError(route: String) extends ActionBuilder[Request] {
  def invokeBlock[A](request: Request[A], block: (Request[A]) => Future[Result]): Future[Result] = {
    implicit val exc = executionContext

    block(request)
      .collect {
        case result: ErrorResult => redirectResult(request, result.error)
      }
      .recover {
        case NonFatal(ex) => redirectResult(request, ex)
      }

  }

  def redirectResult[A](request: Request[A], ex: Throwable) =
    SeeOther(route) // TODO include errors

}
