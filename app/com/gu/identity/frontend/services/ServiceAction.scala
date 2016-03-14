package com.gu.identity.frontend.services

import com.gu.identity.frontend.errors._
import com.gu.identity.frontend.utils.ComposableActionBuilder
import play.api.mvc._

import scala.concurrent.Future

trait ServiceActionBuilder[+R[_]] extends ActionFunction[Request, R] {
  self =>

  type ServiceResult = Either[ServiceExceptions, Result]


  val underlying = new ComposableActionBuilder[R] {
    def invokeBlock[A](request: Request[A], block: (R[A]) => Future[Result]): Future[Result] =
      self.invokeBlock(request, block)
  }


  def apply(block: R[AnyContent] => Future[ServiceResult]): Action[AnyContent] =
    apply(BodyParsers.parse.default)(block)


  def apply[B](bodyParser: BodyParser[B])(block: R[B] => Future[ServiceResult]): Action[B] = {
    val transformBlock = (request: R[B]) =>
      block(request).flatMap(transformServiceResult)(executionContext)

    underlying.async(bodyParser)(transformBlock)
  }

  private def transformServiceResult(result: ServiceResult) = result match {
    case Right(r) => Future.successful(r)

    case Left(error) if error.size == 1 => Future.failed(error.head)

    case Left(errors) if errors.nonEmpty => Future.failed {
      SeqAppExceptions(errors)
    }

    // Should be impossible, but covered just in case
    case Left(empty) => Future.failed(UnexpectedAppException("empty errors from Service"))
  }


  override def andThen[Q[_]](other: ActionFunction[R, Q]): ServiceActionBuilder[Q] = new ServiceActionBuilder[Q] {
    override val underlying = self.underlying.andThen(other)
    override def invokeBlock[A](request: Request[A], block: Q[A] => Future[Result]) =
      self.invokeBlock[A](request, other.invokeBlock[A](_: R[A], block))
  }

}

object ServiceAction extends ServiceActionBuilder[Request] {
  override def invokeBlock[A](request: Request[A], block: Request[A] => Future[Result]): Future[Result] =
    block(request)
}
