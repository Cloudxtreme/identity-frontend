package com.gu.identity.frontend.services

import com.gu.identity.frontend.errors._
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}

trait ServiceActionBuilder[+R[_]] extends ActionFunction[Request, R] {
  self =>

  // TODO read the default context from ActionBuilder
  implicit val exc: ExecutionContext = play.api.libs.concurrent.Execution.defaultContext

  def apply(block: R[AnyContent] => Future[Either[ServiceExceptions, Result]]): Action[AnyContent] =
    apply(BodyParsers.parse.default)(block)


  def apply[B](bodyParser: BodyParser[B])(block: R[B] => Future[Either[ServiceExceptions, Result]]): Action[B] =
    buildAction(bodyParser, request =>
      block(request).flatMap {
        case Right(result) => Future.successful(result)
        case Left(errors) if errors.nonEmpty => Future.failed {
          errors.headOption.getOrElse(SeqAppExceptions(errors))
        }

        // Should be impossible, but covered just in case
        case Left(empty) => Future.failed(UnexpectedAppException("empty errors from Service"))
      }
    )


  protected def composeParser[A](bodyParser: BodyParser[A]): BodyParser[A] = bodyParser

  protected def composeAction[A](action: Action[A]): Action[A] = action


  override def andThen[Q[_]](other: ActionFunction[R, Q]): ServiceActionBuilder[Q] = new ServiceActionBuilder[Q] {
    def invokeBlock[A](request: Request[A], block: Q[A] => Future[Result]) =
      self.invokeBlock[A](request, other.invokeBlock[A](_: R[A], block))
    override protected def composeParser[A](bodyParser: BodyParser[A]): BodyParser[A] = self.composeParser(bodyParser)
    override protected def composeAction[A](action: Action[A]): Action[A] = self.composeAction(action)
  }


  // TODO Can we reuse something from ActionBuilder to build an Action
  private def buildAction[A](bodyParser: BodyParser[A], block: R[A] => Future[Result]) = new Action[A] {
    def parser = composeParser(bodyParser)
    def apply(request: Request[A]) = try {
      invokeBlock(request, block)
    } catch {
      // NotImplementedError is not caught by NonFatal, wrap it
      case e: NotImplementedError => throw new RuntimeException(e)
      // LinkageError is similarly harmless in Play Framework, since automatic reloading could easily trigger it
      case e: LinkageError => throw new RuntimeException(e)
    }

    // TODO use proper execution context, was: ActionBuilder.this.executionContext
    override def executionContext = exc
  }

}

object ServiceAction extends ServiceActionBuilder[Request] {
  override def invokeBlock[A](request: Request[A], block: Request[A] => Future[Result]): Future[Result] =
    block(request)
}
