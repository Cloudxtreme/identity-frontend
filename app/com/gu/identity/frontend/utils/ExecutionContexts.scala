package com.gu.identity.frontend.utils

import scala.concurrent.ExecutionContext

trait ExecutionContexts {

  implicit lazy val executionContext: ExecutionContext = play.api.libs.concurrent.Execution.Implicits.defaultContext

}
