package test.util

import com.typesafe.config.ConfigFactory
import org.slf4j.LoggerFactory
import scala.util.{Failure, Success, Try}

object Config {
  def logger = LoggerFactory.getLogger(this.getClass)

  private val conf = ConfigFactory.load()

  private val baseUrlsByStage = Map(
    "PROD" -> "https://profile.theguardian.com")

  val stage = conf.getString("stage")

  val baseUrl = baseUrlsByStage(stage)

  val testUsersSecret = conf.getString("identity.test.users.secret")

  val webDriverRemoteUrl = Try(conf.getString("webDriverRemoteUrl")) match {
    case Success(url) => url
    case Failure(e) => ""
  }

  object FacebookAppCredentials {
    val id = conf.getString("facebook.app.id")
    val secret = conf.getString("facebook.app.secret")
  }

  object GoogleTestUserCredentials {
    val email = conf.getString("google.test.user.email")
    val password = conf.getString("google.test.user.password")
    val name = conf.getString("google.test.user.name")
  }

  object ResetPasswordEmail {
    val to = conf.getString("password.reset.email.to")
    val from = conf.getString("password.reset.email.from")
  }


  def debug() = conf.root().render()

  def printSummary(): Unit = {
    logger.info("Functional Test Configuration")
    logger.info("=============================")
    logger.info(s"Stage: ${stage}")
    logger.info(s"Identity Frontend: ${Config.baseUrl}")
    logger.info(s"Screencast = https://saucelabs.com/tests/${Driver.sessionId}")
  }
}
