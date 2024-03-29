package test.util

import org.openqa.selenium.support.ui.{ExpectedCondition, ExpectedConditions, WebDriverWait}
import org.openqa.selenium.By
import org.scalatest.selenium.WebBrowser
import scala.util.Try

trait Browser extends WebBrowser {

  lazy implicit val driver = Driver()

  private val timeOutSec = 30

  case class MissingPageElementException(q: Query)
    extends Exception(s"Could not find WebElement with locator: ${q.queryString}")

  def pageHasText(text: String): Boolean = {
    waitUntil(ExpectedConditions.textToBePresentInElementLocated(By.tagName("body"), text))
  }

  def pageHasElement(q: Query): Boolean = {
    waitUntil(ExpectedConditions.visibilityOfElementLocated(q.by))
  }

  def pageHasUrl(urlFraction: String): Boolean = {
    waitUntil(ExpectedConditions.urlContains(urlFraction))
  }

  def elementHasText(q: Query, text: String): Boolean = {
    waitUntil(ExpectedConditions.textToBePresentInElementLocated(q.by, text))
  }

  def waitUntil[T](pred: ExpectedCondition[T]) = {
    Try(new WebDriverWait(driver, timeOutSec).until(pred)).isSuccess
  }

  def clickOn(q: Query): Unit = {
    if (pageHasElement(q))
      click.on(q)
    else
      throw new MissingPageElementException(q)
  }

  def setValue(q: Query, value: String): Unit = {
    if (pageHasElement(q))
      q.webElement.sendKeys(value)
    else
      throw new MissingPageElementException(q)
  }
}
