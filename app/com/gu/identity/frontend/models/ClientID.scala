package com.gu.identity.frontend.models

import play.api.data.{FieldMapping, FormError, Forms}
import play.api.data.format.Formatter


/**
 * Defines a Client ID for altering the skin of all pages.
 *
 * Note: Product is manually defined here with `_1` mapping to the object
 *       itself. This is a workaround to ensure the values of the case object
 *       are available in the handlebars template,
 *       see: https://github.com/bizreach/play2-handlebars/issues/11
 */
sealed trait ClientID extends Product2[ClientID, String] {
  self =>

  val id: String

  def hasSkin: Boolean = false

  def _1 = self
  def _2 = id
}

case object GuardianMembersClientID extends ClientID {
  val id = "members"

  override def hasSkin = true
}

object ClientID {
  def all: Seq[ClientID] = Seq(GuardianMembersClientID)

  def apply(clientId: String): Option[ClientID] =
    all.find(_.id == clientId)

  def apply(clientId: Option[String]): Option[ClientID] =
    clientId.flatMap(apply)

  object FormMapping {
    lazy val clientId: FieldMapping[ClientID] = Forms.of[ClientID](ClientIdFormatter)

    private object ClientIdFormatter extends Formatter[ClientID] {
      def bind(key: String, data: Map[String, String]): Either[Seq[FormError], ClientID] = {
        val value = data.get(key)
          .flatMap(ClientID.apply)

        value.toRight {
          Seq(FormError(key, "Unknown ClientID"))
        }
      }

      def unbind(key: String, value: ClientID): Map[String, String] =
        Map(key -> value.id)
    }
  }
}
