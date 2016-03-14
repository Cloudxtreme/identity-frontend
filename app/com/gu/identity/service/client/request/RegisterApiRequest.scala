package com.gu.identity.service.client.request

import com.gu.identity.frontend.models.{ClientIp, TrackingData}
import com.gu.identity.frontend.request.RegisterActionRequestBody
import com.gu.identity.service.client._


case class RegisterApiRequest(url: String, extraHeaders: HttpParameters = Nil, trackingData: TrackingData, override val body: Option[ApiRequestBody]) extends ApiRequest {
  override val method = POST
  override val headers = Seq("Content-Type" -> "application/json") ++ extraHeaders
  override val parameters = trackingData.parameters
}

object RegisterApiRequest {
  def apply(request: RegisterActionRequestBody, clientIp: ClientIp, trackingData: TrackingData)(implicit configuration: IdentityClientConfiguration): RegisterApiRequest = {
    RegisterApiRequest(
      ApiRequest.apiEndpoint("user"),
      body = Some(RegisterRequestBody(
        request.email,
        request.password,
        RegisterRequestBodyPublicFields(request.username),
        RegisterRequestBodyPrivateFields(
          firstName = request.firstName,
          secondName = request.lastName,
          registrationIp = clientIp.ip
        ),
        RegisterRequestBodyStatusFields(
          receiveGnmMarketing = request.receiveGnmMarketing,
          receive3rdPartyMarketing = request.receive3rdPartyMarketing
        )
      )),
      extraHeaders = ApiRequest.commonApiHeaders(trackingData),
      trackingData = trackingData
    )
  }
}

case class RegisterRequestBody(
    primaryEmailAddress: String,
    password: String,
    publicFields: RegisterRequestBodyPublicFields,
    privateFields: RegisterRequestBodyPrivateFields,
    statusFields: RegisterRequestBodyStatusFields)
  extends ApiRequestBody

case class RegisterRequestBodyPublicFields(username: String)

case class RegisterRequestBodyPrivateFields(
    firstName: String,
    secondName: String,
    registrationIp: String)

case class RegisterRequestBodyStatusFields(
    receiveGnmMarketing: Boolean,
    receive3rdPartyMarketing: Boolean)

