package com.gu.identity.frontend.models

import com.gu.identity.frontend.utils.RemoteAddress
import com.gu.identity.service.client.HttpParameters
import play.api.mvc.RequestHeader

case class TrackingData(returnUrl:Option[String],
                        registrationType: Option[String],
                        omnitureSVi: Option[String],
                        ipAddress: Option[String],
                        referrer: Option[String],
                        userAgent: Option[String]) {
  def parameters: HttpParameters = List(
    returnUrl.map("trackingReturnUrl" -> _),
    registrationType.map("trackingRegistrationType" -> _),
    omnitureSVi.map("trackingOmnitureSVI" -> _),
    ipAddress.map("trackingIpAddress" -> _),
    referrer.map("trackingReferer" -> _),
    userAgent.map("trackingUserAgent" -> _)
  ).flatten
}

object TrackingData extends RemoteAddress {

  def apply(request: RequestHeader, returnUrl: Option[String]): TrackingData = {
    TrackingData(
      returnUrl = returnUrl,
      registrationType = request.getQueryString("type"),
      omnitureSVi =  request.cookies.get("S_VI").map(_.value),
      ipAddress = clientIp(request),
      referrer =  request.headers.get("Referer"),
      userAgent = request.headers.get("User-Agent")
    )
  }
}
