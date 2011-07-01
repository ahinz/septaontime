package org.hinz.septa.ws

import cc.spray.http._
import cc.spray.http.MediaTypes._

object JSONP {
  def apply(callback: String, body: String) = 
    jsonp(callback, body)

  def wrapJSONP(callback: String, body: String) =
    if (callback != null && callback.length > 0) callback + "(" + body + ")"
    else body

  def jsonp(callback: String, body: String) =
    HttpContent(
      ContentType(`application/json`),
      wrapJSONP(callback, body))
}
