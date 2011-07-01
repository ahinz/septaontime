package org.hinz.septa.gtfs

import org.apache.commons.httpclient._, methods._, params._, cookie._, protocol._
import org.ccil.cowan.tagsoup.jaxp._
import scala.xml._
import java.io._

class FixedDataLoader {

  case class Route(route: String, directionID: String, name: String)

  val PROXY_KEY = "http.route.default-proxy"

  val client = new HttpClient()

  def autoconfigureProxy() {
    val proxy = System.getenv("http_proxy")
    if (proxy != null && proxy.length > 0) {
      if (!proxy.startsWith("http://")) {
        throw new Exception("Found an http_proxy env var (" + proxy + ") but it should start with http://")
      }

      val parts = proxy.substring(7,proxy.length).split(":")
      
      if (parts.length != 2) {
        throw new Exception("Found an http_proxy env var (" + proxy + ") but it the format should be: 'host:port'")
      }

      println("* Autoconfigure proxy: " + parts(0) + ":" + parts(1))

      client.getHostConfiguration().setProxy(parts(0), parts(1).toInt)
    }
  }

  autoconfigureProxy

  def postMethodForRoute(r: String):PostMethod = {
    val m = new PostMethod("http://www2.septa.org/stops/direction.php")
    m.addParameter("Route", r )
    m
  }

  def postMethodForStations(route: String, direction: String):PostMethod = {
    val m = new PostMethod("http://www2.septa.org/stops/bus-stop-ids.php")
    m.addParameter("Route", route)
    m.addParameter("Direction", direction)
    m
  }

  def getRoutes = List("44")

  def getStations(route: String, direction: String):Option[List[(String,String)]] = {
    val method = postMethodForStations(route, direction)
    val returnCode = client.executeMethod(method)

    if (returnCode == 200)
      Some(extractStationTags(method.getResponseBodyAsString()))
    else
      None
  }
    

  def getDirections(route: String):Option[List[Route]] = {
    val method = postMethodForRoute(route)
    val returnCode = client.executeMethod(method)

    if (returnCode == 200)
      Some(extractDirectionTags(route, method.getResponseBodyAsString()))
    else
      None
  }

  val parserFactory = new SAXFactoryImpl
  val parser = parserFactory.newSAXParser()
  val adapter = new scala.xml.parsing.NoBindingFactoryAdapter

  def processHTMLDocument(html: String):NodeSeq = {
    adapter.loadXML(new org.xml.sax.InputSource(new StringReader(html)), parser)
  }

  def extractStationTags(html: String) = {
    (processHTMLDocument(html) \\ "tr").map {
      case <tr>{td @ <td><span>{stationID @ _*}</span></td>}<td><span>{station @ _*}</span></td></tr> =>
        td.attribute("class") match {
          case Some(t) => Some((stationID.text, station.text))
          case _ => None
        }
      case _ => None
    }.flatten.toList
  }

  def extractDirectionTags(r: String, s: String):List[Route] = {
    (processHTMLDocument(s) \\ "option").map(e =>
      Route(r, (e \ "@value").text, e.text)).toList
  }
}

object Main {
  def main(args: Array[String]) = {
//    println(FixedDataLoader.getDirections("44"))
//    println(FixedDataLoader.getStations("44","Eastbound"))
    println(GTFSLoader.stations)
  }
}
