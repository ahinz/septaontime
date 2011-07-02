import cc.spray._
import test._
import http._
import http.HttpStatusCodes._
import HttpMethods._
import HttpHeaders._
import MediaTypes._
import HttpStatusCodes._

import org.scalatest._
import org.scalatest.matchers._

import org.hinz.septa._
import org.hinz.septa.ws._
import org.hinz.septa.gtfs._
import org.hinz.gis._

import java.io._

import net.liftweb.json._
import net.liftweb.json.JsonDSL._
import net.liftweb.json.Serialization._

class RouteListingSpec extends Spec with ShouldMatchers with SprayTest with RouteServiceBuilder {
  val routeList = List("1","44","H","XH")
  val dir44w = Map("route" -> "44", "directionID" -> "wb", "name" -> "To 5th St")
  val dir44e = Map("route" -> "44", "directionID" -> "eb", "name" -> "To Ardmore")
  val dirs = List(dir44w,dir44e)

  val stations = List(
    GTFSLoader.Station("10","st1","1.1","2.2"),
    GTFSLoader.Station("11","st2","1.2","2.3"),
    GTFSLoader.Station("12","st3","1.3","2.4"))

  def station2map(s: GTFSLoader.Station):Map[String,String] =
    Map("id" -> s.id, "name" -> s.name, "lat" -> s.lat, "lon" -> s.lon)

  val fixedDataLoader = new FixedDataLoader() {
    def map2route(map: Map[String,String]) =
      RouteDir(map.get("route").get, map.get("directionID").get, map.get("name").get)

    override def getRoutes = routeList
    override def getDirections(route: String) =
      if (route == "44") {
        Some(dirs.map(map2route(_)))
      } else {
        None
      }

    override def findStations(route: String, dir: String) =
      if (route == "44" && dir == "wb")
        Some(stations)
      else
        None
  }
  
  def testRoute(route: String, method: HttpMethod = GET) = {
    val br = new BufferedReader(
      new InputStreamReader(
        testService(
          HttpRequest(method, route))(routeService).response.content.get.inputStream))

    val outp = new StringBuilder()
    var line = br.readLine

    while(line != null) {
      outp.append(line)
      line = br.readLine
    }

    outp.toString()
  }

  def testRouteStatusCode(route: String, method: HttpMethod = GET) = 
    testService(HttpRequest(method, route))(routeService).response.status.code

  describe("Route Listing Services") {
    it("should be able to get a simple route") {
      testRoute("/routes") should equal(compact(JsonAST.render(routeList)))
    }

    it("should be able to get directions on a route") {
      testRoute("/routes/44") should equal(compact(JsonAST.render(dirs)))
    }

    it("should signal a 404 if an invalid route is given for a direction") {
      testRouteStatusCode("/routes/100") should equal(NotFound)
    }

    it ("should be able to get stations on a route") {
      testRoute("/routes/44/wb/") should equal(
        compact(JsonAST.render(stations.map(station2map(_)))))
    }

    it ("should signal a 404 if an invalid route is given for a station") { 
      testRouteStatusCode("/routes/100/wb") should equal(NotFound)
    }

    it ("should signal a 404 if an invalid direction is given for a station") {
      testRouteStatusCode("/routes/44/eb") should equal(NotFound)
    }
  }
}
