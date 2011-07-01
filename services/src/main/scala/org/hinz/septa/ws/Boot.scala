package org.hinz.septa.ws

import akka.config.Supervision._
import akka.actor.Supervisor
import akka.actor.Actor._
import cc.spray._
import utils.ActorHelpers._

import org.hinz.septa.gtfs._

class Boot {
  
  val mainModule = new HelloServiceBuilder {
    // bake your module cake here
  }

  val routeModule = new RouteServiceBuilder {
    val fixedDataLoader = new FixedDataLoader()
  }
  
  // start the root service actor (and any service actors you want to specify supervision details for)
  Supervisor(
    SupervisorConfig(
      OneForOneStrategy(List(classOf[Exception]), 3, 100),
      List(
        Supervise(actorOf[RootService], Permanent)
      )
    )
  )
  
  // attach an HttpService (which is also an actor)
  // the root service automatically starts the HttpService if it is unstarted
  actor[RootService] ! Attach(HttpService(mainModule.nextBusService))
  actor[RootService] ! Attach(HttpService(mainModule.routeService))
  actor[RootService] ! Attach(HttpService(routeModule.routeService))

}
