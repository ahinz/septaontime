package org.hinz.septa

import org.hinz.septa.server._
import scala.annotation.tailrec

object Main {
  val db = "devdb.db"

  def time =
    System.currentTimeMillis()

  @tailrec
  def executeEvery(msec: Int, f: Unit => Unit):Unit = {
    val cur = time
    try {
      f()
    } catch {
      case e: Exception => {
        e.printStackTrace()
        Thread.sleep(1000*60*5) // Sleep 5 minutes in case of error
      }
    }
    Thread.sleep(msec - (time - cur))
    executeEvery(msec,f)
  }

  val server = new Server(new RouteLoader(db))

  def main(args: Array[String]) = {
    executeEvery(1000*60, Unit => server.runMe)
  }

}
