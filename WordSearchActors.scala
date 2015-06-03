
package com.ngs.WordSearch.Actors


import com.ngs.WordSearch.Util._

import java.io.{FileWriter, File}

import akka.actor.SupervisorStrategy.{Escalate, Resume, Restart}
import akka.actor.{OneForOneStrategy, Actor, Props, ActorRef}
import akka.routing.{RoundRobinRoutingLogic, SmallestMailboxRoutingLogic, Router, ActorRefRoutee}
import com.typesafe.config.ConfigFactory
import scala.collection.immutable.{Nil, List}
import scala.collection.mutable
import scala.io.Source
import scala.util.control.NonFatal
import scala.collection.mutable._
import scala.annotation._



sealed trait WordSearchMsg
case class StartSystemMsg(parms: HashMap[String, String]) extends WordSearchMsg
case class StartJobMsg(row: Int, col:Int) extends WordSearchMsg
case class EndJobMsg(resList: List[String]) extends WordSearchMsg
case object StopSystemMsg extends WordSearchMsg


object WordSearchActor {
  def props = Props(classOf[WordSearchActor])
}

class WordSearchActor extends Actor {


  def receive = {
    case StartJobMsg(row: Int, col:Int) => {
      val orgSender = sender
      val res = WordSearchBoard.walkTrail(
        WordSearchBoard.board,
        WordSearchBoard.board(row)(col) :: Nil)

      orgSender ! EndJobMsg(res)

    }
  }
}

object WordSearchSupervisor {
  def props = Props(classOf[WordSearchSupervisor])
}

class WordSearchSupervisor extends Actor {
  val config = ConfigFactory.load()
  var maxActors = config.getInt("app.akka.maxActors")
  var activeJobs = 0

  val resList = new ListBuffer[String]
  val jobQue = new Queue[StartJobMsg]()

  val wordList = new ListBuffer[String]

  var startTime = System.currentTimeMillis()

  override def supervisorStrategy = {
    OneForOneStrategy() {
      case e: Exception if (NonFatal(e)) => {
        println("supervisorStrategy RESTART." + e.toString)
        Restart
      }
      case e => {
        println("supervisorStrategy ESCALATE." + e.toString)
        Escalate
      }
    }
  }

  val router = {
    //println("routees maxActors " +maxActors)
    val routees = Vector.fill(maxActors) {
      val r = context.actorOf(WordSearchActor.props)
      context.watch(r)
      ActorRefRoutee(r)
    }
    //Router(RoundRobinRoutingLogic(), routees)
    Router(SmallestMailboxRoutingLogic(), routees)
  }

  def receive = {

    case StartSystemMsg(parms: HashMap[String, String]) => {
      //println("startSystem")
      val maxRows = parms.getOrElse("rows", "50").toInt
      val maxCols = parms.getOrElse("cols", "50").toInt

      var res = new ListBuffer[String]
      (0 until maxRows).foreach { row => {
        (0 until maxCols).foreach { col => {
          if (activeJobs < maxActors) {
            activeJobs += 1
            router.route(new StartJobMsg(row, col), self)
          } else jobQue += new StartJobMsg(row, col)
        }}
      }}
    }

    case EndJobMsg(res: List[String]) => {
      val orgSender = sender
      wordList ++= res
      activeJobs -= 1
      if (jobQue.isEmpty && activeJobs <= 0)
        self ! StopSystemMsg
      else if (!jobQue.isEmpty && activeJobs < maxActors) {
        activeJobs += 1
        orgSender ! jobQue.dequeue
        //router.route(jobQue.dequeue, self)
      }
    }

    case StopSystemMsg => {
      //println("stopSystem")
      val completeTime = System.currentTimeMillis() -startTime
      println("StopSystemMsg time " +completeTime.toString)

//      wordList.toSet.toList.grouped(15).foreach( row => {
//        println(row)
//      })

      context.stop(self)
      System.exit(0)
    }
  }
}



