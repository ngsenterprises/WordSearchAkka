

package com.ngs.WordSearch

import com.ngs.WordSearch.Actors._
import com.ngs.WordSearch.Util._
import com.typesafe.config.ConfigFactory
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import akka.actor.ActorSystem

object WordSearchApp extends App {

  def initParms(args: Array[String],
                 parms: HashMap[String, String]): Unit = {

    args.toList.foreach(arg => {
      val a = arg.split("=")
      parms += (a(0) -> a(1))
    })

    val config = ConfigFactory.load()
    parms("maxActors") = config.getString("app.akka.maxActors")//, parms.getOrElse("maxActors", "7").toInt)
    parms("rows") = config.getString("app.command.rows")//, parms.getOrElse("rows", "0").toInt)
    parms("cols") = config.getString("app.command.cols")//, parms.getOrElse("cols", "0").toInt)
    parms("wordStorFilePath") = config.getString("app.files.wordStorFilePath")//, parms.getOrElse("wordStorFilePath", ""))
  }//................................................


  val parms = new HashMap[String, String]
  initParms(args, parms)

  val ws = WordStorUtil.loadWordStor(parms("wordStorFilePath"))

  if (ws != EmptyCharTrie) {

    val brd = WordSearchBoard.initWordBoard(
      parms.getOrElse("rows", WordStorUtil.maxWordLength.toString).toInt,
      parms.getOrElse("cols", WordStorUtil.maxWordLength.toString).toInt)

    if (!brd.isEmpty) {
      val sys = ActorSystem("WordSearchSystem")
      val supervisor = sys.actorOf(WordSearchSupervisor.props, name = "WordSearchSupervisor")

      supervisor ! StartSystemMsg(parms)
    }
  }//-----------------------------------------

}
