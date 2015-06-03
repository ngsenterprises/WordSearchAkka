
package com.ngs.WordSearch.Util

import scala.collection.immutable.{Nil, List}
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

import scala.io.Source
import java.io.{FileReader, FileNotFoundException, IOException}


sealed trait CharTrie

case class CharTrieNode(key: Option[Char]) extends CharTrie {
  def this() = { this(None) }
  var isWord = false
  val keys = new scala.collection.mutable.HashMap[Char, CharTrie]
}
case object EmptyCharTrie extends CharTrie


object WordStorUtil {

  var maxWordLength = 0
  var numberOfWords = 0

  var wordStor = new CharTrieNode


  /*
  def printWordTree(): Unit = {
    def print_tree(node: CharTrie): Unit = {
      node match {
        case EmptyCharTrie => ()
        case dn: CharTrieNode => {
          dn.key match {
            case None => ()
            case Some(v: Char) => println(v)
          }
        }
          var itr = dn.keys.keysIterator
          while (itr.hasNext)
            print(itr.next + ", ")
          itr = dn.keys.keysIterator
          while (itr.hasNext)
            print_tree(dn.keys.getOrElse(itr.next, EmptyCharTrie))
      }
    }
    print_tree(dict)
  } //.....................................
  */



  def testSearchWord(path: String): Unit = {
    var bErrors = false
    try {
      val src = Source.fromFile(path).getLines()
      while (src.hasNext) {
        val line = src.next.trim
        val res = searchWord(line)
        //println(line + ": isWord = " + res._1.toString + ", isPrefix = " + res._2.toString)
        if (!res._1) {
          bErrors = true
          println(line + ": isWord = " + res._1.toString + ", isPrefix = " + res._2.toString)
        }
      }
    } catch {
      case ex: FileNotFoundException => println("Couldn't find dictionary file.")
      case ex: IOException => println("IOException reading dictionary file")
    }
    if (!bErrors)
      println("no errors")
  } //............................................


  def searchWord(data: String): (Boolean, Boolean) = {
    val prefix = new ListBuffer[String]
    val v = new StringBuilder
    var nextNode: CharTrie = wordStor

    val itr = data.iterator
    while (itr.hasNext && nextNode != EmptyCharTrie)
      nextNode = nextNode.asInstanceOf[CharTrieNode].keys.getOrElse(itr.next, EmptyCharTrie)

    if (itr.isEmpty) {
      if (nextNode == EmptyCharTrie)
        (false, false)
      else {
        val dn = nextNode.asInstanceOf[CharTrieNode]
        (dn.isWord, !dn.keys.isEmpty)
      }
    }
    else (false, false)

  } //........................................


  def addWord(data: String, root: CharTrieNode): Boolean = {
    def next(data: List[Char], node: CharTrie): Boolean = {
      (data, node) match {
        case (h :: t, dn: CharTrieNode) => {
          if (dn.keys.contains(h))
            next(t, dn.keys.getOrElse(h, EmptyCharTrie))
          else {
            val new_node = new CharTrieNode(Some(h))
            dn.keys += (h -> new_node)
            next(t, new_node)
          }
        }
        case (Nil, dn: CharTrieNode) => {
          dn.isWord = true
          true
        }
        case (_, EmptyCharTrie) => false
      }
    }
    next(data.toList, root)
  } //.....................


  def loadWordStor(path: String): CharTrie = {
    var count = 0
    var maxLen = 0
    //var root = new CharTrieNode
    var wordAdded: Boolean = true
    try {
      val src = Source.fromFile(path).getLines()
      while (src.hasNext && wordAdded) {
        val line = src.next.trim
        wordAdded = addWord(line, root.asInstanceOf[CharTrieNode])
        if (wordAdded) {
          maxLen = math.max(line.length, maxLen)
          count += 1
        }
      }
      if (!wordAdded) {
        numberOfWords = 0
        maxWordLength = 0
        return EmptyCharTrie
      }
    } catch {
      case ex: FileNotFoundException => {
        println("Couldn't find word file.")
        numberOfWords = 0
        maxWordLength = 0
        return EmptyCharTrie
      }
      case ex: IOException => {
        println("IOException reading word file")
        numberOfWords = 0
        maxWordLength = 0
        return EmptyCharTrie
      }
    }
    //println("loadWordStor: count = " +count.toString)

    wordStor = root
    numberOfWords = count
    maxWordLength = maxLen

    wordStor
  } //......................................


}//--------------------------------------------








case class BoardNode(v: Char, row: Int, col: Int) {
  def this() = { this(' ', 0, 0) }
  override def equals(other: Any): Boolean = {
    val o = other.asInstanceOf[BoardNode]
    v == o.v && row == o.row && col == o.col
  }
}


object WordSearchBoard {

  var maxRows = 0
  var maxCols = 0

  var board: Seq[Seq[BoardNode]] = List.empty[Seq[BoardNode]].toSeq


  def printBoard(board: Seq[Seq[BoardNode]]): Unit = {
    board.foreach { row => {
      row.foreach(node => {
        print("[" + node.v + "]")
      })
      println
    }}
  } //.......................................................


  def initWordBoard(rows: Int, cols: Int): Seq[Seq[BoardNode]] = {
    //println("initWordBoard rows " +rows.toString +", cols " +cols.toString)
    val rnd = new scala.util.Random
    val brd = new ArrayBuffer[Seq[BoardNode]]
    val cRange = 'z' - 'a' + 1
    (0 until rows).foreach { r => {
      val row = new ArrayBuffer[BoardNode]
      (0 until cols).foreach { c => {
        row += new BoardNode((rnd.nextInt(cRange) + 'a'.toInt).toChar, r, c)
      }}
      brd += row.toSeq
    }}
    maxRows = rows
    maxCols = cols
    board = brd.toSeq
    board
  } //....................................


  def trailContains(trail: List[BoardNode], e: BoardNode): Boolean = {
    var lst = trail
    while (!lst.isEmpty) {
      if (lst.head.equals(e)) return true
      lst = lst.tail
    }
    false
  } //..........................................

  def walkTrail(board: Seq[Seq[BoardNode]], trail: List[BoardNode]): List[String] = {

    val th = trail.head
    //println("walkTrail: row " +th.row +" col " +th.col)

    val lb = new ListBuffer[String]
    val prefix = trail.reverse.foldLeft(new StringBuilder)((ac, node) => ac += node.v).toString
    val sw = WordStorUtil.searchWord(prefix)

    if (sw._1 && 2 < prefix.length)
      lb += prefix
    else if (sw._2) {
      trail match {

        case Nil => Nil

        case h :: t => {

          // LEFT
          if (0 < h.col) {
            val bn = board(h.row)(h.col - 1)
            if (!trailContains(trail, bn))
              lb ++= walkTrail(board, bn :: trail)
          }

          // LEFT DOWN
          if (0 < h.col && h.row < maxRows - 1) {
            val bn = board(h.row + 1)(h.col - 1)
            if (!trailContains(trail, bn))
              lb ++= walkTrail(board, bn :: trail)
          }

          //DOWN
          if (h.row < maxRows - 1) {
            val bn = board(h.row + 1)(h.col)
            if (!trailContains(trail, bn))
              lb ++= walkTrail(board, bn :: trail)
          }

          //RIGHT DOWN
          if (h.row < maxRows - 1 && h.col < maxCols - 1) {
            val bn = board(h.row + 1)(h.col + 1)
            if (!trailContains(trail, bn))
              lb ++= walkTrail(board, bn :: trail)
          }

          //RIGHT
          if (h.col < maxCols - 1) {
            val bn = board(h.row)(h.col + 1)
            if (!trailContains(trail, bn))
              lb ++= walkTrail(board, bn :: trail)
          }

          //RIGHT UP
          if (0 < h.row && h.col < maxCols - 1) {
            val bn = board(h.row - 1)(h.col + 1)
            if (!trailContains(trail, bn))
              lb ++= walkTrail(board, bn :: trail)
          }

          //UP
          if (0 < h.row) {
            val bn = board(h.row - 1)(h.col)
            if (!trailContains(trail, bn))
              lb ++= walkTrail(board, bn :: trail)
          }

          //LEFT UP
          if (0 < h.row && 0 < h.col) {
            val bn = board(h.row - 1)(h.col - 1)
            if (!trailContains(trail, bn))
              lb ++= walkTrail(board, bn :: trail)
          }
        }
      }
    }
    lb.toList
  } //.........................................................



  /*
  def wordSearch2D(): Set[String] = {

    println("wordSearch2D")

    val stats = loadWordStor(wordStorePath)
    BOARD_WIDTH = stats._2
    BOARD_HEIGHT = stats._2
    //testSearchWord(wordStorePath)

    val board = initWordBoard(BOARD_WIDTH, BOARD_HEIGHT)
    //      println("BOARD_WIDTH " +BOARD_WIDTH.toString)
    //      println("BOARD_HEIGHT " +BOARD_HEIGHT.toString)
    //      println("board.length: " +board.length.toString)
    //      printBoard(board)

    var res = new ListBuffer[String]
    (0 until BOARD_HEIGHT).foreach { r => {
      (0 until BOARD_WIDTH).foreach { c => {
        //println("outer loop row: " +r.toString +" col: " +c.toString)
        res ++= walkTrail(board, board(r)(c) :: Nil)
      }}
    }}
    res.toSet
  } //.....................................
  */


}//---------------------------------------------------
