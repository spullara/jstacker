package jstacker

import util.parsing.combinator.RegexParsers
import java.util.Date
import java.text.SimpleDateFormat
import java.io.Reader

/**
 * Parser
 */

case class Thread(
  name: String,
  daemon: Boolean,
  priority: Int,
  tid: String,
  nid: String,
  mode: String,
  location: Option[String],
  state: Option[String]
)

case class Trace(method: String, line: String)
case class Concurrent(what: String, address: String, where: String)
case class Stack(thread: Thread, lines: List[Either[Trace, Concurrent]])
case class Jstack(date: Date, version: String, stacks: List[Stack], refs: Int)

object JstackParser extends RegexParsers {

  def date = "[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}".r  ^^ {
    new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").parse
  }

  val hex = "0x[0-9a-f]+".r

  val int = "[0-9]+".r ^^ (_.toInt)

  val version = "Full thread dump " ~> "[^:]+".r <~ ":"

  val threadName = "\"" ~> "[^\"]+".r <~ "\""

  val priority = "prio=" ~> int
  
  val tid = "tid=" ~> hex
  
  val nid = "nid=" ~> hex
  
  val daemon = "(daemon)?".r ^^ { _ != "" }
  
  val mode = ( "sleeping" | "waiting for monitor entry" | "waiting on condition" | "runnable" | "in Object.wait()" )
  
  val location = "[" ~> hex <~ "]"
  
  val state = "java.lang.Thread.State: " ~> "(.+)".r
  
  val method = "at" ~> "([a-zA-Z0-9_$.<>]+)".r
  
  val source = "(" ~> ("Native Method" | "Unknown Source" | "\\(inline\\)(:[0-9]+)?".r | "[a-zA-Z0-9_$.]+(:[0-9]+)?".r) <~ ")"
  
  def trace = method ~ source ^^ { case m ~ l => Trace(m, l) }
  
  def concurrent = ("- " ~> ( "locked" | "parking to wait for" | "waiting on" )) ~ ("<" ~> hex) ~ ("> (a " ~> "[a-zA-Z0-9_$.]+".r) <~ ")" ^^ {
    case what ~ address ~ where => Concurrent(what, address, where)
  }

  val jni = "JNI global references: " ~> int

  def thread = threadName ~ daemon ~ priority ~ tid ~ nid ~ mode ~ opt(location) ~ opt(state) ^^ {
    case name ~ daemon ~ priority ~ tid ~ nid ~ mode ~ location ~ state => Thread(name, daemon, priority, tid, nid, mode, location, state)
  }
  
  def lines = (trace | concurrent) ^^ {
    case trace: Trace => Left(trace)
    case concurrent: Concurrent => Right(concurrent)
  }
  
  def stack = thread ~ rep(lines) ^^ {
    case thread ~ lines => Stack(thread, lines)
  }
  
  def jstack = date ~ version ~ rep(stack) ~ jni ^^ {
    case date ~ version ~ stacks ~ refs => Jstack(date, version, stacks, refs)
  }
  
  def jstacks = rep(jstack)

  def apply(reader: Reader) = {
    parseAll(jstacks, reader) match {
      case Success(result, _) => result
      case failure: NoSuccess => throw new IllegalArgumentException(failure.msg)
    }
  }
}