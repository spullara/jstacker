package jstacker

import util.parsing.combinator.RegexParsers
import java.util.Date
import java.text.SimpleDateFormat
import java.io.Reader

/**
 * Parser
 */

case class Thread(name: String,
                  daemon: Boolean,
                  priority: Int,
                  tid: String,
                  nid: String,
                  mode: String,
                  location: Option[String],
                  state: Option[String])

case class Trace(method: String, line: String)
case class Concurrent(what: String, address: String, where: String)
case class Stack(thread: Thread, lines: List[Either[Trace, Concurrent]])
case class Jstack(date: Date, version: String, stacks: List[Stack], refs: Int)

object JstackParser extends RegexParsers {
  def date(): Parser[Date] = "[0-9]+-[0-9]+-[0-9]+ [0-9]+:[0-9]+:[0-9]+".r  ^^ { date =>
    val format = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    format.parse(date)
  }
  def hex(): Parser[String] = "0x[0-9a-f]+".r
  def version(): Parser[String] = "Full thread dump " ~ "[^:]+".r ~ ":" ^^ { case _ ~ version ~ _ => version }
  def threadName(): Parser[String] = "\"" ~ "[^\"]+".r ~ "\"" ^^ { case _ ~ name ~ _ => name }
  def priority(): Parser[Int] = "prio=" ~ "[0-9]+".r ^^ { case _ ~ priority => priority.toInt }
  def tid() = "tid=" ~ hex() ^^ { case hex => hex._2 }
  def nid() = "nid=" ~ hex() ^^ { case hex => hex._2 }
  def daemon(): Parser[Boolean] = "(daemon)?".r ^^ { _ != "" }
  def mode(): Parser[String] = "(sleeping|waiting for monitor entry|waiting on condition|runnable|in Object.wait\\(\\))".r
  def location(): Parser[String] = "[" ~ hex() ~ "]" ^^ { case _ ~ hex ~ _ => hex }
  def state(): Parser[String] = "java.lang.Thread.State: " ~ "(.+)".r ^^ { case _ ~ state => state }
  def method(): Parser[String] = "at" ~ "([a-zA-Z0-9_$.<>]+)".r ^^ { case _ ~ name => name }
  def source(): Parser[String] = "(" ~ ("Native Method" | "Unknown Source" | "\\(inline\\)(:[0-9]+)?".r | "[a-zA-Z0-9_$.]+(:[0-9]+)?".r) ~ ")" ^^ { case _ ~ line ~ _ => line }
  def trace(): Parser[Trace] = method() ~ source() ^^ { case m ~ l => Trace(m, l) }
  def concurrent() = "- " ~ "(locked|parking to wait for|waiting on)".r ~ "<" ~ hex() ~ "> (a " ~ "[a-zA-Z0-9_$.]+".r ~ ")" ^^ {
    case _ ~ what ~ _ ~ address ~ _ ~ where ~ _ => Concurrent(what, address, where)
  }
  def jni(): Parser[Int] = "JNI global references: " ~ "[0-9]+".r ^^ { case _ ~ refs => refs.toInt }
  def thread() = threadName() ~ daemon() ~ priority() ~ tid() ~ nid() ~ mode() ~ opt(location()) ~ opt(state()) ^^ {
    case name ~ daemon ~ priority ~ tid ~ nid ~ mode ~ location ~ state =>
      Thread(name, daemon, priority, tid, nid, mode, location, state)
  }
  def lines() = (trace() | concurrent()) ^^ {
    case trace: Trace => Left(trace)
    case concurrent: Concurrent => Right(concurrent)
  }
  def stack() = thread() ~ rep(lines()) ^^ {
    case thread ~ lines => Stack(thread, lines)
  }
  def jstack() = date() ~ version() ~ rep(stack()) ~ jni() ^^ {
    case date ~ version ~ stacks ~ refs => Jstack(date, version, stacks, refs)
  }
  def jstacks() = rep(jstack())

  def apply(reader: Reader) = {
    parseAll(jstacks(), reader) match {
      case Success(result, _) => result
      case failure: NoSuccess => throw new IllegalArgumentException(failure.msg)
    }
  }
}
