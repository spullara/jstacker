package jstacker

import java.io.{BufferedReader, FileReader}
import com.twitter.util.FuturePool
import java.util.concurrent.Executors
import scala.collection.mutable.Map
import java.util.concurrent.atomic.AtomicInteger

/**
 * Parse the output from multiple jstacks to produce a profile.
 */

object Main {

  val waits = Seq(
    "sun.nio.ch.EPollArrayWrapper.epollWait",
    "sun.misc.Unsafe.park",
    "java.lang.Object.wait"
  )

  def main(args: Array[String]) {

    def noWaiter(stack: Stack): Boolean = {
      if (stack.lines.length == 0) return false
      !(stack.lines.find {
        case Left(trace) => waits.contains(trace.method)
        case _ => false
      }).isDefined
    }

    def parse(filename: String): List[Stack] = {
      (JstackParser(new BufferedReader(new FileReader(filename))) map {
        _.stacks.filter(noWaiter)
      }).flatten
    }

    def sum(a: List[Stack]) = {
      a.foldLeft(Map[String, AtomicInteger]()) {
        case (map, stack) =>
          var lines = stack.lines.filter({
            _.isLeft
          })
          val key = lines.map({
            _.left
          }).head.get.method

          map.getOrElseUpdate(key, new AtomicInteger(0)).incrementAndGet()
          map
      }
    }

    val futurePool = FuturePool(Executors.newCachedThreadPool())

    futurePool {
      parse(args(0))
    } join futurePool {
      parse(args(1))
    } onSuccess {
      case (a, b) =>
        val before = sum(a)
        val after = sum(b)
        val diff = Map[String, (Int, Int)]()
        before foreach {
          case (key, value) =>
            val afterValue = after.getOrElse(key, new AtomicInteger(0)).intValue()
            val beforeValue = before.getOrElse(key, new AtomicInteger(0)).intValue()
            diff.put(key, (afterValue, beforeValue))
        }
        diff.toSeq.sortWith {
          case ((_, a), (_, b)) => (a._1 - a._2) > (b._1 - b._2)
        } foreach {
          case (key, value) =>
            println(key + ": " + value)
        }
    } get()
    System.exit(0)
  }

}