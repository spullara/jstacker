package jstacker

import java.io.{BufferedReader, FileReader}


/**
 * Parse the output from multiple jstacks to produce a profile.
 */

object Main {
  def main(args: Array[String]) {
    JstackParser(new BufferedReader(new FileReader(args(0))))
  }
}