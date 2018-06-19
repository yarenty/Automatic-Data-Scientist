package com.yarenty

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

object Timer {
  private val longFormat = DateTimeFormat.forPattern("dd-MMM HH:mm:ss.SSS")
  private val shortFormat = DateTimeFormat.forPattern("HH:mm:ss.SSS")
  private val logFormat = DateTimeFormat.forPattern("MM-dd HH:mm:ss.SSS")

  /**
    * Used by Logging (for creating a timestamp in front of each output line.
    */
  private[yarenty] def nowAsLogString = logFormat.print(DateTime.now)
}

class Timer {
  final private[yarenty] val _start = System.currentTimeMillis
  final private[yarenty] val _nanos = System.nanoTime

  /** Return the difference between when the timer was created and the current time. */
  def time: Long = System.currentTimeMillis - _start

  def nanos: Long = System.nanoTime - _nanos

  /** Return the difference between when the timer was created and the current
    * time as a string along with the time of creation in date format. */
  override def toString: String = {
    val now = System.currentTimeMillis
    (now - _start) + "ms (Wall: " + Timer.longFormat.print(now) + ") "
  }

  /** return the start time of this timer. **/
  private[yarenty] def startAsString = Timer.longFormat.print(_start)

  private[yarenty] def startAsShortString = Timer.shortFormat.print(_start)
}
