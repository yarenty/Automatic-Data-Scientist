package com.yarenty

import java.io.{File, PrintWriter, StringWriter}
import java.util
import java.util.Properties

import org.apache.log4j.{LogManager, Logger, PropertyConfigurator}


/** Logger
  * */
object Log {

  val FATAL:Byte = 0
  val ERRR:Byte = 1
  val WARN:Byte = 2
  val INFO:Byte = 3
  val DEBUG:Byte = 4
  val TRACE:Byte = 5
  
  val LVLS = Array("FATAL", "ERRR", "WARN", "INFO", "DEBUG", "TRACE")

  private[yarenty] var LOG_DIR:String = _
  private[yarenty] var _level = INFO
  private[yarenty] var _quiet = false
  private var _logger:Logger = _
  // Common pre-header
  private var _preHeader:String = _
  // A little bit of startup buffering
  private var INIT_MSGS = new util.ArrayList[String]

  def valueOf(slvl: String): Byte = {
    if (slvl == null) return -1
    val sl = slvl.toLowerCase
    if (sl.startsWith("fatal")) return FATAL
    if (sl.startsWith("err")) return ERRR
    if (sl.startsWith("warn")) return WARN
    if (sl.startsWith("info")) return INFO
    if (sl.startsWith("debug")) return DEBUG
    if (sl.startsWith("trace")) return TRACE
    -1
  }

  def init(sLvl: String, quiet: Boolean): Unit = {
    val lvl = valueOf(sLvl)
    if (lvl != -1) _level = lvl
    _quiet = quiet
  }

  def setLogLevel(sLvl: String, quiet: Boolean): Unit = {
    init(sLvl, quiet)
  }

  def setLogLevel(sLvl: String): Unit = {
    setLogLevel(sLvl, true)
  }

  def trace(objs: Any*): Unit = {
    log(TRACE, objs)
  }

  def debug(objs: Any*): Unit = {
    log(DEBUG, objs)
  }

  def info(objs: Any*): Unit = {
    log(INFO, objs)
  }

  def warn(objs: Any*): Unit = {
    log(WARN, objs)
  }

  def err(objs: Any*): Unit = {
    log(ERRR, objs)
  }

  def err(ex: Throwable): Unit = {
    val sw = new StringWriter
    ex.printStackTrace(new PrintWriter(sw))
    err(sw.toString)
  }

  def fatal(objs: Any*): Unit = {
    log(FATAL, objs)
  }

  def log(level: Int, objs: Any*): Unit = {
    if (_level >= level) write(level, objs)
  }


  def info(s: String, stdout: Boolean): Unit = {
    if (_level >= INFO) write0(INFO, stdout, Array[String](s))
  }

  // This call *throws* an unchecked exception and never returns (after logging).
  def throwErr(e: Throwable): RuntimeException = {
    err(e) // Log it

    throw if (e.isInstanceOf[RuntimeException]) e.asInstanceOf[RuntimeException]
    else new RuntimeException(e) // Throw it

  }

  private def write(lvl: Int, objs: Any*): Unit = {
    val writeToStdout = lvl <= _level
    write0(lvl, writeToStdout, objs)
  }

  private def write0(lvl: Int, stdout: Boolean, objs: Any*): Unit = {
    val sb = new StringBuilder
    for (o <- objs) {
      sb.append(o)
    }
    val res = sb.toString

    write0(lvl, stdout, res)
  }

  private def write0(lvl: Int, stdout: Boolean, s: String): Unit = {
    val sb = new StringBuilder
    val hdr = header(lvl) // Common header for all lines
    write0(sb, hdr, s)
    // stdout first - in case log4j dies failing to init or write something
    if (stdout && !_quiet) System.out.println(sb) // scalastyle: ignore
    // log something here
    val l4j:org.apache.log4j.Logger = if (_logger != null) _logger
    else createLog4j
    lvl match {
      case FATAL =>
        l4j.fatal(sb)
      case ERRR =>
        l4j.error(sb)
      case WARN =>
        l4j.warn(sb)
      case INFO =>
        l4j.info(sb)
      case DEBUG =>
        l4j.debug(sb)
      case TRACE =>
        l4j.trace(sb)
      case _ =>
        l4j.error("Invalid log level requested")
        l4j.error(s)
    }
  }

  private def write0(sb: StringBuilder, hdr: String, s: String): Unit = {
    if (s.contains("\n")) {
      for (s2 <- s.split("\n")) {
        write0(sb, hdr, s2)
        sb.append("\n")
      }
      sb.setLength(sb.length - 1)
    }
    else sb.append(hdr).append(s)
  }

  // Build a header for all lines in a single message
  private def header(lvl: Int) = {
    val nowString = Timer.nowAsLogString
    val s = nowString + " " + _preHeader + " " + fixedLength(Thread.currentThread.getName + " ", 10) + LVLS(lvl) + ": "
    s
  }

  def flushStdout(): Unit = {
    if (INIT_MSGS != null) {
      import scala.collection.JavaConversions._
      for (s <- INIT_MSGS) {
        System.out.println(s)
      }
      INIT_MSGS.clear()
    }
  }

  def getLogLevel: Int = _level

  def isLoggingFor(strLevel: String): Boolean = {
    val level = valueOf(strLevel)
    if (level == -1) { // in case of invalid log level return false
      return false
    }
    _level >= level
  }

  /**
    * Get the directory where the logs are stored.
    */
  @throws[Exception]
  def getLogDir: String = {
    if (LOG_DIR == null) throw new Exception("LOG_DIR not yet defined")
    LOG_DIR
  }



  /**
    * Get log file name without the path for particular log level.
    */
  @throws[Exception]
  def getLogFileName(level: String): String = "debug.log"



  /** Get full path to a specific log file */
  @throws[Exception]
  def getLogFilePath(level: String): String = getLogDir + File.separator + getLogFileName(level)

  @throws[Exception]
  private def setLog4jProperties(logDir: String, p: Properties): Unit = {
    LOG_DIR = logDir
    p.setProperty("log4j.appender.R1", "org.apache.log4j.RollingFileAppender")
    p.setProperty("log4j.appender.R1.Threshold", "TRACE")
    p.setProperty("log4j.appender.R1.File", getLogFilePath("trace"))
    p.setProperty("log4j.appender.R1.MaxFileSize", "1MB")
    p.setProperty("log4j.appender.R1.MaxBackupIndex", "3")
    p.setProperty("log4j.appender.R1.layout", "org.apache.log4j.PatternLayout")
    p.setProperty("log4j.appender.R1.layout.ConversionPattern", "%m%n")
    p.setProperty("log4j.appender.R2", "org.apache.log4j.RollingFileAppender")
    p.setProperty("log4j.appender.R2.Threshold", "DEBUG")
    p.setProperty("log4j.appender.R2.File", getLogFilePath("debug"))
    p.setProperty("log4j.appender.R2.MaxFileSize", "3MB")
    p.setProperty("log4j.appender.R2.MaxBackupIndex", "3")
    p.setProperty("log4j.appender.R2.layout", "org.apache.log4j.PatternLayout")
    p.setProperty("log4j.appender.R2.layout.ConversionPattern", "%m%n")
    p.setProperty("log4j.appender.R3", "org.apache.log4j.RollingFileAppender")
    p.setProperty("log4j.appender.R3.Threshold", "INFO")
    p.setProperty("log4j.appender.R3.File", getLogFilePath("info"))
    p.setProperty("log4j.appender.R3.MaxFileSize", "2MB")
    p.setProperty("log4j.appender.R3.MaxBackupIndex", "3")
    p.setProperty("log4j.appender.R3.layout", "org.apache.log4j.PatternLayout")
    p.setProperty("log4j.appender.R3.layout.ConversionPattern", "%m%n")
    p.setProperty("log4j.appender.R4", "org.apache.log4j.RollingFileAppender")
    p.setProperty("log4j.appender.R4.Threshold", "WARN")
    p.setProperty("log4j.appender.R4.File", getLogFilePath("warn"))
    p.setProperty("log4j.appender.R4.MaxFileSize", "256KB")
    p.setProperty("log4j.appender.R4.MaxBackupIndex", "3")
    p.setProperty("log4j.appender.R4.layout", "org.apache.log4j.PatternLayout")
    p.setProperty("log4j.appender.R4.layout.ConversionPattern", "%m%n")
    p.setProperty("log4j.appender.R5", "org.apache.log4j.RollingFileAppender")
    p.setProperty("log4j.appender.R5.Threshold", "ERROR")
    p.setProperty("log4j.appender.R5.File", getLogFilePath("error"))
    p.setProperty("log4j.appender.R5.MaxFileSize", "256KB")
    p.setProperty("log4j.appender.R5.MaxBackupIndex", "3")
    p.setProperty("log4j.appender.R5.layout", "org.apache.log4j.PatternLayout")
    p.setProperty("log4j.appender.R5.layout.ConversionPattern", "%m%n")
    p.setProperty("log4j.appender.R6", "org.apache.log4j.RollingFileAppender")
    p.setProperty("log4j.appender.R6.Threshold", "FATAL")
    p.setProperty("log4j.appender.R6.File", getLogFilePath("fatal"))
    p.setProperty("log4j.appender.R6.MaxFileSize", "256KB")
    p.setProperty("log4j.appender.R6.MaxBackupIndex", "3")
    p.setProperty("log4j.appender.R6.layout", "org.apache.log4j.PatternLayout")
    p.setProperty("log4j.appender.R6.layout.ConversionPattern", "%m%n")
    // Turn down the logging for some class hierarchies.
    p.setProperty("log4j.logger.org.apache.http", "WARN")
    p.setProperty("log4j.logger.com.amazonaws", "WARN")
    p.setProperty("log4j.logger.org.apache.hadoop", "WARN")
    p.setProperty("log4j.logger.org.jets3t.service", "WARN")
    p.setProperty("log4j.logger.org.reflections.Reflections", "ERROR")
    p.setProperty("log4j.logger.com.brsanthu.googleanalytics", "ERROR")
    p.setProperty("log4j.logger.org.apache.hadoop.util.NativeCodeLoader", "ERROR")
    // See the following document for information about the pattern layout.
    // http://logging.apache.org/log4j/1.2/apidocs/org/apache/log4j/PatternLayout.html
    //  Uncomment this line to find the source of unwanted messages.
    //     p.setProperty("log4j.appender.R1.layout.ConversionPattern", "%p %C %m%n");
  }

  private def createLog4j: Logger = {
    if (_logger != null) return _logger // Test again under lock
    val log4jConfiguration = System.getProperty("log4j.configuration")
    if (log4jConfiguration != null) PropertyConfigurator.configure(log4jConfiguration)
    else {
      val p = new Properties
      try {
        var dir = new File(".")
        setLog4jProperties(dir.toString, p)
      } catch {
        case e: Exception =>
          System.err.println("ERROR: failed in createLog4j, exiting now.")
          e.printStackTrace()
      }
      
       PropertyConfigurator.configure(p)
    }
    _logger = LogManager.getLogger("yarenty.default")
    _logger
  }

  def fixedLength(s: String, length: Int): String = {
    var r = padRight(s, length)
    if (r.length > length) {
      val a = Math.max(r.length - length + 1, 0)
      val b = Math.max(a, r.length)
      r = "#" + r.substring(a, b)
    }
    r
  }

  private[yarenty] def padRight(stringToPad: String, size: Int) = {
    val strb = new StringBuilder(stringToPad)
    while ( {
      strb.length < size
    }) if (strb.length < size) strb.append(' ')
    strb.toString
  }

  def ignore(e: Throwable): Unit = {
    ignore(e, "[h2o] Problem ignored: ")
  }

  def ignore(e: Throwable, msg: String): Unit = {
    ignore(e, msg, true)
  }

  def ignore(e: Throwable, msg: String, printException: Boolean): Unit = {
    debug(msg + (if (printException) e.toString
    else ""))
  }

 
  def getQuiet: Boolean = _quiet

  def setQuiet(q: Boolean): Unit = {
    _quiet = q
  }
}
