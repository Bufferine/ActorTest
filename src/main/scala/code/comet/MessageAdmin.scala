package code.comet
import java.util.Date
import scala.xml._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.jquery.JqJsCmds.{AppendHtml, FadeOut, Hide, FadeIn}
import net.liftweb.http.js.JsCmds.{Replace, After}
import net.liftweb.actor.LiftActor
import net.liftweb.util.Helpers
import net.liftweb.util.Helpers._
import net.liftweb.http.js.JE.Str
import net.liftweb.http._
import net.liftweb.common.{Box, Empty, Full}
import net.liftweb.common.Loggable
import net.liftweb.util.ActorPing
import net.liftweb._
import http._
import common._
import util._
import js._
import JsCmds._
import JE._
import net.liftweb.http.js.JsExp
import _root_.java.text.SimpleDateFormat
import _root_.java.util.{TimeZone, Calendar, Date, Locale}
import scala.xml.NodeSeq
import net.liftweb.actor._
import scala.xml.{NodeSeq, Text}

import net.liftweb._
import http._
import js._
import JsCmds._

import common._
import util._
import Helpers._
import java.util.Date
import net.liftweb.mapper._
import net.liftweb.common.Loggable
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
import org.joda.time.format.DateTimeFormatter
import java.text.NumberFormat
import java.text.DecimalFormat

class MessageClient extends CometActor with CometListener with Loggable {
       val numberList:List[Long] = List(1,2,3,4,5,6,7,8,9, 10)
	numberList.map(number => {
		new Firer ! number 
	})	
  def registerWith = MessageServer

  override def lowPriority = {
    case x =>{
	logger.info("$$$$$$ GOT MESSAGE: " + x + "$$$$$$$")
    }
  }

  def render =  "* *" #> "Done" 
}
object MessageServer extends LiftActor with ListenerManager with Loggable{

  def createUpdate = "REGISTERED WITH MESSAGE ADMIN"

  override def lowPriority = {
    case x => updateListeners(x)
  }


}

class Firer extends LiftActor {
  	protected def messageHandler = {
		case (number:Long) =>{
			new Validate !? (20 seconds, number) match {
				case x => println("Got " + x + " for " + number)
			}
			//new TimeChecker !? (20 seconds, number)
		}
	}


}

class Validate extends LiftActor with Loggable {
  protected def messageHandler = {

    case (number: Long) => {
      println("validate bet "+ number )
      var valid = true
      var replyMessage: Box[Any] = Full(number)
      val sync = new Object
      var validatorsReturned = 0
      def returnResult(validated: Box[Any], who:String) = {
        sync.synchronized {
          println("response from " + who + " number " + number + " : " + validated)
          validated match {
            case Failure(message, _, _) => {
              replyMessage = Failure(message)
              valid = false;
            }
            case Full(number:Long) => {
              replyMessage= (Full(number))
              validatorsReturned += 1
            }
            case x => {
              replyMessage = Failure("Unexpected response returned to validate: " + x )
              valid = false
            }
          }
          sync.notifyAll
        }
      }

      println("Sending message to TimeChecker " + number)
      new TimeChecker ! new ValidatorMessage(number, returnResult)

      sync.synchronized {
        while (valid && validatorsReturned < 1) {
          sync.wait(50)
        }
      }

      println("Validator returning number " + number + " : " + replyMessage )
      reply(replyMessage)
    }
    case x => {
      println("Validator got unknown message " + x)
      logger.error("Unknown message " + x)
    }

  }
}

class TimeChecker extends LiftActor with Loggable {

  var theFunc: (Box[Long], String) => Unit = {(something: Box[Long], who:String) => ()}

  def messageHandler = {

    case (ValidatorMessage(number, func)) => {
              theFunc = func
              println("Time checker scheduling callback for number " + number)
              ActorPing.schedule(this, ActorPingMessage(number, func), 3500)
    }
    case (number:Long) => {
              println("Time checker scheduling callback for number " + number)
              ActorPing.schedule(this, Pair("callback", number), 3500)
    }
    case (ActorPingMessage(number, func)) => {
      println("Timechecker callback " + number)
      func(Full(number), "TimeChecker")
    }
    case (Pair(description, number)) => {
      println("Timechecker callback " + number)
    }
  }
}


case class ValidatorMessage(number: Long, validated: (Box[Any], String) => Unit)
case class ActorPingMessage(number: Long, validated: (Box[Long], String) => Unit)


