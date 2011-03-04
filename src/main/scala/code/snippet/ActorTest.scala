package code.snippet
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


object ActorTestSnippet extends DispatchSnippet {

  val dispatch: DispatchIt = {
    case "send_messages" => sendMessages
  }

  def sendMessages = {
println("SEND MESSGAES")
     List(1,2,3,4,5,6,7,8,9,10).map(item => new ValidateBet ! (item))
      "#something" #> "nothing"
  }

}

class ValidateBet extends LiftActor with Loggable {
  protected def messageHandler = {

    case (betId: Long) => {
      println("validate bet "+ betId )
      var valid = true
      var replyMessage: Box[Any] = Full(betId)
      val sync = new Object
      var validatorsReturned = 0
      def returnResult(validated: Box[Any], who:String) = {
        sync.synchronized {
          println("response from " + who + " betId " + betId + " : " + validated + " left " + (4 -validatorsReturned))
          validated match {
            case Failure(message, _, _) => {
              replyMessage = Failure(message)
              valid = false;
            }
            case Full(betId:Long) => {
              replyMessage= (Full(betId))
              validatorsReturned += 1
            }
            case x => {
              replyMessage = Failure("Unexpected response returned to validatBet: " + x )
              valid = false
            }
          }
          sync.notifyAll
        }
      }

      println("Sending message to stakeChecker " + betId)
      new StakeChecker ! new ValidatorMessage(betId, returnResult)
      println("Sending message to TimeChecker " + betId)
      new TimeChecker ! new ValidatorMessage(betId, returnResult)
      println("Sending message to GameStateChecker " + betId)
//      new UserChecker ! new ValidatorMessage(betId, returnResult)
      new GameStateChecker ! new ValidatorMessage(betId, returnResult)
      println("Sending message to MarketStateChecker " + betId)
      //new MarketStateChecker ! new ValidatorMessage(betId, returnResult)
      println("All messages sent " + betId)


      sync.synchronized {
        while (valid && validatorsReturned < 4) {
          sync.wait(50)
        }
      }

      println("Validator returning betId " + betId + " : " + replyMessage )
      reply(replyMessage)
        // Send unbet to everyone?
    }
    case x => {
      println("Validator got unknown message " + x)
      logger.error("Unknown message " + x)
    }

  }
}

class PutBet extends LiftActor with Loggable {
  val dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ")

  protected def messageHandler = {

    case PutBetMessage(betId: Long, userId: Long, betDescription:String, oppId:Long) => {

      println("Putbet received message " + userId + " bet " + betId)
      val validator = new ValidateBet
      // sigh reply boxes up the response - which I already box so I can pass back failures. There must be a better way.
      println("Sending message to validate bet" + betId)

      validator !? (betId) match {
        case Full(Failure(msg, _, _)) => {
          logger.debug("Failure returned for betId " + betId + " - reject the bet")
          println("Failure for bet " + betId)
        }

        case Full(betId:Long) => {
          logger.debug("BetId " + betId + " returned - accept the bet")
          println("Success for " + betId)
        }
        case x => {
          println("PutBet " + betId + " unexpectedlyg got " + x + " back from validator")
          logger.error("PutBet " + betId + " unexpectedly got " + x + " back from the Validator")
        }
      }
    }
  }
}


class LiabilityChecker extends LiftActor with Loggable {


  // Properties like this? Create a refresh


  def messageHandler = {

    case (ValidatorMessage(betId, func)) => {
            func(Full(betId), "LiabilityChecker")
    }
  }
}



class StakeChecker extends LiftActor with Loggable {

  def messageHandler = {
    case (ValidatorMessage(betId, func)) => {
                                   func(Full(betId), "StakeChecker")
    }
  }
}

class GameStateChecker extends LiftActor with Loggable {
  def messageHandler = {

    case (ValidatorMessage(betId, func)) => {
                      func(Full(betId), "GameStateChecker")
    }
  }
}

//class MarketStateChecker extends LiftActor with Loggable {
//  def messageHandler = {
//
//    case (ValidatorMessage(betId, func)) => {
//      Bet.find(betId) match {
//        case Full(bet:Bet) => {
//          bet.opportunity.obj match {
//            case Full(opportunity:Opportunity) => {
//              opportunity.market.obj match {
//                case Full(market:Market) => {
//                  if (market.suspended.is || !market.published.is){
//                    func(Failure("You can't place a bet when the market is suspended"), "MarketStateChecker")
//                  }else{
//                    func(Full(betId), "MarketStateChecker")
//                  }
//                }
//                case x => {
//                  logger.error("Opportunity with no Market set! " + opportunity)
//                  func(Failure("Invalid Bet"), "MarketStateChecker")
//                }
//              }
//            }
//            case x => {
//              logger.error("Bet with no Opportunity set! " + bet)
//              func(Failure("Invalid Bet"), "MarketStateChecker")
//            }
//          }
//        }
//        case _ => {
//          logger.error("Bet " + betId + " not found when trying to check Stake! ")
//          func(Failure("Invalid Bet"), "MarketStateChecker")
//        }
//      }
//    }
//  }
//}

class TimeChecker extends LiftActor with Loggable {



  var theFunc: (Box[Long], String) => Unit = {(something: Box[Long], who:String) => ()}

  def messageHandler = {

    case (ValidatorMessage(betId, func)) => {
              theFunc = func
              println("Time checker scheduling callback for bet " + betId)
              ActorPing.schedule(this, ActorPingMessage(betId, func), 5000)
    }
    case (ActorPingMessage(betId, func)) => {
      println("Timechecker callback " + betId)
      func(Full(betId), "TimeChecker")
    }
  }
}


case class PutBetMessage(betId: Long, userId: Long, betDescription:String, oppId:Long)
case class ValidatorMessage(betId: Long, validated: (Box[Any], String) => Unit)
case class ActorPingMessage(betId: Long, validated: (Box[Long], String) => Unit)
case class UnBetMessage(betId: Long)



