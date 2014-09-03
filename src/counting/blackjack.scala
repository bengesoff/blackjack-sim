package counting

object Rank extends Enumeration {
  type Rank = Value
  val Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace = Value
}
object Suit extends Enumeration {
  type Suit = Value
  val Diamonds, Spades, Hearts, Clubs = Value
}
object Decision extends Enumeration {
  type Decision = Value
  val Hit, Stand, DoubleDown, Split = Value
}
import Rank._
import Suit._
import scala.collection.immutable.Vector._
import scala.util.Random
import Decision._
case class Card(suit: Suit, value: Rank) {
  override def toString(): String = (value + " of " + suit)
}

class Deck (var cards: Vector[Card]) {
    // Iterates through the Suits and Values 6 times to produce a shoe of 6 decks {
	def this() = this(
	    (for {
		  s <- Suit.values.toVector; 
		  v <- Rank.values
		  } yield Card(s, v)) ++
		  (for {
		  s <- Suit.values.toVector; 
		  v <- Rank.values
		  } yield Card(s, v)) ++
		  (for {
		  s <- Suit.values.toVector; 
		  v <- Rank.values
		  } yield Card(s, v)) ++
		  (for {
		  s <- Suit.values.toVector; 
		  v <- Rank.values
		  } yield Card(s, v)) ++
		  (for {
		  s <- Suit.values.toVector; 
		  v <- Rank.values
		  } yield Card(s, v)) ++
		  (for {
		  s <- Suit.values.toVector; 
		  v <- Rank.values
		  } yield Card(s, v)))
	  // }
	def shuffle = {cards = Random.shuffle(cards)}
	
	def deal: Card = {
	  val card = cards.head
	  cards = cards.tail
	  card
	}
	
	def length: Int = cards.length
	
	override def toString: String = this.length + " cards remaining. "
	
	def printCards = println(cards mkString ", ")
}
class Hand () {
  var cards: List[Card] = Nil
  
  def length: Int = cards.length
  
  override def toString: String = "Hand: " + cards.mkString(", ")
  
  def total(canSoft: Boolean = true, holeCard: Option[Card] = None): Int = {
    var total = holeCard match {
      case None => 0
      case Some(card) => cardValue(card)
    }
    cards.foreach(card => total = cardValue(card))
    if (total > 21 & canSoft & isSoft) {total = total - 10}
    total
  }
  
  def isSoft = (cards.contains(Card(Diamonds, Ace)) | cards.contains(Card(Hearts, Ace))
        | cards.contains(Card(Spades, Ace)) | cards.contains(Card(Clubs, Ace)))
  
  def addCard = cards = Game.shoe.deal +: cards
  
  def cardValue(card: Card): Int = {
    card match {
        case Card(_, Two) => 2
        case Card(_, Three) => 3
        case Card(_, Four) => 4
        case Card(_, Five) => 5
        case Card(_, Six) => 6
        case Card(_, Seven) => 7
        case Card(_, Eight) => 8
        case Card(_, Nine) => 9
        case Card(_, Ten | Jack | Queen | King) => 10
        case Card(_, Ace) => 11
      }
  }
}
class Dealer() {
  var holeCard = Card(Spades, Jack)
  var hand = new Hand()
  def dealHand() = {
    // check sufficient cards in shoe, if not, reshuffle
    // take cards from shoe and give to players
    if(Game.shoe.length < 10) reshuffle()
    Game.basicPlayers.foreach(player => player.hand.addCard)
    Game.countingPlayer.hand.addCard
    hand.addCard
    Game.basicPlayers.foreach(player => player.hand.addCard)
    Game.countingPlayer.hand.addCard
    holeCard = Game.shoe.deal
    if(Game.shoe.length < 10) reshuffle()
  }
  def reshuffle() = {
    // replace the deck with a new shuffled one
    Game.shoe = new Deck()
    Game.shoe.shuffle
    Game.countingPlayer.resetCount()
  }
  def resolveHand() = {
    // simply follow rules
    // when hold card revealed
    def loop() {
      if (hand.total(false, Some(holeCard)) < 17) {
        hand.addCard
        loop()
      }
    }
    Game.countingPlayer.rememberCount(holeCard)
  }
}

class Player() {
  var moneyBalance = 1000.0
  var bettedMoney = 0.0
  var hand = new Hand()
  def insurance() = {} // work this out
  def placeBet() = {bettedMoney += 1.0; moneyBalance -= 1.0} // constant bet
  def makeDecision(): Decision = {Hit} // execute Basic Strategy
}

class CountingPlayer() extends Player {
  var runningCount = 0
  var trueCount = 0.0
  override def placeBet() = {}// pattern match with count thresholds|work out based on count: Kelly Criterion
  override def makeDecision(): Decision = {Hit} // modify Basic Strategy based on favourable situations
  def rememberCount(card: Card) = {
    runningCount = card match {
      case Card(_, Two | Three | Four | Five | Six) => runningCount + 1
      case Card(_, Seven | Eight | Nine) => runningCount
      case Card(_, Ten | Jack | Queen | King | Ace) => runningCount - 1
    }
    trueCount = runningCount / (Game.shoe.length / 52.0)
  }
  def updateCount() = {
    Game.dealer.hand.cards.foreach(card => rememberCount(card))
    Game.basicPlayers.foreach(player => player.hand.cards.foreach(card => rememberCount(card)))
    hand.cards.foreach(card => rememberCount(card))
    trueCount = (runningCount / (Game.shoe.length / 52.0))
    println(runningCount.toString + " " + trueCount.toString + " " + Game.shoe.length)
  }
  def resetCount() = runningCount = 0; trueCount = 0
}
object Game extends App {
  var shoe = new Deck()
  shoe.shuffle
  val basicPlayers = List[Player](new Player(), new Player(), new Player()) // add the other players
  val countingPlayer = new CountingPlayer()
  basicPlayers.foreach(player => player.placeBet())
  countingPlayer.placeBet
  val dealer = new Dealer()
  dealer.dealHand()
  // iterate through players, use tail recursion on each one, until decision is Stand, then move to next
  // dealer resolve hand
  countingPlayer.updateCount()
}