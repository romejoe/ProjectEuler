package p84

import common.TimedApp
import common.numtheory.ModInt

import scala.collection.immutable.Queue
import scala.util.Random

object Main extends TimedApp {
  /*
In the game, Monopoly, the standard board is set up in the following way:

GO	A1	CC1	A2	T1	R1	B1	CH1	B2	B3	JAIL
H2	 	C1
T2	 	U1
H1	 	C2
CH3	 	C3
R4	 	R2
G3	 	D1
CC3	 	CC2
G2	 	D2
G1	 	D3
G2J	F3	U2	F2	F1	R3	E3	E2	CH2	E1	FP
A player starts on the GO square and adds the scores on two 6-sided dice to determine the number of squares they advance in a clockwise direction. Without any further rules we would expect to visit each square with equal probability: 2.5%. However, landing on G2J (Go To Jail), CC (community chest), and CH (chance) changes this distribution.

In addition to G2J, and one card from each of CC and CH, that orders the player to go directly to jail, if a player rolls three consecutive doubles, they do not advance the result of their 3rd roll. Instead they proceed directly to jail.

At the beginning of the game, the CC and CH cards are shuffled. When a player lands on CC or CH they take a card from the top of the respective pile and, after following the instructions, it is returned to the bottom of the pile. There are sixteen cards in each pile, but for the purpose of this problem we are only concerned with cards that order a movement; any instruction not concerned with movement will be ignored and the player will remain on the CC/CH square.

Community Chest (2/16 cards):
Advance to GO
Go to JAIL
Chance (10/16 cards):
Advance to GO
Go to JAIL
Go to C1
Go to E3
Go to H2
Go to R1
Go to next R (railway company)
Go to next R
Go to next U (utility company)
Go back 3 squares.
The heart of this problem concerns the likelihood of visiting a particular square. That is, the probability of finishing at that square after a roll. For this reason it should be clear that, with the exception of G2J for which the probability of finishing on it is zero, the CH squares will have the lowest probabilities, as 5/8 request a movement to another square, and it is the final square that the player finishes at on each roll that we are interested in. We shall make no distinction between "Just Visiting" and being sent to JAIL, and we shall also ignore the rule about requiring a double to "get out of jail", assuming that they pay to get out on their next turn.

By starting at GO and numbering the squares sequentially from 00 to 39 we can concatenate these two-digit numbers to produce strings that correspond with sets of squares.

Statistically it can be shown that the three most popular squares, in order, are JAIL (6.24%) = Square 10, E3 (3.18%) = Square 24, and GO (3.09%) = Square 00. So these three most popular squares can be listed with the six-digit modal string: 102400.

If, instead of using two 6-sided dice, two 4-sided dice are used, find the six-digit modal string.
  */
  val randSeed = new Random(System.currentTimeMillis())

  implicit class RichQueue[A](q: Queue[A]) {
    def enqueueFinite[B >: A](elem: B, maxSize: Int): Queue[B] = {
      var ret = q.enqueue(elem)
      while (ret.size > maxSize) { ret = ret.dequeue._2 }
      ret
    }

    def enqueueTimes[B >: A](elem: B, amount:Int):Queue[B] = {
      q ++ (0 until amount).map(_ => elem)
    }
  }


  class Dice(sides:Int){
    val rand = new Random(randSeed.nextLong())
    def roll = rand.nextInt(sides) +1
  }

  object Monopoly{
    val GO = ModInt(0, 40)
    val JAIL = ModInt(10, 40)
    val GOTOJAIL = ModInt(10, 40)
    val CC1 = ModInt(2,40)
    val CC2 = ModInt(17,40)
    val CC3 = ModInt(33,40)
    val CH1 = ModInt(7,40)
    val CH2 = ModInt(22,40)
    val CH3 = ModInt(36,40)
  }

  class Monopoly(die:Dice){
    import Monopoly._
    val rand = new Random(randSeed.nextLong())
    var Position = ModInt(0, 40)
    var rollState:Queue[Int] = Queue()
    var chanceDeck = rand shuffle Queue("GO", "JAIL", "C1", "E3", "H2", "R1", "NR", "NR", "NU", "BACK3").enqueueTimes("NA", 6)
    var communityDeck = rand shuffle Queue("GO", "JAIL").enqueueTimes("NA", 14)

    def PerformMove() = {
      val move = die.roll + die.roll

      rollState = rollState.enqueueFinite(move, 3)
      Position = if(rollState.length == 3 && rollState.distinct.size == 1)
        JAIL
      else{
        val tmp = Position + move
        tmp match{
          case CC1|CC2|CC3 =>
            //Community
            val (card, tmpDeck) = communityDeck.dequeue
            communityDeck = tmpDeck enqueue card
            card match {
              case "GO" => GO
              case "JAIL" => JAIL
              case _ => tmp
            }
          case CH1|CH2|CH3 =>
            //Chance
            val (card, tmpDeck) = chanceDeck.dequeue
            chanceDeck = tmpDeck enqueue card
            card match {
              case "GO" => GO
              case "JAIL" => JAIL
              case "C1" => ModInt(11, 40)
              case "E3" => ModInt(24, 40)
              case "H2" => ModInt(39, 40)
              case "R1" => ModInt(5, 40)
              case "NR" => {
                if(tmp.value < 5 || tmp.value > 35)
                  ModInt(5,40) //R1
                else if(tmp.value < 15)
                  ModInt(15, 40) //R2
                else if(tmp.value < 25)
                  ModInt(25,40) //R3
                else
                  ModInt(35, 40) //R4
              }
              case "NU" => {
                if(tmp.value < 12 || tmp.value > 28)
                  ModInt(12,40) //U1
                else
                  ModInt(28, 40) //U2
              }
              case "BACK3" => tmp - 3
              case _ => tmp
            }
          case GOTOJAIL => JAIL
          case _ => tmp
        }
      }
      Position
    }
  }

  val Die = new Dice(6)


  val sampleSize = 100000
  val sampleLength = 200
  val games = (0 until sampleLength).map(_ => new Monopoly(Die))
  val sample = games.map(game =>{
    (0 until sampleSize)
      .map(_ => game.PerformMove())
      .groupBy(_.value)
      .map(p => (p._1,p._2.length))
  })

  println(
    sample.reduce((a,b) => {
      (a.keySet ++ b.keySet)
        .map(k => (k, a.getOrElse(k,0) + b.getOrElse(k,0))).toMap
    })
    .toSeq
    .sortBy(-_._2)
    .take(5).map(p =>{
      if(p._1 < 10)
        "0" + p._1
      else
        p._1
    }).mkString
  )
  //Answer:10,15,24

}
