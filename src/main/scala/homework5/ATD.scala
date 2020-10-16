package homework5

import cats.data.NonEmptyList
import homework5.ATD.HoldemType.{Omaha, Texas}

object ATD {

  sealed trait Suit {
    def abbreviation: Char
  }
  object Suit {
    final case object Diamonds extends Suit {override def abbreviation: Char = 'd'}
    final case object Clubs extends Suit {override def abbreviation: Char = 'c'}
    final case object Hearts extends Suit {override def abbreviation: Char = 'h'}
    final case object Spades extends Suit {override def abbreviation: Char = 's'}
  }

  final case class RankValue private (value: Int) extends AnyVal
  object RankValue {
    def create(value: Int): Option[RankValue] =
      if(value < 2 || value > 14) None
      else Some(RankValue(value))
  }
  sealed trait Rank {
    def abbreviation: Char
    def rankValue: RankValue
  }
  object Rank {
    final case object Two extends Rank {
      override def rankValue: RankValue = RankValue.create(2).getOrElse(None)
      override def abbreviation: Char = '2'
    }
    final case object Three extends Rank {
      override def rankValue: RankValue = RankValue.create(3).getOrElse(None)
      override def abbreviation: Char = '3'
    }
    final case object Four extends Rank {
      override def rankValue: RankValue = RankValue.create(4).getOrElse(None)
      override def abbreviation: Char = '4'
    }
    final case object Five extends Rank {
      override def rankValue: RankValue = RankValue.create(5).getOrElse(None)
      override def abbreviation: Char = '5'
    }
    final case object Six extends Rank {
      override def rankValue: RankValue = RankValue.create(6).getOrElse(None)
      override def abbreviation: Char = '6'
    }
    final case object Seven extends Rank {
      override def rankValue: RankValue = RankValue.create(7).getOrElse(None)
      override def abbreviation: Char = '7'
    }
    final case object Eight extends Rank {
      override def rankValue: RankValue = RankValue.create(8).getOrElse(None)
      override def abbreviation: Char = '8'
    }
    final case object Nine extends Rank {
      override def rankValue: RankValue = RankValue.create(9).getOrElse(None)
      override def abbreviation: Char = '9'
    }
    final case object Ten extends Rank {
      override def rankValue: RankValue = RankValue.create(10).getOrElse(None)
      override def abbreviation: Char = 'T'
    }
    final case object Jack extends Rank {
      override def rankValue: RankValue = RankValue.create(11).getOrElse(None)
      override def abbreviation: Char = 'J'
    }
    final case object Queen extends Rank {
      override def rankValue: RankValue = RankValue.create(12).getOrElse(None)
      override def abbreviation: Char = 'Q'
    }
    final case object King extends Rank {
      override def rankValue: RankValue = RankValue.create(13).getOrElse(None)
      override def abbreviation: Char = 'K'
    }
    final case object Ace extends Rank {
      override def rankValue: RankValue = RankValue.create(14).getOrElse(None)
      override def abbreviation: Char = 'A'
    }
  }

  final case class Card(suit: Suit, rank: Rank)

  sealed trait HoldemType
  object HoldemType {
    case object Texas extends HoldemType
    case object Omaha extends HoldemType
  }

  final case class Hand private (cards: Set[Card])
  object Hand {
    def create(cards: Set[Card], holdemType: HoldemType): Option[Hand] = holdemType match {
      case Texas => if (cards.size == 2) Some(Hand(cards)) else None
      case Omaha => if (cards.size == 4) Some(Hand(cards)) else None
      case _ => None
    }
  }

  final case class Board private (cards: Set[Card])
  object Board {
    def create(cards: Set[Card]): Option[Board] = if(cards.size == 5) Some(Board(cards)) else None
  }

  final case class CombinationValue private (value: Int) extends AnyVal
  object CombinationValue {
    def create(value: Int): Option[CombinationValue] =
      if(value < 1 || value > 10) None
      else Some(CombinationValue(value))
  }
  sealed trait PokerCombination {
    def combinationValue: CombinationValue
  }
  object PokerCombination {
    final case object HighCard extends PokerCombination {
      override def combinationValue: CombinationValue = CombinationValue.create(1).getOrElse(None)
    }
    final case object Pair extends PokerCombination {
      override def combinationValue: CombinationValue = CombinationValue.create(2).getOrElse(None)
    }
    final case object TwoPair extends PokerCombination {
      override def combinationValue: CombinationValue = CombinationValue.create(3).getOrElse(None)
    }
    final case object Set extends PokerCombination {
      override def combinationValue: CombinationValue = CombinationValue.create(4).getOrElse(None)
    }
    final case object Straight extends PokerCombination {
      override def combinationValue: CombinationValue = CombinationValue.create(5).getOrElse(None)
    }
    final case object Flush extends PokerCombination {
      override def combinationValue: CombinationValue = CombinationValue.create(6).getOrElse(None)
    }
    final case object FullHouse extends PokerCombination {
      override def combinationValue: CombinationValue = CombinationValue.create(7).getOrElse(None)
    }
    final case object FourOfAKind extends PokerCombination {
      override def combinationValue: CombinationValue = CombinationValue.create(8).getOrElse(None)
    }
    final case object StraightFlush extends PokerCombination {
      override def combinationValue: CombinationValue = CombinationValue.create(9).getOrElse(None)
    }
    final case object RoyalFlush extends PokerCombination {
      override def combinationValue: CombinationValue = CombinationValue.create(10).getOrElse(None)
    }
  }

  final case class TestCase(board: Board, hands: NonEmptyList[Hand])

  final case class TestResult(pokerCombination: PokerCombination)
}
