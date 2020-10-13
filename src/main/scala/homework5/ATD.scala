package homework5

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
      override def rankValue: RankValue = RankValue(2)
      override def abbreviation: Char = '2'
    }
    final case object Three extends Rank {
      override def rankValue: RankValue = RankValue(3)
      override def abbreviation: Char = '3'
    }
    final case object Four extends Rank {
      override def rankValue: RankValue = RankValue(4)
      override def abbreviation: Char = '4'
    }
    final case object Five extends Rank {
      override def rankValue: RankValue = RankValue(5)
      override def abbreviation: Char = '5'
    }
    final case object Six extends Rank {
      override def rankValue: RankValue = RankValue(6)
      override def abbreviation: Char = '6'
    }
    final case object Seven extends Rank {
      override def rankValue: RankValue = RankValue(7)
      override def abbreviation: Char = '7'
    }
    final case object Eight extends Rank {
      override def rankValue: RankValue = RankValue(8)
      override def abbreviation: Char = '8'
    }
    final case object Nine extends Rank {
      override def rankValue: RankValue = RankValue(9)
      override def abbreviation: Char = '9'
    }
    final case object Ten extends Rank {
      override def rankValue: RankValue = RankValue(10)
      override def abbreviation: Char = 'T'
    }
    final case object Jack extends Rank {
      override def rankValue: RankValue = RankValue(11)
      override def abbreviation: Char = 'J'
    }
    final case object Queen extends Rank {
      override def rankValue: RankValue = RankValue(12)
      override def abbreviation: Char = 'Q'
    }
    final case object King extends Rank {
      override def rankValue: RankValue = RankValue(13)
      override def abbreviation: Char = 'K'
    }
    final case object Ace extends Rank {
      override def rankValue: RankValue = RankValue(14)
      override def abbreviation: Char = 'A'
    }
  }

  final case class Card(suit: Suit, rank: Rank)

  final case class Hand private (cards: List[Card]) extends AnyVal
  object Hand {
    def create(cards: List[Card]): Option[Hand] = if(cards.size == 2) Some(Hand(cards)) else None
  }

  final case class Board private (cards: List[Card]) extends AnyVal
  object Board {
    def create(cards: List[Card]): Option[Board] = if(cards.size == 5) Some(Board(cards)) else None
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
      override def combinationValue: CombinationValue = CombinationValue(1)
    }
    final case object Pair extends PokerCombination {
      override def combinationValue: CombinationValue = CombinationValue(2)
    }
    final case object TwoPair extends PokerCombination {
      override def combinationValue: CombinationValue = CombinationValue(3)
    }
    final case object Set extends PokerCombination {
      override def combinationValue: CombinationValue = CombinationValue(4)
    }
    final case object Straight extends PokerCombination {
      override def combinationValue: CombinationValue = CombinationValue(5)
    }
    final case object Flush extends PokerCombination {
      override def combinationValue: CombinationValue = CombinationValue(6)
    }
    final case object FullHouse extends PokerCombination {
      override def combinationValue: CombinationValue = CombinationValue(7)
    }
    final case object FourOfAKind extends PokerCombination {
      override def combinationValue: CombinationValue = CombinationValue(8)
    }
    final case object StraightFlush extends PokerCombination {
      override def combinationValue: CombinationValue = CombinationValue(9)
    }
    final case object RoyalFlush extends PokerCombination {
      override def combinationValue: CombinationValue = CombinationValue(10)
    }
  }

  final case class TestCase(board: Board, hands: List[Hand])

  final case class TestResult(pokerCombination: PokerCombination)
}
