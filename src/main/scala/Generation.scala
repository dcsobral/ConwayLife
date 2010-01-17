package org.rosettacode.conway_life.scala

class Generation(val alive: Set[Coord]) extends Set[Coord] {
  import Generation._

  // Abstract methods that need to be defined as a Set
  def contains(elem: Coord): Boolean = alive contains elem
  def iterator: Iterator[Coord] = alive.iterator
  def +(elem: Coord): Generation = if (alive contains elem) this else Generation(alive + elem)
  def -(elem: Coord): Generation = if (alive contains elem) Generation(alive - elem) else this

  // Helper methods to be able to pass tuples of Int instead of Coord
  def apply(x: Int, y: Int): Boolean = apply(Coord(x, y))
  def +(x: Int, y: Int): Generation = this.+(Coord(x, y))
  def -(x: Int, y: Int): Generation = this.-(Coord(x, y))
  def ++(coords: Iterator[(Int,Int)]): Generation = Generation(alive ++ (coords map (c => c: Coord)))
  def ++(coords: Traversable[(Int,Int)]): Generation = Generation(alive ++ (coords map (c => c: Coord)))

  /**
   * A list containing all coordinates that are neighbors of a cell which is alive, together
   * with the number of alive cells it is neighbor of.
   */
  lazy val neighbors = alive.toList flatMap (_ neighbors) groupBy identity map { case (c, l) => (c, l.size) }

  // Filter all neighbors for desired characteristics
  def neighborhood(filter: Filter) = for (filter(coord) <- neighbors) yield coord
  def babies = neighborhood(Fecund)
  def adults = alive & neighborhood(Stable).toSet

  /**
   * The next generation is composed of babies from fecund neighborhoods and adults on stable
   * neighborhoods.
   */
  def nextGeneration = Generation(adults ++ babies)

  /**
   * Return a string with the representation of this generation on a window
   * defined by its upper-left and lower-right coordinates.
   */
  def windowToString(upperLeft: Coord, lowerRight: Coord) = {
    def toChar(c: Coord) = if (alive contains c) 'X' else ' '
    def toRow(y: Int) = for (x <- upperLeft.x to lowerRight.x) yield toChar(Coord(x, y))
    def toMatrix = for (y <- upperLeft.y to lowerRight.y by -1) yield toRow(y).mkString
    toMatrix mkString "\n"
  }

  /**
   * This generation's upper left corner
   */
  lazy val upperLeft = {
    val x = alive min Coord.xOrdering x;
    val y = alive max Coord.yOrdering y;
    Coord(x, y)
  }

  /**
   * This generation's lower right corner
   */
  lazy val lowerRight = {
    val x = alive max Coord.xOrdering x;
    val y = alive min Coord.yOrdering y;
    Coord(x, y)
  }

  /**
   * Recenter the pattern without altering its disposition
   */
  def recenter(center: Coord) = {
    val offset = Coord(
      upperLeft.x + (lowerRight.x - upperLeft.x) / 2 - center.x,
      lowerRight.y + (upperLeft.y - lowerRight.y) / 2 - center.y
    )
    Generation(alive map (_ - offset))
  }

  /**
   * Recenter at 0, 0
   */
  def recenter: Generation = recenter(Coord(0, 0))

  override def equals(other: Any) = other match {
    case that: Generation => this.alive == that.alive
    case _ => false
  }
  override def hashCode = alive.hashCode
  override def toString = if (alive.isEmpty) "empty" else windowToString(upperLeft, lowerRight)
}

object Generation {
  def apply(coords: Iterator[Coord]): Generation = apply(coords.toSeq)
  def apply(coords: Traversable[Coord]): Generation = apply(coords.toSet)
  def apply(alive: Set[Coord]) = new Generation(alive)
  def apply() = new Generation(Set.empty[Coord])
  def empty = apply()

  // Helper class to filter neighbors for desired qualities
  class Filter(f: ((Coord, Int)) => Option[Coord]) {
    def unapply(t: (Coord, Int)): Option[Coord] = f(t)
  }
  object Filter {
    def apply(f: ((Coord, Int)) => Option[Coord]) = new Filter(f)
  }

  // A fecund filter will return all coordinates with three neighbors alive
  val Fecund = Filter {
    case (c, 3) => Some(c)
    case _ => None
  }

  // A stable filter will return all coordinates with two or three neighbors alive
  val Stable = Filter {
    case (c, 2) => Some(c)
    case (c, 3) => Some(c)
    case _ => None
  }
}

object ConwayTester {
  import ConwayPatterns._
  val MaxGenerations = 5500 // Give up at MaxGenerations to avoid spending too much time on errors
  val WindowSize = 10 // To check for stable populations, use a window this big
  import Coord.{xOrdering, yOrdering}

  /**
   *  Return an iterator for the generations of a starting pattern
   */
  def conwayIterator(first: Generation) = Iterator.iterate(first)(_.nextGeneration)

  /**
   * Return the period (number of different generations) for oscillators
   */
  def getPeriod(first: Generation) = {
    val it = conwayIterator(first)
    it.next // drop first generation
    (it take MaxGenerations takeWhile (_ != first) length) + 1
  }

  /**
   * Return the period (number of different generations, ignoring offset) for
   * spaceships.
   */
  def getSpaceshipPeriod(first: Generation) = {
    val it = conwayIterator(first) map (_.recenter)
    it.next // drop first generation
    (it take MaxGenerations takeWhile (_ != first) length) + 1
  }

  /**
   * Return the number of generations until the population of a pattern
   * stabilizes. This test only checks a window of generations for
   * population size, as the test for spaceships won't work when multiple
   * spaceships are present.
   */
  def getUnstableGenerations(first: Generation) = (
    conwayIterator(first)
    take MaxGenerations
    map (_.size)
    sliding WindowSize
    map (_.removeDuplicates.length)
    takeWhile (_ > 1)
    length
  )

  /**
   * Return the first generation, properly centered, for a given pattern
   * as represented by a string.
   */
  def initPattern(pattern: String) =
    Generation(pattern: Iterator[Coord]).recenter

  /**
   * For each pattern passed, apply a function which will measure some characteristic
   * of the generations of that pattern, and assert it is equal to expected value.
   */
  def testHarness(patterns: Traversable[(String, Int)], test: Generation => Int, msg: String) =
    assert(patterns forall { case (pattern, period) => test(initPattern(pattern)) == period }, msg)

  // Available tests
  def testStillLives = testHarness(stillLives, getPeriod _, "Failure in still lives")
  def testOscillators = testHarness(oscillators, getPeriod _, "Failure in oscillators")
  def testSpaceships = testHarness(spaceships, getSpaceshipPeriod _, "Failure in spaceships")
  def testMethuselahs = testHarness(methuselahs, getUnstableGenerations _, "Failure in methuselahs")

  /**
   * Do all available tests
   */
  def testAll {
    testStillLives
    testOscillators
    testSpaceships
    testMethuselahs
  }

  /**
   * Do all available tests, and print three generations of a blinker on a 3x3 window.
   */
  def main(args: Array[String]) {
    val upperLeft = Coord(-1, 1)
    val lowerRight = Coord(1, -1)
    testAll
    println("Passed all tests. Printing three generations of blinker:")
    conwayIterator(initPattern(blinker)).zipWithIndex.take(3).toList zip List("st", "nd", "rd") foreach {
      case ((generation, nth), suffix) =>
        println((nth + 1) + suffix + " generation: \n"+generation.windowToString(upperLeft, lowerRight)+"\n")
    }
  }
}

/*
Paralelizar:
List((Set(C1), Set(Neighbors de C1)), alive - C1
Para todo Cx tal que Cx pertence � Neighbors de C1 e � alive,
adicione Cx � Set(C1),
substitua Neighbors de C1 por Neighbors Cx &~ Neighbors de C1
Repita at� n�o existir Cx
Crie uma lista separada
Pegue mais um elemento de alive
Repita

def divide(set: Set[Coord], neighbors: Set[Coord], remaining: Set[Coord]) =
  if (remaining.isEmpty) {
    List(set)
  } else {
    val (joined, separate) = remaining partition (neighbors contains _)
    if (joined.isEmpty)
      joined :: divide(remaining.head, remaining.head.neighbors, remaining.tail)
    else
      divide(set ++ joined, joined.neighbors &~ neighbors, separate)
  }
}

Grupos: alive: Set[Coord], right: Int, left: Int, up: Int, down: Int

A cada nextGeneration, para todos grupos A, B, se A.right == B.left || A.up == B.down, junte ambos

NextGeneration:

Em paralelo, computa o alive de cada grupo
Aplica divide em cada grupo
Calcula left, right, up, down de cada grupo
Aplica join em todos os grupos

Manter grupos centrados, com offset. Cachear grupos, manter offsets separados.

*/
