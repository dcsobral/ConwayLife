class Coord private (val x: Int, val y: Int) {
  private val offsets = List(-1, 0, 1)
  private def offsetsOf(n: Int) = offsets map (_ + n)
  
  /**
   * A memoized list of all neighbors of a coordinate
   */
  lazy val neighbors = for {
    xn <- offsetsOf(x) if Coord.legal(xn)
    yn <- offsetsOf(y) if Coord.legal(yn) && (x, y) != (xn, yn)
  } yield Coord(xn, yn)

  // Coordinates can be used as offsets
  def +(c: Coord) = Coord(x + c.x, y + c.y)
  def -(c: Coord) = Coord(x - c.x, y - c.y)

  override def equals(other: Any) = other match {
    case that: Coord => this.x == that.x && this.y == that.y
    case _ => false
  }
  override def hashCode = ((x * 41) + y) * 41 + 41
  override def toString = "Coord(%d, %d)" format (x, y)
}

object Coord {
  // A Conway board is infinite in size; throw an exception if our hard limits are reached
  private def legal(n: Int) = {
    n.ensuring(Int.MinValue < _, "Coord too low").ensuring(_ < Int.MaxValue, "Coord too high")
    true
  }
  private val cache = new scala.collection.mutable.HashMap[(Int,Int), Coord]
  
  /**
   * Factory for coordinates. All coordinates are memoized.
   */
  def apply(x: Int, y: Int) = {
    require(legal(x) && legal(y))
    cache getOrElseUpdate ((x,y), new Coord(x, y))
  }
  
  /**
   * Coordinate extractor
   */
  def unapply(c: Coord) = Some((c.x, c.y))
  
  /**
   * An Ordering for coordinates which sorts by the X coordinate
   */
  val xOrdering = Ordering.fromLessThan((_: Coord).x < (_: Coord).x)

  /**
   * An Ordering for coordinates which sorts by the Y coordinate
   */
  val yOrdering = Ordering.fromLessThan((_: Coord).y < (_: Coord).y)

  /**
   * Any Tuple2[Int, Int] can be used as a Coordinate through this implict
   * conversion.
   */
  implicit def coordFromTuple(t: (Int, Int)) = apply(t._1, t._2)
}

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
  def recenter = {
    val offset = Coord(
      upperLeft.x + (lowerRight.x - upperLeft.x) / 2,
      lowerRight.y + (upperLeft.y - lowerRight.y) / 2
    )
    Generation(alive map (_ - offset))
  }

  override def equals(other: Any) = other match {
    case that: Generation => this.alive == that.alive
    case _ => false
  }
  override def hashCode = alive.hashCode
  override def toString = windowToString(upperLeft, lowerRight)
}

object Generation {
  def apply(coords: Iterator[Coord]): Generation = apply(coords.toSeq)
  def apply(coords: Traversable[Coord]): Generation = apply(coords.toSet)
  def apply(alive: Set[Coord]) = new Generation(alive)
  def apply() = new Generation(Set.empty[Coord])
  
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

object ConwayPatterns {
  // Lists for all patterns available
  def stillLives = List(block, beehive, loaf, boat) map ((_, 1))
  def oscillators = (eight, 8) :: (pulsar, 3) :: oscillators2.map((_, 2)) 
  def oscillators2 = List(blinker, toad, beacon)
  def oscillators3 = List(pulsar)
  def oscillators8 = List(eight)
  def spaceships = List(glider, LWSS) map ((_, 4)) 
  def methuselahs = List((diehard, 130), (acorn, 5206), (rPentomino, 1103))
  def guns = List(gosperGun)

  // Still Lives patterns
  val block = """|
                 | XX
                 | XX
                 |"""
  val beehive = """|
                   |  XX
                   | X  X
                   |  XX
                   |"""
  val loaf = """|
                |  XX
                | X  X
                |  X X
                |   X
                |"""
  val boat = """|
                | XX
                | X X
                |  X
                |"""
  
  // Oscillators patterns
  val blinker = """|
                   |
                   | XXX
                   |
                   |"""
  val toad = """|
                |
                |  XXX
                | XXX
                |
                |"""
  val beacon = """|
                  | XX
                  | XX
                  |   XX
                  |   XX
                  |"""
  val pulsar = """|
                  |
                  |    XXX   XXX
                  |
                  |  X    X X    X
                  |  X    X X    X
                  |  X    X X    X
                  |    XXX   XXX
                  |
                  |    XXX   XXX
                  |  X    X X    X
                  |  X    X X    X
                  |  X    X X    X
                  |
                  |    XXX   XXX
                  |
                  |"""
  val eight = """|
                 |XXX
                 |XXX
                 |XXX
                 |   XXX
                 |   XXX
                 |   XXX
                 |"""


  // Spaceship patterns
  val glider = """|
                  |   X
                  | X X
                  |  XX
                  |"""
  val LWSS = """|
                |
                |  XXXX
                | X   X
                |     X
                | X  X
                |"""
               
  // Methuselah patterns
  val diehard = """|
                   |       X
                   | XX
                   |  X   XXX
                   |"""
  
  val acorn = """|
                 |  X
                 |    X
                 | XX  XXX
                 |"""
                 
  val rPentomino = """|
                      | XX
                      |  XX
                      |  X
                      |"""
                      
  // Guns
  val gosperGun = """|
                     |                        X
                     |                      X X
                     |            XX      XX            XX
                     |           X   X    XX            XX
                     |XX        X     X   XX
                     |XX        X   X XX    X X
                     |          X     X       X
                     |           X   X
                     |            XX
                     |"""
                      
  // Helper methods
  // Enable constructing sets of coordinates from string patterns.
  implicit def coordsFromPattern(pattern: String) = for {
    (xs, y) <- pattern.stripMargin.split('\n').map(_.zipWithIndex).zipWithIndex.iterator
    (c, x) <- xs.iterator
    if c != ' '
  } yield Coord(x, y)
                      
  // Move a set of coordinates to a point
  def moveTo(pattern: String, to: Coord) = (pattern: Iterator[Coord]) map (_ + to)
  def moveTo(coords: Iterator[Coord], to: Coord) = coords map (_ + to)
  def moveTo(coords: Traversable[Coord], to: Coord) = coords map (_ + to)
  
}

object ConwayTester {
  import ConwayPatterns._
  val MaxGenerations = 5500 // Give up at MaxGenerations to avoid spending too much time on errors
  val WindowSize = 10 // To check for stable populations, use a window this big
  import Coord.{xOrdering, yOrdering} 

  /**
   * Return an iterator for the generations of a starting pattern
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
Para todo Cx tal que Cx pertence à Neighbors de C1 e à alive,
adicione Cx à Set(C1),
substitua Neighbors de C1 por Neighbors Cx &~ Neighbors de C1
Repita até não existir Cx
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

