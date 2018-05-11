import math._
import scala.util._

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
object Player extends App {
  // w: number of columns.
  // h: number of rows.
  def nextCell(x: Int, y: Int, pos: String): (Int, Int) = pos match {
    case "DOWN" => (x, y + 1)
    case "LEFT" => (x - 1, y)
    case "RIGHT" => (x + 1, y)
    case _ => (x,y)
  }

  val instructions: Map[(Int, String), String] = Map(
    (1,"TOP") -> "DOWN",
    (1, "LEFT") -> "DOWN",
    (1, "RIGHT") -> "DOWN",
    (2, "LEFT") -> "RIGHT",
    (2, "RIGHT") -> "LEFT",
    (3, "TOP") -> "DOWN",
    (4, "TOP") -> "LEFT",
    (4, "RIGHT") -> "DOWN",
    (5, "TOP") -> "RIGHT",
    (5, "LEFT") -> "DOWN",
    (6, "LEFT") -> "RIGHT",
    (6, "RIGHT") -> "LEFT",
    (7, "TOP") -> "DOWN",
    (7, "RIGHT") -> "DOWN",
    (8, "LEFT") -> "DOWN",
    (8, "RIGHT") -> "DOWN",
    (9, "LEFT") -> "DOWN",
    (9, "TOP") -> "DOWN",
    (10, "TOP") -> "LEFT",
    (11, "TOP") -> "RIGHT",
    (12, "RIGHT") -> "DOWN",
    (13, "LEFT") -> "DOWN"
  )

  val Array(w, h) = scala.io.StdIn.readLine.split(" ").map(_.toInt)
  val grid = Array.ofDim[Int](h,w)
  for(i <- 0 until h) {
    grid(i) = readLine.split(" ").map(_.toInt) // represents a line in the grid and contains W integers. Each integer represents one room of a given type.
  }
  val ex = readInt // the coordinate along the X axis of the exit (not useful for this first mission, but must be read).

  // game loop
  while(true) {
    val Array(_xi, _yi, pos) = readLine split " "
    val xi = _xi.toInt
    val yi = _yi.toInt
    if (ex == xi && yi == h) System.exit(0);

    val nextPos = instructions.get((grid(yi)(xi), pos.toUpperCase())).getOrElse("DOWN")
    val nextStep = nextCell(xi,yi,nextPos)
    // Write an action using println
    // To debug: Console.err.println("Debug messages...")


    // One line containing the X Y coordinates of the room in which you believe Indy will be on the next turn.
    println(s"${nextStep._1} ${nextStep._2}")

  }
}
