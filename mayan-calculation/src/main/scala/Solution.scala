import math._
import scala.util._
import scala.util.control.Breaks._
/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
object Solution extends App {

  val Array(l, h) = for(i <- scala.io.StdIn.readLine() split " ") yield i.toInt

  var alphabetLines = Array.ofDim[String](l, h)

  for(i <- 0 until l) {
    alphabetLines(i) = scala.io.StdIn.readLine().grouped(h).toArray
  }

  val alphabetSize = alphabetLines(0).length

  var alphabet: Array[Array[String]] = Array.ofDim(alphabetSize, l)

  for (i <- 0 until alphabetSize) {
    val letter = Array.ofDim[String](l)
    for (j <- 0 until l) {
      letter(j) = alphabetLines(j)(i)
    }
    alphabet(i) = letter
  }

  var alphabetMap = Map.empty[Int, Int]
  var alphabetMapReverse = Map.empty[Long, Array[String]]

  for (i <- 0 until alphabetSize) {
    alphabetMap = alphabetMap + (hash(alphabet(i)) -> i)
    alphabetMapReverse = alphabetMapReverse + (i.toLong -> alphabet(i))
  }

  def hash(letter: Array[String]): Int = {
    val fullString = new StringBuilder()
    for (i <- letter.indices) fullString.append(letter(i))
    fullString.toString().hashCode
  }

  def decodeLetter(letter: Array[String]) : Double = {
    val letterCarried = letter.grouped(h).toArray.reverse
    var result: Double = 0
    for (i <- 0 until letterCarried.length) {
      result = result + (alphabetMap.get(hash(letterCarried(i))).getOrElse(0) * Math.pow(alphabetSize, i))
    }
    result
  }

  def encodeLetter(number: Long): Unit = {
    if (number < alphabetSize){
      val result = alphabetMapReverse.get(number).getOrElse(Array.empty)
      for (i <- 0 until result.length) {
        println(result(i))
      }
    } else {
      encodeLetter(number / alphabetSize)
      encodeLetter(number % alphabetSize)
    }
  }

  val s1 = scala.io.StdIn.readInt()
  val letter1 = Array.ofDim[String](s1)

  for(i <- 0 until s1) {
    letter1(i) = scala.io.StdIn.readLine()
  }

  val s2 = scala.io.StdIn.readInt()
  val letter2 = Array.ofDim[String](s2)

  for(i <- 0 until s2) {
    letter2(i) = scala.io.StdIn.readLine()
  }

  val operation = scala.io.StdIn.readLine()

  encodeLetter(((operation) match {
    case "*" => decodeLetter(letter1) * decodeLetter(letter2)
    case "/" => decodeLetter(letter1) / decodeLetter(letter2)
    case "+" => decodeLetter(letter1) + decodeLetter(letter2)
    case "-" => decodeLetter(letter1) - decodeLetter(letter2)
  }).toLong)

  // Write an action using println
  // To debug: Console.err.println("Debug messages...")

}
