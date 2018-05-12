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

  var alphabet: Array[Array[String]] = Array.ofDim(20, l)

  for (i <- 0 until 20) {
    val letter = Array.ofDim[String](l)
    for (j <- 0 until l) {
      letter(j) = alphabetLines(j)(i)
    }
    alphabet(i) = letter
  }

  var alphabetMap = Map.empty[Int, Int]
  var alphabetMapReverse = Map.empty[Int, Array[String]]

  for (i <- 0 until 20) {
    alphabetMap = alphabetMap + (hash(alphabet(i)) -> i)
    alphabetMapReverse = alphabetMapReverse + (i -> alphabet(i))
  }

  def hash(letter: Array[String]): Int = {
    val fullString = new StringBuilder()
    for (i <- letter.indices) fullString.append(letter(i))
    fullString.toString().hashCode
  }

  def decodeLetter(letter: Array[String]) : Double = {
    val letterCarried = letter.grouped(4).toArray.reverse
    var result: Double = 0
    for (i <- 0 until letterCarried.length) {
      result = result + (alphabetMap.get(hash(letterCarried(i))).getOrElse(0) * Math.pow(20, i))
    }
    println(result)
    result
  }

  def encodeLetter(number: Int):Array[String] = {
    //val result = (log10(number)/log10(20)).toInt

    var powers: Array[Int] = Array.ofDim[Int](20)
    var buffer = number.toDouble
    breakable{
      for (i <- 0 until 20){
        while (buffer - Math.pow(20, i) >= 0) {
          buffer = buffer - Math.pow(20, i)
          powers(i) = powers(i) + 1
          if (powers(i) == 20){
            powers(i) = 0
            powers(i + 1) = powers(i + 1) + 1
          }
        }
        if (buffer <= 0) break;
      }
    }
    var result: Array[String] = Array.empty[String]
    for (i <- 0 until powers.length) {
      if (powers(i) > 0){
        result = alphabetMapReverse.get(powers(i)).getOrElse(Array.empty) ++ result
      }
    }
    result
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

  val result = encodeLetter(((operation) match {
    case "*" => decodeLetter(letter1) * decodeLetter(letter2)
    case "/" => decodeLetter(letter1) / decodeLetter(letter2)
    case "+" => decodeLetter(letter1) + decodeLetter(letter2)
    case "-" => decodeLetter(letter1) - decodeLetter(letter2)
  }).toInt)

  // Write an action using println
  // To debug: Console.err.println("Debug messages...")

  for (i <- 0 until result.length) {
    println(result(i))
  }
}