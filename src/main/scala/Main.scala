import scala.collection.immutable.LazyList.cons
object Main extends App {

  val drawStars = () => {

    val getPA = (start: Int, factor: Int, size: Int) =>
      (0 until size).map(i => start + i * factor)

    getPA(1, 2, 3).map(i => println("*" * i))

    getPA(1, 2, 2).reverse.map(i => println("*" * i))
  }
  // drawStars()

  val insertInto = (arr: Array[Int], itemToInsert: Int, at: Int) => {

    arr.filter(_ < at) ++ Array(itemToInsert) ++ arr.filter(_ > at)
  }
  // println(insertInto(Array(1, 2, 3, 4, 5), 99, 2))

  def quickSort(arr: Array[Int]): Array[Int] = {

    if (arr.length <= 1) {

      arr

    } else {

      val pIndex = (arr.length / 2).toInt
      val pivot = arr(pIndex)

      val less = arr.filter(_ < pivot)
      val equal = arr.filter(_ == pivot)
      val greater = arr.filter(_ > pivot)

      quickSort(less) ++ equal ++ quickSort(greater)
    }
  }

  def fibonacci(idx: Integer): Integer = {

    if (idx <= 1) {

      idx

    } else {

      fibonacci(idx - 2) + fibonacci(idx - 1)

    }
  }

  def secondBiggest(arr: Array[Int]): Option[Int] = {

    if (arr.length < 2) None

    var bigger = Int.MinValue
    var second = Int.MinValue

    for (i <- arr) {

      if (i > bigger) {

        second = bigger
        bigger = i

      } else if (i > second && i < bigger) {

        second = i
      }
    }

    if (second == Int.MinValue) None else Some(second)
  }

  def removeVogal(txt: String): String = {

    txt.toLowerCase.filter(i => !"aeiou".contains(i))
  }

  def isAnagram(txt1: String, txt2: String): Boolean = {

    txt1.toLowerCase.sorted == txt2.toLowerCase.sorted
  }

  def palindrome(input: String): Boolean = {

    input.toLowerCase == input.reverse.toLowerCase
  }

  def reverseNumber(input: Int): Int = {

    input.toString.reverse.toInt
  }

  def mapTests() = {

    val myMap = Map(1 -> "one", 2 -> "two", 3 -> "three")

    println(myMap(12))
  }

  mapTests()
}
