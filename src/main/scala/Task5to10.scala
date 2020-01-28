
object Task5to10 {
  val products = Map("Cheese" -> 5, "Milk" -> 2, "Chocolate" -> 10, "Donut" -> 3)

  def task5(): Map[String,Double] = {
    val sale = products.map { case (k, v) => (k, v * 0.9) }
    sale
  }

  def task6(list: List[Int]): List[Int] = {
    list.map(x => x + 1)
  }

  def task7(list: List[Double]): List[Double] = {
    list.map(x => if (x >= -5 && x < 0) x * (-1) else if (x >= 0 && x <= 12) x)
    list.filter(_ >= -5).filter(_ <= 12).map(x => if (x < 0) x * (-1) else x)
  }

  def task8(tuple3: Tuple3[Int, String, List[Double]]): Unit = {
    println("8:")
    tuple3.productIterator.foreach{ i => println(i) }
  }

  def task9(list: List[Double]): List[Double] = {
    if (list.isEmpty) {
      list
    }
    else if(list.head == 0) {
      task9(list.tail)
    }
    else {
      list.head :: task9(list.tail)
    }
  }

  def task10(product: String) = {
    val price: Option[Int] = products.get(product)
    println(price.getOrElse("Sorry, we do not have " + product))
    if (price.isDefined && price.get <= 2)
      println(product + " is on sale")
    else if (price.isDefined && price.get >= 10){
      println("You are rich enough to buy " + product)
    }
    else {
      println("It was nice to see you")
    }
  }

  def main(args: Array[String]): Unit = {
    val result5 = task5()
    println("5: " + result5)

    val list1: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val result6 = task6(list1)
    println("6: " + result6)

    val list2: List[Double] = List(-100, -20, -6, -5, -2.3, 0, 4, 9, 12, 14.5, 15, 15.5, 40, 41, 0.0)
    val result7 = task7(list2)
    println("7: " + result7)

    val tuple3 = (1, "Hello", List(2.1, 3.2, 4.3))
    task8(tuple3)

    val result9 = task9(list2)
    println("9: " + result9)

    println("10:")
    task10("Milk")
    task10("Chocolate")
    task10("Ice cream")
  }
}
