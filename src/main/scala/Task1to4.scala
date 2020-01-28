import scala.annotation.tailrec

object Task1to4 {
  def task1a(daysList: List[String]): String = {
    var daysStr = ""
    for( day <- daysList){
      val newStr = daysStr.concat(day.concat(","))
      daysStr = newStr
    }
    daysStr = daysStr.dropRight(1)
    daysStr
  }

  def task1b(daysList: List[String]): String = {
    var daysStr = ""
    for( day <- daysList){
      if(day.startsWith("S")) {
        val newStr = daysStr.concat(day.concat(","))
        daysStr = newStr
      }
    }
    daysStr = daysStr.dropRight(1)
    daysStr
  }

  def task1c(daysList: List[String]): String = {
    var daysStr = ""
    var index = 0
    while( index < daysList.length){
      val newStr = daysStr.concat(daysList(index).concat(","))
      daysStr = newStr
      index = index + 1
    }
    daysStr = daysStr.dropRight(1)
    daysStr
  }

  def task2a(daysList: List[String]): String = {
    if(daysList.tail.isEmpty) {
       daysList.head
    }
    else {
      daysList.head + "," + task2a(daysList.tail)
    }
  }

  def task2b(daysList: List[String]): String = {
    if (daysList.tail.isEmpty)
       daysList.head
    else {
      task2a(daysList.tail) + "," + daysList.head
    }
  }

  def task3a(daysList1: List[String]): String = {
    @tailrec
    def task3b(daysList2: List[String], str :String): String = {
      if (daysList2.tail.isEmpty)
        str + daysList2.head
      else  {
        task3b(daysList2.tail, str + daysList2.head + ",")
      }
    }
    task3b(daysList1,"")
  }

  def task4a(daysList: List[String]): String = {
    daysList.foldLeft("") { (z, i) =>
      z + i + ","
    }
  }

  def task4b(daysList: List[String]): String = {
    daysList.foldRight("") { (z, i) =>
      z + "," + i
    }
  }

  def task4c(daysList: List[String]): String = {
    daysList.foldLeft("") { (z, i) =>
      if(i.startsWith("S")) {
        z + i + ","
      }
      else {
        ""
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val days: List[String] = List("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

    val result1a = task1a(days)
    println("1a: " + result1a)
    val result1b = task1b(days)
    println("1b: " + result1b)
    val result1c = task1c(days)
    println("1c: " + result1c)
    val result2a = task2a(days)
    println("2a: " + result2a)
    val result2b = task2b(days)
    println("2b: " + result2b)
    val result3 = task3a(days)
    println("3: " + result3)
    val result4a = task4a(days).dropRight(1)
    println("4a: " + result4a)
    val result4b = task4b(days).dropRight(1)
    println("4b: " + result4b)
    val result4c = task4c(days).dropRight(1)
    println("4c: " + result4c)
  }
}
