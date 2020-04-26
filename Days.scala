


import scala.annotation.tailrec

object Cwiczenie1 {

  def main(args: Array[String]) = {

    //TASK 1

    val daysList: List[String] = List("Poniedziałek", "Wtorek", "Środa","Czwartek", "Piątek", "Sobota", "Niedziela");

    var  daysString: String = ""
    
    def concatDays(daysList: List[String]): String = {      
      for(day <- daysList){
          daysString += day +","
        }
      daysString
    }

    /// println(concatDays(daysList))


    def concatDaysFirstLetterP(daysList: List[String]): String = {

      for(d <- daysList; if d.charAt(0).toString().toUpperCase() == "P" ){
         daysString += d +","
      }
      daysString
    }

     ///println(concatDaysFirstLetterP(daysList))

    def concatDaysByWhileLoop(daysList: List[String]): String = {
      var daysStart = 0;
      while(daysStart < daysList.length){
         daysString += daysList(daysStart) +","
         daysStart = daysStart + 1;
      }  
      daysString
      
    }
    ///println(concatDaysByWhileLoop(daysList))

    //TASK 2

    def concatDaysRecur(daysList: List[String]): String = {
      if (daysList.isEmpty)  ""
      else daysList.head + ", " + concatDaysRecur(daysList.tail)
    }

    ///println(concatDaysRecur(daysList))

    def concatDaysRecurFromEndToStart(daysList: List[String]): String = {
      if (daysList.isEmpty) ""
      else concatDaysRecurFromEndToStart(daysList.tail) + " ," + daysList.head
    }

    ///println(concatDaysRecurFromEndToStart(daysList))

    //TASK 3

    def concatDaysRecurTail(daysList: List[String]): String = {
      @tailrec
      def concat(daysList: List[String], result: String): String = {
        daysList match {
          case Nil => result
          case x :: tail => concat(tail, result + x + ", ")
        }

      }

      concat(daysList, "")
    }

    ///println(concatDaysRecurTail(daysList))

    //TASK 4

    def concatDaysFoldLeft(daysList: List[String]): String = {
      daysList.foldLeft("")(_ + _ + ", ")
    }

    ///println(concatDaysFoldLeft(daysList))

    def concatDaysFoldRight(daysList: List[String]): String = {
      daysList.foldRight("")(", " + _ + _).substring(2)
    }

    ///println(concatDaysFoldRight(daysList))
   
     def concatDaysFoldLeftFirstLetterP(daysList: List[String]): String = {
        daysList.foldLeft("")((a,b) => 
          { 
            if(b.charAt(0).toString().toUpperCase() == "P")  a + b + ", "
            else a + ""
          }
        )
      }
      
    ///println(concatDaysFoldLeftFirstLetterP(daysList))


    //TASK 5
     
    val products = Map("masło" -> 5, "papryka" -> 7, "Monte" -> 3)

    var productsDiscount = products map { case (key, value) => (key, value * 0.9) }

    ///println(productsDiscount);

    //TASK 6
    
   def showTuplesVal(tuple3: (Int, Double, String)) = {
      tuple3.productIterator.foreach { i => println(i) }
    }

    ///println(showTuplesVal(17, 9.99, "asd"))
    
    //TASK 7
    val countries = Map("Serbia" -> "Belgrad",
      "Morocco" -> "Rabat", "Denmark" -> "Copenhagen")

    def findCapital(x: Option[String]) = x match {
      case Some(a) => a
      case None => "I haven't found"
    }

    ///println(findCapital(countries.get("Cratia")))
    ///println(findCapital(countries.get("Morocco")))

    
    //TASK 8
    val values = List(1, 2, 0, 2, 2, 0, 1, 12, 0, 0, 0, 1, 10, 11)
    
    
    def removeZero(list: List[Int], n: Int): List[Int] = list match {
        case Nil => Nil
        case h :: t =>
          if (h == n)
           removeZero(t, n) 
          else
            h :: removeZero(t, n)
      }
       
     ///println(removeZero(values, 0))
    
     //TASK 9
    
     def addOneForItems(values: List[Int]): List[Int] = {
      values.map(x => x+1) 
     }
     
     ///println(addOneForItems(values))
     
     //TASK 10
    val numbers = List( -2, 1, -2, 4, -5, 12, 11, -10, -4, -3);
       
    def absoluteValues(numbers: List[Int]): List[Int] = {
      numbers.map(x => x.abs).filter(x => 0 <= x)
    }
     
    ///println( absoluteValues(numbers))

  }
}
