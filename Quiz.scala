case class QandA(question: String, answers: List[(String, Int)], gradeSheet: Map[String, List[Int]]) { 
      import scala.util.Random.shuffle
      
      val options: Map[String, List[Int]] = gradeSheet
      val q = question
      val a = shuffle(answers)
      
       def ask(): Int = {
        def getIntOr(default: Int): Int = {
      def toIntOrElse(input: String, defaultVal: Int) = try {
        input.toInt
      } catch {
        case _: NumberFormatException => defaultVal
      }

      val output = toIntOrElse(io.StdIn.readLine(), default)
      output
    }
        println(q)
        for(i <- a) println((a.indexOf(i) + 1).toString + ". " + i._1)
        val ans = getIntOr(0) - 1
        if(ans <= 0 || ans > a.length) ask()
        else a(ans)._2
      }
    
   def grader(responses: List[Int], options: Map[String, List[Int]]): String = {
 

       def compare(response: List[Int], opts: List[Int]): Int = {
        val amt = response.zip(opts).map(x => if(x._1 == x._2) 1 else 0).reduceLeft(_ + _)
        amt
      }
      
      val grades = (for {
        (k, v) <- options
      } yield (k, compare(responses, v) ) ).toList
      
      
      val ans = (grades.sortBy(x => x._2))
      val answer = ans(ans.length - 1)._1
      answer
    }
    
    def quiz() = { 
     val user: List[Int] = (for {
       i <- q
     } yield ask()).toList
     val output = grader(user, options)
     output
    }
    
  }
