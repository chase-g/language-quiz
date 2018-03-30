//reusable quiz template
    case class Quiz(questions: List[String], answers: List[List[String]], gradeSheet: Map[String, List[Int]]) { 
      import scala.util.Random.shuffle
      
      val options: Map[String, List[Int]] = gradeSheet
      val qu: List[String] = questions
      //zips each answer with corresponding number (of original order starting from 1) for grading purposes
      //shuffles this list of answer-number pairs (so that presentation order will not bias the quiz on average)
      val an: List[List[(String, Int)]] = answers.map((x => x.zip((1 to x.length).toList))).map(x => shuffle(x))
      
       private def ask(q: String, a: List[(String, Int)]): Int = {
      //getIntOr() function ensures that user input matches an available answer choice (by number) or else asks again
        def getIntOr(default: Int): Int = {
         def toIntOrElse(input: String, defaultVal: Int) = try {
        input.toInt
      } catch {
        case _: NumberFormatException => defaultVal
      }

      val output = toIntOrElse(io.StdIn.readLine(), default)
      output
    }
        //print question, for each answer print with a number (in order for the user, but not according to the grading number)
        println(q)
        for(i <- a) println((a.indexOf(i) + 1).toString + ". " + i._1)
        val ans = getIntOr(0) 
        if(ans <= 0 || ans > a.length) ask(q, a)
        else a(ans - 1)._2
      }
    
   private def grader(responses: List[Int], options: Map[String, List[Int]]): List[(String, Int)] = {
       def compare(response: List[Int], opts: List[Int]): Int = {
      //checks whether each the grading number associated with the user's answer choice matches the language in question:
      //zips the choosen answer tuple with the answer number associated with the language
      //if they match, set that list item to 1, or else 0. Sum this list for a cumulative score for that language
        val amt = response.zip(opts).map(x => if(x._1 == x._2) 1 else 0).reduceLeft(_ + _)
        amt
      }
      //run comparison for every language with a for exp. Store the scores in a list 
      val grades = (for {
        (k, v) <- options
      } yield (k, compare(responses, v) ) ).toList
      
      //sort that list by score and return the language with the highest score
      val ans: List[(String, Int)] = (grades.sortBy(x => x._2)).reverse
      ans
      //val answer = ans(ans.length - 1)._1
      //answer
    }
    
   //run the quiz. This is the public method for accessing the quiz
    def quiz(): Unit = { 
     val user: List[Int] = (for {
       i <- 0 until qu.length
     } yield ask(qu(i), an(i))).toList
     val output: List[(String, Int)] = grader(user, options)
     val answer = output(0)._1
     println("\nYou should try " + answer + "\n")
     for(n <- output) println(((n._2.toDouble / qu.length) * 100).toInt.toString + "% " + n._1)
    }
    
  }
