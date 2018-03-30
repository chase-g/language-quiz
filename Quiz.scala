object Quiz {
  def main(args: Array[String]): Unit = {
    
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
        println("\n" + q)
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
    
    case class QandAbuilder() {
    val q1 = "What programming domain would you like your language to excel in?"
    val a1 = List("Web development", "Jack of all trades", "High performance, such as games or operating systems", "App development", "Not sure")

    val q2 = "What kind of origin would you like your language to have?"
    val a2 = List("Classic, with a large community", "Relatively new, and built with improvements in mind", "Created and promoted by a prestigious company.", "Not sure")

    val q3 = "What kind of learning experience would you prefer?"
    val a3 = List("Challenging, but you will learn new paradigms or algorithms along the way", "Easy to get productive quickly", "Somewhere in between", "Not sure")

    val q4 = "Which James Bond?"
    val a4 = List("Sean Connery", "Roger Moore", "Pierce Brosnan", "Daniel Craig", "Timothy Dalton")
    
    val q5 = "What should be your language's greatest strength?"
    val a5 = List("Logically consistent", "Concise", "Usable anywhere", "Fast", "Useful", "Not sure")
    
    val q6 = "What weakness could you most accept in a language?"
    val a6 = List("Difficult to learn", "Error prone for large programs", "Verbose--lots of typing to do anything", "Inconsistent--has traps for the unwary", "Limited usage beyond a particular domain", "Lacks some features of more modern languages", "Not sure")
    
    val q7 = "What are you most curious to learn more about?"
    val a7 = List("Data science", "Functional programming", "Object-oriented programming", "Front-end web development", "Back-end web development", "Apple ecosystem", "Microsoft ecosystem", "Systems programming", "Games and finance programming", "Not sure")  

    val q8 = "Which Star Wars character do you most resemble?"
    val a8 = List(
        "Boba Fett: Has a large arsenal of weapons for a variety of tasks; dominant in a particular field.", 
        "Yoda: Has been around for hundreds of years; teacher to those that follow; focused on first principles and in touch with the force.",
        "Darth Vader: Powerful and conquest-driven; will occasionally betray his master.",
        "Han Solo: Clever, well-rounded, and fast; responds to financial incentives.",
        "Darth Maul: At once flashy but a man of few words.",
        "C-3P0: Verbose but can be understood by all; a survivor that has evolved through many iterations.",
        "R2-D2: Difficult to understand but still waters run deep; compact but useful.",
        "Chewbacca: Terse; excels with improvised solutions to technical problems.",
        "Jar Jar Binks: Occassionally does something useful by complete accident; repetitive."
        )
        
    val q9 = "Which house would your language be sorted into at Hogwarts?" 
    val a9 = List(
        "Griffindor", 
        "Slytherin",
        "Ravenclaw",
        "Hufflepuff")
        
    val questions: List[String] = List(q1, q2, q3, q4, q5, q6, q7, q8, q9)
    val answers: List[List[String]] = List(a1, a2, a3, a4, a5, a6, a7, a8, a9)
    val languages = Map(
      "Java" -> List(2, 1, 3, 1, 3, 3, 3, 6, 1),
      "Python" -> List(2, 1, 2, 2, 1, 2, 1, 8, 1),
      "C" -> List(3, 1, 3, 1, 4, 6, 8, 2, 1),
      "C++" -> List(3, 1, 1, 1, 4, 1, 9, 3, 2),
      "Javascript" -> List(1, 1, 2, 3, 4, 4, 1, 2),
      "PHP" -> List(1, 1, 2, 5, 5, 5, 5, 9, 4),
      "Scala" -> List(2, 2, 1, 3, 1, 1, 2, 1, 3),
      "C#" -> List(2, 3, 3, 2, 3, 1, 5, 7, 4, 3),
      "Ruby" -> List(1, 2, 2, 4, 2, 5, 5, 7, 4),
      "Go" -> List(2, 3, 3, 4, 1, 1, 8, 4, 4),
      "Swift" -> List(4, 3, 4, 4, 5, 5, 6, 4, 2),
      "Haskell" -> List(2, 1, 1, 2, 1, 5, 2, 9, 3)
      )
    val use = Quiz(questions, answers, languages)
    }
    val start = QandAbuilder()
    start.use.quiz()
  }
}
