object Quiz {
  def main(args: Array[String]): Unit = {

    println("")
    println("Which programming language should I learn?")
    println("""This quiz will consider the following programming languages:

Java, Python, C, C++, Javascript, PHP, Scala, C#, Ruby, PHP, Go, Swift, Rust

""")
    val q1 = "What programming domain would you like your language to excel in?"
    val a1 = List("Web development", "Jack of all trades", "High performance, such as games or systems", "App development", "Not sure")

    val q2 = "What kind of background would you like your language to have?"
    val a2 = List("Classic, with a large community", "Relatively new, and built with improvements in mind", "Created and promoted by a prestigious company.", "Not sure")

    val q3 = "What kind of learning experience would you prefer?"
    val a3 = List("Challenging, but you will learn new paradigms or algorithms along the way", "Easy to get productive quickly", "Somewhere in between", "Not sure")

    val q4 = "What would your langauge's defining personality characteristic be?"
    val a4 = List("Easy-going", "Intellectual", "Professional", "Quirky", "Not sure")
    
    val q5 = "What should be your language's greatest strength?"
    val a5 = List("Logically consistent", "Concise", "Usable anywhere", "Fast", "Useful", "Not sure")
    
    val q6 = "What weakness could you most accept in a language?"
    val a6 = List("Difficult to learn", "Error prone for large programs", "Verbose--lots of typing to do anything", "Inconsistent--has traps for the unwary", "Limited usage beyond a particular domain", "Lacks some features of more modern languages", "Not sure")
    
    val q7 = "What interests you most?"
    val a7 = List("Data science", "Functional programming", "Object-oriented programming", "Front-end web development", "Back-end web development", "Apple ecosystem", "Microsoft ecosystem", "Systems programming", "Games and finance programming", "Not sure")  

    val languages = Map(
      "Java" -> List(2, 1, 3, 3, 3, 3, 3),
      "Python" -> List(2, 1, 2, 1, 1, 2, 1),
      "C" -> List(3, 1, 3, 2, 4, 6, 8),
      "C++" -> List(3, 1, 1, 2, 4, 1, 9),
      "Javascript" -> List(1, 1, 2, 1, 4, 4),
      "PHP" -> List(1, 1, 2, 4, 5, 5, 5),
      "Scala" -> List(2, 2, 1, 2, 1, 1, 2),
      "C#" -> List(2, 3, 3, 3, 3, 1, 5, 7),
      "Ruby" -> List(1, 2, 2, 1, 2, 5, 5),
      "Go" -> List(2, 3, 3, 2, 1, 1, 8),
      "Swift" -> List(4, 3, 3, 3, 5, 5, 6),
      "Rust" -> List(3, 2, 1, 2, 4, 1, 8)
      )
    val totalNum = 7
    val questions = List(q1, q2, q3, q4, q5, q6, q7)
    val answers = List(a1, a2, a3, a4, a5, a6, a7)

    def getIntOr(default: Int): Int = {
      def toIntOrElse(input: String, defaultVal: Int) = try {
        input.toInt
      } catch {
        case _: NumberFormatException => defaultVal
      }

      val output = toIntOrElse(io.StdIn.readLine(), default)
      output
    }

    def ask(q: Int = 0, a: Int = 0, scoreCard: List[Int] = List(), num: Int = 0): List[Int] = {
      if (num == totalNum) scoreCard
      else {
        val options = (1 to answers(a).length).toList
        println(questions(q))
        println("")
        for (i <- 0 until answers(a).length) println((i + 1) + ". " + answers(a)(i))
        val answer = getIntOr(0)
        println("")
        if (answer == 0 || !options.contains(answer)) {
          println("Didn't understand the input, try again")
          ask(q, a, scoreCard, num)
        } else ask(q + 1, a + 1, answer :: scoreCard, num + 1)
      }
    }
    val userInput = ask().reverse
  
    def grader(compare: List[Int]): String = {
      val correct = for {
        (k, v) <- languages
        i <- 0 to 3
        if(v(i) == userInput(i))
      } yield(k)
      val output = correct.groupBy(identity).maxBy(_._2.size)._1
      output
    }

    println(grader(userInput))

  }
}