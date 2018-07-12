package com.yarenty.ml.algorithms.montecarlo

import scala.util._

/**
  * A 'Minority Report' Monte Carlo simulation 
  *
  * 
  * “The Precogs are never wrong. But occasionally they do disagree.” ~ Minority Report
  *
  *
  * For the purposes of this simulation, imagine that you have three people that are each “right” roughly 80% of the time.
  * For instance, if they take a test with 100 questions, each of the three individuals will get 80 of the questions right,
  * although they may not get the same questions right or wrong. Given these three people, my question to several statisticians was, 
  * “If two of the people have the same answer to a given question, what are the odds that they are correct? 
  * Furthermore, if all three of them give the same answer to a question, what are the odds that they are right?”
  *
  * Maybe I phrased my question wrong, but the statisticians kept saying, “80%”, which I strongly felt was wrong.
  *
  * Therefore, what I’m doing in this Monte Carlo simulation program is assuming that there are ’N’ questions in a given “test”.
  * To keep this simple I’m assuming that the “correct” answer to each question is A. 
  * I then randomly populate the “answers” of three simulated people, making sure that very close to 80% of each 
  * person’s answers are A. Once I have N answers for each user (in this case N is 10,000), I compare the answers.
  *
  * If you think of each element in the Person1 (P1) array as being a series of answers, you’ll find that P1 answered 
  * A 80% of the time. You can therefore think that this person was correct in all of these answers. 
  * The same thing is true for Person2 (P2) and Person3 (P3). However, because the results are random, 
  * there’s no guarantee that for any value of N that the three “people” will have the same answer. 
  * For the case of N=1, all three people may have the correct answer A, but for N=2, P1 may be A, 
  * P2 may be B, and P1 may be A, or any other possible combination.
  *
  * So now the question becomes, if all three people have the same answer, what are the odds that they are correct? 
  * I do this by walking through the three arrays and finding the cases where all three people have the same answer. 
  * It turns out that when all three people have the same answer (where the answer is either A or B) they have 
  * the correct answer (A) roughly 98.5% of the time.
  *
  * Similarly, it turns out that when you compare P1 and P2, when they agree on an answer, they are correct 
  * ~94% of the time. Intuitively I believe that it is correct that this value is greater than 80%, 
  * and also less than the value when all three people agree.
  *
  * It may be that these results are skewed somewhat by the fact that there are only two possible answers in my questions, 
  * A and B. For instance, the results will surely be different if there are something like four possible answers 
  * to each question (A, B, C, D). How will adding more possible answers modify the test results? 
  * 
  * 
  * In summary the technique is:
  *
  * 1) State the problem you’re trying to solve.
  * 2) Write code to simulate the problem. Generate random data for your problem, where that random data conforms on
  *  average to what you expect, or what you know from real-world observations.
  * 3) Run the simulation a large enough number of times so that the results have a chance to “settle down”.
  * 
  */
object MinorityReportSimulation extends App {

  val N = 100000
  val A = 'a'
  val B = 'b'

  // These arrays will be ~80% randomly populated with A, and 
  // ~20% with B. For instance, it may be that p1(0) = A, 
  // p2(0) = A, and p3(0) = B, and so on.
  val person1 = new Array[Char](N)
  val person2 = new Array[Char](N)
  val person3 = new Array[Char](N)

  populateValuesForFirstPerson
  populateValuesForSecondPerson
  populateValuesForThirdPerson

  printCasesWhereP1AndP2HaveSameAnswer
  printCasesWhereP1AndP2AndP3HaveSameAnswer

  /**
    * the following 'populate' methods have slightly different
    * algorithms to ensure randomness (though this isn't really necessary).
    * also, i thought about passing different algorithms into a main function
    * in an 'fp' style here, but i'm trying to keep this simple at the moment.
    */
  def populateValuesForFirstPerson {
    for (i <- 0 until N) {
      if (Random.nextInt(100) < 80) person1(i) = A else person1(i) = B
    }
  }

  def populateValuesForSecondPerson {
    for (i <- 0 until N) {
      if (Random.nextInt(100) > 19) person2(i) = A else person2(i) = B
    }
  }

  def populateValuesForThirdPerson {
    val random = new Random
    for (i <- 0 until N) {
      val randomInt = random.nextInt(100)
      if (randomInt < 41 || randomInt > 60) person3(i) = A else person3(i) = B
    }
  }

  /**
    * look at the situation where person1 and person2 have the same answer.
    * out of these, what percentage is correct? (i.e., what % of these are 'a'?
    */
  def printCasesWhereP1AndP2HaveSameAnswer {
    var numSame = 0
    var numCorrect = 0
    for (i <- 0 until N) {
      if (person1(i) == person2(i)) {
        numSame += 1
        if (person1(i) == A) numCorrect += 1
      }
    }
    printResults("P1 = P2", numSame, numCorrect)
  }

  /**
    * look at the situation where p1, p2, and p3 have the same answer.
    * out of these, what percentage is correct? (i.e., what % of these are 'a'?
    */
  def printCasesWhereP1AndP2AndP3HaveSameAnswer {
    var numSame = 0
    var numCorrect = 0
    for (i <- 0 until N) {
      if (person1(i) == person2(i) && person2(i) == person3(i)) {
        numSame += 1
        if (person1(i) == A) numCorrect += 1
      }
    }
    printResults("P1 = P2 = P3", numSame, numCorrect)
  }

  def printResults (header: String, numSame: Int, numCorrect: Int) {
    val percentCorrect = numCorrect.asInstanceOf[Float]/numSame.asInstanceOf[Float] * 100F
    val formatter = java.text.NumberFormat.getIntegerInstance
    println(s"\n$header")
    println(s"# same answers:    ${formatter.format(numSame)}")
    println(s"# correct answers: ${formatter.format(numCorrect)}")
    println(f"percent correct:   $percentCorrect%2.2f")
  }
}
