package com.yarenty.ml.algorithms.montecarlo


/**
  *
  * The for-comprehension is the heart of the program: 
  * it repeatedly (once for each question) generates the players’ answers with the specified probability of being correct,
  * figures out how many times each answer was used, picks out the cases where the required number of people agreed, 
  * and yields a Boolean indicating whether or not the agreed-upon value was correct. 
  * Note that there can be more than one consensus for a given question; 
  * for example, if you use ten players, five of which must agree, then you can achieve consensus twice for a single question.
  * Twelve players can reach consensus four times on a question if only three have to agree.  
  * The for-comprehension (with its underlying flatmap) makes all of that trivial.
  *
  *
  * $ scala MonteCarlo 1000000 2 2 80 2
  * Consensus meant correctness 640039 out of 679984 times.
  * That's 94.1%.
  * And they were also interested in the probability of the answer being correct if 3 out of 3 had it:
  *
  * $ scala MonteCarlo 1000000 2 3 80 3
  * Consensus meant correctness 512213 out of 520220 times.
  * That's 98.5%.
  * Those are essentially the same answers Alvin got.
  *
  * Math:
  * In the above runs (where everyone must agree), there are three possibilities for each question:
  *
  * A. They all agree and they are correct.
  * B. They all agree and they are incorrect.
  * C. They don’t all agree.
  * We don’t care about C; what we are interested in is
  *
  * p(A)
  * -------------
  * p(A) + p(B)
  * 
  * Since their answers are uncorrelated, the probability that two players will both be right is the product of 
  * the probabilities that each is right. Since each is right 80% of the time, p(A) = square(0.8) = 0.64, 
  * and p(B) = square(0.2) = 0.04. So the expression above is 0.64 / ( 0.64 + 0.04 ) = 0.941 (roughly).
  *
  * With three people, p(A) = cube(0.8) = 0.512 and p(B) = cube(0.2) = 0.008, so the probability that the consensus 
  * is correct is 0.512 / ( 0.512 + 0.008 ) = 0.9846 (roughly).
  *
  * Let’s say that instead of 2 possibilities there are a million:
  *
  * $ scala MonteCarlo 1000000 1000000 2 80 2
  * Consensus meant correctness 640167 out of 640167 times.
  * That's 100.0%.
  * With so many possible answers, the players almost never agree if they are incorrect. 
  * So, if they do agree, it’s because they are right.
  *
  */

object MonteCarlo extends App {

  // Get the simulation parameters from the command line.
  val Array(numQuestions, numChoices, numPlayers, pctCorrect, numMustAgree) = Array(1000000,2, 2 ,80 ,2)
//  val Array(numQuestions, numChoices, numPlayers, pctCorrect, numMustAgree) = args map (_.toInt)

  // The choices will be 0 .. numChoices-1; call the last choice correct.
  val correctAnswer = numChoices - 1

  // Generates an answer with a pctCorrect chance of being correct.
  def genAnswer =
    if (util.Random.nextInt(100) < pctCorrect)
      correctAnswer
    else  // pick a wrong answer from 0 to correctAnswer-1
      util.Random.nextInt(correctAnswer)

  // For each question, generate player answers and look for consensus.
  // Where consensus is achieved, yield whether or not it is correct.
  // The result is an array, with one element for each consensus reached,
  // containing true if the answer they agreed on was correct.
  val correctnessOfConsensus =
  for { i <- 1 to numQuestions
        (answer,instances) <- Array.fill(numPlayers)(genAnswer) groupBy identity
        if instances.size >= numMustAgree
  } yield answer == correctAnswer

  // Print how often the consensus answer was correct.
  val timesAgreed = correctnessOfConsensus.size
  val timesRight  = correctnessOfConsensus count identity  // num true
  println( s"Consensus meant correctness $timesRight out of $timesAgreed times." )
  if (timesAgreed > 0) {
    println( f"That's ${ 100.0 * timesRight / timesAgreed }%.1f%%." )
  }

}