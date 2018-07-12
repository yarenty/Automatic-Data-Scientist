import java.util.concurrent.ThreadLocalRandom

import scala.math.exp

object MonteCarloParallel {

  def main(args: Array[String]) = {
    println("Start")
    val iters = 1000 //1000000000
    val sums = (1 to iters).toList.par map { x => ThreadLocalRandom.current().nextDouble() } map { x => exp(-x * x) }
    val result = sums.reduce(_ + _)
    println(result / iters)
    println("End")
  }

}