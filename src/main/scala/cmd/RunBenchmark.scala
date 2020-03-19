package cmd

import com.redis.RedisClient
import gp.BenchmarkGP
import io.evvo.island._
import io.evvo.migration.redis.{
  RedisEmigrator,
  RedisImmigrator,
  RedisParetoFrontierRecorder
}
import push4s.{BenchmarkLoader, PushProgram}

import scala.concurrent.duration._

object RunBenchmark {
  def main(args: Array[String]): Unit = {
    val filename = args(0)
    val benchmark = BenchmarkLoader.loadFromFile(filename).get

    val gp = BenchmarkGP(benchmark)

    val redisClient = new RedisClient(args(1), args(2).toInt)
    val island = new EvvoIsland[PushProgram](
      creators = gp.creators(),
      mutators = gp.mutators(),
      deletors = gp.deletors(),
      fitnesses = gp.objectives(),
      immigrationStrategy = AllowAllImmigrationStrategy(),
      immigrator = new RedisImmigrator(redisClient),
      emigrationStrategy = RandomSampleEmigrationStrategy(16),
      emigrator = new RedisEmigrator(redisClient),
      loggingStrategy = LogPopulationLoggingStrategy(),
      paretoFrontierRecorder = new RedisParetoFrontierRecorder(redisClient),
    )

    island.runBlocking(StopAfter(args(3).toInt.seconds))

    val sols: Vector[(Double, PushProgram)] = island
      .currentParetoFrontier()
      .solutions
      .toVector
      .map(sol => {
        (benchmark.evaluate(sol.solution).sum, sol.solution)
      })
    redisClient.sadd("evvo::collatz_case", sols.head, sols.tail:_*)
    redisClient.sadd("evvo::collatz_csv", island.currentParetoFrontier().toCsv())
    sys.exit(0)
  }
}
