package cmd

import com.redis.RedisClient
import gp.BenchmarkGP
import io.evvo.island._
import io.evvo.migration.redis.{RedisEmigrator, RedisImmigrator, RedisParetoFrontierRecorder}
import push4s.{BenchmarkLoader, PushProgram}

import scala.concurrent.duration._
import scala.util.Try

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
      loggingStrategy = LogPopulationLoggingStrategy(10.seconds),
      paretoFrontierRecorder = new RedisParetoFrontierRecorder(redisClient),
    )

    island.runBlocking(StopAfter(args(3).toInt.seconds))

    island
      .currentParetoFrontier()
      .solutions
      .toVector
      .map(
        sol => (benchmark.evaluate(sol.solution).sum, sol.solution).toString()
      )
      .foreach(x => Try { redisClient.sadd("evvo::collatz_case", x) })

    redisClient.sadd(
      "evvo::collatz_csv",
      island.currentParetoFrontier().toCsv()
    )
    sys.exit(0)
  }
}
