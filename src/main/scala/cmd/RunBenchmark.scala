package cmd

import com.redis.RedisClient
import gp.BenchmarkGP
import io.evvo.island.{AllowAllImmigrationStrategy, EvvoIsland, LogPopulationLoggingStrategy, RandomSampleEmigrationStrategy, SendToAllEmigrationTargetStrategy, StopAfter}
import io.evvo.migration.redis.{RedisEmigrator, RedisImmigrator, RedisParetoFrontierRecorder}
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

    island.runBlocking(StopAfter(1.second))
    println("DEBUG CSV")
    println(island.currentParetoFrontier().toCsv())
    println(island.currentParetoFrontier())
    println("DEBUG CSV after")
    sys.exit(0)
  }
}
