package pl.edu.agh.scalamas.app

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import pl.edu.agh.scalamas.random.{ConcurrentRandomGeneratorComponent, RandomGeneratorComponent}
import pl.edu.agh.scalamas.stats.{ConcurrentStatsFactory, StatsComponent}
import pl.edu.agh.scalamas.util.Reaper

import scala.concurrent.duration.FiniteDuration

/**
  * Created by novy on 19.04.16.
  */
class ClusteredStack(port: Int) extends ConcurrentAgentRuntimeComponent
  with ConcurrentStatsFactory
  with ConcurrentRandomGeneratorComponent
  with ClusteredRunner {

  this: EnvironmentStrategy with StatsComponent =>

  val agentRuntime = new ConcurrentAgentRuntime {
    val config = ConfigFactory
      .parseString(s"akka.remote.netty.tcp.port=$port")
      .withFallback(ConfigFactory.load())

    val system = ActorSystem("ClusterSystem", config)
  }
}

trait ClusteredRunner {
  this: ConcurrentAgentRuntimeComponent
    with EnvironmentStrategy
    with StatsComponent
    with RandomGeneratorComponent =>

  def run(duration: FiniteDuration): Unit = {

    implicit val system = agentRuntime.system
    implicit val context = system.dispatcher

    val clusterCoordinator = system.actorOf(environmentProps, "coordinator")
    for (
      _ <- Reaper.terminateAfter(clusterCoordinator, duration);
      _ <- stats.get) {
      system.terminate()
    }
  }
}
