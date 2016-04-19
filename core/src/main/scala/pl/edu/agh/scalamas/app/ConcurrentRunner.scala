/*
 * Copyright 2013 - 2015, Daniel Krzywicki <daniel.krzywicki@agh.edu.pl>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package pl.edu.agh.scalamas.app

import akka.event.Logging
import pl.edu.agh.scalamas.mas.RootEnvironment
import pl.edu.agh.scalamas.random.RandomGeneratorComponent
import pl.edu.agh.scalamas.stats.StatsComponent
import pl.edu.agh.scalamas.util.{Reaper, Logger}

import scala.concurrent.duration._

/**
 * Runner for concurrent apps.
 */
trait ConcurrentRunner {
  this: ConcurrentAgentRuntimeComponent
    with EnvironmentStrategy
    with StatsComponent
    with RandomGeneratorComponent =>

  lazy val islands = agentRuntime.config.getInt("mas.islandsNumber")

  def run(duration: FiniteDuration): Unit = {

    implicit val system = agentRuntime.system
    implicit val context = system.dispatcher

//    val log = Logging(system, classOf[ConcurrentRunner])
//    Logger(frequency = 1 second) {
//      time =>
//        log info (s"$time ${formatter(stats.getNow)}")
//    }

    val root = system.actorOf(RootEnvironment.props(environmentProps, islands, random))
    println(root)
    for (
      _ <- Reaper.terminateAfter(root, duration);
      _ <- stats.get) {
      system.shutdown()
    }
  }

}
