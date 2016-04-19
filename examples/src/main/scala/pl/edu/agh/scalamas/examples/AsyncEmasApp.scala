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
package pl.edu.agh.scalamas.examples

import pl.edu.agh.scalamas.app.ClusteredStack
import pl.edu.agh.scalamas.emas.EmasLogic
import pl.edu.agh.scalamas.genetic.RastriginProblem
import pl.edu.agh.scalamas.mas.clustered.ClusteredEnvironment

import scala.concurrent.duration._

/**
  * Created by novy on 06.04.16.
  */
object AsyncEmasApp {

  def main(args: Array[String]) {
    if (args.isEmpty)
      startup(Seq("2551", "2552"))
    else
      startup(args)
  }

  private def startup(ports: Seq[String]): Unit = ports foreach runRastriginInstance

  private def runRastriginInstance(stringPort: String): Unit = {
    RastriginClusteredProblemRunningOnPort(stringPort).run(15 seconds)
  }

  private implicit def stringToInt(s: String): Int = Integer.parseInt(s)
}

case class RastriginClusteredProblemRunningOnPort(val port: Int) extends ClusteredStack(port)
  with ClusteredEnvironment with EmasLogic with RastriginProblem {
}
