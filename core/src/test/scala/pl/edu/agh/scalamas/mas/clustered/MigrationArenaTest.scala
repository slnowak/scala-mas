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
package pl.edu.agh.scalamas.mas.clustered

import akka.actor.{ActorRef, ActorSelection, ActorSystem}
import akka.testkit._
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, WordSpecLike}
import pl.edu.agh.scalamas.mas.LogicTypes.Agent
import pl.edu.agh.scalamas.mas.RootEnvironment.Add
import pl.edu.agh.scalamas.mas.async.Arena.Join
import pl.edu.agh.scalamas.mas.clustered.island.MigrationArena

/**
  * Created by novy on 10.04.16.
  */
class MigrationArenaTest extends TestKit(ActorSystem()) with WordSpecLike with BeforeAndAfterEach with BeforeAndAfterAll with ImplicitSender {

  var objectUnderTest: ActorRef = _
  var soleNeighbour: TestProbe = _

  override protected def beforeEach(): Unit = {
    soleNeighbour = TestProbe()
    val containsOnlyOneNeighbour: List[ActorSelection] = List(system.actorSelection(soleNeighbour.ref.path))
    objectUnderTest = TestActorRef(MigrationArena.props(
      neighbours = containsOnlyOneNeighbour,
      requiredAgentsToMigrate = 2
    ))
  }

  override protected def afterAll(): Unit = system.terminate()

  "Migration arena " must {

    "not send any message if migration threshold not exceeded" in {
      // when
      objectUnderTest ! Join(randomAgentState())

      // then
      expectNoMsg()
      soleNeighbour expectNoMsg()
    }

    "ask random neighbour to create new agents if migration started" in {
      // given
      val firstAgentState: Agent = randomAgentState()
      objectUnderTest ! Join(firstAgentState)

      // when
      val secondAgentState: Agent = randomAgentState()
      objectUnderTest ! Join(secondAgentState)

      // then
      soleNeighbour expectMsg Add(firstAgentState)
      soleNeighbour expectMsg Add(secondAgentState)
    }

    "kill all agents to migrate if migration started" in {
      // given
      val firstAgent = TestProbe()
      val firstAgentWatcher = TestProbe()
      firstAgentWatcher watch firstAgent.ref

      val secondAgent = TestProbe()
      val secondAgentWatcher = TestProbe()
      secondAgentWatcher watch secondAgent.ref

      val firstAgentState: Agent = randomAgentState()
      firstAgent.send(objectUnderTest, Join(firstAgentState))

      // when
      val secondAgentState: Agent = randomAgentState()
      secondAgent.send(objectUnderTest, Join(secondAgentState))

      // then
      firstAgentWatcher expectTerminated firstAgent.ref
      secondAgentWatcher expectTerminated secondAgent.ref
    }
  }

  private def randomAgentState(): Agent = {
    new Agent {}
  }
}
