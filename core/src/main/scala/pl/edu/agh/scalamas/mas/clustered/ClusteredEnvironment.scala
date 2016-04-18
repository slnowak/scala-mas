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

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import pl.edu.agh.scalamas.app.EnvironmentStrategy
import pl.edu.agh.scalamas.mas.LogicTypes._
import pl.edu.agh.scalamas.mas.RootEnvironment._
import pl.edu.agh.scalamas.mas.async.{Arena, Individual}
import pl.edu.agh.scalamas.mas.clustered.IslandTopologyCoordinator.NeighboursChanged
import pl.edu.agh.scalamas.mas.{Logic, LogicStrategy}

/**
  * Created by novy on 18.04.16.
  */
trait ClusteredEnvironment extends EnvironmentStrategy {
  this: LogicStrategy =>
  override def environmentProps: Props = IslandActor.props(logic)
}

object IslandActor {

  def props(logic: Logic) = Props(new IslandActor(logic))
}

class IslandActor(logic: Logic) extends Actor with ActorLogging {
  val arenas = arenasForBehaviours(logic.behaviours, migration orElse logic.meetingsFunction)
  val switchingBehaviour = (agent: Agent) => arenas(logic.behaviourFunction(agent))

  logic.initialPopulation foreach addAgent

  def receive = {
    case msg: NeighboursChanged =>
      migrationArena forward msg

    case Add(agentState) => addAgent(agentState)
  }

  def addAgent(agent: Agent) = context.actorOf(Individual.props(agent, switchingBehaviour))

  def arenasForBehaviours(behaviours: Seq[Behaviour], meetings: MeetingFunction): Map[Behaviour, ActorRef] =
    behaviours map {
      case m: Migration => migrationBehaviour(m)
      case b: Behaviour => behaviourWithinSingleIsland(b, meetings)
    } toMap


  private def behaviourWithinSingleIsland(behaviour: Behaviour, meetings: MeetingFunction): (Behaviour, ActorRef) = {
    val meeting = (agents: List[Agent]) => meetings((behaviour, agents))
    behaviour -> interIslandArenaFor(behaviour, meeting)
  }

  def interIslandArenaFor(behaviour: Behaviour, meeting: (List[Agent]) => Population): ActorRef = {
    context.actorOf(Arena.props(behaviour.capacity, meeting), behaviour.getClass.getSimpleName)
  }

  def migrationBehaviour(migrationBehaviour: Migration): (Behaviour, ActorRef) = {
    migrationBehaviour -> migrationArena
  }

  val migrationArena: ActorRef = context.actorOf(MigrationArena.props(List.empty, 2))
}
