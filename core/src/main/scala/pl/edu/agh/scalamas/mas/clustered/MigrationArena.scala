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

import akka.actor._
import pl.edu.agh.scalamas.mas.LogicTypes.Agent
import pl.edu.agh.scalamas.mas.RootEnvironment.Add
import pl.edu.agh.scalamas.mas.async.Arena.Join
import pl.edu.agh.scalamas.mas.clustered.IslandTopologyCoordinator.NeighboursChanged
import pl.edu.agh.scalamas.mas.clustered.MigrationArena.{AgentActor, CreateNewAgents}

import scala.util.Random

/**
  * Created by novy on 10.04.16.
  */
class MigrationArena(var neighbours: List[ActorSelection], requiredAgentsToMigrate: Int) extends Actor with ActorLogging {
  var agentsToMigrate: List[AgentActor] = List.empty

  override def receive: Receive = {
    case NeighboursChanged(newNeighbours) =>
      this.neighbours = newNeighbours

    case Join(agentState) if enoughAgentsGathered() =>
      log.info(s"$self: starting migration with agents: $agentsToMigrate to random neighbour from: $neighbours")
      agentsToMigrate = AgentActor(agentState, sender()) :: agentsToMigrate
      randomNeighbour() foreach migrate(agentsToMigrate reverse)
      agentsToMigrate = List.empty

    case Join(agentState) =>
      agentsToMigrate = AgentActor(agentState, sender()) :: agentsToMigrate
  }

  private def migrate(agentsToMigrate: List[AgentActor])(neighbour: ActorSelection): Unit = {
    agentsToMigrate foreach { agent =>
      tellNeighbourToCreateNewAgent(neighbour, agent)
      killAgent(agent)
    }
  }

  private def tellNeighbourToCreateNewAgent(neighbour: ActorSelection, agent: AgentActor): Unit = {
    neighbour ! Add(agent.agentState)
  }

  private def killAgent(agent: AgentActor): Unit = agent.agentActor ! PoisonPill

  private def enoughAgentsGathered(): Boolean = agentsToMigrate.size + 1 == requiredAgentsToMigrate

  private def randomNeighbour(): Option[ActorSelection] = Random.shuffle(neighbours).headOption
}

object MigrationArena {

  def props(neighbours: List[ActorSelection], requiredAgentsToMigrate: Int = 3): Props =
    Props(new MigrationArena(neighbours, requiredAgentsToMigrate))

  trait AgentState

  case class CreateNewAgents(agentStates: List[AgentState])

  case class AgentActor(agentState: Agent, agentActor: ActorRef)

}