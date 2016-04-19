package pl.edu.agh.scalamas.mas.clustered.island

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import pl.edu.agh.scalamas.mas.Logic
import pl.edu.agh.scalamas.mas.LogicTypes._
import pl.edu.agh.scalamas.mas.RootEnvironment._
import pl.edu.agh.scalamas.mas.async.{Arena, Individual}
import pl.edu.agh.scalamas.mas.clustered.IslandTopologyCoordinator.NeighboursChanged

/**
  * Created by novy on 19.04.16.
  */
class IslandActor(logic: Logic) extends Actor with ActorLogging {
  val migrationArena: ActorRef = context.actorOf(MigrationArena.props())
  val arenas = arenasForBehaviours(logic.behaviours, migration orElse logic.meetingsFunction)
  val switchingBehaviour = (agent: Agent) => arenas(logic.behaviourFunction(agent))

  logic.initialPopulation foreach addAgent

  def receive = {
    case msg: NeighboursChanged =>
      migrationArena forward msg

    case Add(agentState) =>
      addAgent(agentState)
  }

  def addAgent(agent: Agent) = context.actorOf(Individual.props(agent, switchingBehaviour))

  def arenasForBehaviours(behaviours: Seq[Behaviour], meetings: MeetingFunction): Map[Behaviour, ActorRef] =
    behaviours map {
      case m: Migration => migrationBehaviour(m)
      case b: Behaviour => behaviourWithinSingleIsland(b, meetings)
    } toMap

  private def behaviourWithinSingleIsland(behaviour: Behaviour, meetings: MeetingFunction): (Behaviour, ActorRef) = {
    val meeting = (agents: List[Agent]) => meetings((behaviour, agents))
    behaviour -> withinSingleIslandArenaFor(behaviour, meeting)
  }

  def withinSingleIslandArenaFor(behaviour: Behaviour, meeting: (List[Agent]) => Population): ActorRef = {
    context.actorOf(Arena.props(behaviour.capacity, meeting), behaviour.getClass.getSimpleName)
  }

  def migrationBehaviour(migrationBehaviour: Migration): (Behaviour, ActorRef) = {
    migrationBehaviour -> migrationArena
  }
}

object IslandActor {

  def props(logic: Logic) = Props(new IslandActor(logic))
}
