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
import akka.cluster.Cluster
import akka.cluster.ClusterEvent._
import pl.edu.agh.scalamas.mas.clustered.IslandTopologyCoordinator.NeighboursChanged
import pl.edu.agh.scalamas.mas.clustered.topology.{Island, IslandTopology, RingTopology}

/**
  * Created by novy on 09.04.16.
  */
class IslandTopologyCoordinator(var topology: IslandTopology, islandActor: ActorRef) extends Actor with ActorLogging {

  val cluster = Cluster(context.system)
  val thisIsland = Island(cluster.selfAddress)

  override def preStart(): Unit = subscribeToClusterChanges()

  override def postStop(): Unit = unsubscribeFromCluster()

  override def receive: Receive = {
    case MemberUp(member) =>
      addIslandToTopology(member.address)
      notifyIslandActorAboutNeighbourhoodChange()

    case ReachableMember(member) =>
      addIslandToTopology(member.address)
      notifyIslandActorAboutNeighbourhoodChange()

    case UnreachableMember(member) =>
      removeIslandFromTopology(member.address)
      notifyIslandActorAboutNeighbourhoodChange()

    case MemberRemoved(member, _) =>
      removeIslandFromTopology(member.address)
      notifyIslandActorAboutNeighbourhoodChange()
  }

  private def addIslandToTopology(islandAddress: Address): Unit = {
    topology = topology.withNew(Island(islandAddress))
  }

  private def removeIslandFromTopology(islandAddress: Address): Unit = {
    topology = topology.withoutExisting(Island(islandAddress))
  }

  private def notifyIslandActorAboutNeighbourhoodChange(): Unit = {
    islandActor ! NeighboursChanged(topology.neighboursOf(thisIsland).map(toActorSelection))
  }

  private def toActorSelection(island: Island): ActorSelection = {
    context.actorSelection(s"akka.tcp://${island.islandAddress.hostPort}/user/island")
  }

  private def subscribeToClusterChanges(): Unit = {
    cluster.subscribe(self, initialStateMode = InitialStateAsEvents,
      classOf[MemberEvent], classOf[UnreachableMember])
  }

  private def unsubscribeFromCluster(): Unit = cluster.unsubscribe(self)
}

object IslandTopologyCoordinator {
  def props(islandActor: ActorRef, initialTopology: IslandTopology = RingTopology()): Props =
    Props(new IslandTopologyCoordinator(initialTopology, islandActor))

  case class NeighboursChanged(neighbours: List[ActorSelection])

}