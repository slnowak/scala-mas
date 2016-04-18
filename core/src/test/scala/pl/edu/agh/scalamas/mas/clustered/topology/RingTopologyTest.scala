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
package pl.edu.agh.scalamas.mas.clustered.topology

import akka.actor.Address
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by novy on 06.04.16.
  */
class RingTopologyTest extends FlatSpec with Matchers {

  implicit def strToAddress(str: String): Address = Address("akka", str)

  "A Ring Topology" should "return no neighbours given 1 Island topology" in {
    // given
    val emptyTopology: RingTopology = RingTopology()
    val soleIsland: Island = Island("addr1")
    val withOneIsland: IslandTopology = emptyTopology.withNew(soleIsland)

    // expect
    withOneIsland neighboursOf soleIsland should be(List())
  }

  it should "return an empty neighbours list in case of island outside topology" in {
    // given
    val topology: RingTopology = RingTopology()

    // expect
    topology neighboursOf Island("666") should be(List())
  }

  it should "return only 1 neighbour in case of 2 element topology" in {
    // given
    val firstIsland: Island = Island("addr1")
    val secondIsland: Island = Island("addr2")
    val topology: IslandTopology = RingTopology().withNew(firstIsland).withNew(secondIsland)

    // expect
    topology neighboursOf firstIsland should be(List(secondIsland))
    topology neighboursOf secondIsland should be(List(firstIsland))
  }

  it should "return previous and next island as neighbours for middle-aligned island" in {
    // given
    val topology = RingTopology()
      .withNew(Island("addr1"))
      .withNew(Island("addr2"))
      .withNew(Island("addr3"))
      .withNew(Island("addr4"))

    // expect
    topology neighboursOf Island("addr2") should be(List(Island("addr1"), Island("addr3")))
  }

  it should "return previous and next island with respect to ring ordering for first island" in {
    // given
    val topology = RingTopology()
      .withNew(Island("addr1"))
      .withNew(Island("addr2"))
      .withNew(Island("addr3"))
      .withNew(Island("addr4"))

    // expect
    topology neighboursOf Island("addr1") should be(List(Island("addr4"), Island("addr2")))
  }

  it should "return previous and next island with respect to ring ordering for last island" in {
    // given
    val topology = RingTopology()
      .withNew(Island("addr1"))
      .withNew(Island("addr2"))
      .withNew(Island("addr3"))
      .withNew(Island("addr4"))

    // expect
    topology neighboursOf Island("addr4") should be(List(Island("addr3"), Island("addr1")))
  }

  it should "remove island from topology on request" in {
    // given
    val topology = RingTopology()
      .withNew(Island("addr1"))
      .withNew(Island("addr2"))
      .withNew(Island("addr3"))
      .withNew(Island("addr4"))

    // when
    val withoutOneIsland = topology withoutExisting Island("addr2")

    // then
    withoutOneIsland neighboursOf Island("addr3") should be(List(Island("addr1"), Island("addr4")))
  }
}