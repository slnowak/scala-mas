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

/**
  * Created by novy on 09.04.16.
  */
class RingTopology private(islands: Vector[Island]) extends IslandTopology {

  override def neighboursOf(island: Island): List[Island] = {
    List(left(island), right(island)).flatten.distinct filterNot (_ == island)
  }

  private def left(island: Island): Option[Island] = {
    val indexOf: Int = islands indexOf island
    if (indexOf == -1) None else islands.lift((indexOf + islands.size - 1) % islands.size)
  }

  private def right(island: Island): Option[Island] = {
    val indexOf: Int = islands indexOf island
    if (indexOf == -1) None else islands.lift((indexOf + 1) % islands.size)
  }

  override def withNew(island: Island): IslandTopology = new RingTopology(islands :+ island)

  override def withoutExisting(island: Island): IslandTopology = new RingTopology(islands filterNot (_ == island))
}

object RingTopology {
  def apply(): RingTopology = new RingTopology(Vector())
}