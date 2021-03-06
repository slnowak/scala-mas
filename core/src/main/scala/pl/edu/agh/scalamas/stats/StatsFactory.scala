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
package pl.edu.agh.scalamas.stats

import pl.edu.agh.scalamas.app.ConcurrentAgentRuntimeComponent

/**
 * Mixin component for a application statistics factory method.
 */
trait StatsFactoryComponent {

  /**
   * The factory method.
   * @return
   */
  def statsFactory: StatsFactory

  trait StatsFactory {
    /**
     * Creates some statistics with the given initial value and update function.
     */
    def apply[T](initialValue: T)(updateFunction: (T, T) => T): Stats[T]
  }
}

/**
 * Factory for simple stats.
 */
trait SimpleStatsFactory extends StatsFactoryComponent {

  def statsFactory = SimpleStatsFactoryImpl

  object SimpleStatsFactoryImpl extends StatsFactory {
    def apply[T](initialValue: T)(updateFunction: (T, T) => T) = Stats.simple(initialValue)(updateFunction)
  }
}

/**
 * Factory for concurrent stats.
 */
trait ConcurrentStatsFactory extends StatsFactoryComponent {
  this: ConcurrentAgentRuntimeComponent =>

  def statsFactory = ConcurrentStatsFactoryImpl

  object ConcurrentStatsFactoryImpl extends StatsFactory {
    def apply[T](initialValue: T)(updateFunction: (T, T) => T) = Stats.concurrent(initialValue)(updateFunction)(agentRuntime.system.dispatcher)
  }
}