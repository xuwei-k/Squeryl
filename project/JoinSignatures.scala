/*******************************************************************************
 * Copyright 2010 Maxime Lévesque
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ***************************************************************************** */

object JoinSignatures {
  private[this] def f(n: Int, s: Int => String) = {
    if (n == 1) {
      ""
    } else {
      (1 until n).map(s).mkString(", ", ", ", "")
    }
  }
  private[this] def B(n: Int) = f(n, "B" + _)
  private[this] def bB(n: Int) = f(n, x => s"b${x}: B${x}")
  private[this] def b(n: Int) = f(n, "b" + _)
  private[this] def q(n: Int) = f(n, "q" + _)
  private[this] def joinedQueryable(n: Int) = {
    if (n == 1) {
      ""
    } else {
      (1 until n).map(x => s"q$x: JoinedQueryable[B$x]").mkString(", ", ", ", "")
    }
  }

  private[this] def joinMethod(n: Int): String = {
s"""
  def join[A${B(n)}, C](
    q: Queryable[A]${joinedQueryable(n)}
  )(
    f: Function${n}[A${B(n)}, JoinQueryYield${n}[C]]
  ): Query[C] =
    from(q${q(n)})(
     (a: A${bB(n)}) => f(a${b(n)}).queryYield
    )
"""
  }

  val value = s"""
package org.squeryl.dsl.boilerplate

import org.squeryl.dsl.{QueryYield}
import org.squeryl.dsl.internal.{JoinedQueryable, InnerJoinedQueryable, OuterJoinedQueryable}
import org.squeryl.{Queryable, Query}

trait JoinSignatures {
  self: FromSignatures =>

  class JoinPrecursor[A](q: Queryable[A]) {
    def leftOuter = new OuterJoinedQueryable[A](q, "left")
    def rightOuter = new OuterJoinedQueryable[A](q, "right")
    def fullOuter = new OuterJoinedQueryable[A](q, "full")
  }

  implicit def queryable2JoinPrecursor[A](q: Queryable[A]) = new JoinPrecursor[A](q)

  implicit def queryable2RightInnerJoinedQueryable[A](q: Queryable[A]) = new InnerJoinedQueryable[A](q, "")

${(1 to 21).map(joinMethod).mkString}
}

${(1 to 22).map(n =>
  s"class JoinQueryYield$n[R](val queryYield: QueryYield[R])"
).mkString("\n")}
"""

}
