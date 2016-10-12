package org.squeryl.test

import org.squeryl._
import org.squeryl.test.PrimitiveTypeModeForTests._
import org.squeryl.framework._

object ReservedWordsTestSchema extends Schema {
  val a = table[ReservedWordsTable]
}

case class ReservedWordsTable(
  id: Int,
  select: Int,
  update: String,
  group: Boolean
) extends KeyedEntity[Int]

abstract class ReservedWordsTest extends SchemaTester with RunTestsInsideTransaction {
  self: DBConnector =>

  def schema = ReservedWordsTestSchema

  test("insert"){
    ReservedWordsTestSchema.a.insert(ReservedWordsTable(0, 1, "xyz", false))
  }

  test("select"){
    assert(ReservedWordsTestSchema.a.where(_.select === 1).toList.size == 1)
  }

}
