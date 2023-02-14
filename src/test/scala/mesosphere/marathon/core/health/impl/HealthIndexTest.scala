package mesosphere.marathon
package core.health.impl

import mesosphere.UnitTest
import mesosphere.marathon.core.health.Health
import mesosphere.marathon.core.health.impl.HealthIndexTest._
import mesosphere.marathon.core.instance.Instance
import mesosphere.marathon.core.task.Task
import mesosphere.marathon.state.{AbsolutePathId, Timestamp}

class HealthIndexTest extends UnitTest {
  "HeathIndex" should {
    "get items by Instance ID" in {
      val indexSimple: HealthIndex = HealthSingleTrieMap()
      val indexDual: HealthIndex = HealthDualTrieMap()

      // Add
      val entry1 = taskId1 -> health1
      indexSimple += entry1
      indexDual += entry1

      // Get by Instance ID
      indexSimple.get(instanceId1) should equal(Some(health1))
      indexDual.get(instanceId1) should equal(Some(health1))

      indexSimple.get(instanceId2) should equal(None)
      indexDual.get(instanceId2) should equal(None)
    }

    "get items by Task ID" in {
      val indexSimple: HealthIndex = HealthSingleTrieMap()
      val indexDual: HealthIndex = HealthDualTrieMap()

      // Add
      val entry1 = taskId1 -> health1
      indexSimple += entry1
      indexDual += entry1

      // Get by Task ID
      indexSimple.getOrElse(taskId1, health2) should equal(health1)
      indexDual.getOrElse(taskId1, health2) should equal(health1)

      indexSimple.getOrElse(taskId3, health2) should equal(health2)
      indexDual.getOrElse(taskId3, health2) should equal(health2)
    }

    "remove leftover items" in {
      val indexSimple: HealthIndex = HealthSingleTrieMap()
      val indexDual: HealthIndex = HealthDualTrieMap()

      // Add
      val entry1 = taskId1 -> health1
      indexSimple += entry1
      indexDual += entry1

      // Remove
      indexSimple.removeLeftOverHealthIfAny(instanceId2) should equal(None)
      indexSimple.removeLeftOverHealthIfAny(instanceId1) should equal(Some(health1))
      indexSimple.get(instanceId1) should equal(None)
      indexSimple.removeLeftOverHealthIfAny(instanceId1) should equal(None)

      indexDual.removeLeftOverHealthIfAny(instanceId2) should equal(None)
      indexDual.removeLeftOverHealthIfAny(instanceId1) should equal(Some(health1))
      indexDual.get(instanceId1) should equal(None)
      indexDual.removeLeftOverHealthIfAny(instanceId1) should equal(None)
    }

    "return items" in {
      val indexSimple: HealthIndex = HealthSingleTrieMap()
      val indexDual: HealthIndex = HealthDualTrieMap()

      // Add
      val entry1 = taskId1 -> health1
      indexSimple += entry1
      indexDual += entry1

      val entry2 = taskId2 -> health2
      indexSimple += entry2
      indexDual += entry2

      // Get values
      val expected = Set(health1, health2)
      indexSimple.values.toSet should equal(expected)
      indexDual.values.toSet should equal(expected)
    }

    "filter items" in {
      val indexSimple: HealthIndex = HealthSingleTrieMap()
      val indexDual: HealthIndex = HealthDualTrieMap()

      // Add
      val entry1 = taskId1 -> health1
      indexSimple += entry1
      indexDual += entry1

      val entry2 = taskId2 -> health2
      indexSimple += entry2
      indexDual += entry2

      val entry3 = taskId3 -> health3
      indexSimple += entry3
      indexDual += entry3

      // Filter by task ID
      indexSimple.filterKeys(taskId => taskId == taskId2) should equal(Map(entry2))

      indexDual.filterKeys(taskId => taskId == taskId2) should equal(Map(entry2))
    }

    "retain items" in {
      val indexSimple: HealthIndex = HealthSingleTrieMap()
      val indexDual: HealthIndex = HealthDualTrieMap()

      // Add
      val entry1 = taskId1 -> health1
      indexSimple += entry1
      indexDual += entry1

      val entry2 = taskId2 -> health2
      indexSimple += entry2
      indexDual += entry2

      val entry3 = taskId3 -> health3
      indexSimple += entry3
      indexDual += entry3

      // Retain
      indexSimple.retain(taskId => taskId == taskId2)
      indexSimple.get(instanceId1) should equal(None)
      indexSimple.get(instanceId2) should equal(Some(health2))

      indexDual.retain(taskId => taskId == taskId2)
      indexDual.get(instanceId1) should equal(None)
      indexDual.get(instanceId2) should equal(Some(health2))
    }

  }

}

object HealthIndexTest {
  val appId: AbsolutePathId = AbsolutePathId("/test")
  val version: Timestamp = Timestamp(1)
  val now: Timestamp = Timestamp(2)

  val instanceId1: Instance.Id = Instance.Id.forRunSpec(appId)
  val taskId1: Task.Id = Task.Id(instanceId1)
  val health1: Health = Health(instanceId1)

  val instanceId2: Instance.Id = Instance.Id.forRunSpec(appId)
  val taskId2: Task.Id = Task.Id(instanceId2)
  val health2: Health = Health(instanceId2)

  val instanceId3: Instance.Id = Instance.Id.forRunSpec(appId)
  val taskId3: Task.Id = Task.Id(instanceId3)
  val health3: Health = Health(instanceId3)
}
