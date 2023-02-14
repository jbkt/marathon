package mesosphere.marathon
package core.health.impl

import mesosphere.marathon.core.health.Health
import mesosphere.marathon.core.instance.Instance
import mesosphere.marathon.core.task.Task

import scala.collection.concurrent.TrieMap

trait HealthIndex {
  def removeLeftOverHealthIfAny(key: Instance.Id): Option[Health]

  def +=(kv: (Task.Id, Health)): this.type

  def values: Iterable[Health]

  def filterKeys(p: Task.Id => Boolean): collection.Map[Task.Id, Health]

  def get(key: Instance.Id): Option[Health]

  def getOrElse(key: Task.Id, default: => Health): Health

  def retain(p: Task.Id => Boolean): this.type
}

/**
  * Old implementation, search by Instance.Id is slow, kept for reference and for tests.
  */
case class HealthSingleTrieMap() extends HealthIndex {
  private val healthByTaskId: TrieMap[Task.Id, Health] = TrieMap.empty[Task.Id, Health]

  def removeLeftOverHealthIfAny(key: Instance.Id): Option[Health] = {
    val healthOfInstanceId = healthByTaskId.find(_._1.instanceId == key)
    if (healthOfInstanceId.isDefined)
      healthByTaskId.remove(healthOfInstanceId.get._1)
    else
      None
  }

  /** Slow, full scan algorithm O(n) */
  def get(key: Instance.Id): Option[Health] = healthByTaskId.find(_._1.instanceId == key).map(_._2)

  def getOrElse(key: Task.Id, default: => Health): Health = healthByTaskId.getOrElse(key, default)

  def filterKeys(p: Task.Id => Boolean): collection.Map[Task.Id, Health] = {
    healthByTaskId.filterKeys(p)
  }

  /** Slow */
  def retain(p: Task.Id => Boolean): this.type = {
    healthByTaskId.retain((taskId, _) => p(taskId))
    this
  }

  def +=(kv: (Task.Id, Health)): this.type = {
    healthByTaskId += kv
    this
  }

  def values: Iterable[Health] = healthByTaskId.values
}

/**
  * New implementation, search by Instance.Id is fast but we need two indexes.
  */
case class HealthDualTrieMap() extends HealthIndex {
  private val healthByTaskId: TrieMap[Task.Id, Health] = TrieMap.empty[Task.Id, Health]
  private val taskIdByInstanceId: TrieMap[Instance.Id, Task.Id] = TrieMap.empty[Instance.Id, Task.Id]

  def removeLeftOverHealthIfAny(key: Instance.Id): Option[Health] =
    taskIdByInstanceId.remove(key).flatMap(oldTaskId => healthByTaskId.remove(oldTaskId))

  /** Fast O(log(n)) */
  def get(key: Instance.Id): Option[Health] = taskIdByInstanceId.get(key).flatMap(t => healthByTaskId.get(t))

  def getOrElse(key: Task.Id, default: => Health): Health = healthByTaskId.getOrElse(key, default)

  def filterKeys(p: Task.Id => Boolean): collection.Map[Task.Id, Health] = {
    healthByTaskId.filterKeys(p)
  }

  /** Slow */
  def retain(p: Task.Id => Boolean): this.type = {
    val oldSize = healthByTaskId.size
    healthByTaskId.retain((taskId, _) => p(taskId))
    if (healthByTaskId.size < oldSize) {
      val retainedInstanceIds = healthByTaskId.keySet.map(_.instanceId)
      taskIdByInstanceId.retain((instanceId, _) => retainedInstanceIds.contains(instanceId))
    }
    this
  }

  def +=(kv: (Task.Id, Health)): this.type = {
    kv match {
      case (taskId: Task.Id, _: Health) =>
        healthByTaskId += kv
        taskIdByInstanceId += taskId.instanceId -> taskId
    }
    this
  }

  def values: Iterable[Health] = healthByTaskId.values
}
