package mesosphere.marathon
package core.health.impl

import mesosphere.marathon.core.health.Health
import mesosphere.marathon.core.instance.Instance
import mesosphere.marathon.core.task.Task

import scala.collection.concurrent.TrieMap

trait HealthIndex {
  def remove(key: Instance.Id): Unit

  def remove(key: Task.Id): Unit

  def +=(kv: (Task.Id, Health)): this.type

  def values: Iterable[Health]

  def filterKeys(p: Task.Id => Boolean): collection.Map[Task.Id, Health]

  def get(key: Instance.Id): Option[Health]

  def getOrElse(key: Task.Id, default: => Health): Health

  def retain(p: Task.Id => Boolean): this.type
}

/** Old implementation, search by Instance.Id is slow */
case class HealthSingleTrieMap() extends HealthIndex {
  val healthByTaskId: TrieMap[Task.Id, Health] = TrieMap.empty[Task.Id, Health]

  def remove(key: Instance.Id): Unit = get(key).foreach(h => remove(h.instanceId))

  def remove(key: Task.Id): Unit = healthByTaskId.remove(key)

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

/** New implementation, search by Instance.Id is fast but we need two indexes */
case class HealthDualTrieMap() extends HealthIndex {
  val healthByTaskId: TrieMap[Task.Id, Health] = TrieMap.empty[Task.Id, Health]
  val healthByInstanceId: TrieMap[Instance.Id, Health] = TrieMap.empty[Instance.Id, Health]

  def remove(key: Instance.Id): Unit = get(key).foreach(h => remove(h.instanceId))

  def remove(key: Task.Id): Unit = {
    healthByTaskId.remove(key)
    healthByInstanceId.remove(key.instanceId)
  }

  /** Fast O(log(n)) */
  def get(key: Instance.Id): Option[Health] = healthByInstanceId.get(key)

  def getOrElse(key: Task.Id, default: => Health): Health = healthByTaskId.getOrElse(key, default)

  def filterKeys(p: Task.Id => Boolean): collection.Map[Task.Id, Health] = {
    healthByTaskId.filterKeys(p)
  }

  /** Slower */
  def retain(p: Task.Id => Boolean): this.type = {
    val oldSize = healthByTaskId.size
    healthByTaskId.retain((taskId, _) => p(taskId))
    if (healthByTaskId.size < oldSize) {
      val retainedInstanceIds = healthByTaskId.keySet.map(_.instanceId)
      healthByInstanceId.retain((instanceId, _) => retainedInstanceIds.contains(instanceId))
    }
    this
  }

  def +=(kv: (Task.Id, Health)): this.type = {
    case (taskId: Task.Id, health: Health) =>
      healthByTaskId += kv
      healthByInstanceId += (taskId.instanceId, health)
      this
  }

  def values: Iterable[Health] = healthByTaskId.values
}
