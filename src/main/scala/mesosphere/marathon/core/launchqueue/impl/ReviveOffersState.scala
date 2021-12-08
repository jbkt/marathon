package mesosphere.marathon
package core.launchqueue.impl

import com.typesafe.scalalogging.StrictLogging
import scala.util.Random
import mesosphere.marathon.core.instance.update.InstancesSnapshot
import mesosphere.marathon.core.instance.{Goal, Instance}
import mesosphere.marathon.core.launchqueue.impl.ReviveOffersState.{OffersWantedInfo, OffersWantedReason, Role}
import mesosphere.marathon.core.launchqueue.impl.ReviveOffersStreamLogic.VersionedRoleState
import mesosphere.marathon.state.RunSpecConfigRef
import mesosphere.marathon.raml.Resources

/**
  * Holds the current state and defines the revive logic.
  *
  * @param instancesWantingOffers All scheduled instances, grouped by role, that want offers
  * @param activeDelays    Delays for run specs.
  * @param version         Monotonically increasing number, used ultimately so that we can tell if new instances for a role want offers
  */
case class ReviveOffersState(
    instancesWantingOffers: Map[Role, Map[Instance.Id, OffersWantedInfo]],
    activeDelays: Set[RunSpecConfigRef],
    version: Long) extends StrictLogging {

  /** whether the instance has a reservation that should be freed. */
  private def shouldUnreserve(instance: Instance): Boolean = {
    instance.reservation.nonEmpty && instance.state.goal == Goal.Decommissioned && instance.state.condition.isTerminal
  }

  private def copyBumpingVersion(
    newInstancesWantingOffers: Map[Role, Map[Instance.Id, OffersWantedInfo]] = instancesWantingOffers,
    newActiveDelays: Set[RunSpecConfigRef] = activeDelays): ReviveOffersState = {

    logWantedOffers(newInstancesWantingOffers)
    copy(newInstancesWantingOffers, newActiveDelays, version + 1)
  }

  /**
    * Process the initial state snapshot from the instance subscription mechanism.
    *
    * In order to stabilize the list of roles to which Marathon is subscribed, we indicate that all known roles for all
    * instances should be subscribed by adding an empty map entry.
    *
    * @return new copy of the state with the change applied
    */
  def withSnapshot(snapshot: InstancesSnapshot, defaultRole: Role): ReviveOffersState = {
    val rolesWithOffersWantedData = snapshot.instances.groupBy(_.role).map {
      case (role, instances) =>
        role -> instances.view.filter(wantsOffers).map { i => i.instanceId -> instanceToWantedInfo(i) }.toMap
    }
    val defaultRoleEntry: Map[Role, Map[Instance.Id, OffersWantedInfo]] = Map(defaultRole -> Map.empty)

    // Note - we take all known roles, whether offers are wanted or not, and create at least an empty map entry in the wantedInfo map
    copyBumpingVersion(newInstancesWantingOffers = defaultRoleEntry ++ rolesWithOffersWantedData)
  }

  private def hasRecordOfInstanceWantingOffers(role: Role, instanceId: Instance.Id): Boolean = {
    instancesWantingOffers.getOrElse(role, Map.empty).contains(instanceId)
  }

  private def updateInstanceState(role: Role, instanceId: Instance.Id, newState: Option[Instance]): ReviveOffersState = {
    val newWantedInfo = newState.filter(wantsOffers).map(instanceToWantedInfo)

    val newInstancesWantingOffers: Map[Role, Map[Instance.Id, OffersWantedInfo]] = newWantedInfo match {
      case Some(wantedInfo) =>
        wantedInfo.reason match {
          case OffersWantedReason.Launching =>
            logger.info(s"Adding ${instanceId} to scheduled instances.")
          case OffersWantedReason.CleaningUpReservations =>
            logger.info(s"$instanceId is terminal but has a reservation.")
        }
        val newRoleOffersWanted = instancesWantingOffers.getOrElse(role, Map.empty) + (instanceId -> wantedInfo)
        instancesWantingOffers + (role -> newRoleOffersWanted)
      case None =>
        if (hasRecordOfInstanceWantingOffers(role, instanceId))
          logger.info(s"Removing ${instanceId} from instances wanting offers.")
        val newRoleOffersWanted = instancesWantingOffers.getOrElse(role, Map.empty) - instanceId

        /* we don't clean up empty entries on purpose; this allows us to continue to signal that at one point in time,
         * either via the initial snapshot or later down the road, offers were wanted for an instance. Marathon will
         * remove any totally unused roles (for which no instances are defined) when a new leader is instated
         */
        instancesWantingOffers + (role -> newRoleOffersWanted)
    }

    copyBumpingVersion(newInstancesWantingOffers = newInstancesWantingOffers)
  }

  /** @return this state updated with an instance. */
  def withInstanceAddedOrUpdated(instance: Instance): ReviveOffersState = {
    if (wantsOffers(instance) && hasRecordOfInstanceWantingOffers(instance.role, instance.instanceId)) {
      this
    } else {
      updateInstanceState(instance.role, instance.instanceId, Some(instance))
    }
  }

  private def wantsOffers(instance: Instance): Boolean = {
    instance.isScheduled || shouldUnreserve(instance)
  }

  private def instanceToWantedInfo(instance: Instance): OffersWantedInfo = {
    OffersWantedInfo(
      version,
      if (shouldUnreserve(instance)) OffersWantedReason.CleaningUpReservations else OffersWantedReason.Launching,
      instance.runSpec.configRef,
      instance.runSpec.resources)
  }

  /** @return this state with passed instance removed from [[instancesWantingOffers]]. */
  def withInstanceDeleted(instance: Instance): ReviveOffersState = {
    updateInstanceState(instance.role, instance.instanceId, Some(instance))
  }

  /**
    * Removes delay from the state. If any instance's offers-wanted signal were filtered, then we bump their
    * version so that a new revive will occur.
    *
    * @return this state with removed ref from [[activeDelays]].
    */
  def withoutDelay(ref: RunSpecConfigRef): ReviveOffersState = {
    logger.info(s"Marking $ref as no longer actively delayed for suppress/revive")

    // This is not optimized
    val bumpedVersions = instancesWantingOffers.map {
      case (role, instancesWantedInfo) =>
        role -> instancesWantedInfo.map {
          case (instanceId, wantedInfo) =>
            if (wantedInfo.ref == ref)
              instanceId -> wantedInfo.copy(version = this.version)
            else
              instanceId -> wantedInfo
        }
    }
    copyBumpingVersion(newInstancesWantingOffers = bumpedVersions, newActiveDelays = activeDelays - ref)
  }

  /** @return this state with updated [[activeDelays]]. */
  def withDelay(ref: RunSpecConfigRef): ReviveOffersState = {
    logger.info(s"Marking $ref as actively delayed for suppress/revive")
    copyBumpingVersion(newActiveDelays = activeDelays + ref)
  }

  /**
    * Returns a map of all known roles and a desired versioned role-state, where the role-state is
    * [[OffersWanted]] or [[OffersNotWanted]].
    *
    * A version is used to help indicate whether or not a new revive is wanted for some role; for example
    *
    * 1. Offers are indicated as wanted for a role with version 1
    * 2. The downstream revive directive interprets this as a role should be unsuppressed
    * 3. Offers are indicated as wanted for a role with version 2
    * 4. The downstream revive directive issues a new revive
    */
  lazy val roleReviveVersions: Map[Role, VersionedRoleState] = {
    instancesWantingOffers.keysIterator.map { role =>
      val iterator = instancesWantingOffers.getOrElse(role, Map.empty).values
        .iterator
        .filter(launchAllowedOrCleanUpRequired)
      val offersWantedList = iterator.toList;
      val minimalResources = findMinimalResources(offersWantedList)
      if (offersWantedList.isEmpty)
        role -> VersionedRoleState(version, OffersNotWanted)
      else
        role -> VersionedRoleState(offersWantedList.map(_.version).max, OffersWanted, minimalResources)
    }.toMap
  }

  private def findMinimalResources(offersWantedInfos: List[OffersWantedInfo]): Resources = {
    var actualMinimal = Resources(Double.MaxValue, Double.MaxValue, Double.MaxValue, Int.MaxValue, Int.MaxValue)
    logger.debug(s"Scheduled Instances for launch are ${offersWantedInfos}")

    // There may be several minimums, since we consider several resources (cpu, mem, disk, gpu, network bandwidth).
    // We shuffle the list of wanted offers to give a chance to all tasks to be scheduled if cluster capacity for one resource is too low.
    // Note that the slave sorter feature in mesos favours small tasks over big ones; the latter may have difficulties to be scheduled
    Random.shuffle(offersWantedInfos).foreach(instance => {
      val requiredResources: Resources = instance.resources
      if (requiredResources.cpus <= actualMinimal.cpus &&
        requiredResources.mem <= actualMinimal.mem &&
        requiredResources.disk <= actualMinimal.disk &&
        requiredResources.networkBandwidth <= actualMinimal.networkBandwidth) {
        actualMinimal = requiredResources
      }
    })

    logger.debug(s"Minimal resources is: ${actualMinimal}")

    actualMinimal
  }

  /** @return true if a instance has no active delay, or the instance requires clean up. */
  private def launchAllowedOrCleanUpRequired(wantedInfo: OffersWantedInfo): Boolean = {
    wantedInfo.reason == OffersWantedReason.CleaningUpReservations || !activeDelays.contains(wantedInfo.ref)
  }

  private def logWantedOffers(update: Map[Role, Map[Instance.Id, OffersWantedInfo]]): Unit = {
    update.keysIterator.foreach { role =>
      val new_list = update.getOrElse(role, Map.empty).values.toList
      val old_list = instancesWantingOffers.getOrElse(role, Map.empty).values.toList
      logger.info(s"instancesWantingOffers for ${role} : ${old_list} -> ${new_list}")
    }
  }
}

object ReviveOffersState {
  private[impl] type Role = String
  def empty = ReviveOffersState(Map.empty, Set.empty, 0)

  private[impl] case class OffersWantedInfo(version: Long, reason: OffersWantedReason, ref: RunSpecConfigRef, resources: Resources)

  private[impl] sealed trait OffersWantedReason

  private[impl] case object OffersWantedReason {

    case object CleaningUpReservations extends OffersWantedReason

    case object Launching extends OffersWantedReason

  }
}
