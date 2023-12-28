package chapter08

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits.*

object SchedulingMeetings {
  case class MeetingTime(startHour: Int, endHour: Int)

  def calendarEntriesApiCall(name: String): List[MeetingTime] = ???

  def createMeetingApiCall(names: List[String], meetingTime: MeetingTime) = ???

  def calendarEntries(name: String): IO[List[MeetingTime]] = {
    IO
      .delay { calendarEntriesApiCall(name) }
      .orElse { IO.delay { calendarEntriesApiCall(name) } }
      .orElse { IO.pure { List.empty } }
  }

  def createMeeting(names: List[String], meetingTime: MeetingTime): IO[Unit] = {
    IO.delay { createMeetingApiCall(names, meetingTime) }
  }

  def scheduleMeetings(attendees: List[String]): IO[List[MeetingTime]] = {
    attendees.map { attendee => retry(calendarEntries(attendee), 10) }.flatSequence
  }

  @main def runScheduleMeetings = {
    val program = scheduleMeetings(List("Alice", "Bob"))
    program.unsafeRunSync()
  }

  def meetingsOverlap(meeting1: MeetingTime, meeting2: MeetingTime): Boolean = {
    meeting1.endHour > meeting2.startHour && meeting2.endHour > meeting1.startHour
  }

  def possibleMeetings(
    existingMeetings: List[MeetingTime],
    startHour: Int,
    endHour: Int,
    lengthHours: Int
  ): List[MeetingTime] = {
    List
      .range(startHour, endHour - lengthHours + 1)
      .map { startHour => MeetingTime(startHour, startHour + lengthHours) }
      .filter { slot => existingMeetings.forall { meeting => !meetingsOverlap(meeting, slot) } }
  }

  def schedule(attendees: List[String], lengthHours: Int): IO[Option[MeetingTime]] = {
    for {
      existingMeetings <- scheduleMeetings(attendees)
      meetings = possibleMeetings(existingMeetings, 8, 16, lengthHours)
      possibleMeeting = meetings.headOption
      _ <- possibleMeeting match {
        case Some(meeting) => retry(createMeeting(attendees, meeting), 10)
        case None          => IO.unit
      }
    } yield meetings.headOption
  }

  def schedulingProgram(getName: IO[String], showMeeting: Option[MeetingTime] => IO[Unit]): IO[Unit] = {
    for {
      name1 <- getName
      name2 <- getName
      possibleMeeting <- schedule(List(name1, name2), 2)
      _ <- showMeeting(possibleMeeting)
    } yield ()
  }

  def cachedCalendarEntries(name: String): IO[List[MeetingTime]] = ???

  def updateCachedEntries(name: String, newEntries: List[MeetingTime]): IO[Unit] = ???

  def calendarEntriesWithCache(name: String): IO[List[MeetingTime]] = {
    val getEntriesAndUpdatedCache: IO[List[MeetingTime]] = for {
      currentEntries <- calendarEntries(name)
      _ <- updateCachedEntries(name, currentEntries)
    } yield currentEntries
    cachedCalendarEntries(name).orElse { getEntriesAndUpdatedCache }
  }

  def retry[A](action: IO[A], maxRetries: Int): IO[A] = {
    List
      .range(0, maxRetries)
      .map { _ => action }
      .foldLeft(action) { (program, retryAction) => program.orElse { retryAction } }
  }
}
