from deampy.discrete_event_sim import SimulationEvent
from enum import Enum
import Inputs as D

class Priority(Enum):
    """ priority for processing the urgent care simulation events
        if they are to occur at the exact same time (low number implies higher priority)"""
    ARRIVAL = 2
    END_OF_EXAM = 1
    END_OF_MH_CONSULT = 0
    CLOSE = 3


class Arrival(SimulationEvent):
    def __init__(self, time, patient, urgent_care):
        """
        creates the arrival of the next patient event
        :param time: time of next patient's arrival
        :param patient: next patient
        :param urgent_care: the urgent care
        """
        # initialize the super class
        SimulationEvent.__init__(self, time=time, priority=Priority.ARRIVAL.value)

        self.patient = patient
        self.urgentCare = urgent_care

        # trace
        urgent_care.trace.add_message(
            str(patient) + ' will arrive at time {t:.{deci}f}.'.format(t=time, deci=D.DECI))

    def process(self, rng=None):
        """ processes the arrival of a new patient """

        # receive the new patient
        self.urgentCare.process_new_patient(patient=self.patient, rng=rng)


class EndOfExam(SimulationEvent):
    def __init__(self, time, physician, urgent_care):
        """
        create the end of service for an specified exam room
        :param time: time of the service completion
        :param physician: the exam room
        :param urgent_care: the urgent care
        """
        # initialize the base class
        SimulationEvent.__init__(self, time=time, priority=Priority.END_OF_EXAM.value)

        self.examRoom = physician
        self.urgentCare = urgent_care
        # trace
        urgent_care.trace.add_message(
            str(physician) + ' will finish exam at time {t:.{deci}f}.'.format(t=time, deci=D.DECI))
    def process(self, rng=None):
        """ processes the end of service event """

        # process the end of service for this exam room
        self.urgentCare.process_end_of_exam(pcp=self.examRoom, rng=rng)


class EndOfMentalHealthConsult(SimulationEvent):
    def __init__(self, time, consult_room, urgent_care):
        """
        create the end of mental health consultation
        :param time: time of the service completion
        :param consult_room: the mental health consultation room
        :param urgent_care: the urgent care
        """
        # initialize the base class
        SimulationEvent.__init__(self, time=time, priority=Priority.END_OF_MH_CONSULT.value)

        self.consultRoom = consult_room
        self.urgentCare = urgent_care
        # trace
        urgent_care.trace.add_message(
            str(consult_room) + ' will finish consultation at time {t:.{deci}f}.'.format(t=time, deci=D.DECI))
    def process(self, rng=None):
        """ processes the end of mental health consultation """

        # process the end of service for this exam room
        self.urgentCare.process_end_of_consultation(mhp=self.consultRoom, rng=rng)


class CloseUrgentCare(SimulationEvent):
    def __init__(self, time, urgent_care):
        """
        create the event to close the urgent care
        :param time: time of closure
        :param urgent_care: the urgent care
        """
        self.time = time
        self.urgentCare = urgent_care

        # call the super class initialization
        SimulationEvent.__init__(self, time=time, priority=Priority.CLOSE.value)

        # trace
        urgent_care.trace.add_message(
            'Urgent care will close at time {t:.{deci}f}.'.format(t=time, deci=D.DECI))

    def process(self, rng=None):
        """ processes the closing event """

        # close the urgent care
        self.urgentCare.process_close_urgent_care()