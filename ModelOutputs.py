from deampy.format_functions import format_number
from deampy.sample_path import PrevalenceSamplePath

import Inputs as D

class SimOutputs:
    # to collect the outputs of a simulation run

    def __init__(self, sim_cal, trace_on=False):
        """
        :param sim_cal: simulation calendar
        :param warm_up_period: warm up period (hours)
        :param trace_on: set to True to report patient summary
        """

        self.simCal = sim_cal             # simulation calendar (to know the current time)
        self.traceOn = trace_on           # if patient summary report should be prepared
        self.nPatientsArrived = 0         # number of patients arrived for physician care
        self.nPatientsArrivedMH = 0       # number of patients arrived for mental health care
        self.nPatientsServed = 0          # number of patients served
        self.nPatientsReceivedConsult = 0 # number of patients received consultation
        self.patientTimeInSystem = []     # observations on patients time spent in urgent care
        self.patientTimeInWaitingRoomExam = []  # observations on patients time spent in the physician waiting room
        self.patientTimeInMHWaitingRoom = []    # observations on patients time spent in the MH waiting room

        self.patientSummary = []    # id, tArrived, tLeft, duration waited, duration in the system, room type
        if self.traceOn:
            self.patientSummary.append(
                ['Patient', 'Time Arrived', 'Time Left', 'Time Waited', 'Time In the System', 'Room Type'])

        # sample path for the patients waiting
        self.nPatientsWaitingExam = PrevalenceSamplePath(
            name='Number of patients waiting for Physician', initial_size=0)
        self.nPatientsWaitingMH = PrevalenceSamplePath(
            name='Number of patients waiting for MH', initial_size=0)

        # sample path for the patients in system
        self.nPatientInSystem = PrevalenceSamplePath(
            name='Number of patients in the urgent care', initial_size=0)

        # sample path for the number of physicians busy
        self.nPhysiciansBusy = PrevalenceSamplePath(
            name='Number of physicians busy', initial_size=0)
        self.nMHSpecialistBusy = PrevalenceSamplePath(
            name='Number of Mental Health Specialist busy', initial_size=0)

    def collect_patient_arrival(self, patient):
            """ collects statistics upon arrival of a patient
            :param patient: the patient who just arrived
            """
        # increment the number of patients arrived
            self.nPatientsArrived += 1

        # update the sample path of patients in the system
            self.nPatientInSystem.record_increment(time=self.simCal.time, increment=+1)

        # store arrival time of this patient
            patient.tArrived = self.simCal.time

    def collect_patient_arrival_MH(self, patient):
        """ collects statistics upon arrival of a patient to MH care
        :param patient: the patient who just arrived after diagnosing with depression
        """
        # increment the number of patients arrived in MH care
        self.nPatientsArrivedMH += 1
        # store arrival time of this patient
        patient.tArrivedMH = self.simCal.time

    def collect_patient_joining_waiting_room(self, patient, room_type):
        """ collects statistics when a patient joins a waiting room
        :param patient: the patient who is joining the waiting room
        """
        # store the time this patient joined the waiting room



        if room_type == "physician_exam":
              patient.tJoinedWaitingRoomExam = self.simCal.time
              self.nPatientsWaitingExam.record_increment(time=self.simCal.time, increment=1)
        elif room_type == "mental_health_consultation":
              patient.tJoinedMHWaitingRoom = self.simCal.time
              self.nPatientsWaitingMH.record_increment(time=self.simCal.time, increment=1)

    def collect_patient_leaving_waiting_room(self, patient, room_type):
        """ collects statistics when a patient leave a waiting room
        :param patient: the patient who is leave the waiting room
        """

        if room_type == "physician_exam":
            self.nPatientsWaitingExam.record_increment(time=self.simCal.time, increment=-1)
            patient.tLeftWaitingRoomExam = self.simCal.time
        elif room_type == "mental_health_consultation":
            self.nPatientsWaitingMH.record_increment(time=self.simCal.time, increment=-1)
            patient.tLeftMHWaitingRoom = self.simCal.time



    def collect_patient_departure_exam(self, patient):
         """ collects statistics for a departing patient from examination """
         time_in_system = None
         if patient.tJoinedWaitingRoomExam is None:
             time_waiting = 0
         else:
             time_waiting = patient.tLeftWaitingRoomExam - patient.tJoinedWaitingRoomExam

         if not patient.ifWithDepression: # if patient isn't diagnosed with depression and leaves urgent care post physician examination
             time_in_system = self.simCal.time - patient.tArrived
             # reduce number of patients in the system by 1
             self.nPatientInSystem.record_increment(time=self.simCal.time, increment=-1)
             self.patientTimeInSystem.append(time_in_system)

         self.nPhysiciansBusy.record_increment(time=self.simCal.time, increment=-1) # reduce number of physicians busy by 1
         self.nPatientsServed += 1
         self.patientTimeInWaitingRoomExam.append(time_waiting)

         if self.traceOn:
             # if patient transferred to MH care, then time in system during physician care stage is shown none/incomplete
             # only concrete record for patient who leaves urgent care after physician exam
             time_in_system_str = format_number(time_in_system, deci=D.DECI) if time_in_system is not None else "None"
             self.patientSummary.append([
                     str(patient), format_number(patient.tArrived, deci=D.DECI),
                     format_number(self.simCal.time, deci=D.DECI), format_number(time_waiting, deci=D.DECI),
                     time_in_system_str, "Physician Exam"])


    def collect_patient_departure_MH(self, patient):
        """ collects statistics for a departing patient from mental health consultation """
        if patient.tJoinedMHWaitingRoom is None: # for patients who directly left the urgent care with negative depression diagnosis
            time_waiting = 0
        else:
            time_waiting = patient.tLeftMHWaitingRoom- patient.tJoinedMHWaitingRoom
        time_in_system = self.simCal.time - patient.tArrived # for patients who were diagnosed for depression
        self.nPatientInSystem.record_increment(time=self.simCal.time, increment=-1)
        self.nMHSpecialistBusy.record_increment(time=self.simCal.time, increment=-1)
        self.nPatientsReceivedConsult += 1
        self.patientTimeInMHWaitingRoom.append(time_waiting)
        self.patientTimeInSystem.append(time_in_system)
        if self.traceOn:
           self.patientSummary.append([
            str(patient), format_number(patient.tArrived, deci=D.DECI),
            format_number(self.simCal.time, deci=D.DECI), format_number(time_waiting, deci=D.DECI),
            format_number(time_in_system, deci=D.DECI), "Mental Health Consultation"
            ])


    def collect_patient_starting_exam(self):
        """ collects statistics for a patient who just started the exam """
        self.nPhysiciansBusy.record_increment(time=self.simCal.time, increment=+1)

    def collect_patient_starting_MH(self):
        """ collects statistics for a patient who just started the consultation """
        self.nMHSpecialistBusy.record_increment(time=self.simCal.time, increment=+1)

    def collect_end_of_simulation(self):
        """ collects the performance statistics at the end of the simulation """
        self.nPatientsWaitingExam.close(time=self.simCal.time)
        self.nPatientsWaitingMH.close(time=self.simCal.time)
        self.nPatientInSystem.close(time=self.simCal.time)
        self.nPhysiciansBusy.close(time=self.simCal.time)
        self.nMHSpecialistBusy.close(time=self.simCal.time)


    def get_ave_patient_time_in_system(self):
        """ returns average patient time in system """
        return sum(self.patientTimeInSystem) / len(self.patientTimeInSystem)


    def get_ave_patient_waiting_time_exam(self):
        """ returns average patient waiting time """
        return sum(self.patientTimeInWaitingRoomExam) / len(self.patientTimeInWaitingRoomExam)

    def get_ave_patient_waiting_time_MH(self):
        """ returns average patient waiting time """
        return sum(self.patientTimeInMHWaitingRoom) / len(self.patientTimeInMHWaitingRoom)




