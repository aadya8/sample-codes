from ModelEvents import Arrival, EndOfExam, EndOfMentalHealthConsult

class Patient:
    def __init__(self, id, if_with_depression):
        """ create a patient
        :param id: (integer) patient ID
        :param if_with_depression: (bool) set to true if the patient has depression
        """
        self.id = id
        self.ifWithDepression = if_with_depression
        self.tArrived = None  # time the patient arrived
        self.tJoinedWaitingRoomExam = None  # time the patient joined the waiting room for physician
        self.tJoinedMHWaitingRoom = None # time the patient joined the waiting room for mental health consultation
        self.tLeftWaitingRoom = None  # time the patient left the waiting room
        self.tLeftMHWaitingRoom = None # time the patient left the waiting room for mental health consultation
    # to create trace of patient with unique id
    def __str__(self):
        return "Patient " + str(self.id)
class WaitingRoom:
    def __init__(self, sim_out, trace, room_type):
        """ create a waiting room
        :param sim_out: simulation output
        :param trace: simulation trace
        :param room_type: physician exam room or mental health consultation room
        """
        self.patientsWaiting = []   # list of patients in the waiting room fpr physician
        self.patientsWaitingForConsult = [] # list of patients in the waiting room fpr mental health consultation
        self.simOut = sim_out # placeholder for retrieving time stamps of simulation steps
        self.trace = trace
        self.roomType = room_type

    def add_patient(self, patient):
        """ add a patient to the waiting room
        :param patient: a patient to be added to the waiting room
        """
        # update statistics for the patient who joins the waiting room for physician/MH Specialist
        self.simOut.collect_patient_joining_waiting_room(patient=patient, room_type=self.roomType)
        # add the patient to the list of patients waiting for physician/MH Specialist

        # trace
        if self.roomType == "mental_health_consultation":
            self.patientsWaitingForConsult.append(patient)
            self.trace.add_message(
                str(patient) + ' joins the MH waiting room. Number waiting = ' + str(
                    len(self.patientsWaitingForConsult))
            )
        else:  # For physician examination waiting room
            self.patientsWaiting.append(patient)
            self.trace.add_message(
                str(patient) + ' joins the waiting room for physician examination. Number waiting = ' + str(
                    len(self.patientsWaiting))
            )


    def get_next_patient(self):
        """
        :returns: the next patient in line
        """

        # update statistics for the patient who leaves the waiting room

        # trace
        if self.roomType == "mental_health_consultation":
            self.simOut.collect_patient_leaving_waiting_room(patient=self.patientsWaitingForConsult[0], room_type=self.roomType)
            self.trace.add_message(
                str(self.patientsWaitingForConsult[0]) + ' leaves the MH waiting room. Number waiting = '
                + str(len(self.patientsWaitingForConsult) - 1) + '.')
            return self.patientsWaitingForConsult.pop(0)

        else: # For physician examination waiting room
            self.simOut.collect_patient_leaving_waiting_room(patient=self.patientsWaiting[0],
                                                             room_type=self.roomType)
            self.trace.add_message(
                str(self.patientsWaiting[0]) + ' leaves the physician waiting room. Number waiting = '
                + str(len(self.patientsWaiting) - 1) + '.')
            return self.patientsWaiting.pop(0) # pop the next patient

    def get_num_patients_waiting(self):
        """
        :return: the number of patient waiting in the waiting room
        """
        if self.roomType == "mental_health_consultation":
            return len(self.patientsWaitingForConsult)
        else: # For physician examination waiting room
            return len(self.patientsWaiting)

class PCP:
    def __init__(self, id, service_time_dist, urgent_care, sim_cal, sim_out, trace):
        """ create a primary care physician
        :param id: (integer) id
        :param service_time_dist: distribution of service time in this exam room
        :param urgent_care: urgent care
        :param sim_cal: simulation calendar
        :param sim_out: simulation output
        :param trace: simulation trace
        """
        self.id = id
        self.serviceTimeDist = service_time_dist
        self.urgentCare = urgent_care
        self.simCal = sim_cal # for storing time stamps to determine exact completion time
        self.simOut = sim_out
        self.trace = trace # for storing descriptions for patient's steps throughout the urgent care
        self.isBusy = False
        self.patientBeingServedPhysician = None  # the patient who is being served

    def __str__(self):
        """ :returns (string) the physician id """
        return "Physician " + str(self.id) # to identify trace of unique physician
    def exam(self, patient, rng):
        """ starts examining the patient
        :param patient: a patient
        :param rng: random number generator
        """

        # the physician is busy
        self.patientBeingServedPhysician = patient
        self.isBusy = True

        # trace
        self.trace.add_message(str(patient) + ' starts service in ' + str(self))
        # collect statistics
        self.simOut.collect_patient_starting_exam()

        # find the exam completion time (current time + service time)
        exam_completion_time = self.simCal.time + self.serviceTimeDist.sample(rng=rng)

        # schedule the end of exam
        self.simCal.add_event(
            event=EndOfExam(time=exam_completion_time, physician=self, urgent_care=self.urgentCare)
        )
    def remove_patient(self):
        returned_patient = self.patientBeingServedPhysician
        self.simOut.collect_patient_departure_exam(patient=self.patientBeingServedPhysician)
        self.trace.add_message(str(self.patientBeingServedPhysician) + ' leaves ' + str(self) + '.')
        self.patientBeingServedPhysician = None
        self.isBusy = False
        return returned_patient


class MHP:
    def __init__(self, id, service_time_dist, urgent_care, sim_cal, sim_out, trace):
        """ create a mental health physician
        :param id: (integer) the room ID
        :param service_time_dist: distribution of service time
        :param urgent_care: urgent care
        :param sim_cal: simulation calendar
        :param sim_out: simulation output
        :param trace: simulation trace
        """
        self.id = id
        self.serviceTimeDist = service_time_dist
        self.urgentCare = urgent_care
        self.simCal = sim_cal
        self.simOut = sim_out
        self.trace = trace
        self.isBusy = False
        self.patientBeingServedMH = None
    def __str__(self):
        """ :returns (string) the MH Specialist id """
        return "MH Specialist" + str(self.id)
    def consult(self, patient, rng):
        """ starts mental health consultation for this patient
        :param patient: a patient
        :param rng: random number generator
        """

        # the room is busy
        self.patientBeingServedMH = patient
        self.isBusy = True

        # trace
        self.trace.add_message(str(patient) + ' starts consultation in ' + str(self))
        # collect statistics
        self.simOut.collect_patient_starting_MH()

        # find the MH consultation completion time (current time + service time)
        exam_completion_time = self.simCal.time + self.serviceTimeDist.sample(rng=rng)

        # schedule the end of MH consultation time
        self.simCal.add_event(
            event=EndOfMentalHealthConsult(time=exam_completion_time, consult_room=self, urgent_care=self.urgentCare)
        )
    def remove_patient(self):
        self.simOut.collect_patient_departure_MH(patient=self.patientBeingServedMH)
        self.trace.add_message(str(self.patientBeingServedMH) + ' leaves ' + str(self) + '.')
        self.patientBeingServedMH = None
        self.isBusy = False
class UrgentCare:
    def __init__(self, id, parameters, sim_cal, sim_out, trace):
        """ creates an urgent care
        :param id: ID of this urgent care
        :param sim_cal: simulation calendar
        :param: parameters of this urgent care
        :param sim_cal: simulation calendar
        :param sim_out: simulation output
        :param trace: simulation trace
        """

        self.id = id              # urgent care id
        self.params = parameters  # parameters of this urgent care
        self.simCal = sim_cal
        self.simOutputs = sim_out
        self.trace = trace
        self.ifOpen = True  # if the urgent care is open and admitting new patients

        # creating waiting room for physician
        self.waitingRoom = WaitingRoom(sim_out=self.simOutputs,
                                       trace=self.trace, room_type="physician_exam")

        # primary care physicians
        self.PCPs = []
        for i in range(0, self.params.nPCPs):
            self.PCPs.append(PCP(id=i,
                                 service_time_dist=self.params.examTimeDist,
                                 urgent_care=self,
                                 sim_cal=self.simCal,
                                 sim_out=self.simOutputs,
                                 trace=self.trace))

        # creating waiting room for mental health consultation
        self.mhConsultWaitingRoom = WaitingRoom(sim_out=self.simOutputs,
                                       trace=self.trace, room_type="mental_health_consultation")

        # create the mental health specialist
        self.MHP = MHP(id=0,
                       service_time_dist=self.params.mentalHealthConsultDist,
                       urgent_care=self,
                       sim_cal=self.simCal,
                       sim_out=self.simOutputs,
                       trace=self.trace
                       )

    def process_new_patient(self, patient, rng):
        """ receives a new patient
        :param patient: the new patient
        :param rng: random number generator
        """
        # trace
        self.trace.add_message(
            'Processing arrival of ' + str(patient) + '.')

        # do not admit the patient if the urgent care is closed
        if not self.ifOpen:
            self.trace.add_message('Urgent care is closed. ' + str(patient) + ' does not get admitted.')
            return
        # collect statistics on new patient
        self.simOutputs.collect_patient_arrival(patient=patient)

        # check if anyone is waiting
        if self.waitingRoom.get_num_patients_waiting() > 0:
            # if anyone is waiting, add the patient to the waiting room
            self.waitingRoom.add_patient(patient=patient)
        else:
            # find an idle physician
            idle_pcp_found = False
            for pcp in self.PCPs:
                # if this pcp is busy
                if not pcp.isBusy:
                    # send the last patient to this pcp
                    pcp.exam(patient=patient, rng=rng)
                    idle_pcp_found = True
                    # break the for loop
                    break

            # if no idle pcp was found
            if not idle_pcp_found:
                # add the patient to the waiting room
                self.waitingRoom.add_patient(patient=patient)

        # find the arrival time of the next patient (current time + time until next arrival)
        next_arrival_time = self.simCal.time + self.params.arrivalTimeDist.sample(rng=rng)

        # find the depression status of the next patient
        if_with_depression = False
        if rng.random_sample() < self.params.probDepression:
            if_with_depression = True

        # schedule the arrival of the next patient
        self.simCal.add_event(
            event=Arrival(
                time=next_arrival_time,
                patient=Patient(id=patient.id + 1, if_with_depression=if_with_depression),
                urgent_care=self
            )
        )

    def process_end_of_exam(self, pcp, rng):
        """ processes the end of exam for this primary care physician
        :param pcp: the pcp that finished the exam
        :param rng: random number generator
        """
        # trace
        self.trace.add_message('Processing the end of exam for ' + str(pcp) + '.')
        # get the patient who is about to be discharged, instead of removing them from the system
        this_patient = pcp.remove_patient()

        # check the mental health status of the patient
        if this_patient.ifWithDepression:
            # trace
            self.trace.add_message(
                'Processing arrival of ' + str(this_patient) + 'to MH care.')
            # collect statistics on new patient
            self.simOutputs.collect_patient_arrival_MH(patient=this_patient)
            # send the patient to the mental health specialist
            # if the mental health specialist is busy
            if self.MHP.isBusy:
                # the patient will join the waiting room in the mental health unity
                self.mhConsultWaitingRoom.add_patient(patient=this_patient)
            else:
                # this patient starts receiving mental health consultation
                self.MHP.consult(patient=this_patient, rng=rng)


        # check if there is any patient waiting
        if self.waitingRoom.get_num_patients_waiting() > 0:
            # start serving the next patient in line
            pcp.exam(patient=self.waitingRoom.get_next_patient(), rng=rng)

    def process_end_of_consultation(self, mhp, rng):
        """ process the end of mental health consultation
        :param mhp: mental health physician
        :param rng: random number generator
        """
        # trace
        self.trace.add_message('Processing the end of consultation for MH Specialist.')

        # get the patient who is about to be discharged
        mhp.remove_patient()


        # check if there is any patient waiting
        if self.mhConsultWaitingRoom.get_num_patients_waiting() > 0:
            # start serving the next patient in line
            mhp.consult(patient=self.mhConsultWaitingRoom.get_next_patient(), rng=rng)

    def process_close_urgent_care(self):
        """ process the closing of the urgent care """
        # trace
        self.trace.add_message('Processing the closing of the urgent care.')
        # close the urgent care
        self.ifOpen = False


