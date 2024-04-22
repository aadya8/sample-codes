# Run this page to execute the simulation

import deampy.plots.sample_paths as path


import UrgentCareModel as M
import Inputs as D
import ModelParameters as P

# create an urgent care model
urgentCareModel = M.UrgentCareModel(id=1, parameters=P.Parameters())

# simulate the urgent care
urgentCareModel.simulate(sim_duration=D.SIM_DURATION)

# sample path for patients waiting for mental health consultation
path.plot_sample_path(
    sample_path=urgentCareModel.simOutputs.nPatientsWaitingMH,
    title='Patients Waiting for MH Specialist',
    x_label='Simulation time (hours)',
)
# sample path for patients in the system
path.plot_sample_path(
    sample_path=urgentCareModel.simOutputs.nPatientInSystem,
    title='Patients In System',
    x_label='Simulation time (hours)',
)
# sample path for physician utilization
path.plot_sample_path(
    sample_path=urgentCareModel.simOutputs.nPhysiciansBusy,
    title='Physician Utilization',
    x_label='Simulation time (hours)'
)
# sample path for MH Specialist utilisation
path.plot_sample_path(
    sample_path=urgentCareModel.simOutputs.nMHSpecialistBusy,
    title='Patients Time in System',
    x_label='Hours'
)



print('Total patients arrived:', urgentCareModel.simOutputs.nPatientsArrived)
print('Total patients arrived in MH:', urgentCareModel.simOutputs.nPatientsArrivedMH)
print('Total patients served:', urgentCareModel.simOutputs.nPatientsServed)
print('Average patient time in system:', urgentCareModel.simOutputs.get_ave_patient_time_in_system())
print('Average patient waiting time for Physician:', urgentCareModel.simOutputs.get_ave_patient_waiting_time_exam())
print('Average patient waiting time in MH:', urgentCareModel.simOutputs.get_ave_patient_waiting_time_MH())
print('Total patients received mental health consultation:', urgentCareModel.simOutputs.nPatientsReceivedConsult)

urgentCareModel.print_trace()