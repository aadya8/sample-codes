# Author: Aadya Mishra
# Task: Simulate using discrete-event simulation an urgent care unit that has 20 physicians and 1 Mental Health (MH) Specialist.
# Patients will also get screened for depression during their exam. If the result is positive, they will be seen by an on-site mental health specialist before leaving the urgent care.
# Forwarded to MH care only after completing their visit with the primary care physician.
# adding a simulation trace to track what exactly happens to each patients, plot different sample paths, and report several performance statistics


# simulation settings
TRACE_ON = True
DECI = 5
SIM_DURATION = 100000   # max hours until simulation is terminated as the simulation continues as long as there is a patient in the urgent care
HOURS_OPEN = 20         # hours the urgent care is open
N_PCP = 10                # number of primary-care physicians
MEAN_ARRIVAL_TIME = 1/60       # mean patients inter-arrival time (hours)
MEAN_EXAM_DURATION = 10/60       # mean of exam duration (hours)
MEAN_MH_CONSULT = 20/60         # mean duration of mental health consultation
PROB_DEPRESSION = 0.1           # probability that a patient is diagnosed with depression