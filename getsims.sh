#!/bin/bash
scp ashiklom@geo.bu.edu:~/dietzelab/sensor-manuscript/run-scripts/samples/*."$1".RData some-simulations
scp ashiklom@geo.bu.edu:~/dietzelab/sensor-manuscript/run-scripts/results/*."$1".csv some-simulations
