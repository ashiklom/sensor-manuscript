#!/bin/bash
qsub -V -v index=1,sensor=identity,runname=identity.1 -N "identity.1" submit.simulated.qsub
qsub -V -v index=1,sensor=aviris.ng,runname=aviris.ng.1 -N "aviris.ng.1" submit.simulated.qsub
qsub -V -v index=1,sensor=aviris.classic,runname=aviris.classic.1 -N "aviris.classic.1" submit.simulated.qsub
qsub -V -v index=1,sensor=hyperion,runname=hyperion.1 -N "hyperion.1" submit.simulated.qsub
qsub -V -v index=1,sensor=chris.proba,runname=chris.proba.1 -N "chris.proba.1" submit.simulated.qsub
qsub -V -v index=1,sensor=landsat5,runname=landsat5.1 -N "landsat5.1" submit.simulated.qsub
qsub -V -v index=1,sensor=landsat7,runname=landsat7.1 -N "landsat7.1" submit.simulated.qsub
qsub -V -v index=1,sensor=landsat8,runname=landsat8.1 -N "landsat8.1" submit.simulated.qsub
qsub -V -v index=1,sensor=modis,runname=modis.1 -N "modis.1" submit.simulated.qsub
qsub -V -v index=1,sensor=viirs,runname=viirs.1 -N "viirs.1" submit.simulated.qsub
qsub -V -v index=1,sensor=avhrr,runname=avhrr.1 -N "avhrr.1" submit.simulated.qsub
