#!/bin/bash -l

#$ -N roads_nat_t ## Name the job
#$ -j y         ## Merge error & output files
#$ -pe omp 8

module load R/4.2.1
Rscript /projectnb/gislane/allison/Mexico/2A_OSM_Road_Length.R $SGE_TASK_ID

## In Terminal, cd to the directory in which this bash script is located. 
## Run the following line:
## qsub -P project -t 1-(# of total states) 2A_OSM_Road_Length_Mexico.R
##
## This bash script will run as an array with an index for every state. The index will be
## read in by the R script 2 to identify the state to process. By processing only by state,
## we reduce the computation time needed. Alternatively, a series of states could
## be specified directly in your R script.