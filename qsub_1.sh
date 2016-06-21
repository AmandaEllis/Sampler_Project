#!/bin/bash

###  Simulation parameters ###
n=100				# Number of replicates
WD="$HOME/PBS_TEST"		# Working directory
BASE="sim_1"			# Base text string used to name scripts

### R parameters ###
R="R"
RFLAGS="--no-restore --no-save --no-readline --args"
RSCRIPT="Simulation_M0.R"		# R script containing simulation code

### Create output directories ###
mkdir PBS 			# Contains PBS scripts
mkdir Logs			# Contains R log files
mkdir Output			# Contains R output files

### Loop over replicates ###
i=1

while [ "$i" -le "$n" ]; do
    
    PBSSCRIPT="PBS/${BASE}_${i}_pbs" # Name of next PBS script
    
    # Write PBS lines to PBS script
    echo "#PBS -l ncpus=1,mem=1G,walltime=10:00:00" > $PBSSCRIPT
    echo "#PBS -N ${BASE}_${i}" >> $PBSSCRIPT
    echo "#PBS -q short" >> $PBSSCRIPT
    echo "#PBS -M $USER@uky.edu" >> $PBSSCRIPT
    echo "#PBS -m ae" >> $PBSSCRIPT
    echo "#PBS -j oe" >> $PBSSCRIPT
    echo "#PBS -o Logs/${BASE}_${i}_pbs.log" >> $PBSSCRIPT
    echo "#PBS -d $WD" >> $PBSSCRIPT

    # Write R command to PBS script
    echo "$R $RFLAGS $i < $RSCRIPT  &> Logs/${BASE}_${i}.Rout" >> $PBSSCRIPT
    
    # Submit PBS script to the batch queue
    qsub $PBSSCRIPT

    i=$(($i + 1))
done

exit
