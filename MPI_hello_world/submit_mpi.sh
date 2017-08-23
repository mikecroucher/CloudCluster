#!/bin/bash
#$ -pe mpislots 4
#$ -cwd

#Save details of node placement
cat $PE_HOSTFILE  > node_info_$JOB_ID.txt
#Run this or module commands won't work
source /etc/profile.d/alces-clusterware.sh

#Load the MPI module
module load  mpi/openmpi/1.10.2/gcc-4.8.5
#Compile
mpicc MPI_hello_world.c -o MPI_hello_world
#Run
mpirun  ./MPI_hello_world
