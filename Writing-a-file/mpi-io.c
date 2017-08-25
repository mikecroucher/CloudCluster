/* This is an incomplete code to be completed so 
   that the data is written out using MPI-IO. */

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char *argv[])
{
  const int array_size=1000000;
  const int data_tag=21;
  int rank, size, i;
  char filename[80];
  double data1[array_size];
  double start, end;
  MPI_Status status;
  MPI_Offset offset;
  MPI_File fh;

  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

/* Initialise data1 array to different values on each process. */
  for (i=0; i<array_size; i++)
     data1[i] = (double)rank;

  strcpy(filename,"test_out_file_mpi_io");


/* Insert your MPI calls here. */


  if(rank==0){
     printf("Time taken was %g seconds.\n", end-start);
  }
  MPI_Finalize();
  return 0;
}
