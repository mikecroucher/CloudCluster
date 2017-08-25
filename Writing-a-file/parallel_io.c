/* This is a parallel code where each process 
   writes its data to its own output file. */

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char *argv[])
{
  const int array_size=1000000;
  int rank, size, i;
  char filesuffix[80], filename[80];
  double data1[array_size];
  double start, end;
  FILE *fh;

  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

/* Initialise data1 array to different values on each process. */
  for (i=0; i<array_size; i++)
     data1[i] = (double)rank;

/* Give each process a unique filename to write to. */
  if(rank < 10){
     strcpy(filesuffix,"test_out_file00");
  }
  else if(rank<100){
     strcpy(filesuffix,"test_out_file0");
  }
  else{
     strcpy(filesuffix,"test_out_file");
  }
  sprintf(filename, "%s%d", filesuffix, rank);
  fh=fopen(filename,"wb");
  
/* Each process writes its data out to its own file. */
  MPI_Barrier(MPI_COMM_WORLD);
  start = MPI_Wtime();
  fwrite(data1,sizeof(double),array_size,fh);
  MPI_Barrier(MPI_COMM_WORLD);
  end = MPI_Wtime();

  fclose(fh);
  if(rank==0){
     printf("Time taken was %g seconds.\n", end-start);
  }
  MPI_Finalize();
  return 0;
}
