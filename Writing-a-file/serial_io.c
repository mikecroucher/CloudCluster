/* This is a parallel code which performs its I/O serially. */

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char *argv[])
{
  const int array_size=1000000;
  const int data_tag=21;
  int rank, size, i, j;
  char filename[80];
  double data1[array_size];
  double *data2;
  MPI_Status status;
  double start, end;
  FILE *fh;

  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

/* Initialise data1 array to different values on each process. */
  for (i=0; i<array_size; i++)
     data1[i] = (double)rank;

  strcpy(filename,"test_out_file_serial");
  if(rank==0)
     fh=fopen(filename,"wb");

/* Each process sends its data1 array to process 0, 
which writes this data to a file.*/
  MPI_Barrier(MPI_COMM_WORLD);
  start = MPI_Wtime();
  if(rank==0){
  /* Allocate the memory for the data2 array on process 0 */
     data2 = calloc(array_size, sizeof(double));
     for (i=0; i<size; i++){
        if(i==rank){
 	   for (j=0; j<array_size; j++)
 	      data2[j] = data1[j];
        }
        else{
           MPI_Recv(data2,array_size,MPI_DOUBLE,i,data_tag,MPI_COMM_WORLD,&status);
	}
        fwrite(data2,sizeof(double),array_size,fh);
     }
  }
  else{
     MPI_Send(data1,array_size,MPI_DOUBLE,0,data_tag,MPI_COMM_WORLD);
  }
  MPI_Barrier(MPI_COMM_WORLD);
  end = MPI_Wtime();

  if(rank==0){
     fclose(fh);
     printf("Time taken was %g seconds.\n", end-start);
  }
  MPI_Finalize();
  return 0;
}
