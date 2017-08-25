#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char ** argv){
  int size, rank, i, j;
  const int NUMBER_OF_ITERATIONS=10000;
  int data_size;
  MPI_Status status;
  float *data;
  double resolution, time1, time2;

  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  if (rank == 0){
     resolution = MPI_Wtick();
     printf("The resolution of the MPI timer is %g seconds.\n", resolution);
  }

  data_size = 1;
  for (i = 1; i <= 7; i++){
     data = calloc(data_size, sizeof(float));
     for (j = 0; j < data_size; j++)
	data[j] = (float)rank;
/*     printf ("Before: rank=%d; data=%g\n", rank, data[0]);*/
     time1 = MPI_Wtime();
     for (j = 1; j <= NUMBER_OF_ITERATIONS; j++){
	if (rank == 0){
           MPI_Ssend(data, data_size, MPI_FLOAT, 1, 0, MPI_COMM_WORLD);
           MPI_Recv(data, data_size, MPI_FLOAT, 1, 0, MPI_COMM_WORLD, &status);
/*          printf ("After: rank=%d; data=%g\n", rank, data[0]);*/
	} else if (rank == 1){
           MPI_Recv(data, data_size, MPI_FLOAT, 0, 0, MPI_COMM_WORLD, &status);
	   MPI_Ssend(data, data_size, MPI_FLOAT, 0, 0, MPI_COMM_WORLD);
/*          printf ("After: rank=%d; data=%g\n", rank, data[0]);*/
        }
     }
     time2 = MPI_Wtime();
     if (rank == 0)
	 printf("Time taken to send %d bytes: %g seconds.\n", data_size*4, (time2-time1)/(double)(2.0*(float)NUMBER_OF_ITERATIONS));
     free(data);
     data_size = data_size * 10;
  }

  MPI_Finalize();
}
