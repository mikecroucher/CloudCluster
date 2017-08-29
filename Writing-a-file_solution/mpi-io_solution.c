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

  MPI_File_open(MPI_COMM_WORLD, filename, MPI_MODE_WRONLY|MPI_MODE_CREATE,
		MPI_INFO_NULL, &fh);
  offset = rank * array_size * sizeof(double);
  MPI_File_set_view(fh, offset, MPI_DOUBLE, MPI_DOUBLE, "native", MPI_INFO_NULL);

  MPI_Barrier(MPI_COMM_WORLD);
  start = MPI_Wtime();
  MPI_File_write_all(fh, data1, array_size, MPI_DOUBLE, &status);
  MPI_Barrier(MPI_COMM_WORLD);
  end = MPI_Wtime();

  MPI_File_close(&fh);
  if(rank==0){
     printf("Time taken was %g seconds.\n", end-start);
  }
  MPI_Finalize();
  return 0;
}
