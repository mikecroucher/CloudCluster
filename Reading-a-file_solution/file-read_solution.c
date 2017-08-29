#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char *argv[])
{
  int array_size;
  const int data_tag=21;
  int rank, size, count;
  char filename[80];
  double *data;
  MPI_Status status;
  MPI_Offset filesize, offset;
  MPI_File fh;

  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

/* Open the file that was written in the first part of the exericse. */
  strcpy(filename,"test_out_file_mpi_io");
  MPI_File_open(MPI_COMM_WORLD, filename, MPI_MODE_RDONLY, MPI_INFO_NULL, &fh);

/* Find out the size of the file in bytes. */
  MPI_File_get_size(fh, &filesize);

/* Convert this file size to be in terms of doubles. */
  filesize = filesize / sizeof(double);

/* Set the size of the local arrays to read the data into. */
  array_size = filesize/size + 1; 
  data = (double *) malloc (array_size*sizeof(double));

  offset = rank * array_size * sizeof(double);
  MPI_File_set_view(fh, offset, MPI_DOUBLE, MPI_DOUBLE, "native", MPI_INFO_NULL);

  MPI_File_read_all(fh, data, array_size, MPI_DOUBLE, &status);
  MPI_Get_count(&status, MPI_DOUBLE, &count);

  MPI_File_close(&fh);

  printf("Rank %d reads in %d doubles and the value of the first element is %g\n", 
          rank, count, data[0]);

  free(data);
  MPI_Finalize();
  return 0;
}
