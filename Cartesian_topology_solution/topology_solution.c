/**********************************************************************

  This code should use 6 processes. Each process will initialize
  an integer array of length data_size. A call to init_data will
  put values in this array based on the rank. If the ranks are
  distributed along the rows of a 2-by-3 topology in numerical
  order, with data_size =4, the data will be as follows:

    1   1   3   3       2   2   4   4       0   0   5   5


    4   4   0   0       5   5   1   1       3   3   2   2

  This is intended to represent the 6 processes of the topology and
  is acheived with the show_topo subroutine.

  The task is to create a topology and with two shifts send from "data"
  to "data2" so that we have the following (given the data above):

    0   0   0   0       1   1   1   1       2   2   2   2


    3   3   3   3       4   4   4   4       5   5   5   5
*********************************************************************/

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void init_data( int *data, int rank, int data_size, 
		int num_cols, int num_rows );

void show_top( char *string, int *data, int *alldata, 
               MPI_Comm comm, int rank, int data_size,
               int num_rows, int num_cols );

int main(int argc, char *argv[])
{
  int *data, *data2, *alldata;
  int data_size=4;
  char string[20];
  int ierror, rank, size, half_size;
  MPI_Status status;

/* Variables for cartesian topology routines */

  int *dims, *coords, *periods;
  int ndims;
  MPI_Comm comm_old, comm_cart;
  int src, dest;
  int num_cols, num_rows;
  int reorder;

  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

/* Ensure that we have 6 processes */

  if (size != 6){
    if (rank == 0)
      printf("You have requested %d processes. This code should only be run on 6.\n", size);
    MPI_Finalize();
    exit(1);
  }

  ndims = 2; /* 2D topology */
  num_rows = 2;
  num_cols = 3;

/* Allocate data and data2. If rank = 0 allocate alldata.
   data will store the initial data and each process will
   receive data into data2 */

  data = calloc(data_size, sizeof(int));
  data2 = calloc(data_size, sizeof(int));
  if ( rank == 0 )
    alldata = calloc(data_size*num_rows*num_cols, sizeof(int));

/* 1. Allocate the arrays dims, periods and coords for a 2D topology 
      and then initialise dims, periods and reorder to specify 
      2 rows and 3 columns, with periodicity and without reordering. */

  dims = calloc(ndims, sizeof(int));
  periods = calloc(ndims, sizeof(int));
  coords = calloc(ndims, sizeof(int));

  dims[0] = num_rows, dims[1] = num_cols;
  periods[0]=1, periods[1]=1;
  reorder = 0;

/* Initialize data */

  init_data ( data, rank, data_size, num_cols, num_rows );
  half_size = data_size/2;
  comm_old = MPI_COMM_WORLD;

/* Show initial data */

  strcpy(string,"BEFORE:");
  show_top( string, data, alldata, comm_old, rank, data_size, 
            num_rows, num_cols );

/* 2. Create the topology */

  MPI_Cart_create( comm_old, ndims, dims, periods, reorder, &comm_cart );

/* 3. Get the coordinates for each process and print them out */

  MPI_Cart_coords( comm_cart, rank, ndims, coords );
  printf("Rank: %d, Coords: (%d,%d)\n", rank, coords[0], coords[1]);

/* 4. Send the first "half_size" elements along the rows from left to right */

  MPI_Cart_shift( comm_cart, 1, 1, &src, &dest );

  MPI_Sendrecv( data, half_size, MPI_INT, dest, 99,
                data2, half_size, MPI_INT, src, 99,
		comm_old, &status );

/* 5. Send the second "half_size" elements down the columns */

  MPI_Cart_shift( comm_cart, 0, 1, &src, &dest );

  MPI_Sendrecv( &data[half_size], half_size, MPI_INT, dest, 99,
                &data2[half_size], half_size, MPI_INT, src, 99,
		comm_old, &status );

/* Show final data */

  strcpy(string,"AFTER:");
  show_top( string, data2, alldata, comm_old, rank, data_size,
            num_rows, num_cols );

  MPI_Finalize();
}


  void init_data( int *data, int rank, int data_size, 
                  int num_cols, int num_rows )
{
/* Initialize data. 
   Requires ranks to be distributed in row-major order on topology. */

  int half_size, i;

  half_size = data_size/2;
  for (i=0; i<half_size; i++)
    data[i] = rank+1;

  if( (rank+1)%num_cols == 0 ){
    for (i=0; i<half_size; i++)
      data[i] = data[i] - 3;
  }

  for (i=half_size; i<=data_size; i++)
    data[i] = (rank + 3)%(num_cols*num_rows);
  return;
}


void show_top( char *string, int *data, int *alldata, 
               MPI_Comm comm, int rank, int data_size,
               int num_rows, int num_cols )
{
/* Print out all data in a form that represents the topology */

  int ierror, i, j, k;
  char des[8], temp;

  /*  WRITE(temp,'(I1)')data_size
      des = '(' // temp // 'I4,4X)'*/

  MPI_Gather( data, data_size, MPI_INT, alldata,
    	            data_size, MPI_INT, 0, comm );

  if( rank == 0 ) {
    printf("\n%s\n", string);
    for (i=0; i<num_rows; i++){
      for (j=i*num_cols; j<(i+1)*num_cols; j++){
        for (k=j*data_size; k<(j+1)*data_size; k++)
          printf("%4d", alldata[k]);
	printf("   ");
      }
      printf("\n\n\n");
    } 
  }

return;
}
