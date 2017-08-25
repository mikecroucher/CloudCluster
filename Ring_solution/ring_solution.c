#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char ** argv){
  int size, rank;
  int next_proc, prev_proc;
  int value_to_send, recvd_value, sum;
  MPI_Status status;

  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

/* Calculate the ranks of the next and previous processes in the ring */
  next_proc = (rank+1)%size;
  prev_proc = (rank-1+size)%size;

/* Initialise value_to_send to my own rank, the sum to zero
   and recvd_value to a value that won't initially equal the rank of any process */
  value_to_send = rank;
  sum = 0;
  recvd_value = -1;

  while (recvd_value != rank){
    MPI_Sendrecv(&value_to_send, 1, MPI_INT, next_proc, 0, 
		 &recvd_value, 1, MPI_INT, prev_proc, 0, MPI_COMM_WORLD, &status);
    sum += recvd_value;
    value_to_send = recvd_value;
  }

  printf("Rank=%d; sum=%d\n", rank, sum);

  MPI_Finalize();
}
