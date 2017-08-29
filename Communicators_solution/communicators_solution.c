#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
  int rank, size;
  MPI_Comm comm, new_comm;
  int colour, key;
  int sum_ranks, new_rank;

  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

/* even ranked processes will supply a colour of 0 and odd ranked, 1 */
  colour = rank%2; 

/* we have no need to fix the ranks in the new communicator */
  key = 0; 

/* the communicator we are splitting */
  comm = MPI_COMM_WORLD; 

/* split the communicator */
  MPI_Comm_split(comm, colour, key, &new_comm);

/* find the rank of the process in the new communicator */
  MPI_Comm_rank(new_comm, &new_rank);

/* print the ranks of each process in the original and new communicators */
  printf("original rank = %d; new rank = %d\n", rank, new_rank);

/* note that the result is being put on rank 0 in the new communicator */
  MPI_Reduce(&rank, &sum_ranks, 1, MPI_INT, MPI_SUM, 0, new_comm);

/* rank 0 in both the odd and even new communicators prints out the result */
  if (new_rank == 0)
    printf("Sum of ranks=%d\n", sum_ranks);

  MPI_Finalize();
}
