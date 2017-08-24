#include <mpi.h>
#include <stdio.h>

int main(int argc, char ** argv){
  int size, rank, rank_self, size_self;

  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  /* Each process prints its own rank */
  printf("My rank = %d\n", rank);

  /* One process prints out the total number of processes */
  if (rank == 0)
    printf("The total number of processes is %d\n", size);

  /* Find the rank and the size of the group associated with the communicator MPI_COMM_SELF */
  MPI_Comm_rank(MPI_COMM_SELF, &rank_self);
  MPI_Comm_size(MPI_COMM_SELF, &size_self);

  /* Print these out, alongside the ranks in MPI_COMM_WORLD */
  printf("rank in MPI_COMM_WORLD is %d; rank in MPI_COMM_SELF is %d; size of MPI_COMM_SELF is %d\n", rank, rank_self, size_self);

  MPI_Finalize();
}
