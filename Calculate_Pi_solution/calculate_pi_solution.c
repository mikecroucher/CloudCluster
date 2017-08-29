#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

int main(int argc, char **argv)
{
  int n, i;
  double mypi, w, sum, x;
  int size, rank;
  double partpi;

  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

/* Take the number of intervals from the command line */
  if (argc!=2){
    if (rank == 0)
      printf("You must supply one command line argument, specifying the number of intervals\n");
    MPI_Finalize();
    exit(1);
  }
  n = atoi(argv[1]);

/* Calculate the width of an interval */
  w = 1.0/(double)n;

  sum = 0.0;
  for(i=rank; i<n; i+=size){
    x = ((double)i+0.5)*w;
    sum += 4.0/(1.0+x*x);
  }
  partpi = w*sum;

  MPI_Reduce(&partpi, &mypi, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);

  if (rank == 0)
    printf("PI is approx %.16f\n", mypi);

  MPI_Finalize();
  return 0;
}
