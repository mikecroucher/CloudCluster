PROGRAM Hello_World
  USE MPI
  IMPLICIT NONE
  INTEGER :: ierr, rank, size, rank_self, size_self

  CALL MPI_INIT(ierr)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)

! Each process prints its own rank
  print '(a10,i0.1)', "My rank = ", rank

! One process prints out the total number of processes
  IF (rank == 0) print '(a33,i0.1)', "The total number of processes is ", size

! Find the rank and the size of the group associated with the communicator MPI_COMM_SELF
  CALL MPI_COMM_RANK(MPI_COMM_SELF, rank_self, ierr)
  CALL MPI_COMM_SIZE(MPI_COMM_SELF, size_self, ierr)

! Print these out, alongside the ranks in MPI_COMM_WORLD
  print '(a26,i0.1,a27,i0.1,a27,i0.1)', "rank in MPI_COMM_WORLD is ", rank, &
       "; rank in MPI_COMM_SELF is ", rank_self, &
       "; size of MPI_COMM_SELF is ", size_self

  CALL MPI_FINALIZE(ierr)
END PROGRAM Hello_World
