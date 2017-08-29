PROGRAM COMMUNICATORS

  USE MPI
  IMPLICIT NONE

  INTEGER :: rank, size, ierror
  INTEGER :: comm, new_comm
  INTEGER :: colour, key
  INTEGER :: sum_ranks, newrank

  CALL MPI_INIT(ierror)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierror)

! even ranked processes will supply a colour of 0 and odd ranked, 1
  colour = MOD(rank,2)

! we have no need to fix the ranks in the new communicator
  key = 0

! the communicator we are splitting
  comm = MPI_COMM_WORLD

! split the communicator
  CALL MPI_COMM_SPLIT(comm, colour, key, new_comm, ierror)

! find the rank of the process in the new communicator
  CALL MPI_COMM_RANK(new_comm, newrank, ierror)

! print the ranks of each process in the original and new communicators
  WRITE(*,*) 'original rank = ',rank,'; new rank = ',newrank

! note that the result is being put on rank 0 in the new communicator
  CALL MPI_REDUCE(rank, sum_ranks, 1, MPI_INTEGER, MPI_SUM, 0, new_comm, ierror)

! rank 0 in both the odd and even new communicators prints out the result
  IF (newrank == 0) THEN
    WRITE(*,*) 'Sum of ranks=', sum_ranks
  ENDIF

  CALL MPI_FINALIZE(ierror)

END PROGRAM

