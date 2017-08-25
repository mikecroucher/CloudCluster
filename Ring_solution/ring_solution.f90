PROGRAM Ring
  USE MPI
  IMPLICIT NONE
  INTEGER :: ierr, rank, size
  INTEGER :: next_proc, prev_proc
  INTEGER :: value_to_send, recvd_value, sum
  INTEGER, DIMENSION(MPI_STATUS_SIZE) :: status

  CALL MPI_INIT(ierr)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)

! Calculate the ranks of the next and previous processes in the ring
  next_proc = MOD(rank+1, size)
  prev_proc = MOD(rank-1+size, size)

! Initialise value_to_send to my own rank, the sum to zero
! and recvd_value to a value that won't initially equal the rank of any process
  value_to_send = rank
  sum = 0
  recvd_value = -1

  DO WHILE (recvd_value /= rank)
    CALL MPI_SENDRECV(value_to_send, 1, MPI_INTEGER, next_proc, 0, &
                      recvd_value, 1, MPI_INTEGER, prev_proc, 0, MPI_COMM_WORLD, status, ierr)
    sum = sum + recvd_value
    value_to_send = recvd_value
  END DO

  PRINT *, 'Rank=',rank,'; sum=',sum

  CALL MPI_FINALIZE(ierr)
END PROGRAM Ring
