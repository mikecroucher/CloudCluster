PROGRAM calculate_pi
  USE MPI
  IMPLICIT NONE
  INTEGER :: n, i
  CHARACTER*80 :: input
  DOUBLE PRECISION :: mypi, w, sum, x
  INTEGER :: rank, size, ierr
  DOUBLE PRECISION :: partpi

  CALL MPI_INIT(ierr)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)

! Take the number of intervals from the command line
  IF (command_argument_count() /= 1) THEN
    IF (rank == 0) print *, 'You must supply one command line argument, specifying the number of intervals'
    CALL MPI_FINALIZE(ierr)
    STOP
  END IF
  CALL get_command_argument(1,input)
  READ(input, FMT='(I20)') n

! Calculate the width of an interval
  w = 1.0D0/DBLE(n)

  sum = 0.0D0
  DO i=rank, n-1, size
    x = (DBLE(i)+0.5D0)*w
    sum = sum + 4.0D0/(1.0D0+x*x)
  END DO
  partpi = w*sum

  CALL MPI_REDUCE(partpi, mypi, 1, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, ierr);

  IF (rank == 0) print *, 'PI is approx ', mypi

  CALL MPI_FINALIZE(ierr)
END PROGRAM calculate_pi
