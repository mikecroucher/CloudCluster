PROGRAM PingPong
  USE MPI
  IMPLICIT NONE
  INTEGER :: ierr, rank, size, i, j
  INTEGER, PARAMETER :: NUMBER_OF_ITERATIONS=10000
  INTEGER :: data_size
  INTEGER, DIMENSION(MPI_STATUS_SIZE) :: status
  REAL, DIMENSION(:), ALLOCATABLE :: data
  DOUBLE PRECISION :: resolution, time1, time2

  CALL MPI_INIT(ierr)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)

  IF (rank .EQ. 0) THEN 
     resolution = MPI_WTICK()
     print *, 'The resolution of the MPI timer is ', resolution, ' seconds.'
  END IF

  data_size = 1
  DO i = 1, 7
     ALLOCATE(data(data_size))
     data = rank*1.0
!    print *, 'Before: rank=',rank,'; data=',data
     time1 = MPI_WTIME()
     DO j = 1, NUMBER_OF_ITERATIONS
        IF (rank .EQ. 0) THEN
           CALL MPI_SSEND(data(1), data_size, MPI_REAL, 1, 0, MPI_COMM_WORLD, ierr)
           CALL MPI_RECV(data(1), data_size, MPI_REAL, 1, 0, MPI_COMM_WORLD, status, ierr)
!          print *, 'After: rank=',rank,'; data=',data
        ELSE IF (rank .EQ. 1) THEN
           CALL MPI_RECV(data(1), data_size, MPI_REAL, 0, 0, MPI_COMM_WORLD, status, ierr)
           CALL MPI_SSEND(data(1), data_size, MPI_REAL, 0, 0, MPI_COMM_WORLD, ierr)
!          print *, 'After: rank=',rank,'; data=',data
        END IF
     END DO
     time2 = MPI_WTIME()
     IF (rank .EQ. 0) THEN
        print *, 'Time taken to send ', data_size*4, 'bytes: ', (time2-time1)/(2.0*NUMBER_OF_ITERATIONS), 'seconds.'
     END IF
     DEALLOCATE(data)
     data_size = data_size * 10
  END DO

  CALL MPI_FINALIZE(ierr)
END PROGRAM PingPong
