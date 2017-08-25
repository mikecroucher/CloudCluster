! This is a parallel code where each process 
! writes its data to its own output file.

PROGRAM parallel_io
  USE mpi
  IMPLICIT NONE

  INTEGER, PARAMETER :: wp=KIND(0.0D0)
  INTEGER, PARAMETER :: array_size=1000000
  INTEGER :: ierror
  CHARACTER(len=80) :: filename
  REAL(wp), DIMENSION(array_size) :: data1
  INTEGER :: rank, size
  REAL(wp) :: start, end

  CALL mpi_init(ierror)
  CALL mpi_comm_rank(MPI_COMM_WORLD,rank,ierror)
  CALL mpi_comm_size(MPI_COMM_WORLD,size,ierror)

! Initialise data1 array to different values on each process.
  data1 = REAL(rank,wp)

! Give each process a unique filename to write to.
  IF(rank<10)THEN
     WRITE(filename,'("test_out_file00",I1)') rank
  ELSE IF(rank<100) THEN
     WRITE(filename,'("test_out_file0",I2)') rank
  ELSE
     WRITE(filename,'("test_out_file",I3)') rank
  END IF
  OPEN(unit=17,file=filename,form='unformatted')

! Each process writes its data out to its own file.
  CALL mpi_barrier(MPI_COMM_WORLD,ierror)
  start = mpi_wtime()
  WRITE(17) data1
  CALL mpi_barrier(MPI_COMM_WORLD,ierror)
  end = mpi_wtime()

  CLOSE(17)
  IF(rank==0)THEN
     WRITE(6,'("Time taken was ",F10.6)') end-start
  END IF
  CALL mpi_finalize(ierror)

END PROGRAM parallel_io
