! This is an incomplete code to be completed so 
! that the data is written out using MPI-IO. 

PROGRAM mpi_io_prog
  USE mpi
  IMPLICIT NONE

  INTEGER, PARAMETER :: wp=KIND(0.0D0)
  INTEGER, PARAMETER :: array_size=1000000
  INTEGER :: ierror
  CHARACTER(len=80) :: filename
  REAL(wp), DIMENSION(array_size) :: data1
  INTEGER, DIMENSION(MPI_STATUS_SIZE) :: status
  INTEGER :: rank, size
  INTEGER :: double_size
  INTEGER :: filehandle
  INTEGER(kind=MPI_OFFSET_KIND) :: offset
  REAL(wp) :: start, end

  CALL mpi_init(ierror)
  CALL mpi_comm_rank(MPI_COMM_WORLD,rank,ierror)
  CALL mpi_comm_size(MPI_COMM_WORLD,size,ierror)

! Initialise data1 array to different values on each process.
  data1 = REAL(rank,wp)

  WRITE(filename,'("test_out_file_mpi_io")')


! Insert your MPI calls here.


  IF(rank==0)THEN
     WRITE(6,'("Time taken was ",F10.6)') end-start
  END IF
  CALL mpi_finalize(ierror)

END PROGRAM mpi_io_prog
