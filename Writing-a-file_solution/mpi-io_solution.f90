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

  CALL mpi_file_open(MPI_COMM_WORLD,filename,MPI_MODE_WRONLY+MPI_MODE_CREATE,&
       &MPI_INFO_NULL,filehandle,ierror)
  CALL mpi_type_size(MPI_DOUBLE_PRECISION,double_size,ierror)
  offset=rank*array_size*double_size
  CALL mpi_file_set_view(filehandle, offset , MPI_DOUBLE_PRECISION,&
       &MPI_DOUBLE_PRECISION, 'native', MPI_INFO_NULL, ierror)

  CALL mpi_barrier(MPI_COMM_WORLD,ierror)
  start = mpi_wtime()
  CALL mpi_file_write_all(filehandle, data1, array_size, MPI_DOUBLE_PRECISION,&
       &status, ierror)
  CALL mpi_barrier(MPI_COMM_WORLD,ierror)
  end = mpi_wtime()

  CALL mpi_file_close(filehandle,ierror)
  IF(rank==0)THEN
     WRITE(6,'("Time taken was ",F10.6)') end-start
  END IF
  CALL mpi_finalize(ierror)

END PROGRAM mpi_io_prog
