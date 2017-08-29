PROGRAM file_read
  USE mpi
  IMPLICIT NONE

  INTEGER, PARAMETER :: wp=KIND(0.0D0)
  INTEGER :: array_size
  INTEGER :: ierror
  CHARACTER(len=80) :: filename
  REAL(wp), DIMENSION(:), ALLOCATABLE :: data
  INTEGER, DIMENSION(MPI_STATUS_SIZE) :: status
  INTEGER :: rank, size, count
  INTEGER :: double_size
  INTEGER :: filehandle
  INTEGER(kind=MPI_OFFSET_KIND) :: filesize, offset

  CALL mpi_init(ierror)
  CALL mpi_comm_rank(MPI_COMM_WORLD,rank,ierror)
  CALL mpi_comm_size(MPI_COMM_WORLD,size,ierror)

!  Open the file that was written in the first part of the exericse.
  WRITE(filename,'("test_out_file_mpi_io")')
  CALL mpi_file_open(MPI_COMM_WORLD,filename,MPI_MODE_RDONLY,&
       &MPI_INFO_NULL,filehandle,ierror)

! Find out the size of the file in bytes.
  CALL mpi_file_get_size(filehandle, filesize, ierror)

! Convert this file size to be in terms of double precision numbers. 
  CALL mpi_type_size(MPI_DOUBLE_PRECISION,double_size,ierror)
  filesize = filesize / double_size

! Set the size of the local arrays to read the data into.
  array_size = filesize/size + 1
  ALLOCATE(data(array_size))

  offset=rank*array_size*double_size
  CALL mpi_file_set_view(filehandle, offset , MPI_DOUBLE_PRECISION,&
       &MPI_DOUBLE_PRECISION, 'native', MPI_INFO_NULL, ierror)

  CALL mpi_file_read_all(filehandle, data, array_size, MPI_DOUBLE_PRECISION,&
       &status, ierror)
  CALL mpi_get_count(status, MPI_DOUBLE_PRECISION, count, ierror)

  CALL mpi_file_close(filehandle,ierror)

  WRITE(6,'("Rank ",I2, " reads in ", I8, " double precision numbers and the value of the first element is ", F5.1)')& 
  &rank, count, data(1)

  DEALLOCATE(data)
  CALL mpi_finalize(ierror)

END PROGRAM file_read
