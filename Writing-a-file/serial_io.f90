! This is a parallel code which performs its I/O serially.

PROGRAM serial_io
  USE mpi
  IMPLICIT NONE

  INTEGER, PARAMETER :: wp=KIND(0.0D0)
  INTEGER, PARAMETER :: array_size=1000000
  INTEGER, PARAMETER :: data_tag=21
  INTEGER :: ierror
  CHARACTER(len=80) :: filename
  REAL(wp), DIMENSION(array_size) :: data1
  REAL(wp), DIMENSION(:), ALLOCATABLE :: data2
  INTEGER, DIMENSION(MPI_STATUS_SIZE) :: status
  INTEGER :: rank, size
  INTEGER :: i, allocerr
  REAL(wp) :: start, end

  CALL mpi_init(ierror)
  CALL mpi_comm_rank(MPI_COMM_WORLD,rank,ierror)
  CALL mpi_comm_size(MPI_COMM_WORLD,size,ierror)

! Initialise data1 array to different values on each process.
  data1 = REAL(rank,wp)

  WRITE(filename,'("test_out_file_serial")')
  IF(rank==0) THEN
     OPEN(unit=17,file=filename,form='unformatted')
  END IF

! Each process sends its data1 array to process 0, 
! which writes this data to a file.
  CALL mpi_barrier(MPI_COMM_WORLD,ierror)
  start = mpi_wtime()
  IF(rank==0) THEN
! Allocate the memory for the data2 array on process 0
     ALLOCATE(data2(array_size), STAT=allocerr)
     DO i=0,size-1
        IF(i==rank) THEN
           data2=data1
        ELSE
           CALL mpi_recv(data2,array_size,MPI_DOUBLE_PRECISION,&
                &i,data_tag,MPI_COMM_WORLD,status,ierror)
        END IF
        WRITE(17) data2
     END DO
  ELSE
     CALL mpi_send(data1,array_size,MPI_DOUBLE_PRECISION,&
                &0,data_tag,MPI_COMM_WORLD,ierror)
  END IF
  CALL mpi_barrier(MPI_COMM_WORLD,ierror)
  end = mpi_wtime()

  IF(rank==0) THEN
     CLOSE(17)
     WRITE(6,'("Time taken was ",F10.6)') end-start
  END IF
  CALL mpi_finalize(ierror)

END PROGRAM serial_io
