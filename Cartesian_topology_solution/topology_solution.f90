PROGRAM MPI_TOPO

! This code should use 6 processes. Each process will initialize
! an integer array of length data_size. A call to init_data will
! put values in this array based on the rank. If the ranks are
! distributed along the rows of a 2-by-3 topology in numerical
! order, with data_size =4, the data will be as follows:
!
!   1   1   3   3       2   2   4   4       0   0   5   5
!
!
!   4   4   0   0       5   5   1   1       3   3   2   2
!
! This is intended to represent the 6 processes of the topology and
! is acheived with the show_topo subroutine.
!
! The task is to create a topology and with two shifts send from "data"
! to "data2" so that we have the following (given the data above):
!
!   0   0   0   0       1   1   1   1       2   2   2   2
!
!
!   3   3   3   3       4   4   4   4       5   5   5   5
!

  USE MPI
  IMPLICIT NONE

  INTEGER, DIMENSION(:), ALLOCATABLE :: data, data2, alldata
  INTEGER, PARAMETER :: data_size = 4 ! must be 2, 4, 6 or 8, and...
  CHARACTER (LEN=20) :: string
  INTEGER, DIMENSION(MPI_STATUS_SIZE) :: status
  INTEGER :: ierror, rank, size, allocerr, half_size

! Variables for cartesian topology routines

  INTEGER, DIMENSION(:), ALLOCATABLE :: dims, coords
  LOGICAL, DIMENSION(:), ALLOCATABLE :: periods
  INTEGER :: ndims 
  INTEGER :: comm_old, comm_cart, src, dest
  INTEGER :: num_cols, num_rows
  LOGICAL :: reorder

  CALL MPI_INIT(ierror)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierror)

! Ensure that we have 6 processes

  IF (size /= 6) THEN
    IF (rank == 0) print *, 'You have requested ', size,' processes. This code should only be run on 6.'
    CALL MPI_FINALIZE(ierror)
    STOP
  END IF

  ndims = 2 ! 2D topology
  num_rows = 2
  num_cols = 3

! Allocate data and data2. If rank = 0 allocate alldata.
! data will store the initial data and each process will
! receive data into data2

  ALLOCATE ( data(data_size), STAT=allocerr )
  ALLOCATE ( data2(data_size), STAT=allocerr )
  IF( rank == 0 ) &
    ALLOCATE ( alldata(data_size*num_rows*num_cols), STAT=allocerr )

! 1. Allocate the arrays dims, periods and coords for a 2D topology 
!    and then initialise dims, periods and reorder to specify 
!    2 rows and 3 columns, with periodicity and without reordering.

  ALLOCATE ( dims(ndims), STAT=allocerr )
  ALLOCATE ( periods(ndims), STAT=allocerr )
  ALLOCATE ( coords(ndims), STAT=allocerr )

  dims(1) = num_rows
  dims(2) = num_cols
  periods = (/ .TRUE., .TRUE. /)
  reorder = .FALSE.

! Initialize data

  CALL init_data ( data, rank, data_size, num_cols, num_rows )
  half_size = data_size/2
  comm_old = MPI_COMM_WORLD

! Show initial data

  string = 'BEFORE:'
  CALL show_top( string, data, alldata, comm_old, rank, data_size )

! 2. Create the topology

  CALL MPI_CART_CREATE( comm_old, ndims, dims, periods, reorder, &
                        comm_cart, ierror )

! 3. Get the coordinates for each process and print them out

  CALL MPI_CART_COORDS( comm_cart, rank, ndims, coords, ierror )
  WRITE(*,'("Rank:",I2,", Coords: (",I1,",",I1,")")') rank, coords

! 4. Send the first "half_size" elements along the rows from left to right

  CALL MPI_CART_SHIFT( comm_cart, 1, 1, src, dest, ierror )

  CALL MPI_SENDRECV( data(1), half_size, MPI_INTEGER, dest, 99, &
                     data2(1), half_size, MPI_INTEGER, src, 99, &
                     comm_old, status, ierror )

! 5. Send the second "half_size" elements down the columns

  CALL MPI_CART_SHIFT( comm_cart, 0, 1, src, dest, ierror )

  CALL MPI_SENDRECV( data(half_size+1), half_size, MPI_INTEGER, dest, 99, &
                     data2(half_size+1), half_size, MPI_INTEGER, src, 99, &
                     comm_old, status, ierror )

! Show final data

  string = 'AFTER:'
  CALL show_top( string, data2, alldata, comm_old, rank, data_size )


  CALL MPI_FINALIZE( ierror )


CONTAINS


  SUBROUTINE init_data( data, rank, data_size, num_cols, num_rows )

! Initialize data. 
! Requires ranks to be distributed in row-major order on topology.

  IMPLICIT NONE
  INTEGER, DIMENSION(:) :: data
  INTEGER :: rank, num_cols, num_rows, data_size, half_size

  half_size = data_size/2
  data(1:half_size) = rank+1

  IF( MOD( rank+1,num_cols) == 0 ) &
    data(1:half_size) = data(1:half_size) - 3

  data( half_size+1:data_size ) = MOD( rank + 3, num_cols*num_rows )

  END SUBROUTINE


  SUBROUTINE show_top( string, data, alldata, comm, rank, data_size )

! Print out all data in a form that represents the topology

  IMPLICIT NONE
  INTEGER, DIMENSION(:) :: data, alldata
  INTEGER :: rank, data_size, comm, ierror, i, j
  CHARACTER(LEN=8):: des
  CHARACTER(LEN=20) :: string
  CHARACTER :: temp

  WRITE(temp,'(I1)')data_size
  des = '(' // temp // 'I4,4X)'

  CALL MPI_GATHER( data, data_size, MPI_INTEGER, alldata, &
                   data_size, MPI_INTEGER, 0, comm, ierror)

  IF( rank == 0 ) THEN
    WRITE(*,*)
    WRITE(*,*) string
    WRITE(*,*)
    DO i = 0, num_rows-1
      DO j = i*num_cols, (i+1)*num_cols-1
        WRITE(*,FMT=des,ADVANCE = 'No')  &
                        alldata(j*data_size+1:(j+1)*data_size)
      END DO
      WRITE(*,*)
      WRITE(*,*)
      WRITE(*,*)
    END DO
  END IF

  END SUBROUTINE

END PROGRAM

