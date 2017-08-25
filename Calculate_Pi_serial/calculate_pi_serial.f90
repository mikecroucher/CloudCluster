PROGRAM calculate_pi
  IMPLICIT NONE
  INTEGER :: n, i
  CHARACTER*80 :: input
  DOUBLE PRECISION :: mypi, w, sum, x

! Take the number of intervals from the command line
  IF (command_argument_count() /= 1) THEN
    print *, 'You must supply one command line argument, specifying the number of intervals'
    STOP
  END IF
  CALL get_command_argument(1,input)
  READ(input, FMT='(I20)') n

! Calculate the width of an interval
  w = 1.0D0/DBLE(n)

  sum = 0.0D0
  DO i=0, n-1
    x = (DBLE(i)+0.5D0)*w
    sum = sum + 4.0D0/(1.0D0+x*x)
  END DO
  mypi = w*sum

  print *, 'PI is approx ', mypi
END PROGRAM calculate_pi
