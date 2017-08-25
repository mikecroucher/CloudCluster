#include <math.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv)
{
  int n, i;
  double mypi, w, sum, x;

/* Take the number of intervals from the command line */
  if (argc!=2){
    printf("You must supply one command line argument, specifying the number of intervals\n");
    exit(1);
  }
  n = atoi(argv[1]);

/* Calculate the width of an interval */
  w = 1.0/(double)n;

  sum = 0.0;
  for(i=0; i<n; i++){
    x = ((double)i+0.5)*w;
    sum += 4.0/(1.0+x*x);
  }
  mypi = w*sum;
  printf("PI is approx %.16f\n", mypi);

  return 0;
}
