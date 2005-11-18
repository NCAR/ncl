#include <stdio.h>
#include <string.h>
#include <ncarg/ngmath.h>

extern int maxmsg;

/*
 *  Comparison function for qsort to sort double precision triples.
 */
int comp_dtriples(const void *q1, const void *q2)
{
  double *p1, *p2;

  p1 = (double *) q1;
  p2 = (double *) q2;

  if (p1[0] < p2[0]) {
    return (-1);
  }
  else if (p1[0] > p2[0]) {
    return (1);
  }
  else {
    if (p1[1] < p2[1]) {
      return (-1);
    }
    else if (p1[1] > p2[1]) {
      return (1);
    }
    else {
      if (p1[2] != p2[2]) {
        printf("\n  Natgrid - two input triples have the same x/y coordinates");
        printf("\n            but different data values: \n\n");
        printf("                First triple:  %f %f %f\n",p1[0],p1[1],p1[2]);
        printf("                Second triple: %f %f %f\n",p2[0],p2[1],p2[2]);
        printf("\n\n");
        exit(1);
      }
    }
  }
}

/*
 *  This function culls duplicate double precision triples 
 *  from an array of "tnum" such in "data".  The triples
 *  in "data" are assumed to have been sorted into
 *  ascending order.  The number of distinct triples
 *  is returned and the distinct triples themselves
 *  are stored back in data.
 */ 
int cull_dtriples(int tnum, double *data) {
  int i, ic, icm1, kout=0, msgmx=0, fmsg=0;
  double *out,*xtmp; 

  ic   = 3*sizeof(double);
  icm1 = 2*sizeof(double);
  out  = (double *) malloc(tnum*ic);

/*
 *  Copy over the first data triple.
 */
  kout++;
  memcpy( (void *) out, (void *) data, ic);
  
/*
 *  Test for duplicate points and copy non-duplicates to out.  All
 *  full duplicates (coordinates plus data values) will be culled,
 *  but duplicate coordinates having different data values will 
 *  produce a fatal error.
 */
  for (i = 1; i < tnum; i++) {
/*
 *  Test for duplicate triples - do not copy them to "out".
 */
    if (memcmp((void *)(data+3*(i-1)), (void *)(data+3*i), ic) == 0) {
      if (msgmx >= maxmsg && fmsg == 0 && maxmsg > 0) {
        printf("Natgrid - maximum number of messages about duplicate input coordinates \n");
        printf("          has been reached - no more such messages will be issued\n");
        printf("          for this Natgrid call.\n");
        fmsg = 1;
      } 
      else if (msgmx < maxmsg && fmsg == 0) {
        printf("Natgrid info message - duplicate coordinate found, and culled,\n");
        printf("                       in the input data for coordinate (%f,%f)\n",
               data[3*(i-1)], data[3*(i-1)+1]);
        msgmx++;
      }
    }

/*
 *  In the case of Natgrid the test for duplicate coordinates with 
 *  different data values has already been done in the call to
 *  qsort using the compare function comp_triples.  If this were 
 *  not the case, the following commented lines would be appropriate 
 *  to uncomment.
 * 
 *  else if ( (memcmp((void *)(data+3*(i-1)), 
 *                    (void *)(data+3*i), 
 *                    2*sizeof(double);) == 0) &&
 *            (memcmp((void *)(data+3*(i-1)), 
 *                    (void *)(data+3*i), 
 *                    ic  ) != 0) ) {
 *      printf("Error: duplicate coordinates found in the input data, at \
 *              coordinate (%f,%f), having different data values.", \
 *              data[i*ic],data[i*ic+1]);
 *  }
 */

/*
 *  Copy non-duplicate value to output.
 */
    else {
        memcpy((void *)(out+3*kout), (void *)(data+3*i), ic);
        kout++;
    }
  }

/*  
 *  Copy unique values back to the input array and free memory.
 */
  memcpy((void *)data, (void *)out, kout*ic);
  free (out);
  return kout;
}
