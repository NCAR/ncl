#include <stdio.h>
#include <math.h>
#include <ncarg/gks.h>
#include <ncarg/ncargC.h>

/*
** Define error file, Fortran unit number, and workstation type,
** and workstation ID.
*/
#define IERRF  "stdout"
#define LUNIT  "gmeta"
#define IWTYPE SED_WSTYPE
#define IWKID  1
#define ISZDM  0

#define K 40            /* rows of data */
#define N 40            /* columns of data */
#define LRWK 1000       /* size of real work array */
#define LIWK 1000       /* size of integer work array */


int main()
{
    int iwrk[LIWK];         /* integer work array */
    
    float rwrk[LRWK];       /* real work array */
    float z[K][N];          /* contour data */
    
    extern void getdat(float *);    /* get contour data */
    
    
    getdat (&z[0][0]);

/* Open GKS */
    gopen_gks(IERRF, ISZDM);
    gopen_ws(IWKID, LUNIT, IWTYPE);
    gactivate_ws(IWKID);
    gset_clip_ind (0);
    
    c_cpseti("NLZ - NUMERIC LEADING ZERO FLAG",1);
    c_cpseti("NOF - NUMERIC OMISSION FLAG",3);

/* Initialize Conpack */
    c_cprect (&z[0][0], N, N, K, rwrk, LRWK, iwrk, LIWK);
/* Draw perimeter */
    c_cpback (&z[0][0], rwrk, iwrk);
/* Draw Contours */
    c_cpcldr (&z[0][0], rwrk, iwrk);
/* Draw Labels */
    c_cplbdr(&z[0][0],rwrk,iwrk);

/* Close frame and close GKS */
    c_frame();
    gdeactivate_ws(IWKID);
    gclose_ws(IWKID);
    gclose_gks();
    return (0);
}


void getdat (float* z)
/*
** Create contour data.
**
** z    - data array
*/
{
    int i,j;        /* counters */

/* Create contour levels */
    for(i=1; i<=K; ++i)
    {
        for(j=1; j<=N; ++j)
        {
            z[(((j-1)*N)+i-1)] = 10.0e-7*(-16.*(float)(i*i*j) +
                                 34.0*(float)(i*j*j) - (float)(6*i) + 93.);
        }
    }
}
