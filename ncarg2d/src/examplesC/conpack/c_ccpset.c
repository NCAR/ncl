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

/* Create contour data */
    extern void getdat(float*);

    getdat (&z[0][0]);

/* Open GKS */
    gopen_gks(IERRF, ISZDM);
    gopen_ws(IWKID, LUNIT, IWTYPE);
    gactivate_ws(IWKID);
    gset_clip_ind (0);
/*
** Draw a perimeter around the whole frame to show off SET call
*/
    c_set (0.,1.,0.,1.,0.,1.,0.,1.,1);
    c_perim(0,0,0,0);
/*
** Force Data to be drawn in a rectangle twice as long as it is wide
** and only draw in the lower right hand corner of the screen
*/
    c_set (0.5,0.98,0.125,0.375,0.,2.,0.,1.,1);
    c_cpseti("SET - DO-SET-CALL FLAG",0);
    c_cpsetr("XC1 - X COORDINATE AT INDEX 1",0.);
    c_cpsetr("XCM - X COORDINATE AT INDEX M",2.);
    c_cpsetr("YC1 - Y COORDINATE AT INDEX 1",0.);
    c_cpsetr("YCN - Y COORDINATE AT INDEX N",1.);

/* Initialize Conpack */
    c_cprect (&z[0][0], N, N, K, rwrk, LRWK, iwrk, LIWK);

/* Draw perimeter */
    c_cpback (&z[0][0], rwrk, iwrk);

/* Draw Contours */
    c_cpcldr (&z[0][0], rwrk, iwrk);

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
            z[(((j-1)*N)+i-1)] = 10.0e-5*(-16.*(float)(i*i*j) +
                                 34.0*(float)(i*j*j) - (float)(6*i) + 93.);
        }
    }
}
