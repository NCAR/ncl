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
    int i;          /* counter */
    int iwrk[LIWK];         /* integer work array */
    
    float rwrk[LRWK];       /* real work array */
    float z[K][N];          /* contour data */
    
    float CIT[10];      /* contour interval table */
    float LIT[10];      /* label interval table */
    
    extern void getdat(float*);     /* get contour data */
    
/* Set up interval tables */
    for(i=0; i<10; ++i)
    {
        CIT[i] = ((i+1)%10) * 1.0;
        LIT[i] = 5.0;
    }
    
    getdat (&z[0][0]);
    
/* Open GKS */
    gopen_gks(IERRF, ISZDM);
    gopen_ws(IWKID, LUNIT, IWTYPE);
    gactivate_ws(IWKID);
    gset_clip_ind (0);
    
/*
     ** Change nice values to match old CONREC nice values
     ** Draw labels at every 5th contour level no matter which contour
     ** level interval is chosen.
     */
    for(i=0; i<10; ++i)
    {
        c_cpseti("PAI - PARAMETER ARRAY INDEX",i+1);
        c_cpsetr("CIT - CONTOUR INTERVAL TABLE",CIT[i]);
        c_cpseti("LIT - LABEL INTERVAL TABLE",LIT[i]);
    }
    
    c_cpseti("NSD - NUMBER OF SIGNIFICANT DIGITS",-5);
    c_cpseti("NLS - NUMERIC LEFTMOST SIGNIFICANT DIGIT",0);
    
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
