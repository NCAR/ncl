#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
    int ierr;
    extern void tstrml();
/*
 *  OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
 */
    gopen_gks ("stdout",0);
    gopen_ws( WKID, NULL, WSTYPE);
    gactivate_ws( WKID );
/* 
 * INVOKE DEMO DRIVER
 */
    tstrml(&ierr);
/*
 *     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}

void tstrml (ierr)
int *ierr;
{
/*
 * PURPOSE                To provide a simple demonstration of STRMLN.
 *
 * USAGE                  tstrml (&ierr)
 *
 * ARGUMENTS
 *
 * ON OUTPUT              ierr
 *                          An integer variable
 *                          = 0, if the test was successful,
 *                          = 1, the test was not successful.
 *
 * I/O                    If the test is successful, the message
 *
 *               STRMLN TEST EXECUTED--SEE PLOT TO CERTIFY
 *
 *                        is printed on unit 6.  In addition, 1
 *                        frame is produced on the machine graphics
 *                        device.  In order to determine if the test
 *                        was successful, it is necessary to examine
 *                        the plot.
 *
 * PRECISION              Single
 *
 * LANGUAGE               FORTRAN 77
 *
 * REQUIRED ROUTINES      STRMLN
 *
 * REQUIRED GKS LEVEL     0A
 *
 * ALGORITHM              Routine TSTRML calls routine STRMLN to
 *                        produce a plot whi*h depicts the flow and
 *                        magnitude of a vector field.
 *
 */
    float u[25][21],v[25][21],wrk[1050];
    float tx,ty;
    int nv,nh;
    int i,j;
    float tpimx,tpjmx;
/*
 * Specify coordinates for plot titles.  The values TX and TY
 * define the center of the title string in a 0. to 1. range.
 */
    tx = .5;
    ty = .9765;
/*
 * Set the grid dimensions.
 */
    nh = 21;
    nv = 25;
/*
 *Initialize the error parameter.
 */
    *ierr = 1;
/*
 * Specify horizontal and vertical vector components U and V on
 * the rectangular grid.
 */
    tpimx = 2.*3.14/((float)nh);
    tpjmx = 2.*3.14/((float)nv);
    
    for(j=0;j<nv;j++){
        for(i=0;i<nh;i++){
            u[j][i] = sin(tpimx*((float)(i+1)-1.));
            v[j][i] = sin(tpjmx*((float)(j+1)-1.));
        }
    }
/*
 * Select normalization transformation 0.
 */
    gsel_norm_tran(0);
/*
 * Call PLCHLQ to write the plot title.
 */
    c_plchlq (tx,ty,"DEMONSTRATION PLOT FOR ROUTINE EZSTRM",16.,0.,0.);
    
/*
 * Define normalization transformation 1, and set up log scaling.
 */
    c_set(0.1, 0.9, 0.1, 0.9,1.0, 21., 1.0, 25.,1);
/*
 * Call STRMLN for vector field streamlines plot.
 */
    c_strmln (&u[0][0],&v[0][0],wrk,nh,nh,nv,1,ierr);
    c_frame();
    
    *ierr = 0;
    
    printf(" STRMLN TEST EXECUTED--SEE PLOT TO CERTIFY\n");
}
    
