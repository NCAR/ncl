/*
 *	$Id: c_tvelvc.c,v 1.2 1994-06-21 15:01:48 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define pow2(x)    ((x)*(x))

#define WSTYPE SED_WSTYPE
#define WKID   1

/*
 * OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
 */

main()
{
    int idum,ierr;
    
    gopen_gks ("stdout",0);
    gopen_ws (WKID, NULL, WSTYPE);
    gactivate_ws (WKID);
/*
 * INVOKE DEMO DRIVER
 */
    tvelvc(&ierr);
/*
 *     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}

tvelvc(ierror)
int *ierror;
{
/*
 * PURPOSE                To provide a simple demonstration of VELVCT.
 *
 * USAGE                  CALL TVELVC (IERROR)
 *
 * ARGUMENTS
 *
 * ON OUTPUT              IERROR
 *                          An integer variable
 *                          = 0, if the test was successful,
 *                          = 1, the test was not successful.
 *
 * I/O                    If the test is successful, the message
 *
 *               VELVCT TEST EXECUTED--SEE PLOTS TO CERTIFY
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
 * REQUIRED ROUTINES      VELVCT
 *
 * REQUIRED GKS LEVEL     0A
 *
 * ALGORITHM              This test program calls entries EZVEC and
 *                        VELVCT.  Each call produces a plot of a
 *                        vector field obtained from the function
 *
 *                          Z(X,Y) = X + Y + 1./((X-.1)**2+Y**2+.09)
 *                                   -1./((X+.1)**2+Y**2+.09),
 *
 *                        by using the direction of the Z gradient
 *                        vectors and the logarithm of the absolute
 *                        value of the components.
 *
 * HISTORY                Originally written in November 1976.
 *                        Converted to FORTRAN 77 and GKS in July 1984.
 *
 */
    float u[25][21],v[25][21];
    float flo,hi,x,y;
    float dzdx,dzdy,uvmag,uvdir;
    int nset,length,ispv;
    float spv[2];
    int ierr,icn;
    int i,j,n,m,ix,iy;
/*
 * Specify coordinates for a plot title.
 */
    ix = 94;
    iy = 1000;
    
/*
 * Specify VELVCT arguments.
 */
    flo = 0.; hi = 0.; nset = 0; length = 0; ispv = 0;
    spv[0] = spv[1] = 0;
/*
 * Initialize the error parameter.
 */
    *ierror = 1;
/*
 * Specify velocity field functions U and V.
 */
    m = 21;
    n = 25;
    for(i=1;i<=m;i++){
        x = .1*(i-11);
        for(j=1;j<=n;j++){
            y = .1*(j-13);
            dzdx = 1.-2.*(x-.10)/pow2(pow2(x-.10)+y*y+.09)+
              2.*(x+.10)/pow2(pow2(x+.10)+y*y+.09);
            dzdy = 1.-2.*y/pow2(pow2(x-.10)+y*y+.09)+
              2.*y/pow2(pow2(x+.10)+y*y+.09);
            uvmag = log(sqrt(dzdx*dzdx+dzdy*dzdy));
            uvdir = atan2(dzdy,dzdx);
            u[j-1][i-1] = uvmag*cos(uvdir);
            v[j-1][i-1] = uvmag*sin(uvdir)  ;
        }
    }
    
    ginq_cur_norm_tran_num(&ierr,&icn);
/*
 * Select normalization transformation 0.
 */
    gsel_norm_tran(0);
/*
 * Call PLCHLQ to write the plot title.
 */
    x = c_cpux(ix);
    y = c_cpuy(iy);
    c_plchlq (x,y,"DEMONSTRATION PLOT FOR ENTRY EZVEC OF VELVCT",16.,0.,-1.);
    gsel_norm_tran(icn);
/*
 * Call EZVEC for a default velocity field plot.
 */
    c_ezvec (&u[0][0],&v[0][0],m,n);
/*
 * Call VELVCT to generate the user tuned velocity field plot.
 */
    c_velvct (&u[0][0],m,&v[0][0],m,m,n,flo,hi,nset,length,ispv,spv);
    ginq_cur_norm_tran_num(&ierr,&icn);
/*
 * Select normalization transformation 0.
 */
    gsel_norm_tran(0);
/*
 * Call PLCHLQ to write the plot title.
 */
    x = c_cpux(ix);
    y = c_cpuy(iy);
    
    c_plchlq (x,y,"DEMONSTRATION PLOT FOR ENTRY VELVCT OF VELVCT",16.,0.,-1.);
    gsel_norm_tran(icn);
    c_frame();
    
    *ierror = 0;
    printf("\n VELVCT TEST EXECUTED--SEE PLOTS TO CERTIFY\n");
    return(1);    
}
