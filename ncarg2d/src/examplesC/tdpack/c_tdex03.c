/*
 *      $Id: c_tdex03.c,v 1.1 1998-06-19 18:07:16 fred Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define IWTYPE 1
#define WKID   1

#define NSX   29
#define NSY   25
#define NIX   21
#define NIY   31
#define NIZ   19
#define RBIG1  6.
#define RBIG2  6.
#define RSML1  2.
#define RSML2  2.
#define TISO   0.

#ifndef MIN
#define MIN(A,B)        (((A) < (B)) ? (A) : (B))
#endif

/*
 *  Illustrate the use of the simplified entry points for Tdpack,
 *  c_tdez2d and c_tdez3d, by drawing a surface and an isosurface.
 */

main()
{

/*
 *  Declare arrays for the surface.
 */
    float xt[NSX],yt[NSY],zt[NSY][NSX];

/*
 *  Declare arrays for the isosurface.
 */
    float x[NIX],y[NIY],z[NIZ],ui[NIZ][NIY][NIX];

    int i,j,k,isxh,isyh,jcent1,jcent2; 
    float t1,t2,f1,f2,fimid,fjmid1,fjmid2,fkmid,fip1,fip2,fjp1,fjp2,fkp1,fkp2;

/*
 *  Open gks, open workstation of type 1, activate workstation
 */
    gopen_gks("stdout",0);
    gopen_ws(WKID, NULL, IWTYPE);
    gactivate_ws(WKID);

/*
 *  Create the data array for the 2-dimensional surface.
 */
    for (i = 0; i < NSX; i++) {
      isxh = NSX/2;
      xt[i] = 0.1 * (float) (i - isxh);
      for (j = 0; j < NSY; j++) {
        isyh = NSY/2;
        yt[j] = 0.1 * (float) (j - isyh);
        zt[j][i] = xt[i] + yt[j];
        t1 = 1./(pow(fabs(xt[i]-0.1),2.75) + pow(fabs(yt[j]),2.75) + 0.09);
        t2 = 1./(pow(fabs(xt[i]+0.1),2.75) + pow(fabs(yt[j]),2.75) + 0.09);
        zt[j][i] = 0.3*(zt[j][i] + t1 - t2);
      }
    }

/*
 *  Draw the surface.
 */
    c_tdez2d(NSX,NSY,xt,yt,&zt[0][0],2.5,-154.,80.,6);
    c_frame();

/*
 *  Fill the 3-dimensional array for plotting an isosurface.
 */
    jcent1 = 0.5 * (float) NIY - 0.5 * RBIG1;
    jcent2 = 0.5 * (float) NIY + 0.5 * RBIG2;
    for (i = 0; i < NIX; i++) {
      x[i] = (float) i;
      fimid = i-(NIX+1)/2;
      for (j = 0; j < NIY; j++) {
        y[j] = (float) j;
        fjmid1 = j+1-jcent1;
        fjmid2 = j+1-jcent2;
        for (k = 0; k < NIZ; k++) {
          z[k] = (float) k;
          fkmid = k-(NIZ+1)/2;
          f1 = sqrt(RBIG1*RBIG1/(fjmid1*fjmid1+fkmid*fkmid+0.1));
          f2 = sqrt(RBIG2*RBIG2/(fimid*fimid+fjmid2*fjmid2+0.1));
          fip1 = (1.-f1)*fimid;
          fip2 = (1.-f2)*fimid;
          fjp1 = (1.-f1)*fjmid1;
          fjp2 = (1.-f2)*fjmid2;
          fkp1 = (1.-f1)*fkmid;
          fkp2 = (1.-f2)*fkmid;
          ui[k][j][i] = MIN(fimid*fimid+fjp1*fjp1+fkp1*fkp1-RSML1*RSML1,
                           fkmid*fkmid+fip2*fip2+fjp2*fjp2-RSML2*RSML2);
        }
      }
    }

/*
 *  Draw the isosurface.
 */
    c_tdez3d(NIX,NIY,NIZ,x,y,z,&ui[0][0][0],TISO,1.8,-45.,58.,-4);
    c_frame();

/*
 * deactivate and close workstation, close gks.
 */
    gdeactivate_ws (WKID);
    gclose_ws(WKID);
    gclose_gks();
}
