/*
 *      $Id: c_srex01.c,v 1.4 2004-08-17 21:02:19 kennison Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#if !defined (cray)
extern struct common {
#else
struct common {
#endif
    int ifr,istp,irots,idrx,idry,idrz,iupper,iskirt,ncla;
    float theta,hskirt,chi,clo,cinc;
    int ispval;
} NGCALLC(srfip1,SRFIP1);

char *plbl = "Longs Peak relief using SRFACE";

#define IWTYPE 1
#define WKID   1

main()
{
/*
 * Define the required arrays.
 */
    float xdat[100],ydat[100],odat[40][40],zdat[100][100],qdat[100][100];
    float stln[6];
    int i, j, k, l, jm1, jp1, im1, ip1, nplt, work[100][100][2];
    float fl, fk;
    char stmp[50];
/*
 * define the line of sight (viewpoint and point looked at).
 */
    stln[0] = 5247.5;
    stln[1] = 5247.5;
    stln[2] = 2530.;
    stln[3] = 247.5;
    stln[4] = 247.5;
    stln[5] = 1280.;
/*
 * Open GKS, open workstation of type 1, activate workstation.
 */
    gopen_gks ("stdout",0);
    gopen_ws (WKID, NULL, IWTYPE);
    gactivate_ws (WKID);
/*
 * Generate x and y coordinate values.
 */
    for( i = 0; i < 100; i++ ) {
        ydat[i] = xdat[i]=5.*(float)i;
    }
/*
 * Put the original Long's Peak data in the array ODAT.
 * Due to the way the data is written, each row must be
 * read in as chunks of 15, 15, and 10.
 */
    for( j = 0; j < 40; j++ ) {
        for( i =  0; i < 15; i++ ) fscanf( stdin, "%f5.0", &odat[i][j] );
        gets(stmp);
        for( i = 15; i < 30; i++ ) fscanf( stdin, "%f5.0", &odat[i][j] );
        gets(stmp);
        for( i = 30; i < 40; i++ ) fscanf( stdin, "%f5.0", &odat[i][j] );
        gets(stmp);
    }
/*
 * Interpolate to get more closely-spaced data in the array QDAT.
 */
    for( j = 1; j <= 100; j++ ) {
        fl=1.+39.*(float)(j-1)/99.;
        l=(int)(fl);
        if (l<1) l=1;
        if (l>39) l=39;
        fl=fl-(float)(l);
        for( i = 1; i <= 100; i++ ) {
            fk=1.+39.*(float)(i-1)/99.;
            k=(int)(fk);
            if (k<1) k=1;
            if (k>39) k=39;
            fk=fk-(float)(k);
            qdat[i-1][j-1]=(1.-fl)*((1.-fk)*odat[k-1][l-1]+fk*odat[k][l-1])+
                                fl*((1.-fk)*odat[k-1][  l]+fk*odat[k][  l]);
        }
    }
/*
 * Apply a nine-point smoother to get smoother data in the array ZDAT.
 */
    for( j = 1; j <= 100; j++ ) {
        jm1= j-1 > 1 ? j-1 : 1;
        jp1=j+1 < 100 ? j+1 : 100;
        for( i = 1; i <= 100; i++ ) {
            im1= i-1 > 1 ? i-1 : 1;
            ip1= i+1 < 100 ? i+1 : 100;
            zdat[j-1][i-1]=.2500*qdat[i-1][j-1]+
                           .1250*(qdat[im1-1][j-1]+qdat[ip1-1][j-1]+
                                  qdat[i-1][jm1-1]+qdat[i-1][jp1-1])+
                           .0625*(qdat[im1-1][jm1-1]+qdat[ip1-1][jm1-1]+
                                  qdat[im1-1][jp1-1]+qdat[ip1-1][jp1-1]);
        }
    }
/*
 * Plot the data four times.
 */
    for( nplt = 1; nplt <= 4; nplt ++ ) {
/*
 * Before the 2nd, 3rd, and 4th plots, rotate the data by 90 degrees.
 */
        if (nplt != 1) {
            for( j = 0; j < 100; j++ ) {
                for( i = 0; i < 100; i++ ) {
                    qdat[j][i]=zdat[j][i];
                }
            }
            for( j = 1; j <= 100; j++ ) {
                k=101-j;
                for( i = 1; i <= 100; i++ ) {
                    l=i;
                    zdat[j-1][i-1]=qdat[l-1][k-1];
                }
            }
        }
/*
 * Call LOGO to add NCAR and SCD logos.
 */
        logo();
/*
 * Set common variables for skirt and frame calls.
 */
        NGCALLC(srfip1,SRFIP1).iskirt = 1;
        NGCALLC(srfip1,SRFIP1).hskirt = 1100.0;
        NGCALLC(srfip1,SRFIP1).ifr    = 0;
/*
 * Use SRFACE to draw a representation of the surface.
 */
        c_srface (xdat,ydat,&zdat[0][0],&work[0][0][0],100,100,100,stln,0.);
/*
 * Put in label on the top of the map.
 */
        c_pwrit (.5,.850,plbl,30,3,0,0);
/*
 * Advance to the next frame.
 */
        c_frame();
    }
/*
 * Deactivate and close workstation, close GKS.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}

logo()
{
    ncar(.55,.92);
    scd(.04,.04);
    irons(0.09,0.090);
    return(1);
}

ncar(xpos,ypos)
float xpos,ypos;
{
    c_pcseti ("CD", 1);
    c_plchhq ( xpos,ypos, "NCAR  GRAPHICS",.038, 0.0, 0.0 );
    c_pcseti ("CD", 0);
    return(1);
}

scd(xbot, ybot)
float xbot, ybot;
{
    c_pcseti ("CD", 1);
    c_plchhq (xbot,ybot,"SCD",.04,0.0,-1.0);
    c_plchhq (xbot+.145,ybot+.008,"SCIENTIFIC COMPUTING DIVISION",.012,0.0,-1.0);
    c_plchhq (xbot+.145,ybot-.016,"NATIONAL CENTER FOR ATMOSPHERIC RESEARCH",.012,0.0,-1.0);
    c_pcseti ("CD",0);
    c_line (0.93,0.07,.18,0.07);
    return(1);
}

irons(x1,y1)
float x1,y1;
{
    c_line (x1+.039,y1+.050,x1+0.015,y1+0.054);
    c_line (x1+.015,y1+.054,x1-.032,y1+0.013);
/*
 * Devil's Thumb.
 */
    c_line (x1+.010,y1+.048,x1+.007,y1+0.054);
    c_line (x1+.007,y1+.054,x1+.007,y1+0.059);
    c_line (x1+.007,y1+.059,x1+.005,y1+0.053);
    c_line (x1+.005,y1+.053,x1+.006,y1+0.044);
/*
 * bear mountain.
 */
    c_line (x1+0.026,y1+0.03,x1+.038,y1+.05);
    c_line (x1+.038,y1+.05,x1+.053,y1+.07);
    c_line (x1+.053,y1+.07,x1+.078,y1+.03);
    c_line (x1+.078,y1+.03,x1+.098,y1+.05);
    c_line (x1+.098,y1+.05,x1+.1,y1+.04);
    c_line (x1+.1,y1+.04,x1+.13,y1+.065);
    c_line (x1+.13,y1+.065,x1+.18,y1+.029);
    c_line (x1+.18,y1+.029,x1+.25,y1+.039);
    c_line (x1+.25,y1+.039,x1+.27,y1+.026);
    c_line (x1+.27,y1+.026,x1+.81,y1+.026);
    return(1);
}
