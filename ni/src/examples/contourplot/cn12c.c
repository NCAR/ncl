/*
 *      $Id: cn12c.c,v 1.5 2010-03-15 22:49:23 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  1995                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
************************************************************************/
/*
 *  File:       cn12c.c
 *
 *  Author:     Mary Haley
 *          National Center for Atmospheric Research
 *          PO 3000, Boulder, Colorado
 *
 *  Date:       Mon Oct 16 10:46:27 MDT 1995
 *
 *   Description:  This example emulates LLU example "cpex08".  It
 *                 draws a filled map with filled contours appearing in
 *                 Africa. In order to mask Africa from the map fill, we
 *                 use the mpMaskAreaSpecifiers resource and mask all of
 *                 the countries in Africa.
 *
 */

#include <stdio.h>
#include <math.h>
#include <ncarg/gks.h>
#include <ncarg/ncargC.h>
#include <ncarg/hlu/hlu.h>

/*
 * Include a header file for each object created
 */

#include <ncarg/hlu/App.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/PDFWorkstation.h>
#include <ncarg/hlu/CairoWorkstation.h>
#include <ncarg/hlu/ScalarField.h>
#include <ncarg/hlu/ContourPlot.h>
#include <ncarg/hlu/MapPlot.h>

#define M 40
#define N 40

/* 
 * Areas we want to fill.
 */
char *fill_specs[] = {"water","land"};

/* 
 * Areas we want to mask (countries of Africa).
 */
char *mask_specs[] = {
  "algeria","angola","angola-exclave-called-cabinda","benin","botswana",
  "burundi","cameroon","central-african-republic","chad","congo",
  "djibouti","egypt","equatorial-guinea","ethiopia","gabon","gambia",
  "ghana","guinea","guinea-bissau","ivory-coast","kenya","lesotho",
  "liberia","libya","madagascar","malawi","mali","mauritania",
  "mauritius","morocco","mozambique","namibia","niger","nigeria",
  "rwanda","senegal","sierra-leone","somalia","south-africa","sudan",
  "swaziland","tanzania","togo","tunisia","uganda","upper-volta",
  "western-sahara","zaire","zambia","zimbabwe"};

int main(int argc, char *argv[])
{
    int appid,wid,dataid,cnid,mpid;
    int rlist;

    float z[N*M];
    ng_size_t len_dims[2];

    extern void bndary();
    extern void gendat (float *,int,int,int,int,int,float,float);
    char const *wks_type = "x11";
/*
 * Initialize the high level utility library
 */
    NhlInitialize();
/*
 * Create an application object.
 */
    rlist = NhlRLCreate(NhlSETRL);
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNappUsrDir,"./");
    NhlCreate(&appid,"cn12",NhlappClass,NhlDEFAULT_APP,rlist);

    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./cn12c.ncgm");
        NhlCreate(&wid,"cn12Work",
                  NhlncgmWorkstationClass,NhlDEFAULT_APP,rlist); 
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlCreate(&wid,"cn12Work",
                  NhlcairoWindowWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPSFileName,"./cn12c.ps");
        NhlCreate(&wid,"cn12Work",
                  NhlpsWorkstationClass,NhlDEFAULT_APP,rlist); 
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPDFFileName,"./cn12c.pdf");
        NhlCreate(&wid,"cn12Work",
                  NhlpdfWorkstationClass,NhlDEFAULT_APP,rlist); 
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"./cn12c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"cn12Work",
                  NhlcairoDocumentWorkstationClass,NhlDEFAULT_APP,rlist); 
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"./cn12c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"cn12Work",
                  NhlcairoImageWorkstationClass,NhlDEFAULT_APP,rlist); 
    }
/*
 * Call the routine 'gendat' to create an array of contour data.
 * Create a ScalarField data object and hand it the data created by
 * 'gendat'.
 */ 
    NhlRLClear(rlist);
    len_dims[0] = N;
    len_dims[1] = M;
    gendat(z,M,M,N,15,15,-10.,110.);

    NhlRLClear(rlist);
    NhlRLSetMDFloatArray(rlist,NhlNsfDataArray,z,2,len_dims);
    NhlRLSetFloat(rlist,NhlNsfXCStartV,-18.);
    NhlRLSetFloat(rlist,NhlNsfXCEndV,52.);
    NhlRLSetFloat(rlist,NhlNsfYCStartV,-35.);
    NhlRLSetFloat(rlist,NhlNsfYCEndV,38.);
    NhlCreate(&dataid,"DataPlot",NhlscalarFieldClass,appid,rlist);
/*
 * Create a ContourPlot object using the above data field, and make sure
 * the LabelBar is displayed.
 */
    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNcnScalarFieldData,dataid);
    NhlCreate(&cnid,"con1",NhlcontourPlotClass,wid,rlist);
/* 
 * Create a map object, specifying the areas we want filled and masked.
 */
    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNvpXF,0.1);
    NhlRLSetStringArray(rlist,NhlNmpMaskAreaSpecifiers,mask_specs,
                        NhlNumber(mask_specs));
    NhlRLSetStringArray(rlist,NhlNmpFillAreaSpecifiers,fill_specs,
                        NhlNumber(fill_specs));
    NhlRLSetString(rlist,NhlNpmLabelBarDisplayMode,"always");
    NhlCreate(&mpid,"map",NhlmapPlotClass,wid,rlist);

    NhlAddOverlay(mpid,cnid,-1);
    NhlDraw(mpid);
    NhlFrame(wid);
/*
 * Destroy the workstation object and exit.
 */
    NhlDestroy(wid);

    NhlClose();
    exit(0);
}

#define max(x,y)    ((x) > (y) ? (x) : (y) )
#define min(x,y)    ((x) < (y) ? (x) : (y) )
#define pow2(x)    ((x)*(x))

void gendat (float *data,int idim,int m,int n,int mlow,int mhgh,float dlow,float dhgh)
{
/*
 * This is a routine to generate test data for two-dimensional graphics
 * routines.  Given an array "DATA", dimensioned "IDIM x 1", it fills
 * the sub-array ((DATA(I,J),I=1,M),J=1,N) with a two-dimensional field
 * of data having approximately "MLOW" lows and "MHGH" highs, a minimum
 * value of exactly "DLOW" and a maximum value of exactly "DHGH".
 *
 * "MLOW" and "MHGH" are each forced to be greater than or equal to 1
 * and less than or equal to 25.
 *
 * The function used is a sum of exponentials.
 */
    float ccnt[3][50], fovm, fovn, dmin, dmax, temp;
    extern float fran();
    int nlow, nhgh, ncnt, i, j, k, ii;

    fovm=9./(float)m;
    fovn=9./(float)n;

    nlow=max(1,min(25,mlow));
    nhgh=max(1,min(25,mhgh));
    ncnt=nlow+nhgh;

    for( k=1; k <= ncnt; k++ ) {
        ccnt[0][k-1]=1.+((float)m-1.)*fran();
        ccnt[1][k-1]=1.+((float)n-1.)*fran();
        if (k <= nlow) {
            ccnt[2][k-1]= -1.;
        }
        else {
            ccnt[2][k-1] = 1.;
        }
    }

    dmin =  1.e36;
    dmax = -1.e36;
    ii = 0;
    for( j = 1; j <= n; j++ ) {
        for( i = 1; i <= m; i++ ) {
            data[ii]=.5*(dlow+dhgh);
            for( k = 1; k <= ncnt; k++ ) {
                temp = -(pow2((fovm*((float)(i)-ccnt[0][k-1])))+
                         pow2(fovn*((float)(j)-ccnt[1][k-1])));
                if (temp >= -20.) data[ii]=data[ii]+.5*(dhgh-dlow)
                                           *ccnt[2][k-1]*exp(temp);
            }
            dmin=min(dmin,data[ii]);
            dmax=max(dmax,data[ii]);
            ii++;
        }
    }

    for( j = 0; j < m*n; j++ ) {
        data[j]=(data[j]-dmin)/(dmax-dmin)*(dhgh-dlow)+dlow;
    }
}

float rseq[] = { .749,.973,.666,.804,.081,.483,.919,.903,.951,.960,
   .039,.269,.270,.756,.222,.478,.621,.063,.550,.798,.027,.569,
   .149,.697,.451,.738,.508,.041,.266,.249,.019,.191,.266,.625,
   .492,.940,.508,.406,.972,.311,.757,.378,.299,.536,.619,.844,
   .342,.295,.447,.499,.688,.193,.225,.520,.954,.749,.997,.693,
   .217,.273,.961,.948,.902,.104,.495,.257,.524,.100,.492,.347,
   .981,.019,.225,.806,.678,.710,.235,.600,.994,.758,.682,.373,
   .009,.469,.203,.730,.588,.603,.213,.495,.884,.032,.185,.127,
   .010,.180,.689,.354,.372,.429 };

float fran()
{
    static int iseq = 0;
    iseq = (iseq % 100) + 1;
    return(rseq[iseq-1]);
}

