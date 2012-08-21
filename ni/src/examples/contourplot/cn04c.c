/*
 *      $Id: cn04c.c,v 1.9 2010-03-15 22:49:23 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  1993                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
************************************************************************/
/*
 *  File:       cn04c.c
 *
 *  Author:     David Brown
 *          National Center for Atmospheric Research
 *          PO 3000, Boulder, Colorado
 *
 *  Date:       Wed Oct 12 13:18:13 MDT 1994
 *
 *  Description:    Emulates the output of the Conpack example 'cpex02.f'
 *          using the HLU library.
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
#include <ncarg/hlu/TextItem.h>

#define M 33
#define N 33

int main(int argc, char *argv[])
{
    int appid,wid,dataid,cnid,txid;
    int rlist,grlist;
    int gkswid;

    float z[N*M];
    int mlow = 20, mhigh = 20;
    float dlow = .000025, dhigh = .000075;
    ng_size_t len_dims[2];

    int i;
    int *lvlflags, *pats; 
    float *levels, *thicknesses;
    ng_size_t lvlflag_count, pat_count, level_count,thick_count;

    extern void bndary();
    extern void gendat (float *,int,int,int,int,int,float,float);
    char const *wks_type = "x11";
/*
 * This program emulates the output of cpex02 with a few differences:
 * 1. Because the information label is implemented as an HLU Annotation
 *    object, Conpack is unaware of its existence, much less its location.
 *    Therefore it is not possible to have Conpack remove the high/low
 *    labels that occupy the same space as the info label.
 * 2. Line labels do not appear in the same positions.
 */

/*
 * Initialize the high level utility library
 */
    NhlInitialize();
/*
 * Create an application context. Set the app dir to the current directory
 * so the application looks for a resource file in the working directory.
 * The resource file sets most of the ContourPlot resources that remain fixed
 * throughout the life of the ContourPlot object.
 */
    rlist = NhlRLCreate(NhlSETRL);
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNappUsrDir,"./");
    NhlCreate(&appid,"cn04",NhlappClass,NhlDEFAULT_APP,rlist);

    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./cn04c.ncgm");
        NhlCreate(&wid,"cn04Work",
                  NhlncgmWorkstationClass,NhlDEFAULT_APP,rlist); 
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlCreate(&wid,"cn04Work",
                  NhlcairoWindowWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPSFileName,"./cn04c.ps");
        NhlCreate(&wid,"cn04Work",
                  NhlpsWorkstationClass,NhlDEFAULT_APP,rlist); 
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPDFFileName,"./cn04c.pdf");
        NhlCreate(&wid,"cn04Work",
                  NhlpdfWorkstationClass,NhlDEFAULT_APP,rlist); 
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"./cn04c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"cn04Work",
                  NhlcairoDocumentWorkstationClass,NhlDEFAULT_APP,rlist); 
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"./cn04c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"cn04Work",
                  NhlcairoImageWorkstationClass,NhlDEFAULT_APP,rlist); 
    }
/*
 * Call the Fortran routine 'GENDAT' to create the first array of contour
 * data. Create a ScalarField data object and hand it the data created by
 * 'GENDAT'.
 */ 
    NhlRLClear(rlist);
    len_dims[0] = N;
    len_dims[1] = M;
    gendat(z,M,M,N,mlow,mhigh,dlow,dhigh);
    NhlRLSetMDFloatArray(rlist,NhlNsfDataArray,z,2,len_dims);
    NhlCreate(&dataid,"Gendat",NhlscalarFieldClass,appid,rlist);
/*
 * Create a ContourPlot object, supplying the ScalarField object as data,
 * and setting the size of the viewport.
 */
    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNcnScalarFieldData,dataid);
    NhlRLSetString(rlist,NhlNtiMainString,"EXAMPLE 2-1");
    NhlRLSetFloat(rlist,NhlNvpWidthF,0.4625);
    NhlRLSetFloat(rlist,NhlNvpHeightF,0.4625);
    NhlCreate(&cnid,"ContourPlot1",NhlcontourPlotClass,wid,rlist);
/*
 * In order to set the contour array resources of interest, you must 
 * allocate memory for the arrays and fill in the correct value for each
 * element. But by calling GetValues for the arrays the ContourPlot object 
 * allocates the space and fills in the current values for you. Then all
 * that is necessary is to modify the values that you want to change. 
 * Remember, however, that you are responsible for freeing the memory
 * after you are done with it. Note also that a GetValues resource list is
 * different that a SetValues list.
 */
    grlist = NhlRLCreate(NhlGETRL);
    NhlRLClear(grlist);
    NhlRLGetIntegerArray(grlist,NhlNcnLevelFlags,&lvlflags,&lvlflag_count);
    NhlRLGetIntegerArray(grlist,NhlNcnFillPatterns,&pats,&pat_count);
    NhlRLGetFloatArray(grlist,NhlNcnLevels,&levels,&level_count);
    NhlRLGetFloatArray(grlist,NhlNcnLineThicknesses,
                           &thicknesses,&thick_count);
    NhlGetValues(cnid,grlist);
/* 
 * Depending on the level flag for each contour line, widen the line if
 * there is a label on the line. Also set the fill style to pattern #6
 * if the level is between certain values. Note that there is always one
 * more element in the fill resource arrays than there are ContourPlot line 
 * levels: the first element of these arrays specifies the attributes
 * of areas less than the minimum contour level and the last element 
 * specifies attributes of areas greater than the maximum contour level. 
 */
    for (i = 0; i < level_count; i++) {
        if (lvlflags[i] == NhlLINEANDLABEL)
          thicknesses[i] = 2.0;
        if (levels[i] >= .000045 && levels[i] < .000055)
          pats[i] = 6;
        else 
          pats[i] = NhlHOLLOWFILL;
    }
    pats[pat_count-1] = NhlHOLLOWFILL;
/*
 * Now that the arrays are correctly filled in set the arrays that have
 * been modified. Also set the position of the first ContourPlot plot and
 * the label scaling mode.
 */
    NhlRLClear(rlist);
    NhlRLSetIntegerArray(rlist,NhlNcnFillPatterns,pats,pat_count);
    NhlRLSetFloatArray(rlist,NhlNcnLineThicknesses,
                       thicknesses,thick_count);
    NhlRLSetFloat(rlist,NhlNvpXF,0.0250);
    NhlRLSetFloat(rlist,NhlNvpYF,0.9750);
    NhlRLSetString(rlist,NhlNcnLabelScalingMode,"ConfineToRange");
    NhlRLSetFloat(rlist,NhlNcnLabelScaleValueF,10.0);
    NhlSetValues(cnid,rlist);

    NhlDraw(cnid);
/*
 * Plot 2 - Set the Scalar Field object with a newly generated data set;
 * Set the ContourPlot object with a new title, position, and a new label
 * scaling mode.
 */
    NhlRLClear(rlist);
    gendat(z,M,M,N,mlow,mhigh,dlow,dhigh);
    NhlRLSetMDFloatArray(rlist,NhlNsfDataArray,z,2,len_dims);
    NhlSetValues(dataid,rlist);
    
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtiMainString,"EXAMPLE 2-2");
    NhlRLSetFloat(rlist,NhlNvpXF,0.5125);
    NhlRLSetInteger(rlist,NhlNcnScalarFieldData,dataid);
    NhlRLSetString(rlist,NhlNcnLabelScalingMode,"MaxSigDigitsLeft");
    NhlSetValues(cnid,rlist);
    NhlDraw(cnid);

/*
 * Plot 3 - Set the Scalar Field object with a newly generated data set;
 * Set the ContourPlot object with a new title, position, and a new label
 * scaling mode.
 */

    NhlRLClear(rlist);
    gendat(z,M,M,N,mlow,mhigh,dlow,dhigh);
    NhlRLSetMDFloatArray(rlist,NhlNsfDataArray,z,2,len_dims);
    NhlSetValues(dataid,rlist);

    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtiMainString,"EXAMPLE 2-3");
    NhlRLSetFloat(rlist,NhlNvpXF,0.0250);
    NhlRLSetFloat(rlist,NhlNvpYF,0.4875);
    NhlRLSetInteger(rlist,NhlNcnScalarFieldData,dataid);
    NhlRLSetString(rlist,NhlNcnLabelScalingMode,"TrimZeros");
    NhlSetValues(cnid,rlist);
    NhlDraw(cnid);
/*
 * Plot 4 - Set the Scalar Field object with a newly generated data set;
 * Set the ContourPlot object with a new title, position, and a new label
 * scaling mode.
 */

    NhlRLClear(rlist);
    gendat(z,M,M,N,mlow,mhigh,dlow,dhigh);
    NhlRLSetMDFloatArray(rlist,NhlNsfDataArray,z,2,len_dims);
    NhlSetValues(dataid,rlist);

    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtiMainString,"EXAMPLE 2-4");
    NhlRLSetFloat(rlist,NhlNvpXF,0.5125);
    NhlRLSetInteger(rlist,NhlNcnScalarFieldData,dataid);
    NhlRLSetString(rlist,NhlNcnLabelScalingMode,"IntegerLineLabels");
    NhlSetValues(cnid,rlist);
    NhlDraw(cnid);
/*
 * Activate the GKS workstation and use the low-level routine that cpex02
 * uses to draw the line around the edge of the plotter frame.
 */
    NhlRLClear(grlist);
    NhlRLGetInteger(grlist,NhlNwkGksWorkId,&gkswid);
    NhlGetValues(wid,grlist);
    gactivate_ws(gkswid);
    bndary();
    gdeactivate_ws(gkswid);

/*
 * Label the plot as an emulation
 */
    NhlRLClear(rlist);
    NhlCreate(&txid,"TextItem1",NhltextItemClass,wid,rlist);
    NhlDraw(txid);

    NhlFrame(wid);

/*
 * Free dynamic arrays, destroy the objects created, 
 * close the HLU library and exit.
 */
    NhlFree(pats);
    NhlFree(thicknesses);
    NhlFree(levels);
    NhlFree(lvlflags);

    NhlDestroy(dataid);
    NhlDestroy(cnid);
    NhlDestroy(wid);
    NhlDestroy(appid);

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
 * routines.  Given a linear array "data", of length at least m x n, it fills
 * the array with a field representing a two-dimensional array stored
 * in C order of "n" rows and "m" columns.  The array will have
 * approximately "mlow" lows and "mhgh" highs, a minimum
 * value of exactly "dlow" and a maximum value of exactly "dhgh".
 *
 * "mlow" and "mhgh" are each forced to be greater than or equal to 1
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

float rseq[] = { .749, .973, .666, .804, .081, .483, .919, .903, .951, .960,
   .039, .269, .270, .756, .222, .478, .621, .063, .550, .798, .027, .569,
   .149, .697, .451, .738, .508, .041, .266, .249, .019, .191, .266, .625,
   .492, .940, .508, .406, .972, .311, .757, .378, .299, .536, .619, .844,
   .342, .295, .447, .499, .688, .193, .225, .520, .954, .749, .997, .693,
   .217, .273, .961, .948, .902, .104, .495, .257, .524, .100, .492, .347,
   .981, .019, .225, .806, .678, .710, .235, .600, .994, .758, .682, .373,
   .009, .469, .203, .730, .588, .603, .213, .495, .884, .032, .185, .127,
   .010, .180, .689, .354, .372, .429 };

float fran()
{
    static int iseq = 0;
    iseq = (iseq % 100) + 1;
    return(rseq[iseq-1]);
}

void bndary()
{
/*
 * Draw a line showing where the edge of the plotter frame is.
 */
    c_plotif (0.,0.,0);
    c_plotif (1.,0.,1);
    c_plotif (1.,1.,1);
    c_plotif (0.,1.,1);
    c_plotif (0.,0.,1);
    c_plotif (0.,0.,2);
}

