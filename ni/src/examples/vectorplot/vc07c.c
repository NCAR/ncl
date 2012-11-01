/*
 *      $Id: vc07c.c,v 1.4 2010-03-15 22:49:25 haley Exp $
 */
/***********************************************************************
*                                                                      *
*                Copyright (C)  1996                                   *
*        University Corporation for Atmospheric Research               *
*                All Rights Reserved                                   *
*                                                                      *
***********************************************************************/
/*
 *  File:       vc07c.c
 *
 *  Author:     David Brown (converted by Mary Haley)
 *              National Center for Atmospheric Research
 *              PO 3000, Boulder, Colorado
 *
 *  Date:       Wed Jul  3 8:26:28 MDT 1996
 *
 *  Description: This example emulates the LLU example "fcover", overlaying
 *               contours and vectors on a map plot.
 *
 */

#include <math.h>
#include <ncarg/gks.h>
#include <ncarg/ncargC.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/PDFWorkstation.h>
#include <ncarg/hlu/CairoWorkstation.h>
#include <ncarg/hlu/VectorPlot.h>
#include <ncarg/hlu/VectorField.h>
#include <ncarg/hlu/ScalarField.h>
#include <ncarg/hlu/MapPlot.h>
#include <ncarg/hlu/ContourPlot.h>

#define NCOLORS 24
#define MSIZE 73
#define NSIZE 73
#define NROWS 11

int ithin[NROWS] = {90,15,5,5,4,4,3,3,2,2,2};

int main(int argc, char *argv[])
{
    char const *wks_type = "x11";
    int i, j;
    int appid, wid, cnid, vcid, mpid;
    int vfield, sfield;
    int rlist, grlist;
    ng_size_t len_dims[2];
    float cmap[NCOLORS][3];
    float U[MSIZE][NSIZE],V[MSIZE][NSIZE],P[MSIZE][NSIZE];
    float vmin, vmax;
    char filename[256];
    const char *dir = _NGGetNCARGEnv("examples");
    char ch;
    FILE *fd;
/*
 * Initialize the high level utility library
 */
    NhlInitialize();
/*
 * Create an application object.
 */
    rlist = NhlRLCreate(NhlSETRL);
    grlist = NhlRLCreate(NhlGETRL);
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNappUsrDir,"./");
    NhlRLSetString(rlist,NhlNappDefaultParent,"True");
    NhlCreate(&appid,"vc07",NhlappClass,NhlDEFAULT_APP,rlist);
/*
 * Modify the color map.
 */
    cmap[0][0] = 1.0; cmap[0][1] = 1.0; cmap[0][2] = 1.0;
    cmap[1][0] = 0.0; cmap[1][1] = 0.0; cmap[1][2] = 0.0;
    cmap[2][0] = 0.9; cmap[2][1] = 0.9; cmap[2][2] = 0.9;
    cmap[3][0] = 0.6; cmap[3][1] = 0.6; cmap[3][2] = 0.6;
    cmap[4][0] = 0.3; cmap[4][1] = 0.3; cmap[4][2] = 0.3;
    cmap[5][0] = 0.8; cmap[5][1] = 0.9; cmap[5][2] = 1.0;
    cmap[6][0] = 0.5; cmap[6][1] = 0.0; cmap[6][2] = 0.5;
    cmap[7][0] = 0.0; cmap[7][1] = 0.5; cmap[7][2] = 0.7;
    cmap[8][0] = 0.0; cmap[8][1] = 0.0; cmap[8][2] = 0.0;
    cmap[9][0] = 0.00000; cmap[9][1] = 1.00000; cmap[9][2] = 0.00000;
    cmap[10][0] = 0.14286; cmap[10][1] = 1.00000; cmap[10][2] = 0.00000;
    cmap[11][0] = 0.28571; cmap[11][1] = 1.00000; cmap[11][2] = 0.00000;
    cmap[12][0] = 0.42857; cmap[12][1] = 1.00000; cmap[12][2] = 0.00000;
    cmap[13][0] = 0.57143; cmap[13][1] = 1.00000; cmap[13][2] = 0.00000;
    cmap[14][0] = 0.71429; cmap[14][1] = 1.00000; cmap[14][2] = 0.00000;
    cmap[15][0] = 0.85714; cmap[15][1] = 1.00000; cmap[15][2] = 0.00000;
    cmap[16][0] = 1.00000; cmap[16][1] = 1.00000; cmap[16][2] = 0.00000;
    cmap[17][0] = 1.00000; cmap[17][1] = 0.85714; cmap[17][2] = 0.00000;
    cmap[18][0] = 1.00000; cmap[18][1] = 0.71429; cmap[18][2] = 0.00000;
    cmap[19][0] = 1.00000; cmap[19][1] = 0.57143; cmap[19][2] = 0.00000;
    cmap[20][0] = 1.00000; cmap[20][1] = 0.42857; cmap[20][2] = 0.00000;
    cmap[21][0] = 1.00000; cmap[21][1] = 0.28571; cmap[21][2] = 0.00000;
    cmap[22][0] = 1.00000; cmap[22][1] = 0.14286; cmap[22][2] = 0.00000;
    cmap[23][0] = 1.00000; cmap[23][1] = 0.00000; cmap[23][2] = 0.00000;
    len_dims[0] = NCOLORS; len_dims[1] = 3;

    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./vc07c.ncgm");
        NhlRLSetMDFloatArray(rlist,NhlNwkColorMap,&cmap[0][0],2,len_dims);
        NhlCreate(&wid,"vc07Work",
                  NhlncgmWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlRLSetMDFloatArray(rlist,NhlNwkColorMap,&cmap[0][0],2,len_dims);
        NhlCreate(&wid,"vc07Work",NhlcairoWindowWorkstationClass,appid,rlist);
    }

    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPSFileName,"vc07c.ps");
        NhlRLSetMDFloatArray(rlist,NhlNwkColorMap,&cmap[0][0],2,len_dims);
        NhlCreate(&wid,"vc07Work",NhlpsWorkstationClass,appid,rlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPDFFileName,"vc07c.pdf");
        NhlRLSetMDFloatArray(rlist,NhlNwkColorMap,&cmap[0][0],2,len_dims);
        NhlCreate(&wid,"vc07Work",NhlpdfWorkstationClass,appid,rlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"vc07c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlRLSetMDFloatArray(rlist,NhlNwkColorMap,&cmap[0][0],2,len_dims);
        NhlCreate(&wid,"vc07Work",NhlcairoDocumentWorkstationClass,appid,rlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"vc07c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlRLSetMDFloatArray(rlist,NhlNwkColorMap,&cmap[0][0],2,len_dims);
        NhlCreate(&wid,"vc07Work",NhlcairoImageWorkstationClass,appid,rlist);
    }
/*
 * Read the data file.
 */
    sprintf( filename, "%s/fcover.dat", dir );
    fd = fopen(filename,"r");
    for( i = 0; i < MSIZE; i++ ) {
        for( j = 0; j < NSIZE; j++ ) {
            fscanf( fd, "%g", &U[i][j] );
            fscanf( fd, "%c", &ch );
        }
    }
    for( i = 0; i < MSIZE; i++ ) {
        for( j = 0; j < NSIZE; j++ ) {
            fscanf( fd, "%g", &V[i][j] );
            fscanf( fd, "%c", &ch );
        }
    }
    for( i = 0; i < MSIZE; i++ ) {
        for( j = 0; j < NSIZE; j++ ) {
            fscanf( fd, "%g", &P[i][j] );
            fscanf( fd, "%c", &ch );
        }
    }
/*
 * Massage the data to eliminate surplus of vectors near the pole
 */
    for( j = 1; j <= NROWS; j++ ) {
        for( i = 1; i <= MSIZE; i++ ) {
            if ( i % ithin[j-1] != 0) {
                U[NSIZE-j][i-1] = -9999.0;
            }
        }
    }
/*
 * Create a MapPlot object.
 */
    NhlCreate(&mpid,"mapplot",NhlmapPlotClass,wid,0);
/*
 * Create a ScalarField.
 */
    len_dims[0] = MSIZE;
    len_dims[1] = NSIZE;
    NhlRLClear(rlist);
    NhlRLSetMDFloatArray(rlist,NhlNsfDataArray,&P[0][0],2,len_dims);
    NhlRLSetFloat(rlist,NhlNsfXCStartV,-180.);
    NhlRLSetFloat(rlist,NhlNsfYCStartV,-90.);
    NhlRLSetFloat(rlist,NhlNsfXCEndV,180.);
    NhlRLSetFloat(rlist,NhlNsfYCEndV,90.);
    NhlRLSetFloat(rlist,NhlNsfMissingValueV,-9999.0);
    NhlCreate(&sfield,"ScalarField",NhlscalarFieldClass,appid,rlist);
/*
 * Create a VectorField.
 */
    NhlRLClear(rlist);
    NhlRLSetMDFloatArray(rlist,NhlNvfUDataArray,&U[0][0],2,len_dims);
    NhlRLSetMDFloatArray(rlist,NhlNvfVDataArray,&V[0][0],2,len_dims);
    NhlRLSetFloat(rlist,NhlNvfXCStartV,-180.);
    NhlRLSetFloat(rlist,NhlNvfYCStartV,-90.);
    NhlRLSetFloat(rlist,NhlNvfXCEndV,180.);
    NhlRLSetFloat(rlist,NhlNvfYCEndV,90.);
    NhlRLSetFloat(rlist,NhlNvfMissingUValueV,-9999.0);
    NhlRLSetFloat(rlist,NhlNvfMissingVValueV,-9999.0);
    NhlCreate(&vfield,"VectorField",NhlvectorFieldClass,appid,rlist);
/*
 * Create a VectorPlot object.
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNvcUseScalarArray,"true");
    NhlRLSetInteger(rlist,NhlNvcVectorFieldData,vfield);
    NhlRLSetInteger(rlist,NhlNvcScalarFieldData,sfield);
    NhlCreate(&vcid,"vectorplot",NhlvectorPlotClass,wid,rlist);

    NhlRLClear(grlist);
    NhlRLGetFloat(grlist,NhlNvcMinMagnitudeF,&vmin);
    NhlRLGetFloat(grlist,NhlNvcMaxMagnitudeF,&vmax);
    NhlGetValues(vcid,grlist);
    
    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNvcMinMagnitudeF,vmin + 0.1 * (vmax-vmin));
    NhlSetValues(vcid,rlist);
/*
 * Create a ContourPlot object.
 */
    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNcnScalarFieldData,sfield);
    NhlCreate(&cnid,"contourplot",NhlcontourPlotClass,wid,rlist);
/*
 * Overlay the vectors and the contours on the map and draw
 * everything.
 */
    NhlDraw(mpid);
    NhlFrame(wid);

    NhlDraw(cnid);
    NhlFrame(wid);

    NhlDraw(vcid);
    NhlFrame(wid);

    NhlAddOverlay(mpid,vcid,-1);
    NhlAddOverlay(mpid,cnid,-1);

    NhlDraw(mpid);
    NhlFrame(wid);
/* 
 *  Destroy the workstation object and exit.
 */
    NhlDestroy(wid);
    NhlClose();
    exit(0);
}

