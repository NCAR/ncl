/*
 *      
 */
/***********************************************************************
*                                                                      *
*                Copyright (C)  1993                                   *
*        University Corporation for Atmospheric Research               *
*                All Rights Reserved                                   *
*                                                                      *
***********************************************************************/
/*
 *  File:       vc04c.c
 *
 *  Author:     David Brown (converted to C by Lynn Hermanson)
 *              National Center for Atmospheric Research
 *              PO 3000, Boulder, Colorado
 *
 *  Date:       June 13, 1996
 *
 *
 * Description: Manipulates the FillArrow resources to demonstrate some
 *               of the possible stylistic variations on the appearance
 *               of the filled vector arrows.
 *               The data is extracted from an NMC forecast dataset for
 *               11/10/1994.
 */

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
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


#define a 2
#define b 37
#define c 37

int main(int argc, char *argv[])
{
    char const *wks_type = "x11";
    int appid,wid,vcid,vfid;
    int rlist,grlist;
    ng_size_t len_dims[3];
    float reflen, ref, tiheight, tiht;
    float x[a][b][c];
    FILE * fd;
    int i,j,k;
/*
 * Generate vector data array
 */
    char  filename[256];   
    const char *dir = _NGGetNCARGEnv("data");
    sprintf( filename, "%s/asc/uvdata0.asc", dir );

    fd = fopen(filename,"r");
       
    for (k = 0; k < a; k++) {
        for (j = 0; j < b; j++) {
            for (i = 0; i < c; i++) {       
                fscanf(fd,"%f", &x[k][j][i]);
            }
        }
    }

/*
 * Initialize the high level utility library
 */
    NhlInitialize();
/*
 * Create an application context. Set the app dir to the current
 * directory so the application looks for a resource file in the working
 * directory. 
 */
    rlist = NhlRLCreate(NhlSETRL);
    grlist = NhlRLCreate(NhlGETRL);
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNappUsrDir,"./");
    NhlCreate(&appid,"vc04",NhlappClass,NhlDEFAULT_APP,rlist);

    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./vc04c.ncgm");
        NhlCreate(&wid,"vc04Work",
                  NhlncgmWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlCreate(&wid,"vc04Work",NhlcairoWindowWorkstationClass,appid,rlist);
    }

    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPSFileName,"vc04c.ps");
        NhlCreate(&wid,"vc04Work",NhlpsWorkstationClass,appid,rlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPDFFileName,"vc04c.pdf");
        NhlCreate(&wid,"vc04Work",NhlpdfWorkstationClass,appid,rlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"vc04c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"vc04Work",NhlcairoDocumentWorkstationClass,appid,rlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"vc04c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"vc04Work",NhlcairoImageWorkstationClass,appid,rlist);
    }
/*
 * Create a VectorField data object using the data set defined above.
 * By default the array bounds will define the data boundaries (zero-based,
 * as in C language conventions)
 */

    len_dims[0] = a;
    len_dims[1] = b;
    len_dims[2] = c;
    NhlRLClear(rlist);
    NhlRLSetMDFloatArray(rlist,NhlNvfDataArray,&x[0][0][0],3,len_dims);
    NhlRLSetFloat(rlist,NhlNvfXCStartV, -180.0);
    NhlRLSetFloat(rlist,NhlNvfXCEndV, 0.0);
    NhlRLSetFloat(rlist,NhlNvfYCStartV, 0.0);
    NhlRLSetFloat(rlist,NhlNvfYCEndV, 90.0);
    NhlRLSetFloat(rlist,NhlNvfYCStartSubsetV, 20.0);
    NhlRLSetFloat(rlist,NhlNvfYCEndSubsetV, 80.0);
   
    NhlCreate(&vfid,"vectorfield",NhlvectorFieldClass,appid,rlist);

/*
 * Create a VectorPlot object, supplying the VectorField object as data
 * Setting vcMonoFillArrowFillColor False causes VectorPlot to color the
 * vector arrows individually based, by default, on the vector magnitude.
 */

    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNvcRefMagnitudeF, 20.0);
    NhlRLSetString(rlist,NhlNvcMonoFillArrowFillColor, "False");
    NhlRLSetString(rlist,NhlNvcFillArrowsOn, "True");
    NhlRLSetFloat(rlist,NhlNvcMinFracLengthF, 0.25);

    NhlRLSetInteger(rlist,NhlNvcVectorFieldData,vfid);

    NhlCreate(&vcid,"vectorplot",NhlvectorPlotClass,wid,rlist);



    NhlRLClear(grlist);
    NhlRLGetFloat(grlist,NhlNvcRefLengthF,&reflen);
    NhlRLGetFloat(grlist,NhlNtiMainFontHeightF,&tiheight);
    NhlGetValues(vcid,grlist);

    ref= 1.75 * reflen;
    tiht = 0.9 * tiheight;
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtiMainString,"How to Rotate a VectorPlot 90:F34:0:F:");
    NhlRLSetFloat(rlist,NhlNvcRefLengthF,ref);
    NhlRLSetFloat(rlist,NhlNtiMainFontHeightF,tiht);
    NhlSetValues(vcid,rlist);

    NhlDraw(vcid);
    NhlFrame(wid);



    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtiMainString,
           "1:: Exchange the Dimensions");
    NhlSetValues(vcid,rlist);

    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNvfExchangeDimensions, "True");
    NhlSetValues(vfid,rlist);

    NhlDraw(vcid);
    NhlFrame(wid);

    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtiMainString,"2:: Exchange the U and V Data");
    NhlSetValues(vcid,rlist);

    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNvfExchangeUVData,"True");
    NhlSetValues(vfid,rlist);

    NhlDraw(vcid);
    NhlFrame(wid);

    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtiMainString,
                   "3a:: Reverse the Y-Axis for Clockwise Rotation");
    NhlRLSetString(rlist,NhlNtrYReverse, "True");
    NhlSetValues(vcid,rlist);

    NhlDraw(vcid);
    NhlFrame(wid);

    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtiMainString,
                   "3b:: Or the X-Axis for Counter-Clockwise Rotation");
    NhlRLSetString(rlist,NhlNtrYReverse, "False");
    NhlRLSetString(rlist,NhlNtrXReverse, "True");
    NhlSetValues(vcid,rlist);

    NhlDraw(vcid);
    NhlFrame(wid);
/*
 * Destroy the objects created, close the HLU library and exit.
 */
    NhlDestroy(appid);
    NhlClose();
    exit(0);
}
