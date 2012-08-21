/*
 *      $Id: vc02c.c,v 1.6 2010-03-15 22:49:25 haley Exp $
 */
/***********************************************************************
*                                                                      *
*                Copyright (C)  1993                                   *
*        University Corporation for Atmospheric Research               *
*                All Rights Reserved                                   *
*                                                                      *
***********************************************************************/
/*
 *  File:       vc02c.c
 *
 *  Author:     David Brown
 *              National Center for Atmospheric Research
 *              PO 3000, Boulder, Colorado
 *
 *  Date:       Wed Apr  3 17:00:55 MST 1996
 *
 *   Description:   Given a simple mathematically generated data set,
 *		    demonstrates line-drawn vector arrows and the use
 *                  of some basic VectorPlot resources
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


#define M 30
#define N 25
#define PI  3.14159    

int main(int argc, char *argv[])
{
    char const *wks_type = "x11";
    int appid,wid,vcid,vfid;
    int rlist,grlist;
    ng_size_t len_dims[2];
    float reflen;
    float U[N][M],V[N][M];

/*
 * Generate vector data arrays
 */
    {
	    float igrid, jgrid;
	    int i,j;
	    igrid = 2.0 * PI / (float) M;
	    jgrid = 2.0 * PI / (float) N;
	    for (j = 0; j < N; j++) {
		    for (i = 0; i < M; i++) {
			    U[j][i] = 10.0 * cos(jgrid * (float) j);
			    V[j][i] = 10.0 * cos(igrid * (float) i);
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
    NhlCreate(&appid,"vc02",NhlappClass,NhlDEFAULT_APP,rlist);

    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(rlist);
	NhlRLSetString(rlist,NhlNwkColorMap,"temp1");
        NhlRLSetString(rlist,NhlNwkMetaName,"./vc02c.ncgm");
        NhlCreate(&wid,"vc02Work",
                  NhlncgmWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X workstation.
 */
        NhlRLClear(rlist);
	NhlRLSetString(rlist,NhlNwkColorMap,"temp1");
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlCreate(&wid,"vc02Work",NhlcairoWindowWorkstationClass,appid,rlist);
    }

    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(rlist);
	NhlRLSetString(rlist,NhlNwkColorMap,"temp1");
        NhlRLSetString(rlist,NhlNwkPSFileName,"vc02c.ps");
        NhlCreate(&wid,"vc02Work",NhlpsWorkstationClass,appid,rlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(rlist);
	NhlRLSetString(rlist,NhlNwkColorMap,"temp1");
        NhlRLSetString(rlist,NhlNwkPDFFileName,"vc02c.pdf");
        NhlCreate(&wid,"vc02Work",NhlpdfWorkstationClass,appid,rlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(rlist);
	NhlRLSetString(rlist,NhlNwkColorMap,"temp1");
        NhlRLSetString(rlist,NhlNwkFileName,"vc02c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"vc02Work",NhlcairoDocumentWorkstationClass,appid,rlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(rlist);
	NhlRLSetString(rlist,NhlNwkColorMap,"temp1");
        NhlRLSetString(rlist,NhlNwkFileName,"vc02c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"vc02Work",NhlcairoImageWorkstationClass,appid,rlist);
    }
/*
 * Create a VectorField data object using the data set defined above.
 * By default the array bounds will define the data boundaries (zero-based,
 * as in C language conventions)
 */

    len_dims[0] = N;
    len_dims[1] = M;
    NhlRLClear(rlist);
    NhlRLSetMDFloatArray(rlist,NhlNvfUDataArray,&U[0][0],2,len_dims);
    NhlRLSetMDFloatArray(rlist,NhlNvfVDataArray,&V[0][0],2,len_dims);
    NhlCreate(&vfid,"vectorfield",NhlvectorFieldClass,appid,rlist);

/*
 * Create a VectorPlot object, supplying the VectorField object as data
 * Setting vcMonoLineArrowColor False causes VectorPlot to color the
 * vector arrows individually, based, by default, on each vector's magnitude.
 */

    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtiMainString,
		   "Line-Drawn Vectors (colored by magnitude)");

    NhlRLSetString(rlist,NhlNvcMonoLineArrowColor,"false");

    NhlRLSetInteger(rlist,NhlNvcVectorFieldData,vfid);
    NhlCreate(&vcid,"vectorplot",NhlvectorPlotClass,wid,rlist);

    NhlDraw(vcid);
    NhlFrame(wid);

/*
 * All the vector arrows are scaled in length based on their magnitude
 * relative to a reference magnitude (the maximum magnitude by default)
 * Setting the arrow length of the reference magnitude, therefore, adjusts
 * the length of all the vector arrows in the plot. In addition, you can
 * specify a length for the smallest vector in the plot, as a fraction of
 * the length of the reference vector. In this case, the remaining arrows
 * are scaled based on both these lengths.
 * Since the initial reference length is established dynamically, the 
 * simplest way to adjust the overall arrow length is to 
 * retrieve the initially set value of 'vcRefLengthF' and then multiply 
 * by the desired factor.
 */

    NhlRLClear(grlist);
    NhlRLGetFloat(grlist,NhlNvcRefLengthF,&reflen);
    NhlGetValues(vcid,grlist);

    reflen *= 1.5;    
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtiMainString,
		   "Adjusting the Reference and Minimum Length");
    NhlRLSetFloat(rlist,NhlNvcRefLengthF,reflen);
    NhlRLSetFloat(rlist,NhlNvcMinFracLengthF,0.3);
    NhlSetValues(vcid,rlist);

    NhlDraw(vcid);
    NhlFrame(wid);

/* 
 * Note that setting the reference magnitude also affects the length
 * of the arrows. In this case it is an inverse relationship.
 */

    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtiMainString,
		   "Adjusting the Reference Magnitude");
    NhlRLSetFloat(rlist,NhlNvcRefMagnitudeF,20.0);
    NhlSetValues(vcid,rlist);

    NhlDraw(vcid);
    NhlFrame(wid);

/*
 * There are two vector annotations known as the reference vector 
 * annotation and the minimum vector annotation. Their resources are prefixed
 * respectively by "vcRefAnno" and "vcMinAnno". Only the reference vector
 * annotation is displayed by default. Here, the minimum vector annotation
 * is turned on, and some resources of each annotation are modified.
 */

    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtiMainString,
		   "Modifying the Vector Annotations");
    NhlRLSetFloat(rlist,NhlNvcRefAnnoFontHeightF,0.015);
    NhlRLSetString(rlist,NhlNvcRefAnnoString1,"$VMG$ meters/sec");
    NhlRLSetString(rlist,NhlNvcRefAnnoString2On,"False");
    NhlRLSetString(rlist,NhlNvcMinAnnoOn,"True");
    NhlRLSetString(rlist,NhlNvcMinAnnoString1,"$VMG$ meters/sec");
    NhlRLSetString(rlist,NhlNvcMinAnnoString2On,"False");
    NhlSetValues(vcid,rlist);

    NhlDraw(vcid);
    NhlFrame(wid);

/*
 * Line-drawn arrowheads are sized proportionally to the arrow length 
 * unless the resulting size would be outside the limits defined by 
 * the arrowhead minimum and maximum size resources. Setting the 
 * minimum and maximum sizes to the same value causes all the
 */

    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtiMainString,"Uniformly-sized Arrow Heads");
    NhlRLSetFloat(rlist,NhlNvcLineArrowHeadMinSizeF,0.01);
    NhlRLSetFloat(rlist,NhlNvcLineArrowHeadMaxSizeF,0.01);
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
