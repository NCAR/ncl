/*
 *      $Id: st01c.c,v 1.5 2010-03-15 22:49:24 haley Exp $
 */
/***********************************************************************
*                                                                      *
*                Copyright (C)  1996                                   *
*        University Corporation for Atmospheric Research               *
*                All Rights Reserved                                   *
*                                                                      *
***********************************************************************/
/*
 *  File:       st01c.c
 *
 *  Author:     David Brown
 *          National Center for Atmospheric Research
 *          PO 3000, Boulder, Colorado
 *
 *  Date:       Wed Apr  3 17:00:55 MST 1996
 *
 *  Description:  Basic StreamlinePlot example
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
#include <ncarg/hlu/StreamlinePlot.h>


#define N 30
#define M 25
#define PI  3.14159    

int main(int argc, char *argv[])
{

    char const *wks_type = "x11";
    int appid,wid,stid,vfid;
    int rlist;
    ng_size_t len_dims[2];
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
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNappUsrDir,"./");
    NhlCreate(&appid,"st01",NhlappClass,NhlDEFAULT_APP,rlist);

    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./st01c.ncgm");
        NhlCreate(&wid,"st01Work",
                  NhlncgmWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlCreate(&wid,"st01Work",NhlcairoWindowWorkstationClass,appid,rlist);
    }

    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPSFileName,"st01c.ps");
        NhlCreate(&wid,"st01Work",NhlpsWorkstationClass,appid,rlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPDFFileName,"st01c.pdf");
        NhlCreate(&wid,"st01Work",NhlpdfWorkstationClass,appid,rlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"st01c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"st01Work",NhlcairoDocumentWorkstationClass,appid,rlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"st01c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"st01Work",NhlcairoImageWorkstationClass,appid,rlist);
    }

/*
 * Create a VectorField object; then use its id as the value of
 * the 'stVectorFieldData' resource when creating the StreamlinePlot object.
 */

    len_dims[0] = N;
    len_dims[1] = M;
    NhlRLClear(rlist);
    NhlRLSetMDFloatArray(rlist,NhlNvfUDataArray,&U[0][0],2,len_dims);
    NhlRLSetMDFloatArray(rlist,NhlNvfVDataArray,&V[0][0],2,len_dims);
    NhlCreate(&vfid,"vectorfield",NhlvectorFieldClass,appid,rlist);

    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtiMainString,"Basic StreamlinePlot Example");
    NhlRLSetInteger(rlist,NhlNstVectorFieldData,vfid);
    NhlCreate(&stid,"streamline",NhlstreamlinePlotClass,wid,rlist);

    NhlDraw(stid);
    NhlFrame(wid);
/*
 * Destroy the objects created, close the HLU library and exit.
 */

    NhlDestroy(appid);
    NhlClose();
    exit(0);
}
