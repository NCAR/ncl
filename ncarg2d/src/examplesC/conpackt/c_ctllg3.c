/*
 *      $Id: c_ctllg3.c,v 1.4 2004-07-23 00:58:24 kennison Exp $
 */

/*
 * Include standard I/O and math libraries.
 */

#include <stdio.h>
#include <math.h>

/*
 * Include GKS definitions and prototypes.
 */

#include <ncarg/gks.h>

/*
 * Include NCAR Graphics function prototypes.
 */

#include <ncarg/ncargC.h>

/*
 * Define the workstation type and identifier.
 */

#define IWTYPE 1
#define WSID 1

/*
 * Define the dimensions of the original rectangular data arrays.
 */

#define IDIM 181                     /* "horizontal" dimension         */
#define JDIM 91                      /* "vertical" dimension           */
#define IDM1 (IDIM-1)                /* "horizontal" dimension, less 1 */
#define JDM1 (JDIM-1)                /* "vertical" dimension, less 1   */

/*
 * Define the lengths of the nodes that define the triangular mesh.
 */

#define LOPN 5                       /* length of a point node         */
#define LOEN 5                       /* length of an edge node         */
#define LOTN 4                       /* length of a triangle node      */

/*
 * Compute the dimensions of the arrays defining the triangular mesh.
 */

#define MNOP IDIM*JDIM               /* maximum number of points       */
#define MNOE (3*IDM1*JDM1+IDM1+JDM1) /* maximum number of edges        */
#define MNOT 2*IDM1*JDM1             /* maximum number of triangles    */
#define MPNT MNOP*LOPN               /* space for points               */
#define MEDG MNOE*LOEN               /* space for edges                */
#define MTRI MNOT*LOTN               /* space for triangles            */

/*
 * Define a constant to use in converting angles from degrees to radians.
 */

#define DTOR .017453292519943

/*
 * Define the lengths of workspaces needed by CONPACKT.
 */

#define LRWK 10000
#define LIWK 1000

/*
 * Define the length of an area map to be used and the lengths of some
 * scratch arrays needed by ARSCAM.
 */

#define LAMA 200000
#define NCRA LAMA/10
#define NGPS 2

/*
 * Define the interface to a function that defines how the rectangular
 * mesh wraps around the globe.
 */

int millg3(int *idim,int *jdim,int *iini,int *jini,int *iino,int *jino);

/*
 * Define the interface to a function to be called by ARSCAM to color the
 * areas defined by the area map.
 */

int dcfocb(float *xcra,float *ycra,int *ncra,int *iaai,int *iagi,int *ngps);

/*
 * Define the interface to a function that defines a group of interpolated
 * colors associated with a particular group of color indices.
 */

void dfclrs(int iwid,int iofc,int iolc,float redf,float grnf,float bluf,
                                       float redl,float grnl,float blul);

/*
 * Define the interface to the GKS routine GFA, in FORTRAN.
 */

extern void NGCALLF(gfa,GFA)(int *ncra,float *xcra,float *ycra);

/*
 * Start of program.
 */

main()
{

/*
 * Declare variables to be used below.
 */

    float           rlat[JDIM][IDIM];      /* latitudes          */
    float           rlon[JDIM][IDIM];      /* longitudes         */
    float           clat,clon;             /* center lat and lon */
    float           rdat[JDIM][IDIM];      /* contour field      */
    int             iscr[JDIM][IDIM][4];   /* scratch array      */
    float           rpnt[MPNT];            /* point array        */
    int             iedg[MEDG];            /* edge array         */
    int             itri[MTRI];            /* triangle array     */
    int             npnt,nedg,ntri;        /* array lengths      */
    float           rwrk[LRWK];            /* real workspace     */
    int             iwrk[LIWK];            /* integer workspace  */
    Gcolr_rep       rgbs[8]={{1.,1.,1.},   /* white (background) */
                             {0.,0.,0.},   /* black (foreground) */
                             {1.,0.,0.},   /* red                */
                             {0.,1.,0.},   /* green              */
                             {0.,0.,1.},   /* blue               */
                             {0.,1.,1.},   /* cyan               */
                             {1.,0.,1.},   /* magenta            */
                             {1.,1.,0.}};  /* yellow             */
    int             iama[LAMA];            /* area map array     */
    float           xcra[NCRA],ycra[NCRA]; /* coordinate arrays  */
    int             iaai[NGPS],iagi[NGPS]; /* area id arrays     */
    int             i,j;                   /* indices            */

/*
 * Fill the rectangular data arrays.
 */

    for (i=0;i<IDIM;i++) {
      for (j=0;j<JDIM;j++) {
        float xdat,ydat,zdat;
        rlat[j][i]= -90.+((double)j/(double)JDM1)*180.;
        rlon[j][i]=-180.+((double)i/(double)IDM1)*360.;
/*      rdat[j][i]=cos(3.*DTOR*rlat[j][i])*cos(3.*DTOR*rlon[j][i]);   */
        xdat=cos(DTOR*rlat[j][i])*cos(DTOR*rlon[j][i]);
        ydat=cos(DTOR*rlat[j][i])*sin(DTOR*rlon[j][i]);
        zdat=sin(DTOR*rlat[j][i]);
/*      rdat[j][i]=sqrt(pow(xdat-.5,2)+pow(ydat-.5,2)+pow(zdat-.5,2)); */
        rdat[j][i]=pow(xdat-.5,2)-pow(ydat-.5,2)-pow(zdat-.5,2);
      }
    }

/*
 * Set the latitude and longitude of the approximate center of the mesh.
 */

    clat=rlat[JDIM/2][IDIM/2];
    clon=rlon[JDIM/2][IDIM/2];

/*
 * Initialize GKS.
 */

    gopen_gks("stdout",0);
    gopen_ws(WSID,NULL,IWTYPE);
    gactivate_ws(WSID);

/*
 * Define the first eight color indices.
 */

    for(i=0;i<8;i++) {
        gset_colr_rep(WSID,i,&rgbs[i]);
    }

/*
 * Define 100 colors, associated with color indices 151 through 250, to
 * be used for color-filled contour bands and in cell arrays, ranging
 * from blue to red.
 */
    dfclrs(WSID,151,250,0.,0.,1.,1.,0.,0.);

/*
 * Call a general-purpose subroutine that accepts a rectangular grid
 * mapped onto the surface of the globe and returns a triangular mesh
 * derived from it.
 */

    c_cttmrg(IDIM,JDIM,
             &rlat[0][0],&rlon[0][0],&rdat[0][0],&iscr[0][0][0],
             0.,millg3,
             rpnt,MPNT,&npnt,LOPN,
             iedg,MEDG,&nedg,LOEN,
             itri,MTRI,&ntri,LOTN);

    printf("Number of points:    %d\n",npnt/LOPN);
    printf("Number of edges:     %d\n",nedg/LOEN);
    printf("Number of triangles: %d\n",ntri/LOTN);

/*
 * Set internal parameters in the utilities.
 */

    c_ctseti("MAP - MAPPING FLAG",1);
    c_ctsetr("ORV - OUT-OF-RANGE VALUE",1.e12);
    c_ctseti("PAI - PARAMETER ARRAY INDEX",-1);
    c_ctseti("CLU - CONTOUR LEVEL USE FLAG",1);
    c_ctseti("AIA - AREA IDENTIFIER FOR AREA",1001);
    c_ctseti("LLP - LINE LABEL POSITIONING FLAG",1);
    c_ctseti("HLR - HIGH/LOW SEARCH RADIUS",.075);
    c_ctseti("HLO - HIGH/LOW LABEL OVERLAP FLAG",11);
    c_ctseti("CAF - CELL ARRAY FLAG",-1);
    c_ctseti("SET - DO-SET-CALL FLAG",0);

    c_mpseti("PE - PERIMETER FLAG",0);
    c_mpseti("DA - DASH PATTERN",65535);

    c_pcseti("FN - FONT NUMBER",25);
    c_pcseti("OF - OUTLINE FLAG",1);

/*
 * Tell EZMAP what map projection to use and how to position it on the
 * globe and then initialize the package.
 */

    c_maproj("OR",clat+45.,clon,0.);
    c_mapint();

/*
 * Initialize CONPACKT.
 */

    c_ctmesh(rpnt,npnt,LOPN,
             iedg,nedg,LOEN,
             itri,ntri,LOTN,
             rwrk,LRWK,
             iwrk,LIWK);

/*
 * Initialize an area map, put contour lines into it, and then scan it
 * to produce colored contour bands.
 */

    c_arinam(iama,LAMA);
    c_ctclam(rpnt,iedg,itri,rwrk,iwrk,iama);
    c_arscam(iama,xcra,ycra,NCRA,iaai,iagi,NGPS,dcfocb);

/*
 * Draw a map, in cyan.
 */

    gset_line_colr_ind(5);
    c_mapdrw();

/*
 * Draw contour lines, in the foreground color.
 */

    gset_line_colr_ind(1);
    c_ctcldr(&rpnt[0],&iedg[0],&itri[0],&rwrk[0],&iwrk[0]);

/*
 * Advance the frame.
 */

    c_frame ();

/*
 * Close gks.
 */

    gdeactivate_ws(WSID);
    gclose_ws(WSID);
    gclose_gks();

}

/*
 * Given the dimensions, IDIM and JDIM, of a simple lat/lon grid on the
 * globe, and the indices, IINI and JINI, of a point on the grid, the
 * function MILLG3 returns the indices, IINO and JINO, of that coincident
 * point on the grid which is to be used to represent it.  This version
 * assumes that the first and last rows of the grid each map into a single
 * point (perhaps the south pole and the north pole, respectively) and
 * that the right and left edges of the grid lie on top of each other.
 * The dimensions and indices are to be interpreted as in FORTRAN:
 *
 *   1 .LE. IINI,IINO .LE. IDIM and 1 .LE. JINI,JINO .LE. JDIM
 */

int millg3(int *idim,int *jdim,int *iini,int *jini,int *iino,int *jino) {

  if (*jini==1) {
    *iino=1;
    *jino=1;
  }

  else if (*jini==*jdim) {
    *iino=1;
    *jino=*jdim;
  }

  else if (*iini==*idim) {
    *iino=1;
    *jino=*jini;
  }

  else {
    *iino=*iini;
    *jino=*jini;
  }

  return 0;

}



/*
 * The routine DCFOCB fills the area defined by the points (XCRA(I),YCRA(I)),
 * for I = 1 to NCRA, if and only if none of the area identifiers for the area
 * are negative.  The color used is determined from the area identifier of the
 * area relative to group 3; it is assumed that 100 colors are defined having
 * color indices 151 through 250.
 */

int dcfocb(float *xcra,float *ycra,int *ncra,int *iaai,int *iagi,int *ngps) {

    int nocl,iai3,i;

/*
 * Retrieve the number of contour levels being used.
 */

    c_ctgeti("NCL - NUMBER OF CONTOUR LEVELS",&nocl);

/*
 * If the number of contour levels is non-zero and the area has more than
 * two points, fill it.  The calls to GFA are sort of cheating; one really
 * the routine "gfill_area", in the C interface to GKS, but doing so would
 * require a lot of effort to create the required structure.
 */
    if (nocl!=0&&*ncra>2) {

        iai3=-1;

        for (i=0;i<*ngps;i++) if (iagi[i]==3) iai3=iaai[i];

        if (iai3>=1&&iai3<=nocl+1) {
            gset_fill_colr_ind(151+(int)
                                   (((float)iai3-.5)/(float)(nocl+1)*100.));
            NGCALLF(gfa,GFA)(ncra,xcra,ycra);
        }
        else if (iai3==1001) {
            gset_fill_colr_ind(2);
            NGCALLF(gfa,GFA)(ncra,xcra,ycra);
        }
        else if (iai3==1002) {
            gset_fill_colr_ind(3);
            NGCALLF(gfa,GFA)(ncra,xcra,ycra);
        }

    }

    return 0;

}



/*
 * The routine DFCLRS defines color indices IOFC through IOLC on workstation
 * IWID by interpolating values from REDF/GRNF/BLUF to REDL/GRNL/BLUL.
 */

void dfclrs(int wsid,int iofc,int iolc,float redf,float grnf,float bluf,
                                       float redl,float grnl,float blul)
{

    int       i;
    float     p,q;
    Gcolr_rep rgbv;

    for (i=iofc;i<=iolc;i++) {
        p=(float)(iolc-i)/(float)(iolc-iofc);
        q=(float)(i-iofc)/(float)(iolc-iofc);
        rgbv.rgb.red  =p*redf+q*redl;
        rgbv.rgb.green=p*grnf+q*grnl;
        rgbv.rgb.blue =p*bluf+q*blul;
        gset_colr_rep(wsid,i,&rgbv);
    }

    return;

}
