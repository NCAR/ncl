/*
 * Draws a uniform field over an azimuthal projection of the globe
 * Continental boundaries are filled with a grayscale value
 *
 */
#include <stdio.h>

/*
 * Include function prototypes here
 */
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define LMAP    150000
#define NWRK    10000
#define ISIZ    5
#define M  18
#define N  18
#define NCLRS 4

int ifilix = 196;

#define WSTYPE  SED_WSTYPE
#define WKID    1

main()
{
    int ival;
    float rval;
    char stmp[11];
    extern int colram(
#ifdef NeedFuncProto
        float *xcs,
        float *ycs,
        int *ncs,
        int *iai,
        int *iag,
        int *nai
#endif
    );
    extern int vvudmv(
#ifdef NeedFuncProto
        float *xcs,
        float *ycs,
        int *ncs,
        int *iai,
        int *iag,
        int *nai
#endif
    );
    extern int stumsl(
#ifdef  NeedFuncProto
        float *xcs,
        float *ycs,
        int *ncs,
        int *iai,
        int *iag,
        int *nai
#endif
    );
    int i, j, idm[1], map[LMAP], iarea[ISIZ], igrp[ISIZ],iclr[NCLRS];
    float rdm[1], xwrk[NWRK], ywrk[NWRK];
    float a[N][M],b[N][M],wrk[M*N*2];
    float p1[2],p2[2],p3[2],p4[2];
    Gcolr_rep rgbv[NCLRS];

    iclr[0] = 2;
    iclr[1] = 3;
    iclr[2] = 64;
    iclr[3] = 196;
    rgbv[0].rgb.red = 0.0;
    rgbv[0].rgb.green = 1.0;
    rgbv[0].rgb.blue = 1.0;
    rgbv[1].rgb.red = 0.75;
    rgbv[1].rgb.green = 0.0;
    rgbv[1].rgb.blue = 1.0;
    rgbv[2].rgb.red = 0.75;
    rgbv[2].rgb.green = 0.75;
    rgbv[2].rgb.blue = 0.75;
    rgbv[3].rgb.red = 0.5;
    rgbv[3].rgb.green = 0.5;
    rgbv[3].rgb.blue = 0.5;
/*
 *  Open GKS, open and activate the metafile workstation.
 */
    gopen_gks("stdout",0);
    gopen_ws (WKID,NULL, WSTYPE);
    gactivate_ws (WKID);
/*
 * Set up colors for fixed table grayscale and color workstations
 */
    for( i = 0; i < NCLRS; i++ ) {
        gset_colr_rep(WKID,iclr[i],&rgbv[i]);
    }
/*     
 * Generate uniform field intended for polar input mode.
 */
    for( i = 0; i < N; i++ ) {
        for( j = 0; j < M; j++ ) {
            a[i][j] = 1.0;
            b[i][j] = 45.0;
        }
    }
/*
 * Set up the map projection
 */
    c_mapstc ("OU - OUTLINE DATASET SELECTOR","CO");
    c_maproj ("AE",0.0,0.0,0.0);
    p1[0] = p2[0] = p3[0] = p4[0] = 0.;
    p1[1] = p2[1] = p3[1] = p4[1] = 0.;
    c_mapset ("MA",p1,p2,p3,p4);
/*
 * Initialize Maps and Areas
 */
    c_mapint();
    c_arinam (map,LMAP);
    c_mapbla (map);
/*    
 * Color fill land masses using a gray scale value
 */
    gset_fill_int_style (GSTYLE_SOLID);
    c_arscam (map, xwrk, ywrk, NWRK, iarea, igrp, ISIZ, colram);
/*
 * Draw boundaries, including the limb
 */
    gset_line_colr_ind(iclr[2]);
    c_mapsti ("C4 - LIMB COLOR",iclr[2]);
    c_mapsti("LA - LABEL FLAG",0);
    c_mapsti("EL - ELLIPTICAL-PERIMETER SELECTOR",1);
    c_maplbl();
    c_maplot();
/*
 * Set up Streamline parameters
 */
    c_stseti("MAP -- Mapping Flag", 1);
    c_stseti("SET -- Do Set Call Flag", 0);
    c_stsetr("XC1 -- Lower X Bound", -180.0);
    c_stsetr("XCM -- Upper X Bound", 180.0);
    c_stsetr("YC1 -- Lower Y Bound", -90.0);
    c_stsetr("YCN -- Upper Y Bound", 90.0);
    c_stseti("PLR -- Streamline Polar Flag", 1);
    c_stseti("TRP -- Interpolation Method", 1);
    c_stsetr("SSP -- Stream Spacing", 0.005);
    c_stsetr("DFM - Differential Magnitude", 0.005);
/*
 * Set up Vectors parameters
 */
    c_vvseti("MAP -- Mapping Flag", 1);
    c_vvseti("SET -- Do Set Call Flag", 0);
    c_vvsetr("XC1 -- Lower X Bound", -180.0);
    c_vvsetr("XCM -- Upper X Bound", 180.0);
    c_vvsetr("YC1 -- Lower Y Bound", -90.0);
    c_vvsetr("YCN -- Upper Y Bound", 90.0);
    c_vvseti("PLR -- Vector Polar Flag", 1);
    c_vvsetr("VFR -- Vector Fractional Minimum", 0.7);
    c_vvsetc("MNT -- Minimum Vector Text", " ");
    c_vvsetc("MXT -- Maximum Vector Text", " ");
/*
 * Draw Vectors
 */
    idm[0] = 0;
    rdm[0] = 0.0;
    gset_line_colr_ind(iclr[1]);
    c_vvinit((float *)a,M,(float *)b,M,rdm,0,M,N,rdm,0);
    c_vvectr((float *)a,(float *)b,rdm,idm,vvudmv,rdm);
/*
 * Draw Streamlines
 */
    gset_line_colr_ind(iclr[0]);
    c_stinit((float *)a,M,(float *)b,M,rdm,0,M,N,wrk,2*M*N);
    c_stream((float *)a,(float *)b,rdm,idm,stumsl,wrk);

    gset_line_colr_ind(1);
/*
 * Draw a perimeter and eject the frame
 */
    c_perim(1,0,1,0);
    c_frame();
/*
 * Test vvgetr
 */
    c_vvsetr("VFR", 0.8);
    c_vvgetr("VFR", &rval);
    if( rval != 0.8 ) {
        printf( "c_vvgetr test UNSUCCESSFUL\n" );
        printf( "    rval should be 0.8, rval is really %g\n", rval );
    }
    else {
        printf( "c_vvgetr test SUCCESSFUL\n" );
    }
/*
 * Test vvgetc
 */
    c_vvsetc("MNT", "hello");
    c_vvgetc("MNT", stmp,10);
    if( strcmp( stmp, "hello" ) ) {
        printf( "stmp should be 'hello', stmp is really '%s'\n", stmp );
        printf( "c_vvgetc test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_vvgetc test SUCCESSFUL\n" );
    }
/*
 * Test vvgeti
 */
    c_vvseti("PLR", 1);
    c_vvgeti("PLR", &ival);
    if( ival != 1 ) {
        printf( "ival should be 1, ival is really %d\n", ival );
        printf( "c_vvgeti test UNSUCCESSFUL\n" );
    }
    else {
        printf( "c_vvgeti test SUCCESSFUL\n" );
    }
/*
 *     Deactivate and close workstation, close GKS.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}


#ifdef __STDC__
int colram(
    float *xwrk,
    float *ywrk,
    int *nwrk,
    int *iarea,
    int *igrp,
    int *idsiz
)
#else 
int colram (xwrk,ywrk,nwrk,iarea,igrp,idsiz)
    float *xwrk;
    float *ywrk;
    int *nwrk;
    int *iarea;
    int *igrp;
    int *idsiz;
#endif
{
    int id, i;
    Gpoint_list fill_area;
/*
 * Retrieve area id for geographic area
 */
    id = 0;
    for( i = 0; i < *idsiz; i++ ) {
        if (igrp[i] == 1) id = iarea[i];
    }
/*
 * If it's not water, draw it
 */
    if (id >= 1 && c_mapaci(id) != 1) {
        gset_fill_colr_ind(ifilix);
/*
 * Create structure to pass to gfill_area
 */
        fill_area.num_points = *nwrk;
        fill_area.points = (Gpoint *) malloc(*nwrk*sizeof(Gpoint));
        if( !fill_area.points ) {
            fprintf( stderr, "colram: Not enough memory to create fill area structure\n" );
            return(0);
        }
        for( i = 0; i < *nwrk; i++ ) {
            fill_area.points[i].x = xwrk[i];
            fill_area.points[i].y = ywrk[i];
        }
        gfill_area(&fill_area);
        free(fill_area.points);
    }
/*
 * Otherwise, do nothing
 */
    return(1);
}

#ifdef __STDC__
int vvudmv(
    float *xcs,
    float *ycs,
    int *ncs,
    int *iai,
    int *iag,
    int *nai
)
#else
int vvudmv(xcs,ycs,ncs,iai,iag,nai)
    float *xcs;
    float *ycs;
    int *ncs;
    int *iai;
    int *iag;
    int *nai;
#endif
{
    return(1);
}

#ifdef __STDC__
int stumsl(
    float *xcs,
    float *ycs,
    int *ncs,
    int *iai,
    int *iag,
    int *nai
)
#else
int stumsl(xcs,ycs,ncs,iai,iag,nai)
    float *xcs;
    float *ycs;
    int *ncs;
    int *iai;
    int *iag;
    int *nai;
#endif
{
    return(1);
}
