#include <stdio.h>
#include <math.h>

/*
 * Function prototypes for NCARG and NCARG-GKS routines
 */
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define MREG 50
#define NREG 50
#define LRWK 5000
#define LIWK 5000
#define LMAP 200000
#define NWRK 15000
#define NOGRPS 5

#define min(x,y)    ((x) < (y) ? (x) : (y))
#define max(x,y)    ((x) > (y) ? (x) : (y))

main()
{
    float zreg[MREG][NREG];
    extern void color(), colcon(), gendat();
/*
 * get data array
 */
    gendat(zreg,MREG,MREG,NREG,13,18,13.,18.);
/*
 * open gks, and turn clipping off
 */
    c_opngks();
    gset_clip_ind(0);
/*
 * call conpack color fill routine
 */
    colcon(zreg,-15,"CE",-90.,90.,-180.,180.,0.,0.);
/*
 * close frame and close gks
 */
    c_frame();
    c_clsgks();
}

void colcon(zreg,ncl,proj,rlatmn,rlatmx,rlonmn,rlonmx,plat,plon)
float *zreg[NREG];
int ncl;
float rlatmn, rlatmx, rlonmn, rlonmx, plat, plon;
char *proj;
{
    int i, ncll, idum;
    float rwrk[LRWK],xwrk[NWRK],ywrk[NWRK];
    float xmin, xmax, ymin, ymax, dum1, dum2, dum3, dum4;
    float rltmn[2], rltmx[2], rlnmn[2], rlnmx[2];
    float ybot, ytop, zmn, clv;
    int iwrk[LIWK], lfin[35];
    int map[LMAP],iarea[NOGRPS],igrp[NOGRPS];
    char *lbls[35];

    extern int fill(), mask();

    rltmn[0] = rlatmn;
    rltmx[0] = rlatmx;
    rlnmn[0] = rlonmn;
    rlnmx[0] = rlonmx;
    rltmn[1] = rltmx[1] = rlnmn[1] = rlnmx[1] = 0.;
/*
 * set color fill to solid
 */
    gset_fill_int_style (1);
/* 
 * initialize areas
 */
    c_arinam(map, LMAP);
/*
 * initialize ezmap and add to area map
 */
    c_mapstr ("gr",0.);
    c_mapstc ("ou","CO");
    c_maproj(proj,plat,plon,0.0);
    if (!strncmp(proj,"sv",2)) c_mapstr ("sa",10.);
    c_mapset("co",rltmn,rlnmn,rltmx,rlnmx);
    c_mapint();
    c_mapbla(map);
/*
 * initialize conpack and add to area map
 */
    c_cpseti("set - do-set-call flag",0);
    c_cpseti("map - mapping flag",1);
    c_cpsetr("orv - out of range flag",1.e12);
    c_cpsetr("xc1 - x coordinate at index 1",rlonmn);
    c_cpsetr("xcm - x coordinate at index m",rlonmx);
    c_cpsetr("yc1 - y coordinate at index 1",rlatmn);
    c_cpsetr("ycn - y coordinate at index n",rlatmx);
    c_cpseti("llp - line label positioning flag",0);
    c_cpseti("hlb - high/low label box flag",2);
    c_cpseti("hlc - high/low label color index",1);
    c_cprect(zreg, NREG, NREG, MREG, rwrk, LRWK, iwrk, LIWK);
    c_cpclam(zreg, rwrk, iwrk, map);
    c_cplbam(zreg, rwrk, iwrk, map);
/*
 * choose a color for every contour level
 */
    c_cpgeti("ncl",&ncll);
    color (ncll);
/*
 * fill contours and areas over land
 */
    c_arscam(map, xwrk, ywrk, NWRK, iarea, igrp, NOGRPS, fill);
/*
 * draw continental outlines, labels, and masked contours
 */
    c_maplot();
    c_cplbdr(zreg,rwrk,iwrk);
    c_cpcldm(zreg,rwrk,iwrk,map,mask);
/*
 * draw and fill a label bar
 */
    c_getset(&xmin,&xmax,&ymin,&ymax,&dum1,&dum2,&dum3,&dum4,&idum);
    ybot = ymin/3.0;
    ytop = ymin - ymin/3.0 ;
    c_cpgetr("zmn - z data array dimension n",&zmn);
    lbls[0] = (char *)malloc(9*sizeof(char));
    sprintf( lbls[0], "%8.3f", zmn );
    for( i = 0; i < ncll; i++ ) {
        lfin[i]=i+2;
        c_cpseti("pai - parameter array index",i+1);
        c_cpgetr("clv - contour level values",&clv);
        lbls[i+1] = (char *)malloc(9*sizeof(char));
        sprintf( lbls[i+1], "%8.3f", clv );
    }
    lfin[ncll]=ncll+2;
    lbls[ncll+1] = (char *)malloc(5*sizeof(char));
    strcpy( lbls[ncll+1], "land" );
    lfin[ncll+1]=ncll+3;
    c_lblbar(0,xmin,xmax,ybot,ytop,ncll+2,1.,.5,lfin,1,lbls,ncll+2,1);
}

int fill (xwrk,ywrk,n,iarea,igrp,ngrps)
float *xwrk, *ywrk;
int *iarea, *igrp;
int *n, *ngrps;
{
    int i, j;
    int idmap, idcont;
    Gpoint_list fill_area;

    idmap = -1;
    idcont = -1;

    for( i = 0; i < *ngrps; i++ ) {
        if (igrp[i] == 1) idmap=iarea[i];
        if (igrp[i] == 3) idcont=iarea[i];
    }
/*
 * if the area is defined by 2 or fewer points, return to arscam
 */
    if (*n <= 3) return(0);
/*
 * check if the area is over the map 
 */
    if ((idmap > 0) && (idcont > 0)) {
/*
 * if the area is over water, fill the contours with colors depending
 *
 * on their level
 */
        if (c_mapaci(idmap) == 1) {
            gset_fill_colr_ind(idcont+2);
        }
        else {
/*
 * if the area is over land, fill with gray
 */
            gset_fill_colr_ind(2);
        }
/*
 * set up struct for fill area
 */
        fill_area.num_points = *n-1;
        fill_area.points = (Gpoint *)calloc(*n-1,sizeof(Gpoint));
        if( !fill_area.points ) {
            fprintf( stderr, "fill:  Not enough memory to create fill area arrays\n" );
            gemergency_close_gks();
            exit(1);
        }
        for( i = 0; i < *n-1; i++ ) {
            fill_area.points[i].x = xwrk[i];
            fill_area.points[i].y = ywrk[i];
        }
        gfill_area(&fill_area);
        free(fill_area.points);
    }
    return(1);
}

int mask(xwrk,ywrk,n,iarea,igrp,ngrps)
float *xwrk, *ywrk;
int *n, *iarea, *igrp, *ngrps;
{
    int i;
    int idmap, idcont;

    idmap = -1;
    idcont = -1;

    for( i = 0; i < *ngrps; i++ ) {
        if (igrp[i] == 1) idmap=iarea[i];
        if (igrp[i] == 3) idcont=iarea[i];
    }
/*
 * if the line is defined by 1 or fewer points, return to cpcldm
 */
    if (*n <  2) return(0);
/*
 * draw the line if the area is over the map, and not over a label, or
 * over land.
 */
    if ((idmap > 0) && (idcont > 0) && (c_mapaci(idmap) == 1)) {
        c_curve(xwrk,ywrk,*n);
    }
        
    return(1);
}

void gendat (data,idim,m,n,mlow,mhgh,dlow,dhgh)
float *data, dlow, dhgh;
int idim, m, n, mlow, mhgh;
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
                temp = -(pow((fovm*((float)(i)-ccnt[0][k-1])),2.)+
                        pow(fovn*((float)(j)-ccnt[1][k-1]),2.));
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

void color (n)
int n;
{
/*
 * background color
 * black
 */
    int i, icnt;
    float xhue, hues;
    Gcolr_rep rgb;

    rgb.rgb.red = rgb.rgb.green = rgb.rgb.blue = 0.;
    gset_colr_rep(1,0,&rgb);
/*
 * first foreground color is white
 */
    rgb.rgb.red = rgb.rgb.green = rgb.rgb.blue = 1.;
    gset_colr_rep(1,1,&rgb);
/*
 * second foreground color is gray
 */
    rgb.rgb.red = rgb.rgb.green = rgb.rgb.blue = 0.75;
    gset_colr_rep(1,2,&rgb);
/*
 * choose other foreground colors spaced equally around the spectrum
 */
    icnt=0;
    hues=360./n;
    for( i = 0; i < n; i++ ) {
        xhue=i*hues;
        c_hlsrgb(xhue,60.,75.,&rgb.rgb.red,&rgb.rgb.green,&rgb.rgb.blue);
        if (xhue<=36.0) {
            gset_colr_rep(1,n+4-i,&rgb);
            icnt=icnt+1;
        }
        else {
            gset_colr_rep(1,i-icnt+3,&rgb);
        }
    }
}
