#include <stdio.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define max(x,y)    ((x) > (y) ? (x) : (y) )
#define min(x,y)    ((x) < (y) ? (x) : (y) )
#define pow2(x)    ((x)*(x))

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
    float zdat[14][23], rwrk[5000];
    int iwrk[1000], iama[20000];
    Gasfs iasf;
    Gpoint point;
    Gvec shift, scale;
    Gtran_matrix tran_matrix;
	extern void gendat(), capsap(), labtop(), bndary();
/*
 * Declare the routine which will draw contour lines, avoiding labels.
 */
    extern int drawcl(
#ifdef NeedFuncProto
        float *xcs,
        float *ycs,
        int *ncs,
        int *iai,
        int *iag,
        int *nai
#endif
    );
    extern int shader(
#ifdef NeedFuncProto
        float *xcs,
        float *ycs,
        int *ncs,
        int *iai,
        int *iag,
        int *nai
#endif
    );
/*
 * Initialize
 */
    point.x = point.y = 0.5;
    shift.delta_x = shift.delta_y = 0.0;
    scale.delta_x = scale.delta_y = 0.5;
/*
 * Initialize the values in the aspect-source-flag array.
 */
    iasf.linetype = 1;
    iasf.linewidth = 1;
    iasf.line_colr_ind = 1;
    iasf.marker_type = 1;
    iasf.marker_size = 1;
    iasf.marker_colr_ind = 1;
    iasf.text_font_prec = 1;
    iasf.char_expan = 1;
    iasf.char_space = 1;
    iasf.text_colr_ind = 1;
    iasf.fill_int_style = 1;
    iasf.fill_style_ind = 1;
    iasf.fill_colr_ind = 1;

    gopen_gks("stdout",0);
/*
 *  Open and activate WISS.
 */
    gopen_ws(2,NULL,3);
    gactivate_ws(2);
/*
 *  Create a segment.
 */
    gcreate_seg(9);
    gset_clip_ind (GIND_NO_CLIP);
    gset_asfs (&iasf);
    gendat (zdat,23,23,14,20,20,-136.148,451.834);
/*
 * Example 1-1 ---------------------------------------------------------
 */
    c_pcseti ("QU - QUALITY FLAG",2);
    c_cprect ((float *)zdat,23,23,14,rwrk,5000,iwrk,1000);
    c_cpback ((float *)zdat,rwrk,iwrk);
    c_cpcldr ((float *)zdat,rwrk,iwrk);
    c_cplbdr ((float *)zdat,rwrk,iwrk);
    capsap ("EXAMPLE 1-1",0.0,iama,0);
    labtop ("EXAMPLE 1-1",.017);
    bndary();
    gclose_seg();
/*
 *  Open and activate an X workstation.
 */
    gopen_ws(WKID,NULL,WSTYPE);
    gactivate_ws(WKID);
/*
 *  Define the segment transformation matrix.
 */
    geval_tran_matrix(&point,&shift,45.*3.14159/180.,&scale,0,tran_matrix);
    gset_seg_tran(9,tran_matrix);
/*
 *  Copy the segment to the X workstation.
 */
    gcopy_seg_ws(WKID,9);
    c_frame();
/*
 *  Deactivate the workstations.
 */
    gdeactivate_ws(WKID);
    gdeactivate_ws(2);
/*
 *  Close the workstations.
 */
    gclose_ws(WKID);
    gclose_ws(2);
/*
 *  Close GKS.
 */
    gclose_gks();
}

#ifdef __STDC__
int drawcl(
    float *xcs,
    float *ycs,
    int *ncs,
    int *iai,
    int *iag,
    int *nai
)
#else
int drawcl (xcs,ycs,ncs,iai,iag,nai)
    float *xcs;
    float *ycs;
    int *ncs;
    int *iai;
    int *iag;
    int *nai;
#endif
{
    int i, idr;
/*
 * this version of drawcl draws the polyline defined by the points
 * ((xcs(i),ycs(i)),i=1,ncs) if and only if none of the area identifiers
 * for the area containing the polyline are negative.  the dash package
 * routine curved is called to do the drawing.
 *
 * turn on drawing.
 */
    idr=1;
/*
 * if any area identifier is negative, turn off drawing.
 */
    for( i = 0; i < *nai; i++ ) {
        if (iai[i] < 0) idr=0;
    }
/*
 * if drawing is turned on, draw the polyline.
 */
    if (idr != 0) c_curved (xcs,ycs,*ncs);
    return(1);
}

#ifdef __STDC__
int shader(
    float *xcs,
    float *ycs,
    int *ncs,
    int *iai,
    int *iag,
    int *nai
)
#else 
int shader(xcs,ycs,ncs,iai,iag,nai)
    float *xcs;
    float *ycs;
    int *ncs;
    int *iai;
    int *iag;
    int *nai;
#endif
{
/*
 * this version of shader shades the polygon whose edge is defined by
 * the points ((xcs(i),ycs(i)),i=1,ncs) if and only, relative to edge
 * group 3, its area identifier is a 1.  the package softfill is used
 * to do the shading.
 */
/*
 * define workspaces for the shading routine.
 */
    float dst[1100];
    int ind[1200], i, ish;
/*
 * turn off shading.
 */
    ish=0;
/*
 * if the area identifier for group 3 is a 1, turn on shading.
 */
    for( i=0; i < *nai; i++ ) {
          if (iag[i] == 3 && iai[i] == 1) ish=1;
      }
/*
 * if shading is turned on, shade the area.  the last point of the
 * edge is redundant and may be omitted.
 */
    if (ish != 0) {
        c_sfseti ("ANGLE",45);
        c_sfsetr ("SPACING",.006);
        c_sfwrld (xcs,ycs,*ncs-1,dst,1100,ind,1200);
        c_sfseti ("ANGLE",135);
        c_sfnorm (xcs,ycs,*ncs-1,dst,1100,ind,1200);
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

void capsap (labl,time,iama,lama)
char *labl;
float time;
int *iama, lama;
{
    float dumi;
    int iiwu, irwu, iamu;
    extern float second();
/*
 * Compute and print how much space was used in the various arrays.
 */
    printf("PLOT TITLE WAS %s\n",labl );
    c_cpgeti("IWU - INTEGER WORKSPACE USAGE",&iiwu);
    c_cpgeti("RWU - REAL WORKSPACE USAGE",&irwu);
    printf("INTEGER WORKSPACE USED %d\n",iiwu );
    printf("   REAL WORKSPACE USED %d\n",irwu );
    if (lama) {
        iamu=lama-(iama[5]-iama[4]-1);
        printf( "   AREA MAP SPACE USED %d\n",iamu);
    }
}

void labtop (labl,size)
char *labl;
float size;
{
    float xvpl,xvpr,yvpb,yvpt,xwdl,xwdr,ywdb,ywdt,szfs,dbos;
    int lnlg, iqua;
/*
 * Put a label just above the top of the plot.  The SET call is re-done
 * to allow for the use of fractional coordinates, and the text extent
 * capabilities of the package PLOTCHAR are used to determine the label
 * position.
 */
    c_getset(&xvpl,&xvpr,&yvpb,&yvpt,&xwdl,&xwdr,&ywdb,&ywdt,&lnlg);
    szfs=size*(xvpr-xvpl);
    c_set (0.,1.,0.,1.,0.,1.,0.,1.,1);
    c_pcgeti ("QU - QUALITY FLAG",&iqua);
    c_pcseti ("QU - QUALITY FLAG",0);
    c_pcseti ("TE - TEXT EXTENT COMPUTATION FLAG",1);
    c_plchhq (.5,.5,labl,szfs,360.,0.);
    c_pcgetr ("DB - DISTANCE TO BOTTOM OF STRING",&dbos);
    c_plchhq (.5*(xvpl+xvpr),yvpt+szfs+dbos,labl,szfs,0.,0.);
    c_pcseti ("QU - QUALITY FLAG",iqua);
    c_set (xvpl,xvpr,yvpb,yvpt,xwdl,xwdr,ywdb,ywdt,lnlg);
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

void dfclrs()
{
/*
 * Define a set of RGB color triples for colors 1 through 15.
 */
    Gcolr_rep rgb[15];
    int i;
/*
 * Define the RGB color triples needed below.
 */
    rgb[0].rgb.red = 1.00; rgb[0].rgb.green = 1.00; rgb[0].rgb.blue = 1.00;
    rgb[1].rgb.red = 0.70; rgb[1].rgb.green = 0.70; rgb[1].rgb.blue = 0.70;
    rgb[2].rgb.red = 0.75; rgb[2].rgb.green = 0.50; rgb[2].rgb.blue = 1.00;
    rgb[3].rgb.red = 0.50; rgb[3].rgb.green = 0.00; rgb[3].rgb.blue = 1.00;
    rgb[4].rgb.red = 0.00; rgb[4].rgb.green = 0.00; rgb[4].rgb.blue = 1.00;
    rgb[5].rgb.red = 0.00; rgb[5].rgb.green = 0.50; rgb[5].rgb.blue = 1.00;
    rgb[6].rgb.red = 0.00; rgb[6].rgb.green = 1.00; rgb[6].rgb.blue = 1.00;
    rgb[7].rgb.red = 0.00; rgb[7].rgb.green = 1.00; rgb[7].rgb.blue = 0.60;
    rgb[8].rgb.red = 0.00; rgb[8].rgb.green = 1.00; rgb[8].rgb.blue = 0.00;
    rgb[9].rgb.red = 0.70; rgb[9].rgb.green = 1.00; rgb[9].rgb.blue = 0.00;
    rgb[10].rgb.red = 1.00; rgb[10].rgb.green = 1.00; rgb[10].rgb.blue = 0.00;
    rgb[11].rgb.red = 1.00; rgb[11].rgb.green = 0.75; rgb[11].rgb.blue = 0.00;
    rgb[12].rgb.red = 1.00; rgb[12].rgb.green = 0.38; rgb[12].rgb.blue = 0.38;
    rgb[13].rgb.red = 1.00; rgb[13].rgb.green = 0.00; rgb[13].rgb.blue = 0.38;
    rgb[14].rgb.red = 1.00; rgb[14].rgb.green = 0.00; rgb[14].rgb.blue = 0.00;
/*
 * Define 16 different color indices, for indices 0 through 15.  The
 * color corresponding to index 0 is black and the color corresponding
 * to index 1 is white.
 */
    rgb[0].rgb.red = rgb[0].rgb.green = rgb[0].rgb.blue = 0.0;
    gset_colr_rep (WKID,0,&rgb[0]);
    rgb[0].rgb.red = rgb[0].rgb.green = rgb[0].rgb.blue = 1.00;
    for( i = 0; i < 15; i++ ) {
        gset_colr_rep(WKID,i+1,&rgb[i]);
    }
}

