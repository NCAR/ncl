/*
 *	$Id: c_cpexcc.c,v 1.3 1994-08-24 16:02:03 haley Exp $
 */
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define max(x,y)    ((x) > (y) ? (x) : (y) )
#define min(x,y)    ((x) < (y) ? (x) : (y) )
#define pow2(x)    ((x)*(x))

#define WKID 1

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

void capsap (labl,iama,lama)
char *labl;
int *iama, lama;
{
    int iiwu, irwu, iamu;
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
    Gcolr_rep rgbv[15];
    int i;
/*
 * Define the RGB color triples needed below.
 */
    rgbv[0].rgb.red = 1.00;
    rgbv[0].rgb.green = 1.00;
    rgbv[0].rgb.blue = 1.00;
    rgbv[1].rgb.red = 0.70;
    rgbv[1].rgb.green = 0.70;
    rgbv[1].rgb.blue = 0.70;
    rgbv[2].rgb.red = 0.75;
    rgbv[2].rgb.green = 0.50;
    rgbv[2].rgb.blue = 1.00;
    rgbv[3].rgb.red = 0.50;
    rgbv[3].rgb.green = 0.00;
    rgbv[3].rgb.blue = 1.00;
    rgbv[4].rgb.red = 0.00;
    rgbv[4].rgb.green = 0.00;
    rgbv[4].rgb.blue = 1.00;
    rgbv[5].rgb.red = 0.00;
    rgbv[5].rgb.green = 0.50;
    rgbv[5].rgb.blue = 1.00;
    rgbv[6].rgb.red = 0.00;
    rgbv[6].rgb.green = 1.00;
    rgbv[6].rgb.blue = 1.00;
    rgbv[7].rgb.red = 0.00;
    rgbv[7].rgb.green = 1.00;
    rgbv[7].rgb.blue = 0.60;
    rgbv[8].rgb.red = 0.00;
    rgbv[8].rgb.green = 1.00;
    rgbv[8].rgb.blue = 0.00;
    rgbv[9].rgb.red = 0.70;
    rgbv[9].rgb.green = 1.00;
    rgbv[9].rgb.blue = 0.00;
    rgbv[10].rgb.red = 1.00;
    rgbv[10].rgb.green = 1.00;
    rgbv[10].rgb.blue = 0.00;
    rgbv[11].rgb.red = 1.00;
    rgbv[11].rgb.green = 0.75;
    rgbv[11].rgb.blue = 0.00;
    rgbv[12].rgb.red = 1.00;
    rgbv[12].rgb.green = 0.38;
    rgbv[12].rgb.blue = 0.38;
    rgbv[13].rgb.red = 1.00;
    rgbv[13].rgb.green = 0.00;
    rgbv[13].rgb.blue = 0.38;
    rgbv[14].rgb.red = 1.00;
    rgbv[14].rgb.green = 0.00;
    rgbv[14].rgb.blue = 0.00;
/*
 * Define 16 different color indices, for indices 0 through 15.  The
 * color corresponding to index 0 is black and the color corresponding
 * to index 1 is white.
 */
    rgbv[0].rgb.red = rgbv[0].rgb.green = rgbv[0].rgb.blue = 0.0;
    gset_colr_rep (WKID,0,&rgbv[0]);
    rgbv[0].rgb.red = rgbv[0].rgb.green = rgbv[0].rgb.blue = 1.00;
    for( i = 0; i < 15; i++ ) {
        gset_colr_rep(WKID,i+1,&rgbv[i]);
    }
}
