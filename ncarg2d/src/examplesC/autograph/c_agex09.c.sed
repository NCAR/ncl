/*
 *	$Id: c_agex09.c.sed,v 1.2 1994-06-21 14:58:42 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>

#define max(x,y)    ((x) > (y) ? (x) : (y) )
#define min(x,y)    ((x) < (y) ? (x) : (y) )
#define pow2(x)    ((x)*(x))

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
/*
 * Define the data arrays.
 */
    int i, idim, xdim, ydim, mlow, mhgh;
    float xdat[400],ydat1[200],ydat[400], xmax, dlow, dhgh,power;
    float ymin, ymax;
    extern void bndary();
/*
 * initialize gks.
 */
	gopen_gks("stdout",0);
	gopen_ws(WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * fill the data arrays.
 */
    for( i = 0; i < 400; i++ ) {
        xdat[i]=((float)(i+1)-1.)/399.;
    }
    
    xdim = 200;
    ydim = 1;
    idim = 200;
    mlow = 3;
    mhgh = 3;
    dlow = .01;
    dhgh = 10.;
    (void)gendat(ydat1,idim,xdim,ydim,mlow,mhgh,dlow,dhgh);
    for( i = 0; i < 200; i++ ) ydat[i] = ydat1[i];
    dlow = -10.;
    dhgh = -.01;
    (void)gendat(ydat1,idim,xdim,ydim,mlow,mhgh,dlow,dhgh);
    for( i = 200; i < 400; i++ ) ydat[i] = ydat1[i-200];
/*
 * the y data ranges over both positive and negative values.
 * it is desired that both ranges be represented on the same
 * graph and that each be shown logarithmically, ignoring
 * values in the range -.01 to +.01, in which we have no
 * interest.  first we map each y datum into its absolute
 * value (.01 if the absolute value is too small).  then we
 * take the base-10 logarithm, add 2.0001 (so as to be sure
 * of getting a positive number), and re-attach the original
 * sign.  we can plot the resulting y data on a linear y axis.
 */
    for( i = 0; i < 400; i++ ) {
        xmax = fabs((double)ydat[i]) > .01 ? fabs((double)ydat[i]) : .01;
        power = (float)log10((double)xmax)+2.0001;
        if( ydat[i] < 0 ) ydat[i] = -fabs((double)power);
        else              ydat[i] =  fabs((double)power);
        if( i ) {
			ymin = ydat[i] < ymin ? ydat[i] : ymin;
			ymax = ydat[i] > ymax ? ydat[i] : ymax;
		}
		else {
			ymin = ymax= ydat[0];
		}
    }
/*
 * in order that the labels on the y axis should show the
 * original values of the y data, we change the user-system-
 * to-label-system mapping on both y axes and force major
 * ticks to be spaced logarithmically in the
 * label system (which will be defined by the subroutine
 * agutol in such a way as to re-create numbers in the
 * original range).
 */
    c_agseti("LEFT/FUNCTION.",1);
    c_agseti("LEFT/MAJOR/TYPE.",2);

    c_agseti("RIGHT/FUNCTION.",1);
    c_agseti("RIGHT/MAJOR/TYPE.",2);
/*
 * change the left-axis label to reflect what's going on.
 */
    c_agsetc("LABEL/NAME.","L");
    c_agseti("LINE/NUMBER.",100);
    c_agsetc("LINE/TEXT.","LOG SCALING, POSITIVE AND NEGATIVE$");
/*
 * draw a boundary around the edge of the plotter frame.
 */
    bndary();
/*
 * draw the curve.
 */
    c_ezxy(xdat,ydat,400,"EXAMPLE 9$");
/*
 * close gks.
 */
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();

}

gendat (data,idim,m,n,mlow,mhgh,dlow,dhgh)
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
    for( j = 1; j <= n; j++ ) {
        for( i = 1; i <= m; i++ ) {
            ii = (i-1)*n+j-1;
            data[ii]=.5*(dlow+dhgh);
            for( k = 1; k <= ncnt; k++ ) {
                temp = -(pow2((fovm*((float)(i)-ccnt[0][k-1])))+
                        pow2(fovn*((float)(j)-ccnt[1][k-1])));
                if (temp >= -20.) data[ii]=data[ii]+.5*(dhgh-dlow)
                                           *ccnt[2][k-1]*exp(temp);
            }
            dmin=min(dmin,data[ii]);
            dmax=max(dmax,data[ii]);
        }
    }

    for( j = 0; j < m*n; j++ ) {
        data[j]=(data[j]-dmin)/(dmax-dmin)*(dhgh-dlow)+dlow;
    }
    return(1);
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


float fran()
{
/*
 * Pseudo-random-number generator.
 */
    static double x = 2.718281828459045;
    extern double fmod();
    x = fmod(9821.*x+.211327,1.);
    return((float)x);
}

agutol_(iaxs,funs,idma,vinp,votp)
int *iaxs,*idma;
float *funs,*vinp,*votp;
{
/*
 * left or right axis.
 */
    if ( *funs == 1.) {
        if ( *idma < 0) {
            if( *vinp < 0 ) {
                *votp = -fabs(log10(max(fabs(*vinp),.01))+2.0001);
            }
            else {
                *votp = fabs(log10(max(fabs(*vinp),.01))+2.0001);
            }
        }
        else {
            if( *vinp < 0 ) {
                *votp = -fabs(pow(10.,(fabs(*vinp)-2.0001)));
            }
            else {
                *votp = fabs(pow(10.,(fabs( *vinp)-2.0001)));
            }
        }
    }
/*
 * all others.
 */
    else {
        *votp = *vinp;
    }
    return(1);
}

agchnl_(iaxs,vils,chrm,mcim,ncim,ipxm,chre,mcie,ncie)
char *chrm, *chre;
int *iaxs, *mcim, *ncim, *ipxm, *mcie, *ncie;
float *vils;
{
/*
 * modify the left-axis numeric label marking the value "0.".
 */
    if ( *iaxs == 1 && *vils == 0.) {
        chrm[0] = ' ';
        *ncim=1;
        *ipxm=0;
        *ncie=0;
    }
    return(1);
}
