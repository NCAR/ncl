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
#define pow2(x)     ((x) * (x))

float zreg[MREG][NREG];

main()
{
    extern void colcon(), gendat();
/*
 * get data array
 */
    gendat(15,13,13.,18.);
/*
 * open gks, and turn clipping off
 */
    c_opngks();
    gset_clip_ind(GIND_NO_CLIP);
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
    extern void color();
    extern int fill(), mask();

    rltmn[0] = rlatmn;
    rltmx[0] = rlatmx;
    rlnmn[0] = rlonmn;
    rlnmx[0] = rlonmx;
    rltmn[1] = rltmx[1] = rlnmn[1] = rlnmx[1] = 0.;
/*
 * set color fill to solid
 */
    gset_fill_int_style (GSTYLE_SOLID);
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
    c_cprect((float *)zreg, NREG, NREG, MREG, rwrk, LRWK, iwrk, LIWK);
    c_cpclam((float *)zreg, rwrk, iwrk, map);
    c_cplbam((float *)zreg, rwrk, iwrk, map);
/*
 * choose a color for every contour level
 */
    c_cpgeti("ncl",&ncll);
    color (ncll+1);
/*
 * fill contours and areas over land
 */
    c_arscam(map, xwrk, ywrk, NWRK, iarea, igrp, NOGRPS, fill);
/*
 * draw continental outlines, labels, and masked contours
 */
    c_maplot();
    c_cplbdr((float *)zreg,rwrk,iwrk);
    c_cpcldm((float *)zreg,rwrk,iwrk,map,mask);
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
    Gpoint_list area;

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
        area.num_points = *n-1;
        area.points = (Gpoint *)malloc(area.num_points*sizeof(Gpoint));
        if( !area.points ) {
            fprintf( stderr, "fill:  Not enough memory to create fill area array\n" );
            gemergency_close_gks();
            exit(1);
        }
        for( i = 0; i < *n-1; i++ ) {
            area.points[i].x = xwrk[i];
            area.points[i].y = ywrk[i];
        }
        gfill_area(&area);
        free(area.points);
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

void gendat (mlow,mhgh,dlow,dhgh)
int mlow, mhgh;
float dlow, dhgh;
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
    int mini, minj, maxi, maxj;
	int i, j, k, nlow, nhgh, ncnt, m, n;
	float fovm, fovn, dmin, dmax, temp, aatr, aabr;
	float q, p, datr, datl;
	float ccnt[3][50];
	extern float fran();
	
	m = MREG;
	n = NREG;
	fovm = 9./(float)m;
	fovn = 9./(float)n;

	nlow = max(1,min(25,mlow));
	nhgh =  max(1,min(25,mhgh));
	ncnt = nlow+nhgh;

	for( k = 0; k < ncnt; k++ ) {
		ccnt[0][k] = 1.+((float)m-1.)*fran();
		ccnt[1][k] = 1.+((float)n-1.)*fran();
		if ((k+1) <= nlow) {
            ccnt[2][k] = -1.;
		}
		else {
            ccnt[2][k] = 1.;
		}
	}

	aabr = 0.;
	aatr = 0.;

	for( i = 0; i < m; i++ ) {
		for( j = 0; j < n; j++ ) {
            zreg[j][i]=.5*(dlow+dhgh);
            for( k = 0; k < ncnt; k++ ) {
				temp = -(pow2((fovm*((float)(i+1)-ccnt[0][k])))+
                         pow2(fovn*((float)(j+1)-ccnt[1][k])));
				if (temp >= -20.) {
                    zreg[j][i] = zreg[j][i]+.5*(dhgh-dlow)*ccnt[2][k]*exp(temp);
                }
			}
		}
		aabr = aabr+zreg[0][i];
		aatr = aatr+zreg[n-1][i];
	}

	aabr = aabr/(float)m;
	aatr = aatr/(float)m;

	for( j = 1; j <= n; j++ ) {
		if ( j <= n/5) {
            p = (float)(j-1)/(float)(n/5-1);
			for( i = 1; i <= m/2; i++ ) {
				q = 1.-max(0.,.5-.5*(float)(i-1)/(float)(m/5-1));
				datl = zreg[j-1][i-1];
				datr = zreg[j-1][m+1-i];
				zreg[j-1][i-1]   = p*(q*datl+(1.-q)*datr)+(1.-p)*aabr;
				zreg[j-1][m+1-i] = p*(q*datr+(1.-q)*datl)+(1.-p)*aabr;
			}
		}
		else if (j >= n+1-n/5) {
            p = (float)(n-j)/(float)(n/5-1);
			for( i = 1; i <= m/2; i++ ) {
				q = 1.-max(0.,.5-.5*(float)(i-1)/(float)(m/5-1));
				datl = zreg[j-1][i-1];
				datr = zreg[j-1][m+1-i];
				zreg[j-1][i-1]   = p*(q*datl+(1.-q)*datr)+(1.-p)*aatr;
				zreg[j-1][m+1-i] = p*(q*datr+(1.-q)*datl)+(1.-p)*aabr;
			}
		}
		else {
			for( i = 1; i <= m/2; i++ ) {
				q = 1.-max(0.,.5-.5*(float)(i-1)/(float)(m/5-1));
				datl = zreg[j-1][i-1];
				datr = zreg[j-1][m+1-i];
				zreg[j-1][i-1]   = q*datl+(1.-q)*datr;
				zreg[j-1][m+1-i] = q*datr+(1.-q)*datl;
			}
		}
	}

	dmin = 1.e36;
	dmax = -1.e36;

	for( j = 0; j < n; j++ ) {
		for( i = 0; i < m; i++ ) {
            if( dmin > zreg[j][i]) {
				mini = i;
				minj = j;
				dmin = zreg[j][i];
			}
            if( dmax < zreg[j][i]) {
				maxi = i;
				maxj = j;
				dmax = zreg[j][i];
			}
		}
	}

	for( j = 0; j < n; j++ ) {
		for( i = 0; i < m; i++ ) {
            zreg[j][i] = (zreg[j][i]-dmin)/(dmax-dmin)*(dhgh-dlow)+dlow;
		}
	}

	return;
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
    int i, icnt,lap;
    float xhue, hues, redln;
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
    hues=360./(float)n;
    redln=36.0;
    lap=(int)(redln/hues);
    for( i = 1; i <= n; i++ ) {
        xhue=i*hues;
        c_hlsrgb(xhue,60.,75.,&rgb.rgb.red,&rgb.rgb.green,&rgb.rgb.blue);
        if (xhue<=redln) {
            gset_colr_rep(1,(n+2)-(lap-i),&rgb);
            icnt=icnt+1;
        }
        else {
            gset_colr_rep(1,i-icnt+2,&rgb);
        }
    }
}
