/*
 * $Id: c_ggdini.c,v 1.1 1994-05-18 15:55:36 haley Exp $
 */

#include <stdio.h>
#include <math.h>

#define NELA  5
#define MFCE  27300
#define MEDG  40950
#define MVTX  10242
#define NELP  6

#define pow(x)   ((x)*(x))
#define min(x,y) ((x) < (y) ? (x) : (y))
#define max(x,y) ((x) > (y) ? (x) : (y))

int nfce,ifce[MFCE+1][5],nedg,iedg[MEDG+1][5];
int nvtx;
float vrtx[MVTX+1][5];
int lfce[NELP+1][3],ledg[NELP+1][3];
float rlow,rhgh,rmin,rmax;

int jfce[21][4]  =  { 0,  0,  0,  0,  0, 1, 11,  2,   0,  2, 19,  3,
                      0,  3, 25,  4,  0,  4, 29,  5,  0,  1, 14,  5,
                      0, 12, 28, 14,  0, 12, 27, 13,  0, 11, 20, 13,
                      0, 20, 30, 21,  0, 19, 26, 21,  0, 16, 18, 26,
                      0, 16, 25, 17,  0, 15, 23, 17,  0, 23, 29, 24,
                      0, 22, 28, 24,  0,  7,  8, 22,  0,  6, 15,  7,
                      0,  6, 18, 10,  0,  9, 30, 10,  0,  8, 27,  9};

int jedg[31][3]  =  {  0,  0,  0,  0,  1,  3,  0,  1,  5,  0,  1,  7,
                       0,  1,  9,  0,  1, 11,  0,  2,  4,  0,  2,  6,
                       0,  2,  8,  0,  2, 10,  0,  2, 12,  0,  3,  5,
                       0,  3,  8,  0,  3, 10,  0,  3, 11,  0,  4,  6,
                       0,  4,  7,  0,  4,  9,  0,  4, 12,  0,  5,  7,
                       0,  5, 10,  0,  5, 12,  0,  6,  8,  0,  6,  9,
                       0,  6, 11,  0,  7,  9,  0,  7, 12,  0,  8, 10,
                       0,  8, 11,  0,  9, 11,  0, 10, 12};

float xcvi[13] = {0., .9510565162952, -.9510565162951,  .4253254041760, 
                 -.4253254041760,  .4253254041760, -.4253254041760, 
                  .4253254041760, -.4253254041760,  .4253254041760, 
                 -.4253254041760,  .4253254041760, -.4253254041760  };

float ycvi[13] = {0., .0000000000000,  .0000000000000,  .8506508083520, 
                 -.8506508083520,  .2628655560596, -.2628655560596, 
                 -.6881909602356,  .6881909602356, -.6881909602356, 
                  .6881909602356,  .2628655560595, -.2628655560596 };

float zcvi[13] = {0.,.0000000000000,  .0000000000000,  .0000000000000, 
                .0000000000000,  .8090169943749, -.8090169943749, 
                .5000000000000, -.5000000000000, -.5000000000000, 
                .5000000000000, -.8090169943749,  .8090169943749  };

int c_ggdini (dlow,dhgh,nrnd,frct)
float dlow, dhgh, frct;
int nrnd;
{
	int i, j, iela;
	float temp, rmul, y;
	extern float fran();

	for( i = 1; i <= nrnd; i++ ) {
		temp = fran();
	}

	nfce = 20;
	for( i = 1; i <= nfce; i++ ) {
		ifce[i][1] = jfce[i][1];
		ifce[i][2] = jfce[i][2];
		ifce[i][3] = jfce[i][3];
		ifce[i][4] = 0;
	}
	nedg = 30;
	for( i = 1; i <= nedg; i++ ) {
		iedg[i][1] = jedg[i][1];
		iedg[i][2] = jedg[i][2];;
		iedg[i][3] = 0;
		iedg[i][4] = 0;
	}
	nvtx = 12;
	for(i = 1; i <= nvtx; i++ ) {
		temp = sqrt((double)(pow(xcvi[i])+pow(ycvi[i])+pow(zcvi[i])));
		vrtx[i][1] = xcvi[i]/temp;
		vrtx[i][2] = ycvi[i]/temp;
		vrtx[i][3] = zcvi[i]/temp;
		vrtx[i][4] = fran();
	}
	lfce[1][1] = 1;
	lfce[1][2] = nfce;
	ledg[1][1] = 1;
	ledg[1][2] = nedg;
	rmul = 1.;
	for( iela = 2; iela <= NELP; iela++ ) {
		lfce[iela][1] = nfce+1;
		ledg[iela][1] = nedg+1;
		rmul = frct*rmul;
		for( i = ledg[iela-1][1]; i <= ledg[iela-1][2]; i++ ) {
			nvtx = nvtx+1;
			if (nvtx > MVTX) {
				fprintf( stderr, "stop in c_ggdini - too many vertices\n" );
				return(1);
			}
			vrtx[nvtx][1] = .5*(vrtx[iedg[i][1]][1]+vrtx[iedg[i][2]][1]);
			vrtx[nvtx][2] = .5*(vrtx[iedg[i][1]][2]+vrtx[iedg[i][2]][2]);
			vrtx[nvtx][3] = .5*(vrtx[iedg[i][1]][3]+vrtx[iedg[i][2]][3]);
			vrtx[nvtx][4] = .5*(vrtx[iedg[i][1]][4]+vrtx[iedg[i][2]][4]);
			temp = sqrt((double)(pow(vrtx[nvtx][1])+pow(vrtx[nvtx][2])+pow(vrtx[nvtx][3])));
			vrtx[nvtx][1] = vrtx[nvtx][1]/temp;
			vrtx[nvtx][2] = vrtx[nvtx][2]/temp;
			vrtx[nvtx][3] = vrtx[nvtx][3]/temp;
			nedg = nedg+2;
			if (nedg > MEDG) {
				fprintf( stderr, "stop in c_ggdini - too many edges\n" );
				return(1);
			}
			iedg[i][3] = nedg-1;
			iedg[i][4] = nedg;
			iedg[nedg-1][1] = iedg[i][1];
			iedg[nedg-1][2] = nvtx;
			iedg[nedg-1][3] = 0;
			iedg[nedg-1][4] = 0;
			iedg[nedg  ][1] = nvtx;
			iedg[nedg  ][2] = iedg[i][2];
			iedg[nedg  ][3] = 0;
			iedg[nedg  ][4] = 0;
		}
		for( i = 1; i <= nvtx; i++ ) {
			y = fran();
			vrtx[i][4] = vrtx[i][4]+rmul*y;
		}
		for( i = lfce[iela-1][1]; i <= lfce[iela-1][2]; i++ ) {
			ifce[i][4] = nfce+1;
			nedg = nedg+3;
			if (nedg > MEDG) {
				fprintf( stderr, "stop in c_ggdini - too many edges\n" );
				return(1);
			}
			iedg[nedg-2][1] = iedg[iedg[ifce[i][1]][3]][2];
			iedg[nedg-2][2] = iedg[iedg[ifce[i][2]][3]][2];
			iedg[nedg-2][3] = 0;
			iedg[nedg-2][4] = 0;
			iedg[nedg-1][1] = iedg[iedg[ifce[i][2]][3]][2];
			iedg[nedg-1][2] = iedg[iedg[ifce[i][3]][3]][2];
			iedg[nedg-1][3] = 0;
			iedg[nedg-1][4] = 0;
			iedg[nedg  ][1] = iedg[iedg[ifce[i][3]][3]][2];
			iedg[nedg  ][2] = iedg[iedg[ifce[i][1]][3]][2];
			iedg[nedg  ][3] = 0;
			iedg[nedg  ][4] = 0;
			nfce = nfce+4;
			if (nfce > MFCE) {
				fprintf( stderr, "stop in c_ggdini - too many faces\n" );
				return(1);
			}
			if (! (iedg[ifce[i][1]][1] == iedg[ifce[i][2]][1])) goto c19991;
			if (! (iedg[ifce[i][1]][2] == iedg[ifce[i][3]][1])) goto c19990;
			ifce[nfce-3][1] = iedg[ifce[i][1]][3];
			ifce[nfce-3][2] = iedg[ifce[i][2]][3];
			ifce[nfce-3][3] = nedg-2;
			ifce[nfce-3][4] = 0;
			ifce[nfce-2][1] = iedg[ifce[i][2]][4];
			ifce[nfce-2][2] = iedg[ifce[i][3]][4];
			ifce[nfce-2][3] = nedg-1;
			ifce[nfce-2][4] = 0;
			ifce[nfce-1][1] = iedg[ifce[i][3]][3];
			ifce[nfce-1][2] = iedg[ifce[i][1]][4];
			ifce[nfce-1][3] = nedg;
			ifce[nfce-1][4] = 0;
			goto c19989;
c19990:
			ifce[nfce-3][1] = iedg[ifce[i][1]][3];
			ifce[nfce-3][2] = iedg[ifce[i][2]][3];
			ifce[nfce-3][3] = nedg-2;
			ifce[nfce-3][4] = 0;
			ifce[nfce-2][1] = iedg[ifce[i][3]][3];;
			ifce[nfce-2][2] = iedg[ifce[i][2]][4];
			ifce[nfce-2][3] = nedg-1;
			ifce[nfce-2][4] = 0;
			ifce[nfce-1][1] = iedg[ifce[i][1]][4];
			ifce[nfce-1][2] = iedg[ifce[i][3]][4];
			ifce[nfce-1][3] = nedg;
			ifce[nfce-1][4] = 0;
c19989:
			goto c19988;
c19991:
			if (! (iedg[ifce[i][1]][1] == iedg[ifce[i][2]][2])) goto c19987;
			if (! (iedg[ifce[i][1]][2] == iedg[ifce[i][3]][1])) goto c19986;
			ifce[nfce-3][1] = iedg[ifce[i][1]][3];
			ifce[nfce-3][2] = iedg[ifce[i][2]][4];
			ifce[nfce-3][3] = nedg-2;
			ifce[nfce-3][4] = 0;
			ifce[nfce-2][1] = iedg[ifce[i][2]][3];
			ifce[nfce-2][2] = iedg[ifce[i][3]][4];
			ifce[nfce-2][3] = nedg-1;
			ifce[nfce-2][4] = 0;
			ifce[nfce-1][1] = iedg[ifce[i][3]][3];
			ifce[nfce-1][2] = iedg[ifce[i][1]][4];
			ifce[nfce-1][3] = nedg;
			ifce[nfce-1][4] = 0;
			goto c19985;
c19986:
			ifce[nfce-3][1] = iedg[ifce[i][1]][3];
			ifce[nfce-3][2] = iedg[ifce[i][2]][4];
			ifce[nfce-3][3] = nedg-2;
			ifce[nfce-3][4] = 0;
			ifce[nfce-2][1] = iedg[ifce[i][2]][3];
			ifce[nfce-2][2] = iedg[ifce[i][3]][3];
			ifce[nfce-2][3] = nedg-1;
			ifce[nfce-2][4] = 0;
			ifce[nfce-1][1] = iedg[ifce[i][1]][4];
			ifce[nfce-1][2] = iedg[ifce[i][3]][4];
			ifce[nfce-1][3] = nedg;
			ifce[nfce-1][4] = 0;
c19985:
			goto c19988;
c19987:
			if (! (iedg[ifce[i][1]][1] == iedg[ifce[i][3]][1])) goto c19984;
			if (! (iedg[ifce[i][1]][2] == iedg[ifce[i][2]][1])) goto c19983;
			ifce[nfce-3][1] = iedg[ifce[i][2]][3];
			ifce[nfce-3][2] = iedg[ifce[i][1]][4];
			ifce[nfce-3][3] = nedg-2;
			ifce[nfce-3][4] = 0;
			ifce[nfce-2][1] = iedg[ifce[i][2]][4];
			ifce[nfce-2][2] = iedg[ifce[i][3]][4];
			ifce[nfce-2][3] = nedg-1;
			ifce[nfce-2][4] = 0;
			ifce[nfce-1][1] = iedg[ifce[i][1]][3];
			ifce[nfce-1][2] = iedg[ifce[i][3]][3];
			ifce[nfce-1][3] = nedg;
			ifce[nfce-1][4] = 0;
			goto c19982;
c19983:
			ifce[nfce-3][1] = iedg[ifce[i][1]][4];
			ifce[nfce-3][2] = iedg[ifce[i][2]][4];
			ifce[nfce-3][3] = nedg-2;
			ifce[nfce-3][4] = 0;
			ifce[nfce-2][1] = iedg[ifce[i][2]][3];
			ifce[nfce-2][2] = iedg[ifce[i][3]][4];
			ifce[nfce-2][3] = nedg-1;
			ifce[nfce-2][4] = 0;
			ifce[nfce-1][1] = iedg[ifce[i][1]][3];
			ifce[nfce-1][2] = iedg[ifce[i][3]][3];
			ifce[nfce-1][3] = nedg;
			ifce[nfce-1][4] = 0;
c19982:
			goto c19988;
c19984:
			if (! (iedg[ifce[i][1]][2] == iedg[ifce[i][2]][1])) goto c19981;
			ifce[nfce-3][1] = iedg[ifce[i][2]][3];
			ifce[nfce-3][2] = iedg[ifce[i][1]][4];
			ifce[nfce-3][3] = nedg-2;
			ifce[nfce-3][4] = 0;
			ifce[nfce-2][1] = iedg[ifce[i][3]][3];
			ifce[nfce-2][2] = iedg[ifce[i][2]][4];
			ifce[nfce-2][3] = nedg-1;
			ifce[nfce-2][4] = 0;
			ifce[nfce-1][1] = iedg[ifce[i][1]][3];
			ifce[nfce-1][2] = iedg[ifce[i][3]][4];
			ifce[nfce-1][3] = nedg;
			ifce[nfce-1][4] = 0;
			goto c19980;
c19981:
			ifce[nfce-3][1] = iedg[ifce[i][1]][4];
			ifce[nfce-3][2] = iedg[ifce[i][2]][4];
			ifce[nfce-3][3] = nedg-2;
			ifce[nfce-3][4] = 0;
			ifce[nfce-2][1] = iedg[ifce[i][2]][3];
			ifce[nfce-2][2] = iedg[ifce[i][3]][3];
			ifce[nfce-2][3] = nedg-1;
			ifce[nfce-2][4] = 0;
			ifce[nfce-1][1] = iedg[ifce[i][1]][3];
			ifce[nfce-1][2] = iedg[ifce[i][3]][4];
			ifce[nfce-1][3] = nedg;
			ifce[nfce-1][4] = 0;
c19980:
c19988:
			ifce[nfce  ][1] = nedg-2;
			ifce[nfce  ][2] = nedg-1;
			ifce[nfce  ][3] = nedg;
			ifce[nfce  ][4] = 0;
		}
		lfce[iela][2] = nfce;;
		ledg[iela][2] = nedg;;
	}
	rlow = dlow;
	rhgh = dhgh;
	rmin =  1.e36;
	rmax = -1.e36;
	for( i = 1; i <= nvtx; i++ ) {
		rmin = min(rmin,vrtx[i][4]);
		rmax = max(rmax,vrtx[i][4]);
	}
	return(0);
}

float c_ggdpnt (rlat,rlon)
float rlat, rlon;
{
	float x;
	float x0, y0, z0;
	double a1, b1, c1, a2, b2, c2, a3, b3, c3;
	float x1, y1, z1, w1, x2, y2, z2, w2, x3, y3, z3, w3;
	float xm, ym, zm, s1, s2, s3, t1, t2, t3;
	float d1, d2, d3, dist, dbst, rval;
	float ap, bp, cp, dp, tp, x4, y4, z4, x5, y5, z5, w5;
	int ifst, numb, itry, ipos, ibst;

	x0 = (float)(cos((double)rlat)*cos((double)rlon));
	y0 = (float)(cos((double)rlat)*sin((double)rlon));
	z0 = (float)(sin((double)rlat));
	ifst = 1;
	numb = 20;
	itry = 1;
c199:
	ipos = ifst;
c198:
	x1 = vrtx[iedg[ifce[ipos][1]][1]][1];
	y1 = vrtx[iedg[ifce[ipos][1]][1]][2];
	z1 = vrtx[iedg[ifce[ipos][1]][1]][3];
	w1 = vrtx[iedg[ifce[ipos][1]][1]][4];
	x2 = vrtx[iedg[ifce[ipos][1]][2]][1];
	y2 = vrtx[iedg[ifce[ipos][1]][2]][2];
	z2 = vrtx[iedg[ifce[ipos][1]][2]][3];
	w2 = vrtx[iedg[ifce[ipos][1]][2]][4];
	if (! (iedg[ifce[ipos][2]][1] != iedg[ifce[ipos][1]][1] && iedg[ifce[ipos][2]][1] != iedg[ifce[ipos][1]][2])) goto c197;
	x3 = vrtx[iedg[ifce[ipos][2]][1]][1];
	y3 = vrtx[iedg[ifce[ipos][2]][1]][2];
	z3 = vrtx[iedg[ifce[ipos][2]][1]][3];
	w3 = vrtx[iedg[ifce[ipos][2]][1]][4];
	goto c196;
c197:
	x3 = vrtx[iedg[ifce[ipos][2]][2]][1];
	y3 = vrtx[iedg[ifce[ipos][2]][2]][2];
	z3 = vrtx[iedg[ifce[ipos][2]][2]][3];
	w3 = vrtx[iedg[ifce[ipos][2]][2]][4];
c196:
	a1 = (y3*z2-y2*z3);
	b1 = (x2*z3-x3*z2);
	c1 = (x3*y2-x2*y3);
	a2 = (y1*z3-y3*z1);
	b2 = (x3*z1-x1*z3);
	c2 = (x1*y3-x3*y1);
	a3 = (y2*z1-y1*z2);
	b3 = (x1*z2-x2*z1);
	c3 = (x2*y1-x1*y2);
	if (! (itry == 1)) goto c195;
	xm = (x1+x2+x3)/3.;
	ym = (y1+y2+y3)/3.;
	zm = (z1+z2+z3)/3.;
	if( a1*xm+b1*ym+c1*zm >= 0. ) s1 =  1.;
	else                          s1 = -1.;
	if( a2*xm+b2*ym+c2*zm >= 0. ) s2 =  1.;
	else                          s2 = -1.;
	if( a3*xm+b3*ym+c3*zm >= 0. ) s3 =  1.;
	else                          s3 = -1.;
	if( a1*x0+b1*y0+c1*z0 >= 0. ) t1 =  1.;
	else                          t1 = -1.;
	if( a2*x0+b2*y0+c2*z0 >= 0. ) t2 =  1.;
	else                          t2 = -1.;
	if( a3*x0+b3*y0+c3*z0 >= 0. ) t3 =  1.;
	else                          t3 = -1.;
	if (s1 == t1 && s2 == t2 && s3 == t3) goto c194;
	goto c193;
c195:
	d1 = (float)(fabs(a1*x0+b1*y0+c1*z0)/sqrt(a1*a1+b1*b1+c1*c1));
	d2 = (float)(fabs(a2*x0+b2*y0+c2*z0)/sqrt(a2*a2+b2*b2+c2*c2));
	d3 = (float)(fabs(a3*x0+b3*y0+c3*z0)/sqrt(a3*a3+b3*b3+c3*c3));
	dist = min(min(d1,d2),d3);
	if (! (ibst == 0 || dist < dbst)) goto c192;
	ibst = ipos;
	dbst = dist;
c192:
c193:
	if (! (ipos == ifst+numb-1)) goto c191;
	if (! (itry == 1)) goto c190;
	ipos = ifst-1;
	itry = 2;
	ibst = 0;
	dbst = 0.;
	goto c189;
c190:
	ipos = ibst;
	x1 = vrtx[iedg[ifce[ipos][1]][1]][1];
	y1 = vrtx[iedg[ifce[ipos][1]][1]][2];
	z1 = vrtx[iedg[ifce[ipos][1]][1]][3];
	w1 = vrtx[iedg[ifce[ipos][1]][1]][4];
	x2 = vrtx[iedg[ifce[ipos][1]][2]][1];
	y2 = vrtx[iedg[ifce[ipos][1]][2]][2];
	z2 = vrtx[iedg[ifce[ipos][1]][2]][3];
	w2 = vrtx[iedg[ifce[ipos][1]][2]][4];
	if (! (iedg[ifce[ipos][2]][1] != iedg[ifce[ipos][1]][1] && iedg[ifce[ipos][2]][1] != iedg[ifce[ipos][1]][2])) goto c188;
	x3 = vrtx[iedg[ifce[ipos][2]][1]][1];
	y3 = vrtx[iedg[ifce[ipos][2]][1]][2];
	z3 = vrtx[iedg[ifce[ipos][2]][1]][3];
	w3 = vrtx[iedg[ifce[ipos][2]][1]][4];
	goto c187;
c188:
	x3 = vrtx[iedg[ifce[ipos][2]][2]][1];
	y3 = vrtx[iedg[ifce[ipos][2]][2]][2];
	z3 = vrtx[iedg[ifce[ipos][2]][2]][3];
	w3 = vrtx[iedg[ifce[ipos][2]][2]][4];
c187:
	goto c194;
c189:
c191:
	ipos = ipos+1;
	goto c198;
c194:
	ifst = ifce[ipos][4];
	numb = 4;
	itry = 1;
	if (! (ifst == 0)) goto c199;
	if (! (pow(x1-x0)+pow(y1-y0)+pow(z1-z0) < .000001)) goto c186;
	rval = w1;
	goto c185;
c186:
	ap = y1*(z3-z2)+y2*(z1-z3)+y3*(z2-z1);
	bp = x1*(z2-z3)+x2*(z3-z1)+x3*(z1-z2);
	cp = x1*(y3-y2)+x2*(y1-y3)+x3*(y2-y1);
	dp = x1*y2*z3-x1*y3*z2-x2*y1*z3+x3*y1*z2+x2*y3*z1-x3*y2*z1;
	tp = -dp/(ap*x0+bp*y0+cp*z0);
	x4 = tp*x0;
	y4 = tp*y0;
	z4 = tp*z0;
	a1 = y3*z2-y2*z3;
	b1 = x2*z3-x3*z2;
	c1 = x3*y2-x2*y3;
	tp = -(a1*x1+b1*y1+c1*z1)/(a1*(x4-x1)+b1*(y4-y1)+c1*(z4-z1));
	x5 = x1+tp*(x4-x1);
	y5 = y1+tp*(y4-y1);
	z5 = z1+tp*(z4-z1);
	w5 = w2+(w3-w2)*sqrt((double)((pow(x5-x2)+pow(y5-y2)+pow(z5-z2))/(pow(x3-x2)+pow(y3-y2)+pow(z3-z2))));
	rval = w1+(w5-w1)*sqrt((double)((pow(x4-x1)+pow(y4-y1)+pow(z4-z1))/(pow(x5-x1)+pow(y5-y1)+pow(z5-z1))));
c185:
	x = rlow+(rhgh-rlow)*(rval-rmin)/(rmax-rmin);
	return(x);
}

float fran()
{
	extern double trunc(double x);
	static double x = 2.718281828459045;
	double y, z;
	y = 9821.0*x+.211327;
	z = trunc(y);
	x = y - z;
	return((float)x);
}
