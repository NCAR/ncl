/*
 *	$Id: c_fagaxclr.c,v 1.1 1994-10-31 02:14:13 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

#define NPTS   200
#define NCURVE  4

main()
{
/*
 *  Define error file, fortran unit number, and workstation type,
 *  and workstation id.
 */
	float ydra[NCURVE][NPTS],xdra[NPTS];
	int i, j;
	extern void defclr();

	for( i = 0; i < NPTS; i++ ) {
		xdra[i  ]=i*0.1;
		for( j = 0; j < NCURVE; j++ ) {
			ydra[j][i] = sin(xdra[i]+0.2*(j+1))*exp(-0.01*xdra[i]*(j+1)*(j+1));
		}
	}
/*
 *  Open gks, open and activate a workstation.
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);

	defclr();

	c_ezmxy (xdra,&ydra[0][0],NPTS,NCURVE,NPTS,"Axis Colors$");
/*
 *  Deactivate and close the workstation, close gks.
 */
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}

void defclr()
{
	Gcolr_rep rgb;

	rgb.rgb.red = 0.0; rgb.rgb.green =  0.0; rgb.rgb.blue =  0.0;
	gset_colr_rep(WKID, 0, &rgb);
	rgb.rgb.red = 1.0; rgb.rgb.green =  1.0; rgb.rgb.blue =  1.0;
	gset_colr_rep(WKID, 1, &rgb);
	rgb.rgb.red = 1.0; rgb.rgb.green =  0.0; rgb.rgb.blue =  0.0;
	gset_colr_rep(WKID, 2, &rgb);
	rgb.rgb.red = 0.0; rgb.rgb.green =  1.0; rgb.rgb.blue =  0.0;
	gset_colr_rep(WKID, 3, &rgb);
	rgb.rgb.red = 0.4; rgb.rgb.green =  0.7; rgb.rgb.blue =  0.9;
	gset_colr_rep(WKID, 4, &rgb);
	rgb.rgb.red = 0.7; rgb.rgb.green =  0.4; rgb.rgb.blue =  0.7;
	gset_colr_rep(WKID, 5, &rgb);
	rgb.rgb.red = 0.9; rgb.rgb.green =  0.7; rgb.rgb.blue =  0.4;
	gset_colr_rep(WKID, 6, &rgb);
	rgb.rgb.red = 0.4; rgb.rgb.green =  0.9; rgb.rgb.blue =  0.7;
	gset_colr_rep(WKID, 7, &rgb);
	return;
}

void NGCALLF(agchax,AGCHAX)(iflg,iaxs,iprt,vils)
int *iflg, *iaxs, *iprt;
float *vils;
{
	c_plotif (0.,0.,2);
	if (*iflg == 0) {
        gset_line_colr_ind(2);
        gset_text_colr_ind(3);
	}
	else {
        gset_line_colr_ind(1);
        gset_text_colr_ind(1);
	}
	return;
}
