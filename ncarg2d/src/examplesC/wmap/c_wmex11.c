#include <stdio.h>
#include <math.h>

/*
 * Function prototypes for NCARG and NCARG-GKS routines
 */
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

#define X1  0.22

char *imdat = "11212833201001120000300004014752028601117706086792";

main()
{
	Gcolr_rep rgb;
	float siz;
	float x2 = 0.65, y2 = 0.73;
    extern void exstnm();
/*
 *  Example of windbarbs at various angles and a chart of wind speeds.
 *
 *  Example 01 - chart of windbarbs for various speeds.
 */
    gopen_gks ("stdout",0);
    gopen_ws (WKID, NULL, WSTYPE);
    gactivate_ws(WKID);

	rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 1.;
	gset_colr_rep(WKID,0,&rgb);
	rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,1,&rgb);
	rgb.rgb.red = 0.4; rgb.rgb.green = 0.; rgb.rgb.blue = 0.4;
	gset_colr_rep(WKID,2,&rgb);
/*
 *  Symbolic station model
 */
    c_wmsetr("WBS",0.20);
    gset_linewidth(3.);
    exstnm(&x2,&y2);
    c_line(0.1,.5,0.9,.5);
    c_plotit(0,0,0);
    siz = 0.03;
    c_pcseti("FN",26);
    c_pcseti("CC",2);
    c_plchhq(X1,.85,"SYMBOLIC",siz,0.,0.);
    c_plchhq(X1,.75,"STATION",siz,0.,0.);
    c_plchhq(X1,.65,"MODEL",siz,0.,0.);
    c_pcseti("CC",1);
/*
 *  Sample plotted report
 */
	c_wmstnm(x2,.23,imdat);
	c_pcseti("FN",26);
	c_pcseti("CC",2);
	c_plchhq(X1,.35,"SAMPLE",siz,0.,0.);
	c_plchhq(X1,.25,"PLOTTED",siz,0.,0.);
	c_plchhq(X1,.15,"DATA",siz,0.,0.);
	c_pcseti("CC",1);

	c_frame();

	gdeactivate_ws (WKID);
	gclose_ws (WKID);
	gclose_gks();

}

void exstnm(x, y)
float *x, *y;
{
	float xndc[1], yndc[1], wbc, wbshft, siz;
    int ibo;
/*
 *  Convert X and Y to NDC and work in NDC space.
 */
    c_wmw2nx(1,x,xndc);
    c_wmw2ny(1,y,yndc);
/*
 *  Draw a wind barb at 320 degrees and cloud cover symbol.
 */
    c_wmseti("WBF",1);
    c_wmbarb(xndc[0],yndc[0],-5.,8.66);
    c_wmgeti("WBF",&ibo);
    c_wmgetr("wbc",&wbc);
    c_wmgetr("WBS",&wbshft);
    c_ngwsym("N",0,xndc[0],yndc[0],wbc*wbshft,1,0);

    siz = 0.15*wbshft;
    c_pcseti("FN",21);
    c_plchhq(xndc[0],yndc[0],"N",siz,0.,0.);
/*
 *  Direction
 */
	c_wmseti("RBS",0);
	c_wmsetr("RMG",.030*wbshft);
	c_wmsetr("tht",0.8*siz);
	c_wmlabt(xndc[0]-0.7*wbshft*0.5,yndc[0]+0.7*wbshft*0.866,"dd",0);
/*
 *  wind speed
 */
	c_wmlabt(xndc[0]-0.92*wbshft*0.5,yndc[0]+0.7*wbshft*1.5,"ff",0);
/*
 *  High clouds (CH).
 */
	c_plchhq(xndc[0],yndc[0]+0.83*wbshft,"C:B1:H",siz,0.,0.);
/*
 *  Medium clouds (CM).
 */
	c_plchhq(xndc[0],yndc[0]+0.47*wbshft,"C:B1:M",siz,0.,0.);
/*
 *  Current temperature (TT).
 */
	c_plchhq(xndc[0]-0.7*wbshft,yndc[0]+0.36*wbshft,"TT",siz,0.,0.);
/*
 *  Barometric pressure (ppp).
 */
	c_plchhq(xndc[0]+0.55*wbshft,yndc[0]+0.36*wbshft,"ppp",siz,0.,0.);
/*
 *  Visibility (VV).
 */
	c_plchhq(xndc[0]-.95*wbshft,yndc[0],"VV",siz,0.,0.);
/*
 *  Present weather (ww).
 */
	c_plchhq(xndc[0]-0.45*wbshft,yndc[0],"ww",siz,0.,0.);
/*
 *  Pressure change (pp).
 */
	c_plchhq(xndc[0]+0.5*wbshft,yndc[0],"pp",siz,0.,0.);
/*
 *  Pressure tendency (a).
 */
	c_plchhq(xndc[0]+wbshft,yndc[0],"a",siz,0.,0.);
/*
 *  Temperature of dewpoint (TD).
 */
	c_plchhq(xndc[0]-0.65*wbshft,yndc[0]-0.42*wbshft,"T:B1:d",siz,0.,0.);
/*
 *  Low clouds (CL).
 */
	c_plchhq(xndc[0]-0.17*wbshft,yndc[0]-0.42*wbshft,"C:B1:L",siz,0.,0.);
/*
 *  Sky cover (NH).
 */
	c_plchhq(xndc[0]+0.31*wbshft,yndc[0]-0.42*wbshft,"N:B1:h",siz,0.,0.);
/*
 *  Past weather (W).
 */
	c_plchhq(xndc[0]+0.75*wbshft,yndc[0]-0.42*wbshft,"W",siz,0.,0.);
/*
 *  Cloud height (h).
 */
	c_plchhq(xndc[0]-0.12*wbshft,yndc[0]-0.72*wbshft,"h",siz,0.,0.);
/*
 *  Precipitation in last 6 hours (RR).
 */
	c_plchhq(xndc[0]+0.53*wbshft,yndc[0]-0.72*wbshft,"RR",siz,0.,0.);
	return;
}
