#include <stdio.h>
#include <math.h>

/*
 * Function prototypes for NCARG and NCARG-GKS routines
 */
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define IWTYPE 1
#define WKID   1

#define T1   0.90
#define T2   0.84

main()
{
	int i;
	Gcolr_rep rgb;
	float xcl, xcc, xcr;
	float xl, xc, xr;
	float finc, size, p1;
	float ang, scl, u, v, wslen;
/*
 *  Example of windbarbs at various angles and a chart of wind speeds.
 *
 *  Example 01 - chart of windbarbs for various speeds.
 */
    gopen_gks ("stdout",0);
    gopen_ws (WKID, NULL, IWTYPE);
    gactivate_ws(WKID);

	rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 1.;
	gset_colr_rep(WKID,0,&rgb);
	rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,1,&rgb);
	rgb.rgb.red = 1.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,2,&rgb);
	rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 1.;
	gset_colr_rep(WKID,3,&rgb);

	c_perim(1,1,1,1);
	c_line(0.000, T1+0.005, 1.000, T1+0.005);
	c_line(0.000, T1-0.005, 1.000, T1-0.005);
	c_line(0.000, T2, 1.000, T2);
	c_line(0.495, 0.000, 0.495, T1-0.005);
	c_line(0.505, 0.000, 0.505, T1-0.005);

	c_plchhq(0.5,0.955,":F25:Wind Speeds",.03,0.,0.);
	c_pcseti("FN",21);
	xcl = 0.12;
	xcc = 0.26;
	xcr = 0.40;
	for( i=1; i <= 2; i++ ) {
        xl = xcl+(i-1)*0.5;
        xc = xcc+(i-1)*0.5;
        xr = xcr+(i-1)*0.5;
        c_plchhq(xl,0.87,"Sym.",0.022,0.,0.);
        c_plchhq(xc,0.87,"Knots",0.022,0.,0.) ;
        c_plchhq(xr,0.87,"Miles/hr.",0.022,0.,0.) ;
	}
	finc = T2/10.;
	size = 0.022;
	xcl = 0.16;
	gset_linewidth(3.);
	c_ngseti("WO",1);
	c_ngseti("CA",0);

	p1 = T2-0.75*finc;
	c_wmsetr("WBS",0.1);
	c_wmseti("COL",1);
	c_wmgetr("WBS",&wslen);
	c_wmbarb(xcl-0.5*wslen,p1-0.5*size,0.,0.);
	c_plchhq(xcc,p1,"Calm",size,0.,0.);
	c_plchhq(xcr,p1,"Calm",size,0.,0.);

	p1 = p1-finc;
	c_wmbarb(xcl,p1-0.5*size,-1.,0.);
	c_plchhq(xcc,p1,"1-2",size,0.,0.);
	c_plchhq(xcr,p1,"1-2",size,0.,0.);

	p1 = p1-finc;
	c_wmbarb(xcl,p1-0.5*size,-5.,0.);
	c_plchhq(xcc,p1,"3-7",size,0.,0.);
	c_plchhq(xcr,p1,"3-8",size,0.,0.);

	p1 = p1-finc;
	c_wmbarb(xcl,p1-0.5*size,-10.,0.);
	c_plchhq(xcc,p1,"8-12",size,0.,0.);
	c_plchhq(xcr,p1,"9-14",size,0.,0.);

	p1 = p1-finc;
	c_wmbarb(xcl,p1-0.5*size,-15.,0.);
	c_plchhq(xcc,p1,"13-17",size,0.,0.);
	c_plchhq(xcr,p1,"15-20",size,0.,0.);

	p1 = p1-finc;
	c_wmbarb(xcl,p1-0.5*size,-20.,0.);
	c_plchhq(xcc,p1,"18-22",size,0.,0.);
	c_plchhq(xcr,p1,"21-25",size,0.,0.);

	p1 = p1-finc;
	c_wmbarb(xcl,p1-0.5*size,-25.,0.);
	c_plchhq(xcc,p1,"23-27",size,0.,0.);
	c_plchhq(xcr,p1,"26-31",size,0.,0.);

	p1 = p1-finc;
	c_wmbarb(xcl,p1-0.5*size,-30.,0.);
	c_plchhq(xcc,p1,"28-32",size,0.,0.);
	c_plchhq(xcr,p1,"32-37",size,0.,0.);

	p1 = p1-finc;
	c_wmbarb(xcl,p1-0.5*size,-35.,0.);
	c_plchhq(xcc,p1,"33-37",size,0.,0.);
	c_plchhq(xcr,p1,"38-43",size,0.,0.);

	xcl = xcl+0.5;
	xcc = xcc+0.5;
	xcr = xcr+0.5;
	p1 = T2-0.75*finc;
	c_wmbarb(xcl,p1-0.5*size,-40.,0.);
	c_plchhq(xcc,p1,"38-42",size,0.,0.);
	c_plchhq(xcr,p1,"44-49",size,0.,0.);

	p1 = p1-finc;
	c_wmbarb(xcl,p1-0.5*size,-45.,0.);
	c_plchhq(xcc,p1,"43-47",size,0.,0.);
	c_plchhq(xcr,p1,"50-54",size,0.,0.);

	p1 = p1-finc;
	c_wmbarb(xcl,p1-0.5*size,-50.,0.);
	c_plchhq(xcc,p1,"48-52",size,0.,0.);
	c_plchhq(xcr,p1,"55-60",size,0.,0.);

	p1 = p1-finc;
	c_wmbarb(xcl,p1-0.5*size,-55.,0.);
	c_plchhq(xcc,p1,"53-57",size,0.,0.);
	c_plchhq(xcr,p1,"61-66",size,0.,0.);

	p1 = p1-finc;
	c_wmbarb(xcl,p1-0.5*size,-60.,0.);
	c_plchhq(xcc,p1,"58-62",size,0.,0.);
	c_plchhq(xcr,p1,"67-71",size,0.,0.);

	p1 = p1-finc;
	c_wmbarb(xcl,p1-0.5*size,-65.,0.);
	c_plchhq(xcc,p1,"63-67",size,0.,0.);
	c_plchhq(xcr,p1,"72-77",size,0.,0.);

	p1 = p1-finc;
	c_wmbarb(xcl,p1-0.5*size,-70.,0.);
	c_plchhq(xcc,p1,"68-72",size,0.,0.);
	c_plchhq(xcr,p1,"78-83",size,0.,0.);

	p1 = p1-finc;
	c_wmbarb(xcl,p1-0.5*size,-75.,0.);
	c_plchhq(xcc,p1,"73-77",size,0.,0.);
	c_plchhq(xcr,p1,"84-89",size,0.,0.);

	p1 = p1-finc;
	c_wmbarb(xcl,p1-0.5*size,-105.,0.);
	c_plchhq(xcc,p1,"103-107",size,0.,0.);
	c_plchhq(xcr,p1,"119-123",size,0.,0.);

	c_frame();
/*
 *  Example 02 - windbarbs at various angles.
 *
 *  Draw direction arrows and lebels.
 */
	c_wmseti("COL",3);
	c_wmsetr("ARL",7.8);
	c_wmsetr("ARD",0.);
	c_wmsetr("ARS",.1);
	c_wmlabs(0.9,0.4,"Arrow");
	c_wmsetr("ARD",90.);
	c_wmlabs(0.5,0.8,"Arrow");
	c_pcseti("CC",3);
	c_plchhq(0.5,0.83,":F22:N",.03,0.,0.);
	c_plchhq(0.93,0.4,":F22:E",.03,0.,0.);
/*
 *  Draw windbarbs.
 */
	c_wmseti("COL",2);
	c_wmsetr("WBS",0.3);
	gset_linewidth(3.);
	scl = 65.;
	for( i=1; i <= 13; i++ ) {
        ang = 15.+(i-1)*30.*3.14159/180.;
        u = cos(ang);
        v = sin(ang);
        c_wmbarb(0.5,0.4,scl*u,scl*v);
	}
/*
 *  Main title.
 */
	c_pcseti("CC",3);
	c_plchhq(0.5,0.93,":F25:Windbarbs",.04,0.,0.);
	c_frame();

	gdeactivate_ws (WKID);
	gclose_ws (WKID);
	gclose_gks();

}

