#include <stdio.h>
#include <math.h>

/*
 * Function prototypes for NCARG and NCARG-GKS routines
 */
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
	int i;
	Gcolr_rep rgb;
	float wavsiz, xb, xwid, xl, xr;
/*
 *  Example of windbarbs at various angles and a chart of wind speeds.
 *
 *  Example 01 - chart of windbarbs for various speeds.
 */
    gopen_gks ("stdout",0);
    gopen_ws (WKID, NULL, WSTYPE);
    gactivate_ws(WKID);

	rgb.rgb.red = 0.87; rgb.rgb.green = 0.87; rgb.rgb.blue = 0.87;
	gset_colr_rep(WKID,0,&rgb);
	rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,1,&rgb);
	rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 1.;
	gset_colr_rep(WKID,2,&rgb);
	rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 1.;
	gset_colr_rep(WKID,3,&rgb);
	rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,4,&rgb);
	rgb.rgb.red = 1.; rgb.rgb.green = 0.65; rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,5,&rgb);
/*
 *  Sun.
 */
	c_wmseti("SC1 - color of the center",4);
	c_wmseti("SC2 - color of the star points",5);
	c_wmseti("SC3 - color of the outlines",1);
	c_wmseti("SC4 - shadow color",1);
	c_wmsetr("SHT - size of sun",.0375);
	c_wmlabs(0.27, 0.65, "SU");
/*
 *  Cloud.
 */
	c_wmseti("CC1 - primary color",3);
	c_wmseti("CC2 - outline color",1);
	c_wmseti("CC3 - shadow color",1);
	c_wmsetr("SHT - size of cloud",0.047);
	c_wmlabs(0.55, 0.33, "C");
/*
 *  Title.
 */
	c_plchhq(.52,.78,":F26:NCAR",.076,0.,-1.);
	c_plchhq(.52,.67,":F26:Graphics",.0555,0.,-1.);
/*
 *  Waves.
 */
	wavsiz = .15;
	c_pcseti("CC",2);
	c_pcseti("TE",1);
	c_plchhq(.5,.2,":F37:n",wavsiz,360.,0.);
	c_pcseti("TE",0);
	c_pcgetr("DL",&xl);
	c_pcgetr("DR",&xr);
	xwid = xl+xr;

	xb = .2;
	gset_linewidth(6.0);
	for( i = 1; i <= 4; i++ ) {
        c_plchhq (xb,.15,":F37:n",wavsiz,0.,0.);
        xb = xb+xwid;
	}

	c_frame();

	gdeactivate_ws (WKID);
	gclose_ws (WKID);
	gclose_gks();

}

