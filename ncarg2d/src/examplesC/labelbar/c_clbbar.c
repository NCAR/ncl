/*
 *	$Id: c_clbbar.c,v 1.3 1994-11-10 21:19:12 haley Exp $
 */
#include <stdio.h>
#include <math.h>

/*
 * Include function prototypes
 */
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

char *labels[18] = {"White", "Orchid", "Red", "OrangeRed", "Orange","Gold", \
                    "Yellow", "GreenYellow", "Chartreuse","Green", "Celeste",\
                    "Aqua","DeepSkyBlue","RoyalBlue","SlateBlue", \
                    "DarkViolet", "Lavender", "Grey"};
	
int index[18] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18};
#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
	extern void color();
	gopen_gks("stdout",0);
	gopen_ws(WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
	color();
	gset_fill_int_style(GSTYLE_SOLID);
	c_lblbar(1,0.,1.,0.,1.,18,.5,1.,index,1,labels,18,1);
/*
** Advance frame
*/
	c_frame();
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}


void color()
{
	Gcolr_rep rgb[19];
	int i;

	rgb[0].rgb.red = 0.; rgb[0].rgb.green = 0.; rgb[0].rgb.blue = 0.;
	rgb[1].rgb.red = 1.0; rgb[1].rgb.green = 1.0; rgb[1].rgb.blue = 1.0;
	rgb[2].rgb.red = 0.85; rgb[2].rgb.green = 0.45; rgb[2].rgb.blue = 0.8;
	rgb[3].rgb.red = 0.9; rgb[3].rgb.green = 0.25; rgb[3].rgb.blue = 0.0;
	rgb[4].rgb.red = 1.0; rgb[4].rgb.green = 0.0; rgb[4].rgb.blue = 0.2;
	rgb[5].rgb.red = 1.0; rgb[5].rgb.green = 0.65; rgb[5].rgb.blue = 0.0;
	rgb[6].rgb.red = 1.0; rgb[6].rgb.green = 0.85; rgb[6].rgb.blue = 0.0;
	rgb[7].rgb.red = 1.0; rgb[7].rgb.green = 1.0; rgb[7].rgb.blue = 0.0;
	rgb[8].rgb.red = 0.7; rgb[8].rgb.green = 1.0; rgb[8].rgb.blue = 0.2;
	rgb[9].rgb.red = 0.5; rgb[9].rgb.green = 1.0; rgb[9].rgb.blue = 0.0;
	rgb[10].rgb.red = 0.2; rgb[10].rgb.green = 0.8; rgb[10].rgb.blue = 0.2;
	rgb[11].rgb.red = 0.2; rgb[11].rgb.green = 1.0; rgb[11].rgb.blue = 0.5;
	rgb[12].rgb.red = 0.0; rgb[12].rgb.green = 0.9; rgb[12].rgb.blue = 1.0;
	rgb[13].rgb.red = 0.0; rgb[13].rgb.green = 0.75; rgb[13].rgb.blue = 1.0;
	rgb[14].rgb.red = 0.25; rgb[14].rgb.green = 0.45; rgb[14].rgb.blue = 0.95;
	rgb[15].rgb.red = 0.4; rgb[15].rgb.green = 0.35; rgb[15].rgb.blue = 0.8;
	rgb[16].rgb.red = 0.6; rgb[16].rgb.green = 0.0; rgb[16].rgb.blue = 0.8;
	rgb[17].rgb.red = 0.8; rgb[17].rgb.green = 0.8; rgb[17].rgb.blue = 1.0;
	rgb[18].rgb.red = 0.5; rgb[18].rgb.green = 0.5; rgb[18].rgb.blue = 0.5;
	for( i = 0; i <= 18; i++ ) {
		gset_colr_rep(WKID,i,&rgb[i]);
	}	return;
}
