/*
 *	$Id: c_clbbar.c,v 1.1 1994-05-23 21:26:05 haley Exp $
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
main()
{
	extern void color();
	c_opngks();
	color();
	gset_fill_int_style(GSTYLE_SOLID);
	c_lblbar(1,0.,1.,0.,1.,18,.5,1.,index,1,labels,18,1);
	c_clsgks();
}


void color()
{
	Gcolr_rep rgb;

	rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
	gset_colr_rep(1,0,&rgb);
	rgb.rgb.red =  1.0; rgb.rgb.green =  1.0; rgb.rgb.blue =  1.0;
	gset_colr_rep(1,  1,&rgb);
	rgb.rgb.red =  0.85; rgb.rgb.green =  0.45; rgb.rgb.blue =  0.8;
	gset_colr_rep(1,  2,&rgb);
	rgb.rgb.red =  0.9; rgb.rgb.green =  0.25; rgb.rgb.blue =  0.0;
	gset_colr_rep(1,  3,&rgb);
	rgb.rgb.red =  1.0; rgb.rgb.green =  0.0; rgb.rgb.blue =  0.2;
	gset_colr_rep(1,  4,&rgb);
	rgb.rgb.red =  1.0; rgb.rgb.green =  0.65; rgb.rgb.blue =  0.0;
	gset_colr_rep(1,  5,&rgb);
	rgb.rgb.red =  1.0; rgb.rgb.green =  0.85; rgb.rgb.blue =  0.0;
	gset_colr_rep(1,  6,&rgb);
	rgb.rgb.red =  1.0; rgb.rgb.green =  1.0; rgb.rgb.blue =  0.0;
	gset_colr_rep(1,  7,&rgb);
	rgb.rgb.red =  0.7; rgb.rgb.green =  1.0; rgb.rgb.blue =  0.2;
	gset_colr_rep(1,  8,&rgb);
	rgb.rgb.red =  0.5; rgb.rgb.green =  1.0; rgb.rgb.blue =  0.0;
	gset_colr_rep(1,  9,&rgb);
	rgb.rgb.red =  0.2; rgb.rgb.green =  0.8; rgb.rgb.blue =  0.2;
	gset_colr_rep(1, 10,&rgb);
	rgb.rgb.red =  0.2; rgb.rgb.green =  1.0; rgb.rgb.blue =  0.5;
	gset_colr_rep(1, 11,&rgb);
	rgb.rgb.red =  0.0; rgb.rgb.green =  0.9; rgb.rgb.blue =  1.0;
	gset_colr_rep(1, 12,&rgb);
	rgb.rgb.red =  0.0; rgb.rgb.green =  0.75; rgb.rgb.blue =  1.0;
	gset_colr_rep(1, 13,&rgb);
	rgb.rgb.red =  0.25; rgb.rgb.green =  0.45; rgb.rgb.blue =  0.95;
	gset_colr_rep(1, 14,&rgb);
	rgb.rgb.red =  0.4; rgb.rgb.green =  0.35; rgb.rgb.blue =  0.8;
	gset_colr_rep(1, 15,&rgb);
	rgb.rgb.red =  0.6; rgb.rgb.green =  0.0; rgb.rgb.blue =  0.8;
	gset_colr_rep(1, 16,&rgb);
	rgb.rgb.red =  0.8; rgb.rgb.green =  0.8; rgb.rgb.blue =  1.0;
	gset_colr_rep(1, 17,&rgb);
	rgb.rgb.red =  0.5; rgb.rgb.green =  0.5; rgb.rgb.blue =  0.5;
	gset_colr_rep(1, 18,&rgb);
	return;
}
