/*
 *	$Id: c_class3.c.sed,v 1.1 1994-08-01 22:15:11 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

#define NPTS   200

main()
{
	float ydra[NPTS],xdra[NPTS];
	int i;
	extern void defclr();
	extern int agchax_(), agchcu_(), agpwrt_(), agchil_();
	for( i = 1; i <= NPTS; i++ ) {
		xdra[i-1] = i*0.1;
		ydra[i-1] = exp(xdra[i-1]*sin(xdra[i-1]));
	}
/*
 *  Open gks, open and activate a workstation.
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Set up a color table
 */
	defclr();

	c_agseti ("Y/LOGARITHMIC.",1);
	c_agseti("DASH/SELECTOR.",-1);

	c_agsetc("LABEL/NAME.","B");
	c_agseti("LINE/NUMBER.",-100);
	c_agsetc("LINE/TEXT.","TIME (SECONDS)$");

	c_agsetc("LABEL/NAME.","L");
	c_agseti("LINE/NUMBER.",100);
	c_agsetc("LINE/TEXT.","POSITION (METERS)$");

	c_ezxy (xdra,ydra,NPTS,"Log scaling and publication quality text$");
/*
 * Deactivate and close workstation, close gks.
 */
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}

void defclr()
{
	Gcolr_rep rgb;
/*
 * Define a color table
 */
    rgb.rgb.red = 0.0;
    rgb.rgb.green = 0.0;
    rgb.rgb.blue = 0.0;
    gset_colr_rep(WKID, 0, &rgb);
    rgb.rgb.red = 1.0;
    rgb.rgb.green = 1.0;
    rgb.rgb.blue = 1.0;
    gset_colr_rep(WKID, 1, &rgb);
    rgb.rgb.red = 1.0;
    rgb.rgb.green = 0.0;
    rgb.rgb.blue = 0.0;
    gset_colr_rep(WKID, 2, &rgb);
    rgb.rgb.red = 0.0;
    rgb.rgb.green = 1.0;
    rgb.rgb.blue = 0.0;
    gset_colr_rep(WKID, 3, &rgb);
    rgb.rgb.red = 0.4;
    rgb.rgb.green = 0.7;
    rgb.rgb.blue = 0.9;
    gset_colr_rep(WKID, 4, &rgb);
    rgb.rgb.red = 0.7;
    rgb.rgb.green = 0.4;
    rgb.rgb.blue = 0.7;
    gset_colr_rep(WKID, 5, &rgb);
    rgb.rgb.red = 0.9;
    rgb.rgb.green = 0.7;
    rgb.rgb.blue = 0.4;
    gset_colr_rep(WKID, 6, &rgb);
    rgb.rgb.red = 0.4;
    rgb.rgb.green = 0.9;
    rgb.rgb.blue = 0.7;
    gset_colr_rep(WKID, 7, &rgb);
    return;
}
        
int agchax_(iflg,iaxs,iprt,vils)
int *iflg, *iaxs, *iprt;
float *vils;
{
	c_plotif (0.,0.,2);
	if (*iflg == 0) {
        gset_line_colr_ind( 2 );
        gset_text_colr_ind( 3 );
	}
	else {
        gset_line_colr_ind(1);
        gset_text_colr_ind(1);		
	}
	return(0);
}

int agchcu_(iflg,kdsh)
int *iflg, *kdsh;
{
	c_plotif (0.,0.,2);
	if (*iflg == 0) {
		gset_line_colr_ind( abs(*kdsh)+3 );
		gset_text_colr_ind( abs(*kdsh)+3 );
	}
	else {
        gset_line_colr_ind(1);
        gset_text_colr_ind(1);		
	}
	return(0);
}

int agchil_(iflg,lbnm,lnno)
int *iflg, *lbnm, *lnno;
{
	c_plotif (0.,0.,2);
	if (*iflg == 0) {
		gset_text_colr_ind( 4 );
	}
	else {
		gset_text_colr_ind( 1 );
	}
	return(0);
}

int agpwrt_(xpos, ypos, chrs, nchs, isiz, iori, icen)
float *xpos, *ypos;
char *chrs;
int *nchs, *isiz, *iori, *icen;
{
	char *stmp;
	float csfl;

	stmp = (char *)malloc((*nchs+1)*sizeof(char));
	c_pcgetr ("CS - CONSTANT SPACING FLAG", &csfl);
/*
 * If the label centering option is on, give wider spacing.
 */
	if (*icen != 0) {
        c_pcsetr ("CS - CONSTANT SPACING FLAG", 1.25);
	}
	else {
        c_pcsetr ("CS - CONSTANT SPACING FLAG", 0.0 );
	}
/*
 * Set the size of the labels to be the same as Autograph
 * would normally use.
 */
	strncpy( stmp, chrs, *nchs );
	stmp[*nchs] = '\0';
	c_plchhq (*xpos, *ypos, stmp,.8*(float)*isiz, (float)*iori, (float)*icen);
/*
 * Return spacing to whatever it was before we wrote this label
 */
	c_pcsetr ("CS - CONSTANT SPACING FLAG", csfl);
	free(stmp);
	return(0);
}                                                            
