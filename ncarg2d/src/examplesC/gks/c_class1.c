/*
 *	$Id: c_class1.c,v 1.1 1994-08-01 22:22:32 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

#define NBALLN 15
#define NSTR1   2
#define NSTR2   8
#define NMOUTH  5
#define NNECK   2
#define NHEAD   28
#define NBLNKT  11
#define NTAIL   16
#define NRHLEG   8
#define NBELLY   3
#define NFFLEG  14

main()
{
	int ind[100];
	float xballn[NBALLN],yballn[NBALLN],xstr1[NSTR1],ystr1[NSTR1];
	float xstr2[NSTR2],ystr2[NSTR2], xmouth[NMOUTH],ymouth[NMOUTH],eye[2];
	float xhead[NHEAD],yhead[NHEAD],xneck[NNECK],yneck[NNECK];
	float xblnkt[NBLNKT],yblnkt[NBLNKT], xtail[NTAIL],ytail[NTAIL];
	float xrhleg[NRHLEG],yrhleg[NRHLEG], xbelly[NBELLY],ybelly[NBELLY];
	float xffleg[NFFLEG],yffleg[NFFLEG],dst[100];
	FILE *fp;
	char stmp[256];
	Gcolr_rep rgb;
	Gpoint_list fill_area;
	int i;
/*
 * malloc some space
 */
	fill_area.num_points = 28;
	fill_area.points = (Gpoint *)malloc(fill_area.num_points * sizeof(Gpoint));
	if( !fill_area.points ) {
		fprintf( stderr, "Not enough memory to create fill area structure\n" );
		gemergency_close_gks();
		exit(1);
	}

	fp = fopen("class1.dat","r" );
	if( !fp ) {
		fprintf( stderr, "Can't open class1.dat.\n" );
		gemergency_close_gks();
		exit(1);
	}
		
/*
 * Read in the data
 */
	for( i = 0; i < NBALLN; i++ ) {
		fscanf( fp, "%g", &xballn[i] );
	}
	gets(stmp);
	for( i = 0; i < NBALLN; i++ ) {
		fscanf( fp, "%g", &yballn[i] );
	}
	gets(stmp);
	for( i = 0; i < NSTR1; i++ ) {
		fscanf( fp, "%g", &xstr1[i] );
	}
	gets(stmp);
	for( i = 0; i < NSTR1; i++ ) {
		fscanf( fp, "%g", &ystr1[i] );
	}
	gets(stmp);
	for( i = 0; i < NSTR2; i++ ) {
		fscanf( fp, "%g", &xstr2[i] );
	}
	gets(stmp);
	for( i = 0; i < NSTR2; i++ ) {
		fscanf( fp, "%g", &ystr2[i] );
	}
	gets(stmp);
	for( i = 0; i < NMOUTH; i++ ) {
		fscanf( fp, "%g", &xmouth[i] );
	}
	gets(stmp);
	for( i = 0; i < NMOUTH; i++ ) {
		fscanf( fp, "%g", &ymouth[i] );
	}
	gets(stmp);
	for( i = 0; i < 2; i++ ) {
		fscanf( fp, "%g", &eye[i] );
	}
	gets(stmp);
	for( i = 0; i < NHEAD; i++ ) {
		fscanf( fp, "%g", &xhead[i] );
	}
	gets(stmp);
	for( i = 0; i < NHEAD; i++ ) {
		fscanf( fp, "%g", &yhead[i] );
	}
	gets(stmp);
	for( i = 0; i < NNECK; i++ ) {
		fscanf( fp, "%g", &xneck[i] );
	}
	gets(stmp);
	for( i = 0; i < NNECK; i++ ) {
		fscanf( fp, "%g", &yneck[i] );
	}
	gets(stmp);
	for( i = 0; i < NBLNKT; i++ ) {
		fscanf( fp, "%g", &xblnkt[i] );
	}
	gets(stmp);
	for( i = 0; i < NBLNKT; i++ ) {
		fscanf( fp, "%g", &yblnkt[i] );
	}
	gets(stmp);
	for( i = 0; i < NTAIL; i++ ) {
		fscanf( fp, "%g", &xtail[i] );
	}
	gets(stmp);
	for( i = 0; i < NTAIL; i++ ) {
		fscanf( fp, "%g", &ytail[i] );
	}
	gets(stmp);
	for( i = 0; i < NRHLEG; i++ ) {
		fscanf( fp, "%g", &xrhleg[i] );
	}
	gets(stmp);
	for( i = 0; i < NRHLEG; i++ ) {
		fscanf( fp, "%g", &yrhleg[i] );
	}
	gets(stmp);
	for( i = 0; i < NBELLY; i++ ) {
		fscanf( fp, "%g", &xbelly[i] );
	}
	gets(stmp);
	for( i = 0; i < NBELLY; i++ ) {
		fscanf( fp, "%g", &ybelly[i] );
	}
	gets(stmp);
	for( i = 0; i < NFFLEG; i++ ) {
		fscanf( fp, "%g", &xffleg[i] );
	}
	gets(stmp);
	for( i = 0; i < NFFLEG; i++ ) {
		fscanf( fp, "%g", &yffleg[i] );
	}
/*
 *  Open GKS, open and activate a workstation.
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Set up color table
 */
	rgb.rgb.red = 0.0; rgb.rgb.green = 0.0; rgb.rgb.blue = 0.0;
	gset_colr_rep(WKID,0,&rgb);
	rgb.rgb.red = 1.0; rgb.rgb.green = 1.0; rgb.rgb.blue = 1.0;
	gset_colr_rep(WKID,1,&rgb);
	rgb.rgb.red = 1.0; rgb.rgb.green = 0.7; rgb.rgb.blue = 0.7;
	gset_colr_rep(WKID,2,&rgb);
	rgb.rgb.red = 0.0; rgb.rgb.green = 1.0; rgb.rgb.blue = 1.0;
	gset_colr_rep(WKID,3,&rgb);
	rgb.rgb.red = 1.0; rgb.rgb.green = 1.0; rgb.rgb.blue = 0.3;
	gset_colr_rep(WKID,4,&rgb);
/*
 * Set title color and draw title before changing 
 * from default normalization transformation
 */
	gset_line_colr_ind(4);
	c_plchhq(0.5,.92,"Every dog has his day",.017,0.0,0.0);
/*
 * Set normalization transformation and scaling
 */
	c_set(0.1,0.9,0.1,0.9,-4.,14.,0.0,18.0,1);
/*
 * Set eye color and draw eye
 */
	gset_marker_colr_ind(2);
	c_points(&eye[0],&eye[1],1,-4,0);
/*
 * Fill balloon using GKS
 */
	gset_fill_int_style(GSTYLE_SOLID);
	gset_fill_colr_ind(3);
	fill_area.num_points = NBALLN;
	for( i = 0; i < NBALLN; i++ ) {
		fill_area.points[i].x = xballn[i];
		fill_area.points[i].y = yballn[i];
	}
	gfill_area(&fill_area);
/*
 * Stipple in patch on dogs back
 */
	gset_marker_colr_ind(1);
	c_sfsetr("SP - Spacing",.005);
	c_sfseti("DO - Dot fill",1);
	c_sfwrld(xblnkt,yblnkt,NBLNKT,dst,100,ind,100);
/*
 * Set dash pattern, and draw outlines
 */
	c_dashdc("$$$$$$$$$$$$$$$$",1,1);
	c_curved(xballn,yballn,NBALLN);
	c_curved(xstr1,ystr1,NSTR1);
	c_curved(xstr2,ystr2,NSTR2);
	c_curved(xmouth,ymouth,NMOUTH);
	c_curved(xhead,yhead,NHEAD);
	c_curved(xneck,yneck,NNECK);
	c_curved(xtail,ytail,NTAIL);
	c_curved(xrhleg,yrhleg,NRHLEG);
	c_curved(xbelly,ybelly,NBELLY);
	c_curved(xffleg,yffleg,NFFLEG);
/*
 * Deactivate and close workstation, close GKS.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}
