/*
 *	$Id: c_tconpa.c,v 1.1 1994-05-13 14:26:04 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define pow2(x)    ((x)*(x))

char *llbs[9] = {"-4","-3","-2","-1"," 0"," 1"," 2"," 3"," 4"};

main()
{
    int idum,ierr;

    gopen_gks ("stdout",0);
    gopen_ws (1, NULL, 1);
    gactivate_ws(1);

    tconpa(&ierr);

    gdeactivate_ws(1);
    gclose_ws(1);
    gclose_gks();
}

tconpa(ierr)
int *ierr;
{
    float zdat[25][21], rwrk[1000],x,y;
    float xcra[1000],ycra[1000];
    float x1,y1,x2,y2,x3,y3,x4,y4;
    int i,j,i1,i2,i3,i4,i5;
    int iwrk[1000],iama[20000],iaia[10],igia[10],lind[10];
    Gasfs iasf;
    extern int cpcolr(
#ifdef NeedFuncProto
        float *xcra,
        float *ycra,
        int *ncra,
        int *iaia,
        int *igia,
        int *naia
#endif
    );
    iasf.linetype = 1;
    iasf.linewidth = 1;
    iasf.line_colr_ind = 1;
    iasf.marker_type = 1;
    iasf.marker_size = 1;
    iasf.marker_colr_ind = 1;
    iasf.text_font_prec = 1;
    iasf.char_expan = 1;
    iasf.char_space = 1;
    iasf.text_colr_ind = 1;
    iasf.fill_int_style = 1;
    iasf.fill_style_ind = 1;
    iasf.fill_colr_ind = 1;
    for( i = 0; i < 10; i++ ) lind[i] = i + 6;
    *ierr = 0;

    for( i = 0; i < 25; i++ ) {
        y=.1*(float)(i-12);
        for( j = 0; j < 21; j++ ) {
            x = .1*(float)(j-10);
            zdat[i][j]=x+y+1./(pow2(x-.10)+y*y+.09)-1./(pow2(x+.10)+y*y+.09);
        }
    }
/*
 * Frame 1 -- CPEZCT.
 *
 * The routine CPEZCT requires only the array name and dimensions.
 */
    x1 = x2 = x3 = x4 = 0.;
    y1 = y2 = y3 = y4 = 1.;
    i1 = 1;
    c_set(0.,1.,0.,1.,0.,1.,0.,1.,1);
    c_wtstr(.5,.9765,"DEMONSTRATION PLOT FOR CONPACK ROUTINE CPEZCT",2,0,0);

    c_cpezct(&zdat[0][0],21,25);

/*
 * Frame 2 -- CPCNRC.
 *
 * The routine CPCNRC is called just like the old routine CONREC.
 *
 * In this example, the lowest contour level (-4.5), the highest contour
 * level (4.5), and the increment between contour levels (0.3) are set.
 * Line labels are positioned using the penalty scheme and the smoother
 * is turned on.
 */
    x1 = x2 = x3 = x4 = 0.;
    y1 = y2 = y3 = y4 = 1.;
    i1 = 1;
    c_set(0.,1.,0.,1.,0.,1.,0.,1.,1);
    c_wtstr(.5,.9765,"DEMONSTRATION PLOT FOR CONPACK ROUTINE CPCNRC",2,0,0);
    c_cpseti("LLP - LINE LABEL POSITIONING",3);
    c_cpsetr("T2D - TENSION ON 2D SPLINES",3.6);
    c_cpcnrc(&zdat[0][0],21,21,25,-4.5,4.5,.3,0,0,0);
    c_frame();
/*
 * Frame 3 - A solid-filled contour plot.
 *
 * Put a label at the top of the frame.
 */
    x1 = x2 = x3 = x4 = 0.;
    y1 = y2 = y3 = y4 = 1.;
    i1 = 1;
    c_set(0.,1.,0.,1.,0.,1.,0.,1.,1);
    c_wtstr(.5,.9765,"DEMONSTRATION PLOT FOR BASIC CONPACK ROUTINES",2,0,0);

    gset_asfs(&iasf);
    gset_fill_int_style (GSTYLE_SOLID);
    gset_clip_ind (GIND_NO_CLIP);
    cpclrs();
    c_cpsetr("VPR - VIEWPORT RIGHT",.75);
    c_cpseti("CLS - CONTOUR LEVEL SELECTOR",0);
    c_cpseti("NCL - NUMBER OF CONTOUR LEVELS",9);

    for( i = 0; i < 9; i++ ) {
        c_cpseti("PAI - PARAMETER ARRAY INDEX",i+1);
        c_cpsetr("CLV - CONTOUR LEVEL",(float)(i-4));
        c_cpseti("CLU - CONTOUR LEVEL USE",1);
        c_cpseti("AIB - AREA IDENTIFIER BELOW LEVEL",i+1);
        c_cpseti("AIA - AREA IDENTIFIER ABOVE LEVEL",i);
    }
    c_cprect(&zdat[0][0],21,21,25,rwrk,1000,iwrk,1000);
    c_arinam(iama,20000);
    c_cpclam(&zdat[0][0],rwrk,iwrk,iama);

    c_arscam(iama,xcra,ycra,1000,iaia,igia,10,cpcolr);
    gset_line_colr_ind(0);
    c_cpcldr(&zdat[0][0],rwrk,iwrk);
    gset_line_colr_ind(1);
    c_lbseti("CBL - COLOR OF BOX LINES",2);
    c_pcsetr("CS - CONSTANT SPACING FLAG",1.25);
    c_lblbar(1,.80,.95,.05,.95,10,.5,1.,lind,0,llbs,9,1);
    c_frame();

    printf( "CONPACK TEST EXECUTED--SEE PLOTS TO CERTIFY\n" );
    return(1);
}

cpclrs()
{
    Gcolr_rep rgbv[16];
    int i;
/*
 * Define the RGB color triples needed below.
 */
    rgbv[0].rgb.red = 0.00;
    rgbv[0].rgb.green = 0.00;
    rgbv[0].rgb.blue = 0.00;
    rgbv[1].rgb.red = 1.00;
    rgbv[1].rgb.green = 1.00;
    rgbv[1].rgb.blue = 1.00;
    rgbv[2].rgb.red = 0.70;
    rgbv[2].rgb.green = 0.70;
    rgbv[2].rgb.blue = 0.70;
    rgbv[3].rgb.red = 0.75;
    rgbv[3].rgb.green = 0.50;
    rgbv[3].rgb.blue = 1.00;
    rgbv[4].rgb.red = 0.50;
    rgbv[4].rgb.green = 0.00;
    rgbv[4].rgb.blue = 1.00;
    rgbv[5].rgb.red = 0.00;
    rgbv[5].rgb.green = 0.00;
    rgbv[5].rgb.blue = 1.00;
    rgbv[6].rgb.red = 0.00;
    rgbv[6].rgb.green = 0.50;
    rgbv[6].rgb.blue = 1.00;
    rgbv[7].rgb.red = 0.00;
    rgbv[7].rgb.green = 1.00;
    rgbv[7].rgb.blue = 1.00;
    rgbv[8].rgb.red = 0.00;
    rgbv[8].rgb.green = 1.00;
    rgbv[8].rgb.blue = 0.60;
    rgbv[9].rgb.red = 0.00;
    rgbv[9].rgb.green = 1.00;
    rgbv[9].rgb.blue = 0.00;
    rgbv[10].rgb.red = 0.70;
    rgbv[10].rgb.green = 1.00;
    rgbv[10].rgb.blue = 0.00;
    rgbv[11].rgb.red = 1.00;
    rgbv[11].rgb.green = 1.00;
    rgbv[11].rgb.blue = 0.00;
    rgbv[12].rgb.red = 1.00;
    rgbv[12].rgb.green = 0.75;
    rgbv[12].rgb.blue = 0.00;
    rgbv[13].rgb.red = 1.00;
    rgbv[13].rgb.green = 0.38;
    rgbv[13].rgb.blue = 0.38;
    rgbv[14].rgb.red = 1.00;
    rgbv[14].rgb.green = 0.00;
    rgbv[14].rgb.blue = 0.38;
    rgbv[15].rgb.red = 1.00;
    rgbv[15].rgb.green = 0.00;
    rgbv[15].rgb.blue = 0.00;
/*
 * Define 16 different color indices, for indices 0 through 15.  The
 * color corresponding to index 0 is black and the color corresponding
 * to index 1 is white.
 */
    for( i = 0; i <= 15; i++ ) {
        gset_colr_rep(1,i,&rgbv[i]);
    }
    return(1);
}

#ifdef __STDC__
int cpcolr(
    float *xcra,
    float *ycra,
    int *ncra,
    int *iaia,
    int *igia,
    int *naia
)
#else 
int cpcolr(xcra,ycra,ncra,iaia,igia,naia)
    float *xcra;
    float *ycra;
    int *ncra;
    int *iaia;
    int *igia;
    int *naia;
#endif
{
/*
 * the arrays xcra and ycra, for indices 1 to ncra, contain the x and y
 * coordinates of points defining a polygon.  the area identifiers in
 * the array iaia, each with an associated group identifier in the array
 * igia, tell us whether the polygon is to be color-filled or not.
 *
 *
 * assume the polygon will be filled until we find otherwise.
 */
    int i, ifll;
    Gpoint_list fill_area;

    ifll=1;
/*
 * if any of the area identifiers is negative, don't fill the polygon.
 */
    for( i = 0; i <= *naia; i++ ) {
        if (iaia[i] < 0) ifll=0;
    }
/*
 * otherwise, fill the polygon in the color implied by its area
 * identifier relative to edge group 3 (the contour-line group).
 */
    if (ifll != 0) {
        ifll=0;
        for( i = 0; i <= *naia; i++ ) {
            if (igia[i] == 3) ifll=iaia[i];
        }
        if (ifll >= 1 && ifll <= 10) {
            gset_fill_colr_ind (ifll+5);
/*
 * Create structure to pass to gfill_area
 */
			fill_area.num_points = *ncra-1;
			fill_area.points = (Gpoint *) malloc(2*(*ncra-1)*sizeof(Gfloat));
			if( !fill_area.points ) {
				fprintf( stderr, "colram: Not enough memory to create fill area structure\n" );
				gemergency_close_gks();
				exit(1);
			}
			for( i = 0; i < *ncra-1; i++ ) {
				fill_area.points[i].x = xcra[i];
				fill_area.points[i].y = ycra[i];
			}
/*
 * Fill area
 */
            gfill_area (&fill_area);
            free(fill_area.points);
        }
    }
    return(1);
}
