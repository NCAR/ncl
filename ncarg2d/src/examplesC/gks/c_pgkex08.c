
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

#define NXD 11

main()
{
/*
 *  demo plot describing the gks font coordinate system.
 */
    Gcolr_rep       rgbs[5];
    Gpoint_list     area;
    Gpoint          *ptr;
    Gint ierr;
    void arw();
    int i;
    float top, xcent, cap, base, half, tleft, tright;
    float extl, extr, xvo, xhz, bot,sz,cntl,cwidth;
    int fac,rfac,fai;
    Gfill_int_style rfai;
    Gint rfsi;
/* 
 * create memory for fill area
 */
    area.num_points = 4;
    area.points = (Gpoint *)malloc(area.num_points*sizeof(Gpoint));
    if( !area.points ) {
		fprintf( stderr, "c_pgkex08: Not enough memory to create fill area structure\n" );
		gemergency_close_gks();
		exit(1);
	}
/*
 *  open gks, open and activate the metafile workstation.
 */
    gopen_gks("stdout",0);
    gopen_ws (WKID,NULL, WSTYPE);
    gactivate_ws (WKID);
/*
 *  Get the character body limits.
 */
	xcent = .5;
	cap =  .7;
	base = .3;
	half = .5*(cap+base);
	top = half+.7*(cap-base);
	bot = half-.8*(cap-base);
/*
 *  Convert the character height to width for Plotchar.
 */
	cwidth = (6./7.)*(cap-base);
/*
 *  turn computation of text extent information on.
 */
	c_pcseti("TE",1);
/*
 *  Specify the text font and color.
 */
	c_pcseti("FN",21);
	c_pcseti("CC",1);
/*
 *  Compute the text extent information.
 */
	c_plchhq(xcent,half,"B",cwidth,360.,0.);
/*
 *  Turn text extent computation off.
 */
	c_pcseti("TE",0);
	c_pcgetr("XB",&tleft);
	c_pcgetr("XE",&tright);
	extl = .15;
	extr = .05;
/*
 *  Draw a hatch pattern in the character body limits.
 */
	area.points[0].x = tleft;
	area.points[1].x = tright;
	area.points[2].x = tright;
	area.points[3].x = tleft;
	area.points[0].y = bot;
	area.points[1].y = bot;
	area.points[2].y = top;
	area.points[3].y = top;
	gset_fill_int_style(GSTYLE_HATCH);
	gset_fill_style_ind(6);
	gfill_area(&area);
	gset_fill_int_style(GSTYLE_SOLID);
/* 
 * Free memory
 */
    free(area.points);
/*
 *  Draw the character.
 */
	c_plchhq(xcent,half,"B",cwidth,0.,0.);
/*
 *  Label the plot.
 */
	c_line(tleft-extr,top,tright+extr,top);
	c_line(tleft-extl,cap,tright+extr,cap);
	c_line(tleft,half,tright+extr,half);
	c_line(tleft-extl,base,tright+extr,base);;
	c_line(tleft-extr,bot,tright+extr,bot);

	c_line(tleft,top+extr,tleft,bot-extr);
	c_line(tright,top+extr,tright,bot-extr);
	c_line(xcent,top+extr,xcent,bot-extr);

	sz = .015;
	c_plchhq(tright+extr+.01,top,"top",sz,0.,-1.);
	c_plchhq(tright+extr+.01,cap,"cap",sz,0.,-1.);
	c_plchhq(tright+extr+.01,half,"half",sz,0.,-1.);
	c_plchhq(tright+extr+.01,base,"base",sz,0.,-1.);
	c_plchhq(tright+extr+.01,bot,"bottom",sz,0.,-1.);
	c_plchhq(tleft,bot-extr-.025,"left",sz,0.,0.);
	c_plchhq(xcent,bot-extr-.025,"center",sz,0.,0.);
	c_plchhq(tright,bot-extr-.025,"right",sz,0.,0.);

	sz = .013;
	cntl = tleft-.08;
	c_plchhq(cntl,half+.050,"character",sz,0.,0.);
	c_plchhq(cntl,half+.020,"height",sz,0.,0.);

	arw(cntl,cap,0);
	arw(cntl,base,1);
	c_line(cntl,half+.050+.02,cntl,cap);
	c_line(cntl,half,cntl,base);

	c_plchhq(xcent,.89,"the hatched area denotes the character body",.016,0.,0.);
	c_pcseti("FN",25);
	c_plchhq(xcent,.94,"Font Coordinate System",.024,0.,0.);
	c_frame();
/*
 * Tests
 */
	fai = 3;
    gset_fill_int_style(fai);
    ginq_fill_int_style(&ierr,&rfai);
    if( rfai != fai || ierr ){
        printf("GINQ_FILL_INT_STYLE test UNSUCCESSFUL \n ");
    }
    gset_fill_style_ind(6);
    ginq_fill_style_ind(&ierr,&rfsi);
    if( rfsi != 6  || ierr) {
        printf("GINQ_FILL_STYLE_IND test UNSUCCESSFUL \n ");
    }
    fac = 4;
    gset_fill_colr_ind(fac);
    ginq_fill_colr_ind(&ierr,&rfac);
    if( rfac != fac || ierr ){
        printf("GINQ_FILL_COLR_IND test UNSUCCESSFUL\n");
    }
/*
 *  deactivate and close the workstation, close gks.
 */
    gdeactivate_ws (WKID);
    gclose_ws (WKID);
    gclose_gks();
}


void arw(xp,yp,ip)
float xp, yp;
int ip;
{
/*
 *  draws an arrow tip at (x,y) which is up if ip=0 and down if ip=1
 */
    Gpoint_list area;
    float x[3], y[3], dx, dy;
    int i, iys;

    area.num_points = 3;
    area.points = (Gpoint *)malloc(area.num_points*sizeof(Gpoint));
    if( !area.points ) {
		fprintf( stderr, "arw: Not enough memory to create polyline structure\n" );
		gemergency_close_gks();
		exit(1);
	}
    dx = 0.01;
    dy = 0.035;
    iys = 1;
    if (!ip) {
        iys = -1;
    }
    x[0] = xp-dx;
    x[1] = xp;
    x[2] = xp+dx;
    y[0] = yp+iys*dy;
    y[1] = yp;
    y[2] = yp+iys*dy;
    gset_fill_colr_ind(1);
    for( i = 0; i <= 2; i++ ) {
        area.points[i].x = x[i];
        area.points[i].y = y[i];
    }
    gfill_area(&area);
	free(area.points);
    return;
}
