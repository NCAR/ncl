
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define nxd 11

main()
{
/*
 *  demo plot describing the gks font coordinate system.
 */
    Gcolr_rep       rgbs[5];
    Gpoint_list     point_list;
    Gpoint          *ptr;
    Gint ierr;
    int i, wd1,wd2,imdy, irt, imdx;
    float xb[4],yb[4], top, cap, base, half, tleft, tright, tcent;
    float extl, extr, xvo, xhz, scln,bot,sz,cntl;
    int nx[nxd],ny[nxd];
    float xn[nxd],yn[nxd], tmp1;
    int fac,rfac,fai;
    Gfill_int_style rfai;
    Gint rfsi;
/*
 *  data for an "a"; wd1 and wd2 specify the widths of the stems;
 *  imdy specifies the height of the cross piece; irt specifies
 *  the cordinate of the right stem.
 */
    wd1 = 56;
    wd2 = 80;
    imdy = 270;
    irt = 654;
    imdx = irt/2;
    nx[0] = 0;
    ny[0] = 0;
    nx[1] = imdx-wd1/2;
    ny[1] = 1000;
    nx[2] = imdx+wd1/2;
    ny[2] = 1000;
    nx[3] = irt;
    ny[3] = 0;
    nx[4] = nx[3]-wd2;
    ny[4] = 0;
    nx[5] = (nx[4]+wd1)/2;
    tmp1 = 500./(float)(nx[1]);
    ny[5] = (int)(tmp1*(float)(nx[4]-wd1));
    ny[6] = imdy+wd1/2;
    tmp1 = (float)(nx[1])/1000.;
    nx[6] = (int)(tmp1*(float)(ny[6])+wd1);
    ny[7] = ny[6];
    tmp1 = (float)(nx[2]-irt)/1000.;
    nx[7] = (int)(tmp1*(float)(ny[7])+(float)(nx[4]));
    ny[8] = imdy-wd1/2;
    nx[8] = (int)(tmp1*(float)(ny[8])+(float)(nx[4]));
    ny[9] = ny[8];
    nx[9] = nx[1]*ny[9]/1000+wd1;
    ny[10] = 0;
    nx[10] = wd1;
/*
 *  open gks, open and activate the metafile workstation.
 */
    gopen_gks("stdout",0);
    gopen_ws (1,NULL, 8);
    gactivate_ws (1);
/*
 *  define the required color indices.
 */
    rgbs[0].rgb.red = 0.;    rgbs[0].rgb.green = 0.;    rgbs[0].rgb.blue = .6;
    rgbs[1].rgb.red = 1.;    rgbs[1].rgb.green = 1.;    rgbs[1].rgb.blue = 1.;
    rgbs[2].rgb.red = 0.;    rgbs[2].rgb.green = 0.;    rgbs[2].rgb.blue = 1.;
    rgbs[3].rgb.red = 1.;    rgbs[3].rgb.green = 1.;    rgbs[3].rgb.blue = 0.;
    rgbs[4].rgb.red = 1.;    rgbs[4].rgb.green = .4;    rgbs[4].rgb.blue = .4;
    for( i = 0; i <= 4; i++ ) {
        gset_colr_rep(1,i,&rgbs[i]);
    }
/*
 *  label the plot
 */
    scln = .0005;
    xvo  = .25;
    xhz  = .30;
    c_pcseti("CD",1);
    gset_line_colr_ind(1);
    top = xvo+.55;
    cap = xvo+.5;
    base = xvo;
    half = .5*(cap+base);
    bot  = xvo-.1;
    tleft = xhz-.05;
    tright = xhz+.375;
    tcent = .5*(tleft+tright);
    extl = .15;
    extr = .05;
/*
 *  draw character body in pink.
 */
    xb[0] = tleft;
    xb[1] = tright;
    xb[2] = tright;
    xb[3] = tleft;
    yb[0] = bot;
    yb[1] = bot;
    yb[2] = top;
    yb[3] = top;
    fai = 3;
    gset_fill_int_style(fai);
    ginq_fill_int_style(&ierr,&rfai);
    if( rfai != fai || ierr ){
        printf("GINQ_FILL_INT_STYLE test UNSUCCESSFUL \n ");
    }
    else { 
        printf("GINQ_FILL_INT_STYLE test SUCCESSFUL \n ");
    }
    gset_fill_style_ind(6);
    ginq_fill_style_ind(&ierr,&rfsi);
    if( rfsi != 6  || ierr) {
        printf("GINQ_FILL_STYLE_IND test UNSUCCESSFUL \n ");
    }
    else {
        printf("GINQ_FILL_STYLE_IND test SUCCESSFUL \n ");
    }
    fac = 4;
    gset_fill_colr_ind(fac);
    ginq_fill_colr_ind(&ierr,&rfac);
    if( rfac != fac || ierr ){
        printf("GINQ_FILL_COLR_IND test UNSUCCESSFUL\n");
    }
    else {
        printf("GINQ_FILL_COLR_IND test SUCCESSFUL\n");
    }
    gset_fill_int_style (GSTYLE_SOLID);
    gset_fill_colr_ind(4);
    point_list.points = (Gpoint *)calloc(nxd,sizeof(Gpoint));
    point_list.num_points = 4;
    for( ptr = point_list.points, i = 0; i < 4; i++, ptr++ ) {
        ptr->x = xb[i];
        ptr->y = yb[i];
    }
    gfill_area(&point_list);
/*
 *  draw the character
 */
    for( i = 0; i < nxd; i++ ) {
        xn[i] = xhz+scln*nx[i];
        yn[i] = xvo+scln*ny[i];
    }
    gset_fill_colr_ind(3);
    gset_fill_int_style (GSTYLE_SOLID);
    point_list.num_points = nxd;
    for( ptr = point_list.points, i = 0; i < nxd; i++, ptr++ ) {
        ptr->x = xn[i];
        ptr->y = yn[i];
    }
    gfill_area(&point_list);

    c_line(tleft-extr,top,tright+extr,top);
    c_line(tleft-extl,cap,tright+extr,cap);
    c_line(tleft,half,tright+extr,half);
    c_line(tleft-extl,base,tright+extr,base);
    c_line(tleft-extr,bot,tright+extr,bot);

    c_line(tleft,top+extr,tleft,bot-extr);
    c_line(tright,top+extr,tright,bot-extr);
    c_line(tcent,top+extr,tcent,bot-extr);

    sz = .013;
    c_plchhq(tright+extr+.01,top,"TOP",sz,0.,-1.);
    c_plchhq(tright+extr+.01,cap,"CAP",sz,0.,-1.);
    c_plchhq(tright+extr+.01,half,"HALF",sz,0.,-1.);
    c_plchhq(tright+extr+.01,base,"BASE",sz,0.,-1.);
    c_plchhq(tright+extr+.01,bot,"BOTTOM",sz,0.,-1.);
    c_plchhq(tleft,bot-extr-.025,"LEFT",sz,0.,0.);
    c_plchhq(tcent,bot-extr-.025,"CENTER",sz,0.,0.);
    c_plchhq(tright,bot-extr-.025,"RIGHT",sz,0.,0.);

    sz = .011;
    cntl = tleft-.08;
    c_plchhq(cntl,half+.050,"CHARACTER",.011,0.,0.);
    c_plchhq(cntl,half+.020,"HEIGHT",.011,0.,0.);

    arw(cntl,cap,0);
    arw(cntl,base,1);
    c_line(cntl,half+.050+.02,cntl,cap);
    c_line(cntl,half,cntl,base);

    c_plchhq(tcent,.94,"FONT COORDINATE SYSTEM",.020,0.,0.);
    c_plchhq(tcent,.89,"Pink area denotes the character body",.015,0.,0.);
    c_frame();
/*
 *  deactivate and close the workstation, close gks.
 */
    gdeactivate_ws (1);
    gclose_ws (1);
    gclose_gks();
}


arw(xp,yp,ip)
float xp, yp;
int ip;
{
/*
 *  draws an arrow tip at (x,y) which is up if ip=0 and down if ip=1
 */
    Gpoint_list area;
    float x[3], y[3], dx, dy;
    int i, iys;

    area.points = (Gpoint *)calloc(3,sizeof(Gpoint));
    area.num_points = 3;
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
    return(1);
}
