/*
 *      $Id: c_tdex01.c,v 1.4 2004-07-23 01:00:33 kennison Exp $
 */

/*
 * Include standard I/O and math libraries.
 */

#include <stdio.h>
#include <math.h>

/*
 * Include NCAR Graphics function prototypes.
 */
#include <ncarg/ncargC.h>

/*
 * Define the workstation type and identifier.
 */
#define IWTYPE 1
#define WKID 1

/*
 * Define the maximum dimensions of the data arrays.
 */
#define IMAX 41
#define JMAX 41
#define KMAX 41

/*
 * Define the size of the triangle list.
 */
#define MTRI 200000

/*
 * Define a constant to use in converting angles from degrees to radians.
 */
#define DTOR .017453292519943


/*
 * Declare local dimensioned variables to hold data defining a simple
 * surface and an isosurface.
 */
float u[IMAX],v[JMAX],w[KMAX],s[JMAX][IMAX],f[KMAX][JMAX][IMAX];

/*
 * Declare the triangle-list array and a couple of temporary arrays to
 * be used in sorting the list.
 */
float rtri[MTRI][10],rtwk[2][MTRI];
int itwk[MTRI];

/*
 * Start of program.
 */
main()
{
/*
 * Set the desired values of the dimensions of the data arrays.  Note that
 * "idim" must not exceed IMAX, that "jdim" must not exceed JMAX, and that
 * "kdim" must not exceed KMAX.
 */
    int idim=31;
    int jdim=31;
    int kdim=31;

/*
 * Declare the desired minimum and maximum values of U, V, and W.
 */
    float umin=-1.;
    float vmin=-1.;
    float wmin=-1.;

    float umax= 1.;
    float vmax= 1.;
    float wmax= 1.;

/*
 * Set the desired values of parameters determining the eye position.
 * "ang1" is a bearing angle, "ang2" is an elevation angle, and "rmul"
 * is a multiplier of the length of the diagonal of the data box,
 * specifying the distance from the center of the box to the eye.
 */
    float ang1=-31.;
    float ang2= 35.;
    float rmul= 2.9;

/*
 * Set the desired value of the flag that says whether the basic color
 * scheme will be white on black ("ibow"=0) or black on white ("ibow"=1).
 */
    int ibow=1;

/*
 * Define the range of color indices to be used for a gray scale ranging
 * all the way from pure white to pure black.  (Following blocks of color
 * indices of the same length will be used for shades of other colors.)
 */
    int iofc= 8;
    int iolc=27;

/*
 * Set the desired values of the shading parameters.  Values of "shde" near
 * 0 give brighter colors and values near 1 give pastel shades.  Values of
 * "SHDR" near 0 give a narrow range of shades and values near 1 give a wide
 * range of shades.
 */
    float shde=.1;
    float shdr=.8;

/*
 * The flag "iste" says whether to do a simple image ("iste"=0), a
 * one-frame stereo image ("iste"=-1), or a two-frame stereo image
 * ("iste"=+1).
 */
    int iste=-1;

/*
 * The value of "aste" is the desired angle (in degrees) between the lines
 * of sight for a pair of stereo views.
 */
    float aste=4.;

/*
 * The value of "wosw" is the width of the stereo windows to be used in
 * one-frame stereo images; the width is stated as a fraction of the width
 * of the plotter frame.  (The windows are centered vertically; horizontally,
 * they are placed as far apart as possible in the plotter frame.)  The value
 * used must be positive and non-zero; it may be slightly greater than .5, if
 * it is desired that the stereo windows should overlap slightly.
 */
    float wosw=.5;

/*
 * Define labels for the edges of the box.
 */
    char* unlb=" -1 -.8 -.6 -.4 -.2 0 .2 .4 .6 .8 1 ";
    char* vnlb=" -1 -.8 -.6 -.4 -.2 0 .2 .4 .6 .8 1 ";
    char* wnlb=" -1 -.8 -.6 -.4 -.2 0 .2 .4 .6 .8 1 ";

    char* uilb="Values of U Coordinate";
    char* vilb="Values of V Coordinate";
    char* wilb="Values of W Coordinate";

/*
 * Declare other variables to be used below.
 */
    int i,irst,j,k,nshd,ntri;
    float otep,r,ueye,veye,weye,umid,vmid,wmid,uthi,vthi,wthi;
    float xvpl,xvpr,yvpb,yvpt,xwdl,xwdr,ywdb,ywdt;
    int lnlg;

    float xzro,yzro,xone,yone,xzbk,yzbk,xobk,yobk;
    float csm1,csm2,save_csm2;

/*
 * Fill the data arrays.
 */
    for (i=0;i<idim;i++) u[i]=umin+((float)i/(float)(idim-1))*(umax-umin);

    for (j=0;j<jdim;j++) v[j]=vmin+((float)j/(float)(jdim-1))*(vmax-vmin);

    for (k=0;k<kdim;k++) w[k]=wmin+((float)k/(float)(kdim-1))*(wmax-wmin);

    for (i=0;i<idim;i++) {
      for (j=0;j<jdim;j++) {
        s[j][i]=2.*exp(-2.*(u[i]*u[i]+v[j]*v[j]))-1.;
        for (k=0;k<kdim;k++) {
           f[k][j][i]=1.28*u[i]*u[i]+1.28*v[j]*v[j]+5.12*w[k]*w[k];
        }
      }
    }

/*
 * Initialize GKS.
 */
    gopen_gks("stdout",0);
    gopen_ws(WKID, NULL, IWTYPE);
    gactivate_ws(WKID);

/*
 * Define colors to use, as follows:
 *   Color 0 - background color (black or white, depending on "ibow")
 *   Color 1 - foreground color (white or black, depending on "ibow")
 *   Color 2 - pure red
 *   Color 3 - pure green
 *   Color 4 - pure blue
 *   Color 5 - pure cyan
 *   Color 6 - pure magenta
 *   Color 7 - pure yellow
 *   Colors "iofc" through "iolc" - gray shades, from white to black
 *   Colors "iofc"+  "nshd" through "iolc"+  "nshd" - shades of gray
 *   Colors "iofc"+2*"nshd" through "iolc"+2*"nshd" - shades of red
 *   Colors "iofc"+3*"nshd" through "iolc"+3*"nshd" - shades of green
 *   Colors "iofc"+4*"nshd" through "iolc"+4*"nshd" - shades of blue
 *   Colors "iofc"+5*"nshd" through "iolc"+5*"nshd" - shades of cyan
 *   Colors "iofc"+6*"nshd" through "iolc"+6*"nshd" - shades of magenta
 *   Colors "iofc"+7*"nshd" through "iolc"+7*"nshd" - shades of yellow
 * (The value of "nshd" is "iolc-iofc+1": the number of elements in
 * each block of color shades.)
 */
    c_tdclrs(1,ibow,shde,shdr,iofc,iolc,0);

/*
 * Define TDPACK rendering styles 1 through 7.  The indices 1 through 7
 * can then be used as final arguments in calls to c_tditri, c_tdstri,
 * and c_tdmtri.
 */
    nshd=iolc-iofc+1;

    for (irst=1;irst<8;irst++) {
      if (irst==2)
        c_tdstrs (irst,iofc+irst*nshd,iolc+irst*nshd,  /* gray bottom */
                       iofc+irst*nshd,iolc+irst*nshd,  /*   red top   */
                                     1,1,0,0.,0.,0.);  /*  lines off  */
      else
        c_tdstrs (irst,iofc+irst*nshd,iolc+irst*nshd,  /* gray bottom */
                       iofc+irst*nshd,iolc+irst*nshd,  /* colored top */
                                     3,3,0,.2,.2,.2);  /* green lines */
    }

/*
 * Tell PLOTCHAR to use font number 25, turn on the outlining of filled
 * fonts, set the line width to 1, and turn off the setting of the outline
 * color.
 */
    c_pcseti("FN - FONT NUMBER",25);
    c_pcseti("OF - OUTLINE FLAG",1);
    c_pcsetr("OL - OUTLINE LINE WIDTH",1.);
    c_pcsetr("OC - OUTLINE LINE COLOR",-1.);

/*
 * Tell TDPACK to do the shading differently and put the light source at
 * a different position.
 */
    c_tdseti("SHD - SHADING TYPE",1);
    c_tdsetr("LSU - LIGHT SOURCE - U POSITION",100.);
    c_tdsetr("LSV - LIGHT SOURCE - V POSITION",100.);
    c_tdsetr("LSW - LIGHT SOURCE - W POSITION",100.);

/*
 * Make TDPACK characters a bit bigger.
 */
    c_tdsetr("CS1 - CHARACTER SIZE PARAMETER 1",1.25);

/*
 * Initialize the count of triangles in the triangle list.
 */
    ntri=0;

/*
 * Add to the triangle list triangles representing a simple surface.
 */
    c_tdstri(&u[0],idim,&v[0],jdim,&s[0][0],IMAX,&rtri[0][0],MTRI,&ntri,6);

    if (ntri==MTRI) {
      fprintf(stderr,"c_tdex01: triangle list overflow in TDSTRI.\n" );
      return 1;
    }

/*
 * Add to the triangle list triangles representing an isosurface.
 */
    c_tditri(&u[0],idim,&v[0],jdim,&w[0],kdim,&f[0][0][0],IMAX,JMAX,1.,
                                              &rtri[0][0],MTRI,&ntri,7);

    if (ntri==MTRI) {
      fprintf(stderr,"c_tdex01: triangle list overflow in TDITRI.\n" );
      return 1;
    }

/*
 * Add to the triangle list triangles representing three red markers.
 */
    c_tdmtri(-5, .84,-.29,.13,.08,&rtri[0][0],MTRI,&ntri,2,umin,vmin,wmin,
                                                           umax,vmax,wmax);

    c_tdmtri(-5,-.17, .81,.13,.08,&rtri[0][0],MTRI,&ntri,2,umin,vmin,wmin,
                                                           umax,vmax,wmax);

    c_tdmtri(-5,-.60,-.66,.13,.08,&rtri[0][0],MTRI,&ntri,2,umin,vmin,wmin,
                                                           umax,vmax,wmax);

    if (ntri==MTRI) {
      fprintf(stderr,"c_tdex01: triangle list overflow in TDMTRI.\n" );
      return 1;
    }

/*
 * Find the midpoint of the data box (to be used as the point looked at).
 */
    umid=.5*(umin+umax);
    vmid=.5*(vmin+vmax);
    wmid=.5*(wmin+wmax);

/*
 * Determine the distance "r" from which the data box will be viewed, the
 * coordinates of the eye position, and the coordinates of a "third point"
 * defining which way is "up".
 */
    r=rmul*sqrt((umax-umin)*(umax-umin)+(vmax-vmin)*(vmax-vmin)+
                                        (wmax-wmin)*(wmax-wmin));

    ueye=umid+r*cos(DTOR*ang1)*cos(DTOR*ang2);
    veye=vmid+r*sin(DTOR*ang1)*cos(DTOR*ang2);
    weye=wmid+r*sin(DTOR*ang2);

    uthi=umid;
    vthi=vmid;
    wthi=wmid+r;

/*
 * Initialize the stereo offset argument to do either a single view or
 * a left-eye view (whichever is selected by the value of ISTE).
 */
    if (iste==0)
      otep=0.;                    /* single view */
    else
      otep=-r*tan(DTOR*aste/2.);  /* left-eye view */

/*
 * Loop to do either one or two images.
 */
    do {

/*
 * Initialize TDPACK.
 */
      c_tdinit(ueye,veye,weye,umid,vmid,wmid,uthi,vthi,wthi,otep);

/*
 * If stereo views are being done, do the requested thing, either by
 * redoing the c_set call to put them side by side on the same frame,
 * or by calling c_frame to put them on separate frames.
 */
      if (otep!=0.) {
        if (iste<0) {
          c_getset(&xvpl,&xvpr,&yvpb,&yvpt,&xwdl,&xwdr,&ywdb,&ywdt,&lnlg);
          if (otep<0.)
            c_set(1.-wosw,1.,.5-.5*wosw,.5+.5*wosw,xwdl,xwdr,ywdb,ywdt,lnlg);
          else
            c_set(  0., wosw,.5-.5*wosw,.5+.5*wosw,xwdl,xwdr,ywdb,ywdt,lnlg);
        } else if (otep>0.) c_frame();
      }

/*
 * Order the triangles in the triangle list.
 */
      c_tdotri(&rtri[0][0],MTRI,&ntri,&rtwk[0][0],&itwk[0],1);

/*
 * Draw the sides of the box that could be hidden.
 */
      c_tdgrds(umin,vmin,wmin,umax,vmax,wmax,
               .1*(umax-umin),.1*(vmax-vmin),.1*(wmax-wmin),03,1);

/*
 * Add a couple of "equatorial plane" lines on the far sides of the box.
 */
      gset_line_colr_ind((Gint)(iofc+7*nshd+1*nshd/8));
      gset_linewidth((Gdouble)2.);
      c_tdline( 1., 1.,0.,-1., 1.,0.);
      c_tdline(-1., 1.,0.,-1.,-1.,0.);
      gset_linewidth((Gdouble)1.);
      gset_line_colr_ind((Gint)1);

/*
 * Draw the triangles in the triangle list.
 */
      c_tddtri(&rtri[0][0],MTRI,&ntri,&itwk[0]);

/*
 * Draw the sides of the box that cannot be hidden.
 */
      c_tdgrds(umin,vmin,wmin,umax,vmax,wmax,
               .1*(umax-umin),.1*(vmax-vmin),.1*(wmax-wmin),03,0);

/*
 * Add a couple of "equatorial plane" lines on the near sides of the box.
 */
      gset_line_colr_ind((Gint)(iofc+7*nshd+1*nshd/8));
      gset_linewidth((Gdouble)2.);
      c_tdline(-1.,-1.,0., 1.,-1.,0.);
      c_tdline( 1.,-1.,0., 1., 1.,0.);
      gset_linewidth((Gdouble)1.);
      gset_line_colr_ind((Gint)1);

/*
 * Draw labels for the axes.
 */
      c_tdlbls(umin,vmin,wmin,umax,vmax,wmax,unlb,vnlb,wnlb,
                                             uilb,vilb,wilb,1);

/*
 * Put a label above the data box, with a double-width arrow from the
 * label to the feature that it labels.
 */
      gset_line_colr_ind((Gint)(iofc+6*nshd+1*nshd/8));
      c_tdpara(0.,.25,1.25,0.,1.,0.,0.,0.,1.);
      c_pcseti("CC - CHARACTER COLOR",iofc+6*nshd+1*nshd/8);
      c_tdplch(0.,0.,"Maximum Z Value",.08,0.,-1.05);
      c_pcseti("CC - CHARACTER COLOR",-1);
      gset_linewidth((Gdouble)2.);
      c_tdline(0.,0.,1.,0.,.25,1.25);
      c_tdline(0.,0.,1.,0.,.09,1.05);
      c_tdline(0.,0.,1.,0.,.05,1.09);
      gset_linewidth((Gdouble)1.);
      gset_line_colr_ind((Gint)1);

/*
 * Add a label for the "equatorial plane".
 */
      gset_line_colr_ind((Gint)(iofc+7*nshd+1*nshd/8));
      c_tdpara(1.1,-1.1,0.,0.,1.,0.,-1.,0.,0.);
      c_pcseti("CC - CHARACTER COLOR",iofc+7*nshd+1*nshd/8);
      c_tdplch(0.,0.,"\"Equatorial Plane\"",.08,0.,-1.15);
      c_pcseti("CC - CHARACTER COLOR",-1);
      gset_line_colr_ind((Gint)1);

/*
 * If a left-eye view has just been done, loop back for a right-eye view.
 */
    } while ((otep=-otep)>0.);

/*
 * Advance the frame.
 */
    c_frame ();

/*
 * Close gks.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();

}
