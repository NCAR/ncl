/*
 *      $Id: c_cssex02.c,v 1.8 2000-01-12 22:58:01 fred Exp $
 */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>

/*
 * Function prototypes for ngmath functions.
 */
#include <ncarg/ngmath.h>

/*
 * Function prototypes for c_cssex02.
 */
int   csdraw(float *, float *, int *, int *, int *, int *);
void  c_draw_box(float, float, float, float, int);
void  c_genrs(int, float [], float []);
void  c_geni(int, int, float, float);
float c_genpnt(float, float);
float c_csrand();


/*
 * Function prototypes for NCARG and NCARG-GKS routines
 */
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

/*
 *  Gks workstation type and ID.
 */
#define IWTYPE 1
#define WKID   1

/*
 *  Number of input data values.
 */
#define N      500

/*
 *  Number of points to be used in drawing arcs.
 */
#define NARC  50

/*
 *  Constants for converting between radians and degrees.
 */
#define  D2R   0.017453293
#define  R2D  57.295780000

main()
{
/*
 *  Define random points and functional values on the globe,
 *  triangulate, interpolate to a uniform grid, then use Ezmap
 *  and Conpack to draw a contour plot.
 */

  int i, j, k, np, ns, indx;
  Gcolr_rep rgb;
  Gint fnt;
  Gtext_prec prec;
  Gtext_font_prec fandp;

  float vlat[2*N], vlon[2*N], rc[2*N];
  float rlat1, rlon1, rlat2, rlon2, pnm, rcd, *ff;
  int   numv=0, nca, nv[2*N];

/*
 *  c_supmap arguments.
 */
  float plm1[2] = {0., 0.};
  float plm2[2] = {0., 0.};
  float plm3[2] = {0., 0.};
  float plm4[2] = {0., 0.};

/*
 *  Input dataset on the globe (latitudes and longitudes in degrees).
 *  These data points do not cover the globe, but rather are confined
 *  to the nothern hemisphere and are enclosed by a boundary.
 */
  float rlat[N], rlon[N], fval[N];

/*
 *  Declare the arrays for holding the points for drawing
 *  circular arcs.
 */
  float arclat[NARC], arclon[NARC];

/*
 *  Arrays for defining the interpolation grid.
 */
#define NI      73
#define NJ     145
  float plat[NI], plon[NJ];

/*
 *  Work arrays for Conpack plot.
 */
#define IWKL 2000
#define RWKL 1000
#define IAMASZ 10000
  int   iwk[IWKL];
  float rwk[RWKL];
  int   iama[IAMASZ];

/*
 *  Pointer to array of triangle indices; number of triangles; error value.
 */
  int *ltri, nt, ier;

/*
 *  Generate a default set of nodes as latitudinal and longitudinal
 *  coordinates (latitudes in the range -90. to 90. and longitudes
 *  in the range -180. to 180).
 */
  c_genrs(N, rlat, rlon);

/*
 *  Generate functional values at the input nodes.
 */
  c_geni(5, 10, -200., 500.);
  for (i = 0; i < N; i++) {
    fval[i] = c_genpnt(D2R*rlat[i], D2R*rlon[i]); 
  }

/*
 *  Create the triangulation.  This step is not necessary if all
 *  you want to do is obtain interpolated values.  It is here since
 *  a plot of the triangulation is to be drawn as an independent picture.
 */
  ltri = c_csstri(N, rlat, rlon, &nt, &ier);

/*
 *  Plot the Delaunay triangulation and
 *  the Voronoi polygons on a sphere.
 */

/*
 *  Open GKS, open and actibvate a workstation.
 */
  gopen_gks ("stdout",0);
  gopen_ws (WKID, NULL, IWTYPE);
  gactivate_ws(WKID);

/*
 *  Color table.
 */
  rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 1.;
  gset_colr_rep(WKID,0,&rgb);
  rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
  gset_colr_rep(WKID,1,&rgb);
  rgb.rgb.red = 1.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
  gset_colr_rep(WKID,2,&rgb);
  rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 1.;
  gset_colr_rep(WKID,3,&rgb);
  rgb.rgb.red = 0.; rgb.rgb.green = .8; rgb.rgb.blue = 0.;
  gset_colr_rep(WKID,4,&rgb);

/*
 *  Plot title.
 */
  c_plchhq(0.5,0.95,":F26:Triangulation",.030,0.,0.);

/*
 *  Draw a map of the North Atlantic, as seen by a satellite.
 */
  gset_line_colr_ind(1);
  c_mapstr ("SA", 4.);
  c_mappos (0.06, 0.94, 0.02, 0.90);
  c_supmap (7, 40., -105., 0., plm1, plm2, plm3, plm4, 1,
            -1000, 5, 0, &ier);

/*
 *  Draw the Delaunay triangles.
 */
  gset_line_colr_ind(2);
  for (np = 0; np < nt; np++) {
    indx = 3*np;
    for (ns = 0; ns < 3; ns++) {
      c_mapgci(rlat[ltri[indx+ns]], rlon[ltri[indx+ns]], 
               rlat[ltri[indx+(ns+1)%3]], rlon[ltri[indx+(ns+1)%3]],
               NARC, arclat, arclon);
      c_mapit (rlat[ltri[indx+ns]], rlon[ltri[indx+ns]], 0);
      for (i = 0; i < NARC; i++) {
        c_mapit (arclat[i], arclon[i], 1);
      }
      c_mapit (rlat[ltri[indx+(ns+1)%3]], rlon[ltri[indx+(ns+1)%3]], 1);
      c_mapiq();
    }
  }

/*
 *  Mark the original data points (create a circular
 *  marker by drawing concentric circles at the data points).
 */
  gset_line_colr_ind(1);
  for (k = 0; k < 8; k++) {
    for (i = 0; i < N; i++) {
      c_nggcog (rlat[i], rlon[i], 0.1 * (float) k, arclat, arclon, NARC);
      c_mapit (arclat[0], arclon[0], 0);
      for (j = 1; j < NARC-1; j++) {
        c_mapit (arclat[j], arclon[j], 1);
      }
      c_mapit (arclat[NARC-1], arclon[NARC-1], 1);
      c_mapiq();
    }
  }

/*
 *  Draw the Voronoi polygons.
 */
  c_csvoro(N, rlat, rlon, 0, 1, vlat, vlon, rc, &nca, &numv, nv, &ier);
  gset_line_colr_ind(3);
  for (i = 0; i < N; i++) {
    c_csvoro(N, rlat, rlon, i, 0, vlat, vlon, rc, &nca, &numv, nv, &ier);
    for (j = 1; j < numv; j++) {
      rlat1 = vlat[nv[j-1]];
      rlon1 = vlon[nv[j-1]];
      rlat2 = vlat[nv[j]];
      rlon2 = vlon[nv[j]];

      c_mapgci(rlat1, rlon1, rlat2, rlon2, NARC, arclat, arclon); 
      c_mapit (rlat1, rlon1, 0);
      for (k = 0; k < NARC; k++) {
        c_mapit (arclat[k], arclon[k], 1);
      }
      c_mapit (rlat2, rlon2, 1);
      c_mapiq();
    }
  }

  c_frame();

/*
 *  Set up the latitudes and longitudes for the interpolated grid.
 */
  for (i = 0; i < NI; i++) {
    plat[i] = -90.+i*2.5;
  }
  for (j = 0; j < NJ; j++) {
    plon[j] = -180.+j*2.5;
  }

/*
 *  Do the interpolation to the specified uniform grid.
 */
  ff = c_cssgrid(N, rlat, rlon, fval, NI, NJ, plat, plon, &ier);


/*
 *  Note:  ff is stored as a 2-dimensional would be in row-dominant
 *         order with latitute being the first dimension and
 *         longitude the second dimension.  For calling Conpack 
 *         we want ff to be stored as longitudes vs. latitudes in 
 *         column-dominant order, which is equivalent to how ff is
 *         stored.
 */


/*
 *  Draw a contour map of the gridded data on the globe.
 */
  c_mapsti("PE",0);
  c_mapsti("GR",0);
  c_mapstc("OU","PS");
  c_mapsti("DO",1);
  c_mapdrw();
  gset_line_colr_ind(3);
  gset_text_colr_ind(3);
  fandp.font = -13;
  fandp.prec = GPREC_STROKE;
  gset_text_font_prec(&fandp);
  c_cpseti("SET",0);
  c_cpsetr("DPS",0.02);
  c_cpsetr("T2D",0.02);
  c_cpseti("CLS",16);
  c_cpsetc("HLT"," ");
  c_cpsetc("ILT"," ");

  c_cpsetr("XC1",-180.);
  c_cpsetr("XCM", 180.);
  c_cpsetr("YC1", -90.);
  c_cpsetr("YCN",  90.);
  c_cpsetr("MAP",1);
  c_cpsetr("ORV",1.E12);
  
  c_cprect(ff, NJ, NJ, NI, rwk, RWKL, iwk, IWKL);
  c_arinam(iama,IAMASZ);
  c_cplbam(ff, rwk, iwk, iama);
  c_cpcldm(ff, rwk, iwk, iama, csdraw);

/*
 *  Plot picture title.
 */
  c_set(0.,1.,0.,1.,0.,1.,0.,1.,1);
  c_plchhq(0.5,0.95,":F26:Contour Plot of Gridded Data",.025,0.,0.);

  c_frame();

  gdeactivate_ws (WKID);
  gclose_ws (WKID);
  gclose_gks();

}

void c_genrs(int n, float rlat[], float rlon[])
{
/*
 *  Generate random positions on a sphere in latitudes and longitudes
 *  (latitude values between -90. and 90. and longitude values
 *  between -180. and 180.
 *
 *  First select a random longitude, and then select a random value
 *  on the Z axis -R to R (-1 to 1 in the case of the unit sphere).
 *  From the random Z value, calculate the latitude.
 */
  int   i;
  float r = 1., rz;

  for (i = 0; i < n; i++) {
    rlon[i] = -180.+360.*c_csrand();
    rz = r*(2.*c_csrand()-1.);
    rlat[i] = R2D*asin(rz);
  }
}


/*
 *  Portable random number generator.  This is here in place
 *  of the random number generator in the Standard C Library
 *  so that the results for this C example will be the same 
 *  as for the Fortran example.
 */
float c_csrand()
{
  int mplier=16807, modlus=2147483647, mobymp=127773, momdmp=2836;

  int hvlue, lvlue, testv;
  static int nextn, jseed=9, ifrst=0;

  if (ifrst == 0) {
    nextn = jseed;
    ifrst = 1;
  }

  hvlue = nextn / mobymp;
  lvlue = nextn%mobymp;
  testv = mplier*lvlue - momdmp*hvlue;
  if (testv > 0) {
    nextn = testv;
  }
  else {
    nextn = testv + modlus;
  }
  
  return( (float) nextn / (float) modlus);
  
}

/*
 *  Define functions for generating random data on a sphere.
 *
 *  The functions c_geni and c_genpnt are used to generate test 
 *  data on the globe.  The data generated will have approximately 
 *  "mlow" lows and "mhgh" highs within each of the twenty areas 
 *  defined by an inscribed icosahedron, a minimum value of 
 *  approximately "dlow" and a maximum value of approximately 
 *  "dhgh".  c_geni is called to initialize the process and 
 *  the function c_genpnt returns a value of the function at
 *  the point (rlat,rlon).
 *
 *  The function used is a sum of exponentials.
 *
 *  Versions of these codes were originally written in Fortran
 *  David Kennison at NCAR.
 *
 */

#ifndef MAX
#define MAX(A,B)        (((A) > (B)) ? (A) : (B))
#endif
 
#ifndef MIN
#define MIN(A,B)        (((A) < (B)) ? (A) : (B))
#endif

int   ncnt, nlow, nhgh;
float rlow, rhgh, rmin, rmax;
float cmul[160], xcoc[160], ycoc[160], zcoc[160];

void c_geni(int mlow, int mhgh, float dlow, float dhgh) {

  float r0[] = 
  {
     .968,.067,.478,.910,.352,.933,.654,.021,.512,.202,.940,.204,
     .379,.793,.288,.267,.357,.128,.703,.737,.803,.915,.511,.762,
     .456,.471,.300,.613,.073,.498,.220,.041,.565,.698,.951,.917,
     .630,.605,.938,.143,.807,.878,.347,.186,.671,.635,.453,.028,
     .763,.157,.765,.566,.072,.276,.328,.528,.747,.627,.141,.821,
     .126,.360,.862,.690,.058,.813,.607,.689,.419,.545,.831,.226,
     .422,.178,.412,.093,.813,.866,.121,.576,.023,.886,.142,.095,
     .162,.470,.623,.910,.097,.764,.730,.223,.124,.593,.913,.183,
     .406,.520,.871,.825,.065,.703,.051,.488,.881,.463,.581,.694,
     .329,.702,.270,.352,.587,.412,.446,.750,.882,.069,.659,.979,
     .833,.390,.202,.957,.982,.115,.140,.389,.635,.011,.213,.701,
     .714,.264,.188,.594,.727,.769,.288,.056,.471,.558,.408,.058,
     .970,.854,.808,.851,.923,.467,.830,.756,.857,.032,.713,.839,
     .147,.852,.228,.783,.863,.441,.483,.577,.705,.671,.171,.432,
     .441,.459,.489,.911,.017,.897,.969,.987,.751,.777,.838,.674,
     .244,.669,.430,.101,.701,.143,.940,.848,.995,.168,.631,.858,
     .608,.114,.435,.313,.785,.606,.746,.226,.065,.234,.137,.082,
     .131,.106,.069,.882,.883,.907,.556,.127,.576,.986,.228,.276,
     .128,.168,.124,.123
  };
 
  float r1[] = 
  {
     .023,.003,.880,.088,.237,.017,.170,.368,.123,.239,.250,.006,
     .146,.806,.134,.722,.791,.361,.998,.920,.529,.122,.043,.864,
     .877,.025,.808,.746,.442,.065,.400,.464,.068,.280,.552,.305,
     .297,.722,.673,.420,.961,.923,.426,.107,.729,.560,.829,.520,
     .921,.827,.440,.451,.950,.483,.315,.827,.508,.123,.573,.949,
     .188,.973,.414,.256,.253,.966,.561,.550,.689,.234,.970,.650,
     .157,.396,.757,.885,.956,.587,.405,.877,.414,.845,.328,.363,
     .328,.643,.190,.836,.766,.763,.786,.954,.737,.199,.211,.990,
     .165,.772,.540,.854,.006,.510,.504,.163,.906,.261,.048,.862,
     .848,.454,.740,.262,.299,.068,.625,.627,.711,.815,.464,.477,
     .579,.249,.431,.315,.449,.642,.305,.614,.414,.845,.468,.420,
     .355,.972,.582,.261,.234,.631,.123,.082,.084,.863,.343,.383,
     .930,.968,.011,.641,.784,.474,.118,.362,.723,.549,.678,.172,
     .191,.983,.786,.605,.828,.254,.024,.183,.226,.607,.444,.460,
     .237,.567,.541,.322,.430,.885,.705,.361,.853,.715,.002,.637,
     .190,.119,.999,.913,.668,.677,.085,.859,.660,.871,.464,.488,
     .124,.489,.671,.350,.095,.115,.810,.333,.683,.351,.654,.113,
     .236,.359,.473,.089,.075,.475,.726,.264,.594,.725,.177,.263,
     .402,.262,.122,.062
  };
   
  float r2[] = 
  {
     .337,.417,.503,.020,.769,.158,.133,.005,.517,.606,.094,.591,
     .081,.820,.855,.675,.545,.033,.938,.947,.294,.060,.009,.427,
     .646,.559,.684,.721,.781,.291,.892,.118,.708,.395,.138,.476,
     .552,.270,.481,.069,.877,.575,.660,.957,.395,.516,.633,.939,
     .548,.570,.886,.843,.630,.895,.270,.276,.455,.953,.998,.236,
     .244,.889,.354,.952,.284,.492,.428,.837,.762,.909,.906,.639,
     .484,.566,.596,.879,.082,.229,.818,.631,.799,.704,.473,.430,
     .600,.743,.706,.055,.696,.704,.291,.940,.593,.645,.892,.877,
     .137,.321,.714,.899,.230,.620,.538,.714,.186,.134,.593,.268,
     .364,.411,.899,.163,.116,.372,.593,.716,.115,.298,.770,.812,
     .002,.061,.752,.595,.706,.645,.472,.843,.965,.186,.742,.195,
     .806,.280,.910,.992,.414,.503,.260,.778,.915,.159,.941,.030,
     .531,.533,.746,.647,.832,.516,.458,.834,.577,.211,.429,.283,
     .855,.901,.126,.821,.087,.868,.016,.893,.148,.926,.885,.562,
     .429,.145,.340,.343,.304,.281,.374,.835,.814,.120,.482,.646,
     .636,.940,.479,.213,.151,.908,.497,.006,.809,.623,.827,.895,
     .490,.843,.788,.638,.769,.673,.200,.198,.817,.540,.541,.121,
     .821,.915,.956,.635,.035,.438,.280,.671,.377,.760,.884,.528,
     .668,.381,.534,.477
  };

  float xcvi[] =
  {
     .9510565162952 , -.9510565162951 ,  .4253254041760 ,
    -.4253254041760 ,  .4253254041760 , -.4253254041760 ,
     .4253254041760 , -.4253254041760 ,  .4253254041760 ,
    -.4253254041760 ,  .4253254041760 , -.4253254041760
  };

  float ycvi[] =
  {
     .0000000000000 ,  .0000000000000 ,  .8506508083520 ,
    -.8506508083520 ,  .2628655560596 , -.2628655560596 ,
    -.6881909602356 ,  .6881909602356 , -.6881909602356 ,
     .6881909602356 ,  .2628655560595 , -.2628655560596
  };
   
  float zcvi[] =
  {
     .0000000000000 ,  .0000000000000 ,  .0000000000000 ,
     .0000000000000 ,  .8090169943749 , -.8090169943749 ,
     .5000000000000 , -.5000000000000 , -.5000000000000 ,
     .5000000000000 , -.8090169943749 ,  .8090169943749
  };

  int ivof[20][3] = 
  {
      {0, 2, 4}  , {0, 4, 6}  , {0, 6, 8} ,
      {0, 8,10}  , {0, 2,10}  , {2, 7,10} ,
      {2, 7, 9}  , {2, 4, 9}  , {4, 9,11} ,
      {4, 6,11}  , {6, 3,11}  , {3, 6, 8} ,
      {3, 5, 8}  , {5, 8,10}  , {5, 7,10} ,
      {5, 1, 7}  , {1, 3, 5}  , {1, 3,11} ,
      {1, 9,11}  , {1, 7, 9}
  };

  int   i,j,k,nlow,nhgh,indx;
  float wgt1,wgt2,wgt3,wsum,xtmp,ytmp,ztmp,temp,rlat,rlon,
        crln,srln,crlt,srlt,rval,dist,angl;

  nlow = MAX(0,MIN(4,mlow));
  nhgh = MAX(0,MIN(4,mhgh));

  ncnt = 20*(nlow+nhgh);
  for (k = 0; k < ncnt; k++) {
    indx = k%20;
    wgt1 = r0[k];
    wgt2 = r1[k];
    wgt3 = r2[k];
    wsum = wgt1+wgt2+wgt3;
    wgt1 /= wsum;
    wgt2 /= wsum;
    wgt3 /= wsum;
    xtmp = wgt1*xcvi[ivof[indx][0]] +
           wgt2*xcvi[ivof[indx][1]] +
           wgt3*xcvi[ivof[indx][2]];
    ytmp = wgt1*ycvi[ivof[indx][0]] +
           wgt2*ycvi[ivof[indx][1]] +
           wgt3*ycvi[ivof[indx][2]];
    ztmp = wgt1*zcvi[ivof[indx][0]] +
           wgt2*zcvi[ivof[indx][1]] +
           wgt3*zcvi[ivof[indx][2]];
    temp = sqrt(xtmp*xtmp +ytmp*ytmp + ztmp*ztmp);
    xcoc[k] = xtmp/temp;
    ycoc[k] = ytmp/temp;
    zcoc[k] = ztmp/temp;
    if (k < 20*nlow) {
      cmul[k] = -1.;
    }
    else {
      cmul[k] = 1.;
    }
  }

  rlow = dlow;
  rhgh = dhgh;
  rmin =  1.e+36;
  rmax = -1.e+36;
  for (i = -180; i <= 175; i = i+5) {
    rlon = D2R * (float) i;
    crln = cos(rlon);
    srln = sin(rlon);
    for (j = -85; j <= 85; j = j+5) {
      rlat = D2R * (float) j;
      crlt = cos(rlat);
      srlt = sin(rlat);
      rval = 0.5*(rlow+rhgh);
      for (k = 0; k < ncnt; k++) {
        dist = sqrt(pow(crlt*crln-xcoc[k],2) +
                    pow(crlt*srln-ycoc[k],2) +
                    pow(srlt     -zcoc[k],2));
        angl = 2.*R2D*asin(0.5*dist);
        dist = angl/18.;
        rval = rval + 0.5*(rhgh-rlow)*cmul[k]*exp(-dist*dist)
                                     *(2.-sin(6.283*dist)/2.);
      }
      rmin = MIN(rmin,rval);
      rmax = MAX(rmax,rval);
    }
  } 
}

float c_genpnt(float rlat, float rlon)
{
  float crlt,crln,srln,srlt,rval,dist,angl;
  int   k;

  crlt = cos(rlat);
  crln = cos(rlon);
  srlt = sin(rlat);
  srln = sin(rlon);
  rval = 0.5*(rlow+rhgh);
  for (k = 0; k < ncnt; k++) {
    dist = sqrt(pow(crlt*crln-xcoc[k],2) +
                pow(crlt*srln-ycoc[k],2) +
                pow(srlt     -zcoc[k],2));
    angl = 2.*R2D*asin(0.5*dist);
    dist = angl/18.;
    rval = rval + 0.5*(rhgh-rlow)*cmul[k]*exp(-dist*dist)
                                 *(2.-sin(6.283*dist)/2.);
  }
  return(rlow+(rhgh-rlow)*(rval-rmin)/(rmax-rmin));
}

int csdraw(float *xcs, float *ycs, int *ncs, int *iai, int *iag, int *nai)
{
  int i, idr;

  idr = 1;
  for (i = 0; i < *nai; i++) {
    if (iai[i] < 0) idr = 0;
  }
  if (idr != 0) c_curved(xcs,ycs,*ncs);
  return(0);
}
