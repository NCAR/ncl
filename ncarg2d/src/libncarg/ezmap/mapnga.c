
/*
 * This file contains the EZMAP routines, in C, needed to use a PNG
 * file to construct a cell-array background for a map.
 */

#include <math.h>
#include <ncarg/c.h>
#include <ncarg/gks.h>
#include <ncarg/ncargC.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


/*
 * Prototype declarations:
 */

void NGCALLF(mapngr,MAPNGR)(char* flnm,int* lcfa);
void NGCALLF(mdpngr,MDPNGR)(char* flnm,int* lcfa);
void NGCALLF(mapngd,MAPNGD)(int* loca,int* iorc,int* igsc,int* ngsc);
void NGCALLF(mdpngd,MDPNGD)(int* loca,int* iorc,int* igsc,int* ngsc);
void NGCALLF(mapngq,MAPNGQ)();
void NGCALLF(mdpngq,MDPNGQ)();
int irange(int imin,int imax,int ival);

extern void NGCALLF(gsvini,GSVINI)(char* fnmp,int* mgsv,int* ngsv,
                                   int* ired,int* igrn,int* ierr);
extern float NGCALLF(gsvalu,GSVALU)(int* ipix,int* jpix);
extern void NGCALLF(gsvend,GSVEND)();


/*
 * Global variables:
 */

static int init=0,mgsv,ngsv,ninp,mcfa,ncfa;
static char ptyp[3],ltyp[3];
static float plat,plon,rota,salt,san1,san2;
static float plm1[2],plm2[2],plm3[2],plm4[2];
static float xwdl,xwdr,ywdb,ywdt;
static float* xinp=(float*)NULL;
static float* yinp=(float*)NULL;
static float* zixo=(float*)NULL;
static float* ziyo=(float*)NULL;
static int* iwrk=(int*)NULL;
static float* fwrk=(float*)NULL;
static float* xcfa=(float*)NULL;
static float* ycfa=(float*)NULL;
static float* zcfx=(float*)NULL;
static float* zcfy=(float*)NULL;


/*
 * Either MAPNGR or MDPNGR may be called to read a ".png" file and an
 * associated ".pngi" file and extract from them information that will
 * be needed during a later call to MAPNGD or MDPNGD to construct and
 * draw a cell array background.  The arguments are as follows:
 *
 *   FLNM is a NULL-terminated character string specifying the name
 *   of the PNG file to be read.  (The ".pngi" file has the same name,
 *   but with a suffixed "i".)
 *
 *   LCFA, if non-zero, specifies that two-dimensional arrays are to be
 *   used to approximate the correction function constructed when the
 *   ".pngi" file contains at least four reference points.  The amount
 *   of space used for each of the arrays will be no more than MAX(100,
 *   MIN(10000,LCFA)) and exactly equal to MCFAxNCFA, where the values
 *   of MCFA and NCFA are picked so that the ratio of MCFA to NCFA is
 *   approximately equal to the ratio of the width of the PNG to its
 *   height.
 *
 */

void NGCALLF(mapngr,MAPNGR)(flnm,lcfa)
  char* flnm;
  int*  lcfa;
{
  NGCALLF(mdpngr,MDPNGR)(flnm,lcfa);
}

void NGCALLF(mdpngr,MDPNGR)(flnm,lcfa)
  char* flnm;
  int*  lcfa;
{
  char* fnmp=(char*)NULL;
  FILE* pngi;
  int schr,i,igrn=71514,ired=21268,ista,j,lnlg,nprp,temp;
  float dumi;
  float xcop,ycop;
  float* pxlt=(float*)NULL;
  float* pxln=(float*)NULL;
  float* pxlx=(float*)NULL;
  float* pxly=(float*)NULL;

/*
 * Check for an uncleared prior error.
 */

  if (c_icfell("MDPNGR - UNCLEARED PRIOR ERROR",1)) return;

/*
 * If a previous ".png" is still sitting in memory, force call that
 * will discard it, along with any workspaces still allocated.
 */

  if (init) NGCALLF(mapngq,MAPNGQ)();

/*
 * Open and read the ".png" file.
 */

  NGCALLF(gsvini,GSVINI)(flnm,&mgsv,&ngsv,&ired,&igrn,&ista);

  if (ista) {
    c_seter("MDPNGR - ERROR OPENING OR READING PNG FILE",2,1);
    return;
  }

/*
 * Allocate space for the name of the ".pngi" file.
 */

  if (!(fnmp=(char*)malloc((strchr(flnm,'\0')-flnm+2)*sizeof(char)))) {
    c_seter("MDPNGR - COULDN'T CREATE PNGI FILE NAME",3,1);
    NGCALLF(gsvend,GSVEND)();
    return;
  }

/*
 * Construct the name of the ".pngi" file.
 */

  fnmp=strcat(strcpy(fnmp,flnm),"i");

/*
 * Open the ".pngi" file and then free the space allocated for its name.
 */

  pngi=fopen(fnmp,"r"),free(fnmp),fnmp=(char*)NULL;

  if (!pngi) {
    c_seter("MDPNGR - COULDN'T OPEN PNGI FILE",4,1);
    NGCALLF(gsvend,GSVEND)();
    return;
  }

/*
 * Skip the first line of the ".pngi" file.
 */

  while ((schr=fgetc(pngi))!='\n') {
    if (schr==EOF) {
      c_seter("MDPNGR - ERROR IN PNGI FILE, 1ST LINE",5,1);
      NGCALLF(gsvend,GSVEND)();
      return;
    }
  }

/*
 * Read the basic projection information.
 */

  if (fscanf(pngi,"Projection:         %2s%f%f%f\n",
      ptyp,&plat,&plon,&rota)!=4) {
    c_seter("MDPNGR - ERROR IN PNGI FILE, PROJECTION LINE",6,1);
    NGCALLF(gsvend,GSVEND)();
    return;
  }

/*
 * The satellite-view projection requires three additional parameters.
 */

  if (!strcmp(ptyp,"SV")) {
    if (fscanf(pngi,"Satellite values:     %f%f%f\n",
        &salt,&san1,&san2)!=3) {
      c_seter("MDPNGR - ERROR IN PNGI FILE, SATELLITE VALUES LINE",7,1);
      NGCALLF(gsvend,GSVEND)();
      return;
    }
  }

/*
 * Read the basic map-limits information.
 */

  if (fscanf(pngi,"Limits:             %2s%f%f%f%f\n",
      ltyp,&plm1[0],&plm2[0],&plm3[0],&plm4[0])!=5) {
    c_seter("MDPNGR - ERROR IN PNGI FILE, 1ST LIMITS LINE",8,1);
    NGCALLF(gsvend,GSVEND)();
    return;
  }

/*
 * The "PO" ("POints") map limits specification requires more info.
 */

  if (strcmp(ltyp,"PO")) {
    plm1[1]=plm2[1]=plm3[1]=plm4[1]=0.f;
  }
  else {
    if (fscanf(pngi,"                      %f%f%f%f\n",
        &plm1[1],&plm2[1],&plm3[1],&plm4[1])!=4) {
      c_seter("MDPNGR - ERROR IN PNGI FILE, 2ND LIMITS LINE",9,1);
      NGCALLF(gsvend,GSVEND)();
      return;
    }
  }

/*
 * Determine the number of "reference points" (points for which we
 * are given a latitude, a longitude, and the x and y coordinates of
 * the point on the PNG).
 */

  if (fscanf(pngi,"Reference points: %4d\n",&nprp)!=1) nprp=0;

/*
 * If there are four or more reference points, read all information
 * about them into space allocated for the purpose.  Otherwise, set
 * the number of reference points to zero.
 */

  if (nprp<4) {
    nprp=0;
  } else {
    if (!(pxlt=(float*)malloc(4*nprp*sizeof(float)))) {
      c_seter("MDPNGR - CAN'T ALLOCATE REFERENCE-POINT SPACE",9,1);
      NGCALLF(gsvend,GSVEND)();
      return;
    }
    pxln=pxlt+nprp,pxlx=pxln+nprp,pxly=pxlx+nprp;
  }

  for (i=0;i<nprp;i++) {
    if (fscanf(pngi,"%*d%f%f%f%f\n",pxlt+i,pxln+i,pxlx+i,pxly+i)!=4) {
      c_seter("MDPNGR - ERROR IN PNGI FILE, REFERENCE-POINT LINE",10,1);
      NGCALLF(gsvend,GSVEND)();
      free(pxlt);
      pxlt=(float*)NULL;
      return;
    }
  }

/*
 * Close the ".pngi" file.
 */

  fclose(pngi);

/*
 * Initialize EZMAP to geo-reference the PNG image.
 */

  c_maproj(ptyp,plat,plon,rota);

  if (!strcmp(ptyp,"SV")) {
    c_mpsetr("SA",salt);
    c_mpsetr("S1",san1);
    c_mpsetr("S2",san2);
  }

  c_mapset(ltyp,plm1,plm2,plm3,plm4);

  c_mapint();

  if (c_icfell("MDPNGR",11)) {
    NGCALLF(gsvend,GSVEND)();
    if (pxlt!=(float*)NULL) free(pxlt),pxlt=(float*)NULL;
    return;
  }

/*
 * Retrieve the limits of the EZMAP window (in projection space).
 */

  c_getset(&dumi,&dumi,&dumi,&dumi,&xwdl,&xwdr,&ywdb,&ywdt,&lnlg);

/*
 * Tell EZMAP to save all the current projection parameters for use
 * by MDPNGD as the alternate projection.
 */

  c_maqini();

/*
 * Set the flag that tells MDPNGD whether or not to use BIVAR to
 * interpolate a correction function for the projection.
 */

  ninp=nprp;

/*
 * If that flag is non-zero, use BIVAR to initialize the correction
 * function.
 */

  if (ninp!=0) {
   /*
    * The space allocated for reference-point information is reused
    * for BIVAR input arrays.
    */
    xinp=pxlt;
    yinp=pxln;
    zixo=pxlx;
    ziyo=pxly;
   /*
    * The point (xinp[i],yinp[i]) on the PNG should be shifted by
    * zixo[i] units to the right and ziyo[i] units upward.
    */
    for (i=0;i<ninp;i++) {
      c_maptrn(pxlt[i],pxln[i],&xcop,&ycop);
      xinp[i]=((xcop-xwdl)/(xwdr-xwdl))*(float)mgsv;
      yinp[i]=((ycop-ywdb)/(ywdt-ywdb))*(float)ngsv;
      zixo[i]=pxlx[i]-xinp[i];
      ziyo[i]=pxly[i]-yinp[i];
    }
   /*
    * If LCFA is zero, the correction function is evaluated at every
    * point where it is needed; this can be expensive when the cell
    * array used is very large.
    */
    if (*lcfa==0) {
      mcfa=ncfa=0;
     /*
      * Allocate integer workspace.
      */
      if (!(iwrk=(int*)malloc((31*ninp+1)*sizeof(int)))) {
        c_seter("MDPNGR - CAN'T ALLOCATE INTEGER WORKSPACE",11,1);
        NGCALLF(gsvend,GSVEND)();
        free(xinp),xinp=(float*)NULL;
        return;
      }
     /*
      * Allocate real workspace.
      */
      if (!(fwrk=(float*)malloc(8*ninp*sizeof(float)))) {
        c_seter("MDPNGR - CAN'T ALLOCATE REAL WORKSPACE",12,1);
        NGCALLF(gsvend,GSVEND)();
        free(xinp),xinp=(float*)NULL;
        free(iwrk),iwrk=(int*)NULL;
        return;
      }
    /*
     * If LCFA is non-zero, the correction function is approximated
     * using 2D arrays, so that evaluation only involves bi-linear
     * interpolation; this is much more efficient and the results
     * are nearly identical.
     */
    } else {
      temp=(float)irange(100,10000,*lcfa);
      mcfa=(int)sqrt(((float)mgsv/(float)ngsv)*temp);
      ncfa=(int)sqrt(((float)ngsv/(float)mgsv)*temp);
     /*
      * Allocate integer workspace.
      */
      if (!(iwrk=(int*)malloc((31*ninp+mcfa*ncfa)*sizeof(int)))) {
        c_seter("MDPNGR - CAN'T ALLOCATE INTEGER WORKSPACE",13,1);
        NGCALLF(gsvend,GSVEND)();
        free(xinp),xinp=(float*)NULL;
        return;
      }
     /*
      * Allocate real workspace.
      */
      if (!(fwrk=(float*)malloc(6*ninp*sizeof(float)))) {
        c_seter("MDPNGR - CAN'T ALLOCATE REAL WORKSPACE",14,1);
        NGCALLF(gsvend,GSVEND)();
        free(xinp),xinp=(float*)NULL;
        free(iwrk),iwrk=(int*)NULL;
        return;
      }
     /*
      * Allocate space for the approximating arrays.
      */
      if (!(xcfa=(float*)malloc((mcfa+ncfa+2*mcfa*ncfa)*
                                                      sizeof(float)))) {
        c_seter("MDPNGR - CAN'T ALLOCATE FUNCTION WORKSPACE",15,1);
        NGCALLF(gsvend,GSVEND)();
        free(xinp),xinp=(float*)NULL;
        free(iwrk),iwrk=(int*)NULL;
        free(fwrk),fwrk=(float*)NULL;
        return;
      }
     /*
      * Set up required pointers.
      */
      ycfa=xcfa+mcfa;
      zcfx=ycfa+ncfa;
      zcfy=zcfx+mcfa*ncfa;
     /*
      * The point (i,j) has coordinates (xcfa[i],ycfa[j]).
      */
      for (i=0;i<mcfa;i++) {
        xcfa[i]=((float)i/(float)(mcfa-1))*(float)mgsv;
      }
      for (j=0;j<ncfa;j++) {
        ycfa[j]=((float)j/(float)(ncfa-1))*(float)ngsv;
      }
     /*
      * Call the BIVAR routine IDSFFT to generate approximating arrays.
      */
      c_idsfft(1,ninp,xinp,yinp,zixo,mcfa,ncfa,mcfa,xcfa,ycfa,zcfx,
                                                             iwrk,fwrk);
      c_idsfft(3,ninp,xinp,yinp,ziyo,mcfa,ncfa,mcfa,xcfa,ycfa,zcfy,
                                                             iwrk,fwrk);
     /*
      * Negate NINP to indicate the approximating arrays are being used.
      */
      ninp=-ninp;
     /*
      * Free up workspace that will not be needed by MDPNGD.
      */
      free(xinp),xinp=(float*)NULL;
      free(iwrk),iwrk=(int*)NULL;
      free(fwrk),fwrk=(float*)NULL;
    }
  }
 /*
  * Set the flag that indicates initialization has been done.
  */
  init=1;
 /*
  * Done.
  */
  return;
}


/*
 * Either MAPNGD or MDPNGD may be called to create and plot a cell
 * array constructed from the data read by a previous call to MAPNGR
 * or MDPNGR.  The arguments are as follows:
 *
 *   LOCA specifies how much space is to be allocated and used for the
 *   the cell array.  The actual amount used will be no less than 100
 *   and no more than 10,000,000, and the actual dimensions of the cell
 *   array will be chosen so as to make its cells approximately square
 *   in the viewport defined by the EZMAP transformation currently in
 *   effect.
 *
 *   IORC is the color index to be used for "out-of-range" areas.
 *
 *   IGSC is the first gray-scale color index defined for use here.
 *
 *   NGSC is the number of gray-scale color indices defined.
 *
 */

void NGCALLF(mapngd,MAPNGD)(loca,iorc,igsc,ngsc)
  int*  loca;
  int*  iorc;
  int*  igsc;
  int*  ngsc;
{
  NGCALLF(mdpngd,MDPNGD)(loca,iorc,igsc,ngsc);
}

void NGCALLF(mdpngd,MDPNGD)(loca,iorc,igsc,ngsc)
  int*  loca;
  int*  iorc;
  int*  igsc;
  int*  ngsc;
{
  int i,igsv,itmp,j,jgsv,jtmp,k,lnlg,mdca,ndca;
  float dumi,oorv=9.E11f,temp,xcul,xcur,xtmp,ycub,ycut,ytmp;
  float rlat,rlon,xcoc,xcop,xcus,xotp,ycoc,ycop,ycus,yotp,zoxo,zoyo;
  int* icra=(int*)NULL;
  Gpoint llco,urco;
  Grect rect;
  Gint_size csze;
  Gpat_rep ccra;

/*
 * Check for an uncleared prior error.
 */

  if (c_icfell("MDPNGD - UNCLEARED PRIOR ERROR",1)) return;

/*
 * Make sure the user has initialized this package.
 */

  if (!init) {
    c_seter("MDPNGD - NO CALL TO MAPNGR/MDPNGR WAS DONE",2,1);
    return;
  }

/*
 * Retrieve the limits of the EZMAP window.
 */

  c_getset(&dumi,&dumi,&dumi,&dumi,&xcul,&xcur,&ycub,&ycut,&lnlg);

/*
 * Allocate space for the cell array.
 */

  temp=(float)irange(100,10000000,*loca);

  mdca=(int)sqrt(((float)(xcur-xcul)/(float)(ycut-ycub))*temp);
  ndca=(int)sqrt(((float)(ycut-ycub)/(float)(xcur-xcul))*temp);

  if (!(icra=(int*)malloc(mdca*ndca*sizeof(int)))) {
    c_seter("MDPNGD - CAN'T ALLOCATE CELL ARRAY SPACE",3,1);
    return;
  }

/*
 * Construct the cell array.  For each cell of the cell array, we use
 * EZMAP's current inverse transformation to get a latitude and a
 * longitude.  Then, we use EZMAP's alternate transformation (saved
 * by MDPNGR) to find out what pixel of the PNG, if any, covers that
 * lat/lon position.  The gray-scale value for that pixel is then
 * used to compute a color index to put in the cell array.
 */

  k=1;

  for (i=0;i<mdca;i++) {
    xcoc=(.5+(float)i)/(float)mdca;
    xcus=xcul+xcoc*(xcur-xcul);
    for (j=0;j<ndca;j++) {
      ycoc=(.5+(float)j)/(float)ndca;
      ycus=ycub+ycoc*(ycut-ycub);
      c_maptri(xcus,ycus,&rlat,&rlon);
      if (rlat>=oorv) {
        *(icra+i+j*mdca)=*iorc;
      } else {
        c_maqtra(rlat,rlon,&xcop,&ycop);
        if (xcop>=oorv) {
          *(icra+i+j*mdca)=*iorc;
        } else {
          xotp=((xcop-xwdl)/(xwdr-xwdl))*(float)mgsv;
          yotp=((ycop-ywdb)/(ywdt-ywdb))*(float)ngsv;
          if (ninp==0) {
            igsv=irange(1,mgsv,1+(int)xotp);
            jgsv=irange(1,ngsv,1+(int)yotp);
          } else if (ninp>0) {
            c_idbvip(k,ninp,xinp,yinp,zixo,1,&xotp,&yotp,&zoxo,
                                                             iwrk,fwrk);
            c_idbvip(3,ninp,xinp,yinp,ziyo,1,&xotp,&yotp,&zoyo,
                                                             iwrk,fwrk);
            k=2;
            igsv=irange(1,mgsv,1+(int)(xotp+zoxo));
            jgsv=irange(1,ngsv,1+(int)(yotp+zoyo));
          } else {
            xtmp=((xcop-xwdl)/(xwdr-xwdl))*(float)(mcfa-1);
            ytmp=((ycop-ywdb)/(ywdt-ywdb))*(float)(ncfa-1);
            itmp=irange(0,mcfa-2,(int)xtmp);
            jtmp=irange(0,ncfa-2,(int)ytmp);
            xtmp=xtmp-(float)itmp;
            ytmp=ytmp-(float)jtmp;
            zoxo=(1.f-ytmp)*((1.f-xtmp)*(*(zcfx+itmp  + jtmp   *mcfa))+
                                  xtmp *(*(zcfx+itmp+1+ jtmp   *mcfa)))
                    + ytmp *((1.f-xtmp)*(*(zcfx+itmp  +(jtmp+1)*mcfa))+
                                  xtmp *(*(zcfx+itmp+1+(jtmp+1)*mcfa)));
            zoyo=(1.f-ytmp)*((1.f-xtmp)*(*(zcfy+itmp  + jtmp   *mcfa))+
                                  xtmp *(*(zcfy+itmp+1+ jtmp   *mcfa)))
                    + ytmp *((1.f-xtmp)*(*(zcfy+itmp  +(jtmp+1)*mcfa))+
                                  xtmp *(*(zcfy+itmp+1+(jtmp+1)*mcfa)));
            igsv=irange(1,mgsv,1+(int)(xotp+zoxo));
            jgsv=irange(1,ngsv,1+(int)(yotp+zoyo));
          }
          *(icra+i+j*mdca)=*igsc+(int)
                 ((float)(*ngsc-1)*NGCALLF(gsvalu,GSVALU)(&igsv,&jgsv));
        }
      }
    }
  }

/*
 * Display the cell array.
 */

  llco.x=xcul,llco.y=ycub;
  urco.x=xcur,urco.y=ycut;
  rect.p=llco,rect.q=urco;
  csze.size_x=mdca,csze.size_y=ndca;
  ccra.dims=csze;
  ccra.colr_array=icra;
  gcell_array(&rect,&ccra);

/*
 * Free the space allocated for the cell array.
 */

  free(icra),icra=(int*)NULL;
}


/*
 * Either MAPNGQ or MDPNGQ may be called to release all the space
 * allocated by a previous call to MAPNRG or MDPNGR.  It has no
 * arguments.
 */

void NGCALLF(mapngq,MAPNGQ)()
{
  NGCALLF(mdpngq,MDPNGQ)();
}

void NGCALLF(mdpngq,MDPNGQ)()
{

/*
 * Free the space allocated for the ".png".
 */

    NGCALLF(gsvend,GSVEND)();

/*
 * Free the space allocated by MDPNGR.
 */

    if (xinp!=(float*)NULL) free(xinp),xinp=(float*)NULL;
    if (iwrk!=(int*)NULL) free(iwrk),iwrk=(int*)NULL;
    if (fwrk!=(float*)NULL) free(fwrk),fwrk=(float*)NULL;

/*
 * Turn off the initialization flag.
 */

    init=0;
}


/*
 * The function irange(imin,imax,ival) returns the equivalent of the
 * Fortran expression MAX(imin,MIN(imax,ival)).
 */

int irange(imin,imax,ival)
  int imin;
  int imax;
  int ival;
{
  if (ival<imin) return imin;
  if (ival>imax) return imax;
  return ival;
}
