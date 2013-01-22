#include <string.h>
#include <stdio.h>
#include <ncarg/hlu/Workstation.h>
#include "wrapper.h"

extern void NGCALLF(ctdriver,CTDRIVER)(int *wks, float *lat, float *lon, 
                                  float *data, int *nlat, int *nlon, 
                                  int *idim, int *jdim, float *rpnt,
                                  int *mpnt, float *rwrk, int *lrwk,
                                  float *xcra, float *ycra, int *ncra, 
                                  int *iwrk, int *liwk, int *iama,
                                  int *lama, int *iaai, int *iagi,
                                  int *ngps, int *icra, int *icam,
                                  int *ican, int *iedg, int *medg,
                                  int *itri, int *mtri, int *ippp,
                                  int *mnop, int *ippe, int *mnoe,
                                  int *lopn, int *loen, int *lotn,
                                  int *igrd, int *imsh, int *icon,
                                  int *icol,  int *icap, int *imap,
                                  char *fnam, int *itim, int *ilev,
                                  int *idbg, int clen);

NhlErrorTypes ctwrap_W( void )
{
  int *wks;
  float *lat, *lon, *data;
  ng_size_t nlat, nlon, nlon8, dsizes_lat[2], dsizes_lon[2], dsizes_data[2];
  int inlat, inlon, idim, jdim;
  logical *opt;

/*
 * Variables for retrieving workstation information.
 */
  int grlist, gkswid, nwks;
  NclHLUObj tmp_hlu_obj;

/*
 * Work arrays.
 */
  float *rpnt, *rwrk, *xcra, *ycra;
  int *iedg, *itri, *ippp, *ippe, *iwrk, *icra, *iama, *iaai, *iagi;
  int mpnt, medg, mtri, mnop, mnoe, mnot, lrwk, liwk, lama, ncra, ngps;
  int icam, ican, lopn, loen, lotn;

/*
 * Variables for retrieving attributes from "opt".
 */
  logical idbg = False, igrd = False, imsh = False;
  logical icon = True, icol = False, icap = False;
  NrmQuark *MapProjection, *fnam;
  char *cMapProjection, *cnam = NULL;
  int imap = 2, ilev = -1, itim = -1;
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry   stack_entry;

/*
 * Retrieve parameters.
 */
  wks    =     (int*)NclGetArgValue(0,5, NULL, NULL,NULL,NULL,NULL,DONT_CARE);
  lat    =   (float*)NclGetArgValue(1,5, NULL, dsizes_lat, NULL,NULL,NULL,DONT_CARE);
  lon    =   (float*)NclGetArgValue(2,5, NULL, dsizes_lon, NULL,NULL,NULL,DONT_CARE);
  data   =   (float*)NclGetArgValue(3,5, NULL, dsizes_data, NULL,NULL,NULL,DONT_CARE);
  opt    = (logical*)NclGetArgValue(4,5, NULL,NULL,NULL,NULL,NULL,DONT_CARE);

/*
 * Check input sizes.
 */
  nlat = dsizes_lat[0]; 
  nlon = dsizes_lat[1]; 
  if( dsizes_lon[0]  != nlat || dsizes_lon[1]  != nlon ||
      dsizes_data[0] != nlat || dsizes_data[1] != nlon ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ctwrap: the dimension sizes of the lat/lon arrays and the data must be same");
    return(NhlFATAL);
  }

/*
 * Hopefully the number of longitudes is divisible by 8 (the nature of the
 * SEAM grid), so get that number here.
 */
  nlon8 = nlon/8; 

  if ((nlat > INT_MAX) || (nlon > INT_MAX) || (nlon8 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ctwrap: one or more input dimension sizes is > INT_MAX");
    return(NhlFATAL);
  }
  inlat = (int) nlat;
  inlon = (int) nlon;
  idim  = jdim = (int) nlon8;

/*
 *  Determine the NCL identifier for the graphic object.
 */
    tmp_hlu_obj = (NclHLUObj) _NclGetObj(*wks);
    nwks        = tmp_hlu_obj->hlu.hlu_id;
/*
 * Retrieve the GKS workstation id from the workstation object.
 */
    grlist = NhlRLCreate(NhlGETRL);
    NhlRLClear(grlist);
    NhlRLGetInteger(grlist,NhlNwkGksWorkId,&gkswid);
    NhlGetValues(nwks,grlist);
/*
 * Activate workstation.
 */
    gactivate_ws (gkswid);
 
/*
 * Create work arrays, lots of them.
 */
    lopn = 5;
    loen = 5;
    lotn = 4;
    mnop = 7352;
    mnoe = 22050;
    mnot = 14700;
    lrwk = 10000;
    liwk = 1000;
    lama = 400000;
    ncra = lama/10;
    icam = 512;             /* could be 1024? */
    ican = 512;             /* could be 1024? */
    ngps = 2;

    mpnt = mnop*lopn;   /*  space for points */
    medg = mnoe*loen;   /*  space for edges  */
    mtri = mnot*lotn;   /*  space for triangles */

    rpnt = (float *)calloc(mpnt,sizeof(float));
    rwrk = (float *)calloc(lrwk,sizeof(float));
    xcra = (float *)calloc(ncra,sizeof(float));
    ycra = (float *)calloc(ncra,sizeof(float));

    iwrk = (int *)calloc(liwk,sizeof(int));
    iama = (int *)calloc(lama,sizeof(int));
    iaai = (int *)calloc(ngps,sizeof(int));
    iagi = (int *)calloc(ngps,sizeof(int));
    icra = (int *)calloc(icam*ican,sizeof(int));
    iedg = (int *)calloc(medg,sizeof(int));
    itri = (int *)calloc(mtri,sizeof(int));
    ippp = (int *)calloc(2*mnop,sizeof(int));
    ippe = (int *)calloc(2*mnoe,sizeof(int));

    if(rpnt == NULL || rwrk == NULL || xcra == NULL || ycra == NULL || 
       iwrk == NULL || iama == NULL || iaai == NULL || iagi == NULL || 
       icra == NULL || iedg == NULL || itri == NULL || ippp == NULL || 
       ippe == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ctwrap: Unable to allocate memory for work arrays");
      return(NhlFATAL);
    }

/* 
 * If "opt" is True, then check if any attributes have been set.
 */
    if(*opt) {
      stack_entry = _NclGetArg(4, 5, DONT_CARE);
      switch (stack_entry.kind) {
      case NclStk_VAR:
        if (stack_entry.u.data_var->var.att_id != -1) {
          attr_obj = (NclAtt) _NclGetObj(stack_entry.u.data_var->var.att_id);
          if (attr_obj == NULL) {
            break;
          }
        }
        else {
/*
 * att_id == -1 ==> no optional args given.
 */
          break;
        }
/* 
 * Get optional arguments.
 */
        if (attr_obj->att.n_atts <= 0) {
          break;
        }
/*
 * Get list of attributes.
 */
        attr_list = attr_obj->att.att_list;
/*
 * Loop through attributes and check them. The current ones recognized are:
 *
 *   "RectangularMesh"
 *   "TriangularMesh"
 *   "LineContours"
 *   "FilledContours"
 *   "CellArray"
 *   "MapProjection"
 *   "FieldName"
 *   "TimeStep"
 *   "Level"
 *   "Debug"
 */
        while (attr_list != NULL) {
/*
 * Check for "RectangularGrid".
 */
          if (!strcmp(attr_list->attname, "RectangularGrid")) {
            if(attr_list->attvalue->multidval.data_type != NCL_logical) {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"ctwrap: The 'RectangularGrid' attribute must be a logical, defaulting to False.");
            }
            else {
              igrd = *(logical*) attr_list->attvalue->multidval.val;
            }
          }

/*
 * Check for "TriangularMesh".
 */
          if (!strcmp(attr_list->attname, "TriangularMesh")) {
            if(attr_list->attvalue->multidval.data_type != NCL_logical) {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"ctwrap: The 'TriangularMesh' attribute must be a logical, defaulting to False.");
            }
            else {
              imsh = *(logical*) attr_list->attvalue->multidval.val;
            }
          }

/*
 * Check for "LineContours".
 */
          if (!strcmp(attr_list->attname, "LineContours")) {
            if(attr_list->attvalue->multidval.data_type != NCL_logical) {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"ctwrap: The 'LineContours' attribute must be a logical, defaulting to True.");
            }
            else {
              icon = *(logical*) attr_list->attvalue->multidval.val;
            }
          }

/*
 * Check for "FilledContours".
 */
          if (!strcmp(attr_list->attname, "FilledContours")) {
            if(attr_list->attvalue->multidval.data_type != NCL_logical) {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"ctwrap: The 'FilledContours' attribute must be a logical, defaulting to False.");
            }
            else {
              icol = *(logical*) attr_list->attvalue->multidval.val;
            }
          }

/*
 * Check for "CellArray".
 */
          if (!strcmp(attr_list->attname, "CellArray")) {
            if(attr_list->attvalue->multidval.data_type != NCL_logical) {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"ctwrap: The 'CellArray' attribute must be a logical, defaulting to False.");
            }
            else {
              icap = *(logical*) attr_list->attvalue->multidval.val;
            }
          }

/*
 * Check for "FieldName".
 */
          if (!strcmp(attr_list->attname, "FieldName")) {
            if(attr_list->attvalue->multidval.data_type != NCL_string) {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"ctwrap: The 'FieldName' attribute must be a string, ignoring...");
            }
            else {
              fnam = (NrmQuark *) attr_list->attvalue->multidval.val;
              cnam = NrmQuarkToString(*fnam);
            }
          }
/*
 * Check for "TimeStep".
 */
          if (!strcmp(attr_list->attname, "TimeStep")) {
            if(attr_list->attvalue->multidval.data_type != NCL_int) {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"ctwrap: The 'TimeStep' attribute must be an integer, defaulting to -1.");
            }
            else {
              itim = *(int*) attr_list->attvalue->multidval.val;
            }
          }

/*
 * Check for "Level".
 */
          if (!strcmp(attr_list->attname, "Level")) {
            if(attr_list->attvalue->multidval.data_type != NCL_int) {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"ctwrap: The 'Level' attribute must be an integer, defaulting to -1.");
            }
            else {
              ilev = *(int*) attr_list->attvalue->multidval.val;
            }
          }

/*
 * Check for "Debug".
 */
          if (!strcmp(attr_list->attname, "Debug")) {
            if(attr_list->attvalue->multidval.data_type != NCL_logical) {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"ctwrap: The 'Debug' attribute must be a logical, defaulting to False.");
            }
            else {
              idbg = *(logical*) attr_list->attvalue->multidval.val;
            }
          }

/*
 * Check for "MapProjection".
 */
          if (!strcmp(attr_list->attname, "MapProjection")) {
            if(attr_list->attvalue->multidval.data_type != NCL_string) {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"ctwrap: The 'MapProjection' attribute must be a string, defaulting to 'CylindricalEquidistant.'");
            }
            else {
              MapProjection  = (NrmQuark *) attr_list->attvalue->multidval.val;
              cMapProjection = NrmQuarkToString(*MapProjection);
              if(!strcmp(cMapProjection,"Orthographic")) {
                imap = 1;
              }
              else if(!strcmp(cMapProjection,"CylindricalEquidistant")) {
                imap = 2;
              }
              else if(!strcmp(cMapProjection,"Robinson")) {
                imap = 3;
              }
              else if(!strcmp(cMapProjection,"LambertEqualArea")) {
                imap = 4;
              }
              else {
                NhlPError(NhlWARNING,NhlEUNKNOWN,"ctwrap: Unrecognized value for the 'MapProjection' attribute. Defaulting to 'CylindricalEquidistant'.");
                imap = 2;
              }
            }
          }
          attr_list = attr_list->next;
        }
      default:
        break;
      }
    }
      
    if(cnam == NULL) {
      cnam = (char*)calloc(2,sizeof(char));
      strcpy(cnam,"");
    }

    NGCALLF(ctdriver,CTDRIVER)(&gkswid,lat,lon,data,&inlat,&inlon,&idim,&jdim,
                               rpnt,&mpnt,rwrk,&lrwk,xcra,ycra,&ncra,iwrk,
                               &liwk,iama,&lama,iaai,iagi,&ngps,icra,&icam,
                               &ican,iedg,&medg,itri,&mtri,ippp,&mnop,ippe,
                               &mnoe,&lopn,&loen,&lotn,&igrd,&imsh,&icon,&icol,
                               &icap,&imap,cnam,&itim,&ilev,&idbg,
                               strlen(cnam));
/*
 * Free work arrays. 
 */
    NclFree(rpnt);
    NclFree(rwrk);
    NclFree(xcra);
    NclFree(ycra);
    NclFree(iwrk);
    NclFree(iama);
    NclFree(iaai);
    NclFree(iagi);
    NclFree(icra);
    NclFree(iedg);
    NclFree(itri);
    NclFree(ippp);
    NclFree(ippe);


/*
 * Deactivate workstation.
 */
    gdeactivate_ws (gkswid);

    return(NhlNOERROR);
}

