#include <stdio.h>
#include <ctype.h>
#include <unistd.h>
#include <math.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/View.h>
#include "wrapper.h"

NhlErrorTypes output_gif_W( void )
{
/*
 * Variables to retrieve from script.
 */
  obj *ncl_objs;
  int ndims_ncl_objs;
  ng_size_t dsizes_ncl_objs[NCL_MAX_DIMENSIONS], total_ncl_objs;
  NrmQuark *giffile;
  int *resltn, *MaximizeBB;
/*
 * Other variables.
 */
  char *giffile2, buf[1024], buf1[1024], stderrfile[1024];
  char *basename = "tmp.ncgm";
  const char *tmpdir;
  pid_t pid;
  float pw,ph,wlx,wly,wux,wuy;
  float min_l, min_b, max_t, max_r, margin;
  int nobjs, rh, rw;
  NhlBoundingBox the_box;
  struct stat statbuf;
  extern void output_ncgm(NclHLUObj*,int,char *);
  NclHLUObj *tmp_hlu_ptr;

  ncl_objs = (obj *)NclGetArgValue(
           0,
           4,
           &ndims_ncl_objs,
           dsizes_ncl_objs,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

  giffile = (NrmQuark *)NclGetArgValue(
           1,
           4,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

  resltn = (int *)NclGetArgValue(
           2,
           4,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

  MaximizeBB = (int *)NclGetArgValue(
           3,
           4,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

  giffile2 = NrmQuarkToString(*giffile);
/*
 * Calculate number of objects.
 */
  total_ncl_objs = 1;
  for(nobjs = 0; nobjs < ndims_ncl_objs; nobjs++ ) {
    total_ncl_objs *= dsizes_ncl_objs[nobjs];
  }
/*
 * Convert NCL objects to HLU objects.
 */
  tmp_hlu_ptr = (NclHLUObj*)calloc(total_ncl_objs*sizeof(NclHLUObj),1);
  min_l = min_b = 1.;
  max_t = max_r = 0.;
  for(nobjs = 0; nobjs < total_ncl_objs; nobjs++ ) {
    tmp_hlu_ptr[nobjs] = (NclHLUObj)_NclGetObj(ncl_objs[nobjs]);

    if (*MaximizeBB) {
/*
 * Get the smallest possible bounding box that holds all of the objects.
 */
      NhlGetBB(tmp_hlu_ptr[nobjs]->hlu.hlu_id,&the_box);
      min_l = max(0.,min(min_l,the_box.l));
      min_b = max(0.,min(min_b,the_box.b));
      max_r = min(1.,max(max_r,the_box.r));
      max_t = min(1.,max(max_t,the_box.t));
    }
  }
  if (*MaximizeBB) {
/*
 * Calculate a new resolution size based on the size of the bounding box.
 */
    pw = max_r - min_l;
    ph = max_t - min_b;
    margin = 0.05 * min(pw,ph);
    wlx = max(0.0,min_l - margin);
    wly = max(0.0,min_b - margin);
    wux = min(1.0,max_r + margin);
    wuy = min(1.0,max_t + margin);
    ph = wuy - wly;
    pw = wux - wlx;
    if (ph > pw) {
      rh = resltn[0];
      rw = pw / ph * resltn[0];
    }
    else {
      rw = resltn[0];
      rh = ph / pw * resltn[0];
    }
  }
  else {
    rw = rh = resltn[0];
    wlx = wly = 0.0;
    wux = wuy = 1.0;
  }
/*
 * Initialize some stuff.
 */
  tmpdir = GetNCARGPath("tmp");
  pid = getpid();
        
  if (strlen(tmpdir) + strlen(basename) + 12 > 1024) {
    NHLPERROR((NhlFATAL,NhlEUNKNOWN,"output_gif: command exceeds buffer length"));
    return(NhlFATAL);
  }
/*
 * Initialize names of error file and NCGM.
 */
  sprintf(buf,"%s/%d%s",tmpdir,pid,basename);
  sprintf(stderrfile,"%s/stderr.%d",tmpdir,pid);

/*
 * Draw objects to a temporary NCGM.
 */
  output_ncgm(tmp_hlu_ptr,total_ncl_objs,buf);

/*
 * Convert NCGM to GIF.
 */
  sprintf(buf1,
          "sh -c '( ctrans -d sun -res %dx%d -window %f:%f:%f:%f %s 2>%s | rasttopnm -quiet 2>>%s | ppmtogif -quiet 2>>%s >%s )' 1>>%s 2>&1",
          rw,rh,wlx,wly,wux,wuy,buf,
          stderrfile,stderrfile,stderrfile,giffile2,stderrfile);
        
/*
 * Clean up (remove temporary NCGM and error files).
 */
  system(buf1);
  unlink(buf);

  if (! stat(stderrfile,&statbuf)) {
    int i,rlen,len = min(1024,statbuf.st_size);
    if (len == 0) {
      unlink(stderrfile);
    }
    else {
      FILE *fp = fopen(stderrfile,"r");
      rlen = fread(buf,1,len,fp);
      buf[min(1023,rlen)] = '\0';
                
      for (i = rlen-1; i >= 0; i--) {
        if (isspace(buf[i]))
          buf[i] = '\0';
        else 
          break;
      }
      fclose(fp);
      unlink(stderrfile);
      return(NhlFATAL);
    }
  }
  return(NhlNOERROR);
}


void output_ncgm(
  NclHLUObj *hlu_ptr,
  int total_objs,
  char *outfile
)
{
  NhlErrorTypes   ret = NhlNOERROR;
  int old_wks, wks;
  int rl, gl, i, hlu_id, num_dims;
  ng_size_t *len_dims;
  float *cmap;
/*
 * Need to get the workstation so we can retrieve  the color map.
 * We are assuming here that all the objects are being drawn to the
 * same workstation, so they all have the same color map.
 */
  hlu_id = hlu_ptr[0]->hlu.hlu_id;
  old_wks = NhlGetParentWorkstation(hlu_id);

  gl = NhlRLCreate(NhlGETRL);
  NhlRLClear(gl);
  NhlRLGetMDFloatArray(gl,"wkColorMap",&cmap,&num_dims,&len_dims);
  NhlGetValues(old_wks,gl);

/*
 * Open a resource list and write the NCGM file name to it.
 */
  rl = NhlRLCreate(NhlSETRL);
  NhlRLSetMDFloatArray(rl,"wkColorMap",cmap,num_dims,len_dims);
  NhlRLSetString(rl,NhlNwkMetaName,outfile);

/* 
 * Open an NCGM workstation.
 */
  ret = NhlCreate(&wks,"NcgmWrk",NhlncgmWorkstationClass,NhlDEFAULT_APP,rl);
  if (ret < NhlWARNING) return;

  NhlRLDestroy(rl);   /* Destroy resource list. */
  NhlRLDestroy(gl);   /* Destroy resource list. */

/*
 * For each object to be drawn, first retrieve its original parent
 * workstation, draw it to the new NCGM workstation, then change it
 * back to original parent workstation.
 */
  for (i = 0; i < total_objs; i++ ) {
    hlu_id = hlu_ptr[i]->hlu.hlu_id;
    old_wks = NhlGetParentWorkstation(hlu_id);
    ret = NhlChangeWorkstation(hlu_id,wks);
    NhlDraw(hlu_id);
    ret = NhlChangeWorkstation(hlu_id,old_wks);
  }
/*
 * Advance the frame of the NCGM workstation and then destroy it
 * since it is no longer needed.
 */
  NhlFrame(wks);
  NhlDestroy(wks);
}

