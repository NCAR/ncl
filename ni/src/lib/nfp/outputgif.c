#include <stdio.h>

/*
 * The following are the required NCAR Graphics include files.
 * They should be located in ${NCARG_ROOT}/include.
 */
#include "wrapper.h"
#include "NclHLUObj.h"
#include <ncarg/hlu/NcgmWorkstation.h>
#include <math.h>

#define min(x,y)  ((x) < (y) ? (x) : (y))

NhlErrorTypes output_gif_W( void )
{
/*
 * Variables to retrieve from script.
 */
  obj *ncl_objs;
  int ndims_ncl_objs, dsizes_ncl_objs[NCL_MAX_DIMENSIONS], total_ncl_objs;
  string *giffile;
  int *resltn;
/*
 * Other variables.
 */
  char *giffile2;
  const char	*tmpdir;
  char		buf[1024];
  char		buf1[1024];
  char		stderrfile[1024];
  pid_t		pid;
  char		*basename = "tmp.ncgm";
  float		wlx,wly,wux,wuy;
  int		is_scalar_gif, nobjs, rh,rw;
  long		res;
  struct    stat statbuf;
  extern void output_ncgm(NclHLUObj*,int,char *);
  NclHLUObj *tmp_hlu_ptr;

  ncl_objs = (obj *)NclGetArgValue(
	   0,
	   3,
	   &ndims_ncl_objs,
	   dsizes_ncl_objs,
	   NULL,
	   NULL,
	   NULL,
	   2);

  giffile = (string *)NclGetArgValue(
	   1,
	   3,
	   NULL,
	   NULL,
	   NULL,
	   NULL,
	   NULL,
	   2);

  resltn = (int *)NclGetArgValue(
	   2,
	   3,
	   NULL,
	   NULL,
	   NULL,
	   NULL,
	   NULL,
	   2);

  rw = resltn[0];
  rh = resltn[1];

  giffile2 = NrmQuarkToString(*giffile);
/*
 * Calculate number of objects.
 */
  total_ncl_objs = 1;
  for(nobjs = 0; nobjs < ndims_ncl_objs; nobjs++ ) {
	total_ncl_objs *= dsizes_ncl_objs[nobjs];
  }
/*
 * Initialize some stuff.
 */
  wlx = wly = 0.0;
  wux = wuy = 1.0;

  tmpdir = GetNCARGPath("tmp");
  pid = getpid();
	
  if (strlen(tmpdir) + strlen(basename) + 12 > 1024) {
	NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			   "output_gif: command exceeds buffer length"));
	return(NhlFATAL);
  }
  sprintf(buf,"%s/%d%s",tmpdir,pid,basename);
  sprintf(stderrfile,"%s/stderr.%d",tmpdir,pid);
 

/*
 * Convert NCL objects to HLU objects.
 */
  tmp_hlu_ptr = (NclHLUObj*)calloc(total_ncl_objs*sizeof(NclHLUObj),1);
  for(nobjs = 0; nobjs < ndims_ncl_objs; nobjs++ ) {
	tmp_hlu_ptr[nobjs] = (NclHLUObj)_NclGetObj(ncl_objs[nobjs]);
  }
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
	char buf[512];
	int	rl, i, hlu_id;
/*
 * Open a resource list and write the NCGM file name to it.
 */
	rl = NhlRLCreate(NhlSETRL);
	NhlRLSetString(rl,NhlNwkMetaName,outfile);
 	
/* 
 * Open an NCGM workstation.
 */
	ret = NhlCreate(&wks,"NcgmWrk",NhlncgmWorkstationClass,
					NhlDEFAULT_APP,rl);
	if (ret < NhlWARNING) return;

	NhlRLDestroy(rl);   /* Destroy resource list. */

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

