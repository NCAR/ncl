/*
 *  This file contains the settings for most of the
 *  global variables by way of including nnmhead.h .
 */

#include "nnmhead.h"
#include "nnghead.h"
#include "nntypes.h"
#include "nntpvrs.h"
#include "nnexver.h"

void Terminate()
{
   struct simp *tmp,*tmp0;
   struct datum *dtmp,*dtmp0;
   struct neig *ntmp,*ntmp0;
   struct temp *ttmp,*ttmp0;
   tmp = rootsimp;
   while(tmp!=NULL) {
	tmp0 =tmp->nextsimp;
	free(tmp);
	tmp = tmp0;
   }
   rootsimp = cursimp = holdsimp = lastsimp = prevsimp = NULL;
   dtmp = rootdat;
   while(dtmp!=NULL) {
        dtmp0 =dtmp->nextdat;
        free(dtmp);
        dtmp = dtmp0;
   }
   rootdat = curdat = holddat = NULL;
   ntmp = rootneig;
   while(ntmp!=NULL) {
        ntmp0 =ntmp->nextneig;
        free(ntmp);
        ntmp = ntmp0;
   }
   rootneig = curneig = lastneig = NULL;
   ttmp = roottemp;
   while(ttmp!=NULL) {
        ttmp0 =ttmp->nexttemp;
        free(ttmp);
        ttmp = ttmp0;
   }
   roottemp = curtemp = lasttemp= prevtemp= NULL;

   if(points!=NULL) { 
	FreeMatrixd(points); 
   	points = NULL;
   }
   if(joints!=NULL) {
	FreeMatrixd(joints);
   	joints = NULL;
   }
   if(jndx != NULL) {
	FreeVecti(jndx);
   	jndx = NULL;
   }
   

   magx = magx_orig;
   magy = magy_orig;
   magz = magz_orig;
}
