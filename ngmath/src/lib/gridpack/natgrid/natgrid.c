/*
 * $Id: natgrid.c,v 1.6 2000-08-25 23:29:43 fred Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU General Public License as published     *
* by the Free Software Foundation; either version 2 of the License, or  *
* (at your option) any later version.                                   *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* General Public License for more details.                              *
*                                                                       *
* You should have received a copy of the GNU General Public License     *
* along with this software; if not, write to the Free Software         *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

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
   if(wts != NULL) {
	free(wts);
   }
   if(nbrs != NULL) {
	free(nbrs);
   }

   magx = magx_orig;
   magy = magy_orig;
   magz = magz_orig;
}
