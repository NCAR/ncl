#include <stdio.h>

/*
* The following are the required NCAR Graphics include files.
* They should be located in ${NCARG_ROOT}/include
*/
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/ncl/defs.h>
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/NclBuiltInSupport.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>
#include <ncarg/hlu/XWorkstation.h>
#include <ncarg/ncl/NclBuiltIns.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/ngmath.h>
#include <ncarg/ncargC.h>
#include "wrapper.h"

/*
 *  The following is required in order to obtain an NCL object ID
 *  when you know the HLU ID.
 */
#include "NclHLUObj.h"

NhlErrorTypes nglogo_W( void )
{
	float *x;
	float *y;
	float *size;
	int *itype, *icol1, *icol2;

	/* Declaring temporary variables */
	int grlist, gkswid;
	int *nwid, nid;
 
	/*
	 *  Definte a variable to store the HLU object identifier.
	 */
	NclHLUObj tmp_hlu_obj;

	/*
	 * Retrieve parameters
	 */

	/*
	 *  nwid points to the HLU identifier of the graphic object; this is
	 *  converted to the NCL workstation identifier below.
	 */
	nwid = (int*)  NclGetArgValue(0,7,     NULL,     NULL, NULL,NULL,NULL,2);

	x       = (float*) NclGetArgValue(1,7, NULL, NULL, NULL,NULL,NULL,2);
	y       = (float*) NclGetArgValue(2,7, NULL, NULL, NULL,NULL,NULL,2);
	size    = (float*) NclGetArgValue(3,7, NULL, NULL, NULL,NULL,NULL,2);
	itype   = (int*) NclGetArgValue(4,7, NULL, NULL, NULL,NULL,NULL,2);
	icol1   = (int*) NclGetArgValue(5,7, NULL, NULL, NULL,NULL,NULL,2);
	icol2   = (int*) NclGetArgValue(6,7, NULL, NULL, NULL,NULL,NULL,2);

	/*
	 *  Determine the NCL identifier for the graphic object in nid.
	 */
	tmp_hlu_obj = (NclHLUObj) _NclGetObj(*nwid);
	nid = tmp_hlu_obj->hlu.hlu_id;

	/*
	 * Retrieve the GKS workstation id from the workstation object.
	 */
  
	grlist = NhlRLCreate(NhlGETRL);
	NhlRLClear(grlist);
	NhlRLGetInteger(grlist,NhlNwkGksWorkId,&gkswid);
	NhlGetValues(nid,grlist);

	/*
	* The following section calls the c_nglogo function.
	*/
	gactivate_ws (gkswid);

	c_nglogo(gkswid, *x, *y, *size, *itype, *icol1, *icol2);

	gdeactivate_ws (gkswid);

	NhlRLDestroy(grlist);

	return(NhlNOERROR);
}

NhlErrorTypes ngezlogo_W( void )
{
	/* Declaring temporary variables */

	int grlist, gkswid;
	int *nwid, nid;
 
	/*
	 *  Definte a variable to store the HLU object identifier.
	 */
	NclHLUObj tmp_hlu_obj;

	/*
	 * Retrieve parameters
	 */

	/*
	 *  nwid points to the HLU identifier of the graphic object; this is
	 *  converted to the NCL workstation identifier below.
	 */
	nwid = (int*)  NclGetArgValue(0,1,     NULL,     NULL, NULL,NULL,NULL,2);

	/*
	 *  Determine the NCL identifier for the graphic object in nid.
	 */
	tmp_hlu_obj = (NclHLUObj) _NclGetObj(*nwid);
	nid = tmp_hlu_obj->hlu.hlu_id;

	/*
	 * Retrieve the GKS workstation id from the workstation object.
	 */
  
	grlist = NhlRLCreate(NhlGETRL);
	NhlRLClear(grlist);
	NhlRLGetInteger(grlist,NhlNwkGksWorkId,&gkswid);
	NhlGetValues(nid,grlist);

	/*
	* The following section calls the c_ngezlogo function.
	*/
	gactivate_ws (gkswid);

	c_ngezlogo();

	gdeactivate_ws (gkswid);

	NhlRLDestroy(grlist);

	return(NhlNOERROR);
}
