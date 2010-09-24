#include <stdio.h>
#include "wrapper.h"
#include "mpp.h"
#include "mpp_domain.h"
#include "mpp_io.h"

extern void nccf_make_conformal_cubic_hgrid(int nxbnds, int *nlon, 
					int nratio, int *ndivx, int *ndivy,
					char *gridname, char *history);

NhlErrorTypes nccfmakeconformalcubichgridW(void)
{


NhlErrorTypes ret = NhlNOERROR;
/*
* NCL Input Arguments
*/


string *gridname;
int *nxbnds, *nlon, *nratio, *ndivx, *ndivy;
char *history = "history!!!";
int argc = 0;
char **argv=NULL;
/*
* Input Array Variables
*/

/* nxbnds*/
int ndims_nxbnds;
ng_size_t dsizes_nxbnds[NCL_MAX_DIMENSIONS];
NclScalar missing_nxbnds;
int has_missing_nxbnds;
NclBasicDataTypes type_nxbnds;

/* nlon*/
int ndims_nlon;
ng_size_t dsizes_nlon[NCL_MAX_DIMENSIONS];
NclScalar missing_nlon;
int has_missing_nlon;
NclBasicDataTypes type_nlon;

/* nratio*/
int ndims_nratio;
ng_size_t dsizes_nratio[NCL_MAX_DIMENSIONS];
NclScalar missing_nratio;
int has_missing_nratio;
NclBasicDataTypes type_nratio;

/* ndivx*/

int ndims_ndivx;
ng_size_t dsizes_ndivx[NCL_MAX_DIMENSIONS];
NclScalar missing_ndivx;
int has_missing_ndivx;
NclBasicDataTypes type_ndivx;

/* ndivy*/

int ndims_ndivy;
ng_size_t dsizes_ndivy[NCL_MAX_DIMENSIONS];
NclScalar missing_ndivy;
int has_missing_ndivy;
NclBasicDataTypes type_ndivy;

/* gridname*/
int ndims_gridname;
ng_size_t dsizes_gridname[NCL_MAX_DIMENSIONS];
NclScalar missing_gridname;
int has_missing_gridname;
NclBasicDataTypes type_gridname;

/* history*/
/*
int ndims_history;
ng_size_t dsizes_history[NCL_MAX_DIMENSIONS];
NclScalar missing_history;
int has_missing_history;
NclBasicDataTypes type_history;
*/
/*
* Retrieve Parameters
*/

nxbnds = (int *)NclGetArgValue(
	0,
	6,
	&ndims_nxbnds,
	dsizes_nxbnds,
	&missing_nxbnds,
	&has_missing_nxbnds,
	&type_nxbnds,
	2);

nlon = (int *)NclGetArgValue(
	1,
	6,
	&ndims_nlon,
	dsizes_nlon,
	&missing_nlon,
	&has_missing_nlon,
	&type_nlon,
	2);

nratio = (int *)NclGetArgValue(
	2,
	6,
	&ndims_nratio,
	dsizes_nratio,
	&missing_nratio,
	&has_missing_nratio,
	&type_nratio,
	2);

ndivx= (int *)NclGetArgValue(
	3,
	6,
	&ndims_ndivx,
	dsizes_ndivx,
	&missing_ndivx,
	&has_missing_ndivx,
	&type_ndivx,
	2);

ndivy= (int *)NclGetArgValue(
	4,
	6,
	&ndims_ndivy,
	dsizes_ndivy,
	&missing_ndivy,
	&has_missing_ndivy,
	&type_ndivy,
	2);

gridname= (string *)NclGetArgValue(
	5,
	6,
	&ndims_gridname,
	dsizes_gridname,
	&missing_gridname,
	&has_missing_gridname,
	&type_gridname,
	2);

char *gridname_charray = NrmQuarkToString(gridname[0]);
if (!strncmp(gridname_charray, "null", strlen(gridname_charray)))
	*gridname_charray = (char)NULL;
/*
history= (string *)NclGetArgValue(
	6,
	7,
	&ndims_history,
	dsizes_history,
	&missing_history,
	&has_missing_history,
	&type_history,
	2);

char *history_charray = NrmQuarkToString(history[0]);
*/
/*
* Call C Code from gridspec: make_tripolar_hgrid()
*/
mpp_init(&argc, &argv);
mpp_domain_init();

nccf_make_conformal_cubic_hgrid(*nxbnds, nlon, *nratio,
			ndivx, ndivy, gridname_charray, history);

return ret;
}
