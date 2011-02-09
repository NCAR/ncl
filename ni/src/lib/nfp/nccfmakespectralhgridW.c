#include <stdio.h>
#include "wrapper.h"

extern void nccf_make_spectral_hgrid(int nxbnds, int nybnds, int *nlon, 
					int *nlat, int *ndivx, int *ndivy,
					char *gridname, char *history);

NhlErrorTypes nccfmakespectralhgridW(void)
{

NhlErrorTypes ret = NhlNOERROR;

/*
* NCL Input Arguments
*/

string *gridname;
int *nxbnds, *nybnds, *nlon, *nlat, *ndivx, *ndivy;

char *history = "history!!!";

/*
* Input Array Variables
*/

/* nxbnds*/
int ndims_nxbnds;
ng_size_t dsizes_nxbnds[NCL_MAX_DIMENSIONS];
NclScalar missing_nxbnds;
int has_missing_nxbnds;
NclBasicDataTypes type_nxbnds;

/* nybnds*/
int ndims_nybnds;
ng_size_t dsizes_nybnds[NCL_MAX_DIMENSIONS];
NclScalar missing_nybnds;
int has_missing_nybnds;
NclBasicDataTypes type_nybnds;

/* nlon*/
int ndims_nlon;
ng_size_t dsizes_nlon[NCL_MAX_DIMENSIONS];
NclScalar missing_nlon;
int has_missing_nlon;
NclBasicDataTypes type_nlon;

/* nlat*/
int ndims_nlat;
ng_size_t dsizes_nlat[NCL_MAX_DIMENSIONS];
NclScalar missing_nlat;
int has_missing_nlat;
NclBasicDataTypes type_nlat;

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
	7,
	&ndims_nxbnds,
	dsizes_nxbnds,
	&missing_nxbnds,
	&has_missing_nxbnds,
	&type_nxbnds,
	2);

nybnds = (int *)NclGetArgValue(
	1,
	7,
	&ndims_nybnds,
	dsizes_nybnds,
	&missing_nybnds,
	&has_missing_nybnds,
	&type_nybnds,
	2);

nlon = (int *)NclGetArgValue(
	2,
	7,
	&ndims_nlon,
	dsizes_nlon,
	&missing_nlon,
	&has_missing_nlon,
	&type_nlon,
	2);

nlat = (int *)NclGetArgValue(
	3,
	7,
	&ndims_nlat,
	dsizes_nlat,
	&missing_nlat,
	&has_missing_nlat,
	&type_nlat,
	2);

ndivx= (int *)NclGetArgValue(
	4,
	7,
	&ndims_ndivx,
	dsizes_ndivx,
	&missing_ndivx,
	&has_missing_ndivx,
	&type_ndivx,
	2);

ndivy= (int *)NclGetArgValue(
	5,
	7,
	&ndims_ndivy,
	dsizes_ndivy,
	&missing_ndivy,
	&has_missing_ndivy,
	&type_ndivy,
	2);

gridname= (string *)NclGetArgValue(
	6,
	7,
	&ndims_gridname,
	dsizes_gridname,
	&missing_gridname,
	&has_missing_gridname,
	&type_gridname,
	2);

char *gridname_charray = NrmQuarkToString(gridname[0]);
if (!strncmp(gridname_charray, "null", strlen(gridname_charray)))
	*gridname_charray = '\0';

/*
history= (string *)NclGetArgValue(
	7,
	8,
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

nccf_make_spectral_hgrid(*nxbnds, *nybnds, nlon, nlat,
			ndivx, ndivy, gridname_charray, history);
return ret;

}
