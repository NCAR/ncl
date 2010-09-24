#include <stdio.h>
#include "wrapper.h"

extern void nccf_make_simple_cartesian_hgrid(int nxbnds, int nybnds, double *xbnds, double *ybnds, int *nlon, 
					int *nlat, double simple_dx, double simple_dy, int *ndivx,
					int *ndivy, char *gridname, char *history);

NhlErrorTypes nccfmakesimplecartesianhgridW(void)
{

NhlErrorTypes ret = NhlNOERROR;

/*
* NCL Input Arguments
*/

string *gridname;
int *nxbnds, *nybnds, *nlon, *nlat, *ndivx, *ndivy;
double *xbnds, *ybnds, *simple_dx, *simple_dy;

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

/* xbnds*/
int ndims_xbnds;
ng_size_t dsizes_xbnds[NCL_MAX_DIMENSIONS];
NclScalar missing_xbnds;
int has_missing_xbnds;
NclBasicDataTypes type_xbnds;

/* ybnds*/
int ndims_ybnds;
ng_size_t dsizes_ybnds[NCL_MAX_DIMENSIONS];
NclScalar missing_ybnds;
int has_missing_ybnds;
NclBasicDataTypes type_ybnds;

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

/* simple_dx*/
int ndims_simple_dx;
ng_size_t dsizes_simple_dx[NCL_MAX_DIMENSIONS];
NclScalar missing_simple_dx;
int has_missing_simple_dx;
NclBasicDataTypes type_simple_dx;

/* simple_dy*/
int ndims_simple_dy;
ng_size_t dsizes_simple_dy[NCL_MAX_DIMENSIONS];
NclScalar missing_simple_dy;
int has_missing_simple_dy;
NclBasicDataTypes type_simple_dy;

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
	11,
	&ndims_nxbnds,
	dsizes_nxbnds,
	&missing_nxbnds,
	&has_missing_nxbnds,
	&type_nxbnds,
	2);

nybnds = (int *)NclGetArgValue(
	1,
	11,
	&ndims_nybnds,
	dsizes_nybnds,
	&missing_nybnds,
	&has_missing_nybnds,
	&type_nybnds,
	2);

xbnds = (double *)NclGetArgValue(
	2,
	11,
	&ndims_xbnds,
	dsizes_xbnds,
	&missing_xbnds,
	&has_missing_xbnds,
	&type_xbnds,
	2);

ybnds = (double *)NclGetArgValue(
	3,
	11,
	&ndims_ybnds,
	dsizes_ybnds,
	&missing_ybnds,
	&has_missing_ybnds,
	&type_ybnds,
	2);

nlon = (int *)NclGetArgValue(
	4,
	11,
	&ndims_nlon,
	dsizes_nlon,
	&missing_nlon,
	&has_missing_nlon,
	&type_nlon,
	2);

nlat = (int *)NclGetArgValue(
	5,
	11,
	&ndims_nlat,
	dsizes_nlat,
	&missing_nlat,
	&has_missing_nlat,
	&type_nlat,
	2);

simple_dx= (double *)NclGetArgValue(
	6,
	11,
	&ndims_simple_dx,
	dsizes_simple_dx,
	&missing_simple_dx,
	&has_missing_simple_dx,
	&type_simple_dx,
	2);

simple_dy= (double *)NclGetArgValue(
	7,
	11,
	&ndims_simple_dy,
	dsizes_simple_dy,
	&missing_simple_dy,
	&has_missing_simple_dy,
	&type_simple_dy,
	2);

ndivx= (int *)NclGetArgValue(
	8,
	11,
	&ndims_ndivx,
	dsizes_ndivx,
	&missing_ndivx,
	&has_missing_ndivx,
	&type_ndivx,
	2);

ndivy= (int *)NclGetArgValue(
	9,
	11,
	&ndims_ndivy,
	dsizes_ndivy,
	&missing_ndivy,
	&has_missing_ndivy,
	&type_ndivy,
	2);

gridname= (string *)NclGetArgValue(
	10,
	11,
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
	11,
	12,
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

nccf_make_simple_cartesian_hgrid(*nxbnds, *nybnds, xbnds, ybnds, nlon, nlat, *simple_dx, *simple_dy,
			ndivx, ndivy, gridname_charray, history);

return ret;

}
