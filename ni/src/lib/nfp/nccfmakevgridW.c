#include <stdio.h>
#include "wrapper.h"

extern void nccf_make_vgrid(char *history, int nbnds, double *bnds, int n1,
			    int n2, int *nz, char *gridname, char *center);

NhlErrorTypes nccfmakevgridW(void)
{

NhlErrorTypes ret = NhlNOERROR;

/*
* NCL Input Arguments
*/

string *gridname, *center;
int *nbnds, n1, n2, *nz;
double *bnds;

char *history = "history!!!";

/*
* Input Array Variables
*/

/* history */
/*
int ndims_history;
ng_size_t dsizes_history[NCL_MAX_DIMENSIONS];
NclScalar missing_history;
int has_missing_history;
NclBasicDataTypes type_history;
*/

/* nbnds */
int ndims_nbnds;
ng_size_t dsizes_nbnds[NCL_MAX_DIMENSIONS];
NclScalar missing_nbnds;
int has_missing_nbnds;
NclBasicDataTypes type_nbnds; 

/* bnds */
int ndims_bnds;
ng_size_t dsizes_bnds[NCL_MAX_DIMENSIONS];
NclScalar missing_bnds;
int has_missing_bnds;
NclBasicDataTypes type_bnds; 


/* nz */
int ndims_nz;
ng_size_t dsizes_nz[NCL_MAX_DIMENSIONS];
NclScalar missing_nz;
int has_missing_nz;
NclBasicDataTypes type_nz;

/* gridname */
int ndims_gridname;
ng_size_t dsizes_gridname[NCL_MAX_DIMENSIONS];
NclScalar missing_gridname;
int has_missing_gridname;
NclBasicDataTypes type_gridname;

/* center */
int ndims_center;
ng_size_t dsizes_center[NCL_MAX_DIMENSIONS];
NclScalar missing_center;
int has_missing_center;
NclBasicDataTypes type_center;

/*
* Retrieve Parameters
*/
/*
history = (string *)NclGetArgValue(
	0,
	8,
	&ndims_history,
	dsizes_history,
	&missing_history,
	&has_missing_history,
	&type_history,
	2);
*/

/*char *history_charray = NrmQuarkToString(history[0]);*/

nbnds = (int *)NclGetArgValue(
	0,
	5,
	&ndims_nbnds,
	dsizes_nbnds,
	&missing_nbnds,
	&has_missing_nbnds,
	&type_nbnds,
	2);

bnds = (double *)NclGetArgValue(
	1,
	5,
	&ndims_bnds,
	dsizes_bnds,
	&missing_bnds,
	&has_missing_bnds,
	&type_bnds,
	2);

/*
n1 = (int *)NclGetArgValue(
	3,
	8,
	&ndims_n1,
	dsizes_n1,
	&missing_n1,
	&has_missing_n1,
	&type_n1,
	2);
*/

n1 = dsizes_bnds[0];

/*
n2 = (int *)NclGetArgValue(
	4,
	8,
	&ndims_n2,
	dsizes_n2,
	&missing_n2,
	&has_missing_n2,
	&type_n2,
	2);
*/

nz = (int *)NclGetArgValue(
	2,
	5,
	&ndims_nz,
	dsizes_nz,
	&missing_nz,
	&has_missing_nz,
	&type_nz,
	2);
n2 = dsizes_nz[0];

gridname = (string *)NclGetArgValue(
	3,
	5,
	&ndims_gridname,
	dsizes_gridname,
	&missing_gridname,
	&has_missing_gridname,
	&type_gridname,
	2);

char *gridname_charray = NrmQuarkToString(gridname[0]);
if (!strncmp(gridname_charray, "null", strlen(gridname_charray)))
	*gridname_charray = '\0';

center = (string *)NclGetArgValue(
	4,
	5,
	&ndims_center,
	dsizes_center,
	&missing_center,
	&has_missing_center,
	&type_center,
	2);

char *center_charray = NrmQuarkToString(center[0]);

if (!strncmp(center_charray, "null", strlen(center_charray)))
	*center_charray = (char)NULL;

/*
* Call C Function from gridspec: make_vgrid()
*/

nccf_make_vgrid(history, *nbnds, bnds, n1, n2, nz, gridname_charray, center_charray);

return ret;

} 
