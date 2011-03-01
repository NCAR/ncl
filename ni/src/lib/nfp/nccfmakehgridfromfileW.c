#include <stdio.h>
#include "wrapper.h"
#define STRING 255
#include "mpp.h"
#include "mpp_io.h"
#include "mpp_domain.h"

extern void nccf_make_hgrid_from_file(int ntiles_file, int *nlon, 
					int *nlat, char my_grid_file[][STRING], 
					int *ndivx, int *ndivy, char *gridname, char *history);

NhlErrorTypes nccfmakehgridfromfileW(void)
{

NhlErrorTypes ret = NhlNOERROR;

/*
* NCL Input Arguments
*/

string *gridname, *my_grid_file;
int *ntiles_file, *nlon, *nlat, *ndivx, *ndivy;
int argc = 0;
char **argv = NULL;
char *history = "history!!";
/*
* Input Array Variables
*/

/* grid_file*/
int ndims_ntiles_file;
ng_size_t dsizes_ntiles_file[NCL_MAX_DIMENSIONS];
NclScalar missing_ntiles_file;
int has_missing_ntiles_file;
NclBasicDataTypes type_ntiles_file;

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

/* my_grid_file*/
int ndims_my_grid_file;
ng_size_t dsizes_my_grid_file[NCL_MAX_DIMENSIONS];
NclScalar missing_my_grid_file;
int has_missing_my_grid_file;
NclBasicDataTypes type_my_grid_file;

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
/*
history
int ndims_history;
ng_size_t dsizes_history[NCL_MAX_DIMENSIONS];
NclScalar missing_history;
int has_missing_history;
NclBasicDataTypes type_history;
*/
/*
* Retrieve Parameters
*/

ntiles_file = (int *)NclGetArgValue(
	0,
	7,
	&ndims_ntiles_file,
	dsizes_ntiles_file,
	&missing_ntiles_file,
	&has_missing_ntiles_file,
	&type_ntiles_file,
	2);

nlon = (int *)NclGetArgValue(
	1,
	7,
	&ndims_nlon,
	dsizes_nlon,
	&missing_nlon,
	&has_missing_nlon,
	&type_nlon,
	2);

nlat = (int *)NclGetArgValue(
	2,
	7,
	&ndims_nlat,
	dsizes_nlat,
	&missing_nlat,
	&has_missing_nlat,
	&type_nlat,
	2);

my_grid_file= (string *)NclGetArgValue(
	3,
	7,
	&ndims_my_grid_file,
	dsizes_my_grid_file,
	&missing_my_grid_file,
	&has_missing_my_grid_file,
	&type_my_grid_file,
	2);

char my_grid_file_array[dsizes_my_grid_file[0]][STRING];
ng_size_t i;
for (i=0; i<dsizes_my_grid_file[0]; i++)
{
       strncpy(my_grid_file_array[i], NrmQuarkToString(my_grid_file[i]), 254);
       my_grid_file_array[i][254] = '\0';
	if(!strncmp(my_grid_file_array[i], "null", strlen(my_grid_file_array[i])))
		my_grid_file_array[i][0] = 0;
}

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

if(!strncmp(gridname_charray, "null", strlen(gridname_charray)))
	*gridname_charray = (char)NULL;

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

mpp_init(&argc,&argv);
mpp_domain_init();

nccf_make_hgrid_from_file(*ntiles_file, nlon, nlat, my_grid_file_array,
			ndivx, ndivy, gridname_charray, history);

return ret;
}
