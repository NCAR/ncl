#include <stdio.h>
#include "wrapper.h"
#define STRING 255

extern void nccf_make_mosaic(char *history, int ntile, char *mosaic_name, int nmosaic,
			     char mosaicfile[][STRING], char *grid_descriptor, int n_tilefile,
			     char tilefile[][STRING], double periodx, double periody, 
			     int generate_contact, char *tile_dir);

NhlErrorTypes nccfmakemosaicW(void)
{

NhlErrorTypes ret = NhlNOERROR;

/*
* NCL Input Arguments
*/

string *mosaic_name, *mosaicfile, *grid_descriptor, *tilefile, *tile_dir;
int *ntile, *n_tilefile, *generate_contact, *nmosaic;
double *periodx, *periody;

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
/* ntile */
int ndims_ntile;
ng_size_t dsizes_ntile[NCL_MAX_DIMENSIONS];
NclScalar missing_ntile;
int has_missing_ntile;
NclBasicDataTypes type_ntile;

/* mosaic_name */
int ndims_mosaic_name;
ng_size_t dsizes_mosaic_name[NCL_MAX_DIMENSIONS];
NclScalar missing_mosaic_name;
int has_missing_mosaic_name;
NclBasicDataTypes type_mosaic_name;

/* nmosaic */
int ndims_nmosaic;
ng_size_t dsizes_nmosaic[NCL_MAX_DIMENSIONS];
NclScalar missing_nmosaic;
int has_missing_nmosaic;
NclBasicDataTypes type_nmosaic;

/* mosaicfile */
int ndims_mosaicfile;
ng_size_t dsizes_mosaicfile[NCL_MAX_DIMENSIONS];
NclScalar missing_mosaicfile;
int has_missing_mosaicfile;
NclBasicDataTypes type_mosaicfile;

/* grid_descriptor */
int ndims_grid_descriptor;
ng_size_t dsizes_grid_descriptor[NCL_MAX_DIMENSIONS];
NclScalar missing_grid_descriptor;
int has_missing_grid_descriptor;
NclBasicDataTypes type_grid_descriptor;

/* n_tilefile */
int ndims_n_tilefile;
ng_size_t dsizes_n_tilefile[NCL_MAX_DIMENSIONS];
NclScalar missing_n_tilefile;
int has_missing_n_tilefile;
NclBasicDataTypes type_n_tilefile;

/* tilefile */
int ndims_tilefile;
ng_size_t dsizes_tilefile[NCL_MAX_DIMENSIONS];
NclScalar missing_tilefile;
int has_missing_tilefile;
NclBasicDataTypes type_tilefile;

/* periodx */
int ndims_periodx;
ng_size_t dsizes_periodx[NCL_MAX_DIMENSIONS];
NclScalar missing_periodx;
int has_missing_periodx;
NclBasicDataTypes type_periodx;

/* periody */
int ndims_periody;
ng_size_t dsizes_periody[NCL_MAX_DIMENSIONS];
NclScalar missing_periody;
int has_missing_periody;
NclBasicDataTypes type_periody;

/* generate_contact */
int ndims_generate_contact;
ng_size_t dsizes_generate_contact[NCL_MAX_DIMENSIONS];
NclScalar missing_generate_contact;
int has_missing_generate_contact;
NclBasicDataTypes type_generate_contact;

/* tile_dir */
int ndims_tile_dir;
ng_size_t dsizes_tile_dir[NCL_MAX_DIMENSIONS];
NclScalar missing_tile_dir;
int has_missing_tile_dir;
NclBasicDataTypes type_tile_dir;

/*
* Retrieve Parameters
*/
/*
history = (string *)NclGetArgValue(
	0,
	12,
	&ndims_history,
	dsizes_history,
	&missing_history,
	&has_missing_history,
	&type_history,
	2);


char *history_charray = NrmQuarkToString(history[0]);
*/
ntile = (int *)NclGetArgValue(
	0,
	11,
	&ndims_ntile,
	dsizes_ntile,
	&missing_ntile,
	&has_missing_ntile,
	&type_ntile,
	2);

mosaic_name = (string *)NclGetArgValue(
	1,
	11,
	&ndims_mosaic_name,
	dsizes_mosaic_name,
	&missing_mosaic_name,
	&has_missing_mosaic_name,
	&type_mosaic_name,
	2);

/* Convert quark to a string to pass into c function */

char *mosaic_name_charray = NrmQuarkToString(mosaic_name[0]);
if (!strncmp(mosaic_name_charray, "null", strlen(mosaic_name_charray)))
	*mosaic_name_charray = '\0';

nmosaic = (int *)NclGetArgValue(
	2,
	11,
	&ndims_nmosaic,
	dsizes_nmosaic,
	&missing_nmosaic,
	&has_missing_nmosaic,
	&type_nmosaic,
	2);
 
mosaicfile = (string *)NclGetArgValue(
	3,
	11,
	&ndims_mosaicfile,
	dsizes_mosaicfile,
	&missing_mosaicfile,
	&has_missing_mosaicfile,
	&type_mosaicfile,
	2);
 
/*
* Create an array of strings to pass into c function
*/
char mosaicfile_array[dsizes_mosaicfile[0]][STRING];
ng_size_t i;
for (i=0; i<dsizes_mosaicfile[0]; i++)
{
	strncpy(mosaicfile_array[i], NrmQuarkToString(mosaicfile[i]), 254);
	mosaicfile_array[i][254] = '\0';
	if(!strncmp(mosaicfile_array[i], "null", strlen(mosaicfile_array[i])))
		mosaicfile_array[i][0] = 0;
}

grid_descriptor= (string *)NclGetArgValue(
	4,
	11,
	&ndims_grid_descriptor,
	dsizes_grid_descriptor,
	&missing_grid_descriptor,
	&has_missing_grid_descriptor,
	&type_grid_descriptor,
	2);

/* Convert quark to a string to pass into c function */

char *grid_descriptor_charray = NrmQuarkToString(grid_descriptor[0]);

if (!strncmp(grid_descriptor_charray, "null", strlen(grid_descriptor_charray)))
	*grid_descriptor_charray = '\0';

n_tilefile= (int *)NclGetArgValue(
	5,
	11,
	&ndims_n_tilefile,
	dsizes_n_tilefile,
	&missing_n_tilefile,
	&has_missing_n_tilefile,
	&type_n_tilefile,
	2);

tilefile = (string *)NclGetArgValue(
	6,
	11,
	&ndims_tilefile,
	dsizes_tilefile,
	&missing_tilefile,
	&has_missing_tilefile,
	&type_tilefile,
	2);
 
/*
* Create an array of strings to pass into c function
*/
char tilefile_array[dsizes_tilefile[0]][STRING];
for (i=0; i<dsizes_tilefile[0]; i++)
{
	strncpy(tilefile_array[i], NrmQuarkToString(tilefile[i]), 254);
	tilefile_array[i][254] = '\0';
	if(!strncmp(tilefile_array[i], "null", strlen(tilefile_array[i])))
		tilefile_array[i][0] = 0;
}

periodx= (double *)NclGetArgValue(
	7,
	11,
	&ndims_periodx,
	dsizes_periodx,
	&missing_periodx,
	&has_missing_periodx,
	&type_periodx,
	2);

periody= (double *)NclGetArgValue(
	8,
	11,
	&ndims_periody,
	dsizes_periody,
	&missing_periody,
	&has_missing_periody,
	&type_periody,
	2);

generate_contact= (int *)NclGetArgValue(
	9,
	11,
	&ndims_generate_contact,
	dsizes_generate_contact,
	&missing_generate_contact,
	&has_missing_generate_contact,
	&type_generate_contact,
	2);

tile_dir= (string *)NclGetArgValue(
	10,
	11,
	&ndims_tile_dir,
	dsizes_tile_dir,
	&missing_tile_dir,
	&has_missing_tile_dir,
	&type_tile_dir,
	2);

/* Convert quark to a string to pass into c function */

char *tile_dir_charray = NrmQuarkToString(tile_dir[0]);

if (!strncmp(tile_dir_charray, "null", strlen(tile_dir_charray)))
	*tile_dir_charray = '\0';

nccf_make_mosaic(history, *ntile, mosaic_name_charray, *nmosaic, mosaicfile_array,
		 grid_descriptor_charray, *n_tilefile, tilefile_array, *periodx, *periody,
		 *generate_contact, tile_dir_charray);
return ret;

}
