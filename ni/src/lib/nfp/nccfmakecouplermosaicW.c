#include <stdio.h>
#include "wrapper.h"
#include "mpp.h"
#include "mpp_domain.h"
#include "mpp_io.h"

extern void nccf_make_coupler_mosaic(char *history, char *amosaic, char *lmosaic,
				char *omosaic, char *imosaic, char *itopog, int interp_order,
				double sea_level, char *mosaic_name, int check);

NhlErrorTypes nccfmakecouplermosaicW(void)
{

NhlErrorTypes ret = NhlNOERROR;

/*
* NCL Input Argumenta
*/

string *amosaic, *lmosaic, *omosaic, *imosaic, *itopog, *mosaic_name;
int *interp_order, *check;
double *sea_level;
int argc = 0;
char **argv = NULL;
char *history = "history!";
/*
* Input Array Variables
*/
/*
 history
int ndims_history;
ng_size_t dsizes_history[NCL_MAX_DIMENSIONS];
NclScalar missing_history;
int has_missing_history;
NclBasicDataTypes type_history;
*/
/* amosaic*/
int ndims_amosaic;
ng_size_t dsizes_amosaic[NCL_MAX_DIMENSIONS];
NclScalar missing_amosaic;
int has_missing_amosaic;
NclBasicDataTypes type_amosaic;

/* lmosaic*/
int ndims_lmosaic;
ng_size_t dsizes_lmosaic[NCL_MAX_DIMENSIONS];
NclScalar missing_lmosaic;
int has_missing_lmosaic;
NclBasicDataTypes type_lmosaic;

/* omosaic*/
int ndims_omosaic;
ng_size_t dsizes_omosaic[NCL_MAX_DIMENSIONS];
NclScalar missing_omosaic;
int has_missing_omosaic;
NclBasicDataTypes type_omosaic;

/* imosaic*/
int ndims_imosaic;
ng_size_t dsizes_imosaic[NCL_MAX_DIMENSIONS];
NclScalar missing_imosaic;
int has_missing_imosaic;
NclBasicDataTypes type_imosaic;

/* itopog*/
int ndims_itopog;
ng_size_t dsizes_itopog[NCL_MAX_DIMENSIONS];
NclScalar missing_itopog;
int has_missing_itopog;
NclBasicDataTypes type_itopog;

/* interp_order*/
int ndims_interp_order;
ng_size_t dsizes_interp_order[NCL_MAX_DIMENSIONS];
NclScalar missing_interp_order;
int has_missing_interp_order;
NclBasicDataTypes type_interp_order;

/* sea_level*/
int ndims_sea_level;
ng_size_t dsizes_sea_level[NCL_MAX_DIMENSIONS];
NclScalar missing_sea_level;
int has_missing_sea_level;
NclBasicDataTypes type_sea_level;

/* mosaic_name*/
int ndims_mosaic_name;
ng_size_t dsizes_mosaic_name[NCL_MAX_DIMENSIONS];
NclScalar missing_mosaic_name;
int has_missing_mosaic_name;
NclBasicDataTypes type_mosaic_name;

/* check*/
int ndims_check;
ng_size_t dsizes_check[NCL_MAX_DIMENSIONS];
NclScalar missing_check;
int has_missing_check;
NclBasicDataTypes type_check;

/*
* Retrieve Parameters
*/
/*
history = (string *)NclGetArgValue(
        0,
        10,
        &ndims_history,
        dsizes_history,
        &missing_history,
        &has_missing_history,
        &type_history,
        2);

char *history_charray = NrmQuarkToString(history[0]);
*/
amosaic= (string *)NclGetArgValue(
        0,
        9,
        &ndims_amosaic,
        dsizes_amosaic,
        &missing_amosaic,
        &has_missing_amosaic,
        &type_amosaic,
        2);

char *amosaic_charray = NrmQuarkToString(amosaic[0]);
if (!strncmp(amosaic_charray, "null", strlen(amosaic_charray)))
	*amosaic_charray = '\0';

lmosaic= (string *)NclGetArgValue(
        1,
        9,
        &ndims_lmosaic,
        dsizes_lmosaic,
        &missing_lmosaic,
        &has_missing_lmosaic,
        &type_lmosaic,
        2);

char *lmosaic_charray = NrmQuarkToString(lmosaic[0]);

if (!strncmp(lmosaic_charray, "null", strlen(lmosaic_charray)))
	*lmosaic_charray = '\0';

omosaic= (string *)NclGetArgValue(
        2,
        9,
        &ndims_omosaic,
        dsizes_omosaic,
        &missing_omosaic,
        &has_missing_omosaic,
        &type_omosaic,
        2);

char *omosaic_charray = NrmQuarkToString(omosaic[0]);

if (!strncmp(omosaic_charray, "null", strlen(omosaic_charray)))
	*omosaic_charray = '\0';

imosaic= (string *)NclGetArgValue(
        3,
        9,
        &ndims_imosaic,
        dsizes_imosaic,
        &missing_imosaic,
        &has_missing_imosaic,
        &type_imosaic,
        2);

char *imosaic_charray = NrmQuarkToString(imosaic[0]);

if (!strncmp(imosaic_charray, "null", strlen(imosaic_charray)))
	*imosaic_charray = '\0';

itopog= (string *)NclGetArgValue(
        4,
        9,
        &ndims_itopog,
        dsizes_itopog,
        &missing_itopog,
        &has_missing_itopog,
        &type_itopog,
        2);

char *itopog_charray = NrmQuarkToString(itopog[0]);

if (!strncmp(itopog_charray, "null", strlen(itopog_charray)))
	*itopog_charray = '\0';

interp_order= (int *)NclGetArgValue(
        5,
        9,
        &ndims_interp_order,
        dsizes_interp_order,
        &missing_interp_order,
        &has_missing_interp_order,
        &type_interp_order,
        2);

sea_level= (double *)NclGetArgValue(
        6,
        9,
        &ndims_sea_level,
        dsizes_sea_level,
        &missing_sea_level,
        &has_missing_sea_level,
        &type_sea_level,
        2);

mosaic_name= (string *)NclGetArgValue(
        7,
        9,
        &ndims_mosaic_name,
        dsizes_mosaic_name,
        &missing_mosaic_name,
        &has_missing_mosaic_name,
        &type_mosaic_name,
        2);

char *mosaic_name_charray = NrmQuarkToString(mosaic_name[0]);

if (!strncmp(mosaic_name_charray, "null", strlen(mosaic_name_charray)))
	*mosaic_name_charray = '\0';

check= (int *)NclGetArgValue(
        8,
        9,
        &ndims_check,
        dsizes_check,
        &missing_check,
        &has_missing_check,
        &type_check,
        2);

/*
* call C function from gridspec: make_coupler_mosaic()
*/

mpp_init(&argc, &argv);
mpp_domain_init();

nccf_make_coupler_mosaic(history, amosaic_charray, lmosaic_charray, omosaic_charray,
			imosaic_charray, itopog_charray, *interp_order, *sea_level,
			mosaic_name_charray, *check);
return ret;
}
