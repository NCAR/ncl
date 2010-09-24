#include <stdio.h>
#include "wrapper.h"
#include "mpp.h"
#include "mpp_domain.h"
#include "mpp_io.h"

extern void nccf_make_topog(char *history, char *mosaic_file, char *topog_type,
			int x_refine, int y_refine, double basin_depth,
			char *topog_file, char *topog_field, double bottom_depth,
			double min_depth, double scale_factor, int num_filter_pass,
			double gauss_amp, double gauss_scale, double slope_x,
			double slope_y, double bowl_south, double bowl_north,
			double bowl_west, double bowl_east, int fill_first_row,
			int filter_topog, int round_shallow, int fill_shallow,
			int deepen_shallow, int smooth_topo_allow_deepening,
			char *topog_mosaic);

NhlErrorTypes nccfmaketopogW(void)
{

NhlErrorTypes ret = NhlNOERROR;

string *mosaic_file, *topog_type, *topog_file, *topog_field, *topog_mosaic;
int *x_refine, *y_refine, *num_filter_pass, *fill_first_row, *filter_topog, *round_shallow;
int *fill_shallow, *deepen_shallow, *smooth_topo_allow_deepening;
double *basin_depth, *bottom_depth, *min_depth, *scale_factor, *gauss_amp;
double *gauss_scale, *slope_x, *slope_y, *bowl_south, *bowl_north, *bowl_west;
double *bowl_east;
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
/* mosaic_file*/
int ndims_mosaic_file;
ng_size_t dsizes_mosaic_file[NCL_MAX_DIMENSIONS];
NclScalar missing_mosaic_file;
int has_missing_mosaic_file;
NclBasicDataTypes type_mosaic_file;

/* topog_type*/
int ndims_topog_type;
ng_size_t dsizes_topog_type[NCL_MAX_DIMENSIONS];
NclScalar missing_topog_type;
int has_missing_topog_type;
NclBasicDataTypes type_topog_type;

/* x_refine*/
int ndims_x_refine;
ng_size_t dsizes_x_refine[NCL_MAX_DIMENSIONS];
NclScalar missing_x_refine;
int has_missing_x_refine;
NclBasicDataTypes type_x_refine;

/* y_refine*/
int ndims_y_refine;
ng_size_t dsizes_y_refine[NCL_MAX_DIMENSIONS];
NclScalar missing_y_refine;
int has_missing_y_refine;
NclBasicDataTypes type_y_refine;

/* basin_depth*/
int ndims_basin_depth;
ng_size_t dsizes_basin_depth[NCL_MAX_DIMENSIONS];
NclScalar missing_basin_depth;
int has_missing_basin_depth;
NclBasicDataTypes type_basin_depth;

/* topog_file*/
int ndims_topog_file;
ng_size_t dsizes_topog_file[NCL_MAX_DIMENSIONS];
NclScalar missing_topog_file;
int has_missing_topog_file;
NclBasicDataTypes type_topog_file;

/* topog_field*/
int ndims_topog_field;
ng_size_t dsizes_topog_field[NCL_MAX_DIMENSIONS];
NclScalar missing_topog_field;
int has_missing_topog_field;
NclBasicDataTypes type_topog_field;

/* bottom_depth*/
int ndims_bottom_depth;
ng_size_t dsizes_bottom_depth[NCL_MAX_DIMENSIONS];
NclScalar missing_bottom_depth;
int has_missing_bottom_depth;
NclBasicDataTypes type_bottom_depth;

/* min_depth*/
int ndims_min_depth;
ng_size_t dsizes_min_depth[NCL_MAX_DIMENSIONS];
NclScalar missing_min_depth;
int has_missing_min_depth;
NclBasicDataTypes type_min_depth;

/* scale_factor*/
int ndims_scale_factor;
ng_size_t dsizes_scale_factor[NCL_MAX_DIMENSIONS];
NclScalar missing_scale_factor;
int has_missing_scale_factor;
NclBasicDataTypes type_scale_factor;

/* num_filter_pass*/
int ndims_num_filter_pass;
ng_size_t dsizes_num_filter_pass[NCL_MAX_DIMENSIONS];
NclScalar missing_num_filter_pass;
int has_missing_num_filter_pass;
NclBasicDataTypes type_num_filter_pass;

/* gauss_amp*/
int ndims_gauss_amp;
ng_size_t dsizes_gauss_amp[NCL_MAX_DIMENSIONS];
NclScalar missing_gauss_amp;
int has_missing_gauss_amp;
NclBasicDataTypes type_gauss_amp;

/* gauss_scale*/
int ndims_gauss_scale;
ng_size_t dsizes_gauss_scale[NCL_MAX_DIMENSIONS];
NclScalar missing_gauss_scale;
int has_missing_gauss_scale;
NclBasicDataTypes type_gauss_scale;

/* slope_x*/
int ndims_slope_x;
ng_size_t dsizes_slope_x[NCL_MAX_DIMENSIONS];
NclScalar missing_slope_x;
int has_missing_slope_x;
NclBasicDataTypes type_slope_x;

/* slope_y*/
int ndims_slope_y;
ng_size_t dsizes_slope_y[NCL_MAX_DIMENSIONS];
NclScalar missing_slope_y;
int has_missing_slope_y;
NclBasicDataTypes type_slope_y;

/* bowl_south*/
int ndims_bowl_south;
ng_size_t dsizes_bowl_south[NCL_MAX_DIMENSIONS];
NclScalar missing_bowl_south;
int has_missing_bowl_south;
NclBasicDataTypes type_bowl_south;

/* bowl_north*/
int ndims_bowl_north;
ng_size_t dsizes_bowl_north[NCL_MAX_DIMENSIONS];
NclScalar missing_bowl_north;
int has_missing_bowl_north;
NclBasicDataTypes type_bowl_north;

/* bowl_west*/
int ndims_bowl_west;
ng_size_t dsizes_bowl_west[NCL_MAX_DIMENSIONS];
NclScalar missing_bowl_west;
int has_missing_bowl_west;
NclBasicDataTypes type_bowl_west;

/* bowl_east*/
int ndims_bowl_east;
ng_size_t dsizes_bowl_east[NCL_MAX_DIMENSIONS];
NclScalar missing_bowl_east;
int has_missing_bowl_east;
NclBasicDataTypes type_bowl_east;

/* fill_first_row*/
int ndims_fill_first_row;
ng_size_t dsizes_fill_first_row[NCL_MAX_DIMENSIONS];
NclScalar missing_fill_first_row;
int has_missing_fill_first_row;
NclBasicDataTypes type_fill_first_row;

/* filter_topog*/
int ndims_filter_topog;
ng_size_t dsizes_filter_topog[NCL_MAX_DIMENSIONS];
NclScalar missing_filter_topog;
int has_missing_filter_topog;
NclBasicDataTypes type_filter_topog;

/* round_shallow*/
int ndims_round_shallow;
ng_size_t dsizes_round_shallow[NCL_MAX_DIMENSIONS];
NclScalar missing_round_shallow;
int has_missing_round_shallow;
NclBasicDataTypes type_round_shallow;

/* fill_shallow*/
int ndims_fill_shallow;
ng_size_t dsizes_fill_shallow[NCL_MAX_DIMENSIONS];
NclScalar missing_fill_shallow;
int has_missing_fill_shallow;
NclBasicDataTypes type_fill_shallow;

/* deepen_shallow*/
int ndims_deepen_shallow;
ng_size_t dsizes_deepen_shallow[NCL_MAX_DIMENSIONS];
NclScalar missing_deepen_shallow;
int has_missing_deepen_shallow;
NclBasicDataTypes type_deepen_shallow;

/* shallow_topo_allow_deepening*/
int ndims_smooth_topo_allow_deepening;
ng_size_t dsizes_smooth_topo_allow_deepening[NCL_MAX_DIMENSIONS];
NclScalar missing_smooth_topo_allow_deepening;
int has_missing_smooth_topo_allow_deepening;
NclBasicDataTypes type_smooth_topo_allow_deepening;

/* topog_mosaic*/
int ndims_topog_mosaic;
ng_size_t dsizes_topog_mosaic[NCL_MAX_DIMENSIONS];
NclScalar missing_topog_mosaic;
int has_missing_topog_mosaic;
NclBasicDataTypes type_topog_mosaic;

/* Retrieve Parameters */
/*
history = (string *) NclGetArgValue(
	0,
	27,
	&ndims_history,
	dsizes_history,
	&missing_history,
	&has_missing_history,
	&type_history,
	2);

char *history_charray = NrmQuarkToString(history[0]);
*/
mosaic_file= (string *) NclGetArgValue(
	0,
	26,
	&ndims_mosaic_file,
	dsizes_mosaic_file,
	&missing_mosaic_file,
	&has_missing_mosaic_file,
	&type_mosaic_file,
	2);

char *mosaic_file_charray = NrmQuarkToString(mosaic_file[0]);
if (!strncmp(mosaic_file_charray, "null", strlen(mosaic_file_charray)))
	*mosaic_file_charray = '\0';

topog_type= (string *) NclGetArgValue(
	1,
	26,
	&ndims_topog_type,
	dsizes_topog_type,
	&missing_topog_type,
	&has_missing_topog_type,
	&type_topog_type,
	2);

char *topog_type_charray = NrmQuarkToString(topog_type[0]);

if (!strncmp(topog_type_charray, "null", strlen(topog_type_charray)))
	*topog_type_charray = '\0';

x_refine= (int *) NclGetArgValue(
	2,
	26,
	&ndims_x_refine,
	dsizes_x_refine,
	&missing_x_refine,
	&has_missing_x_refine,
	&type_x_refine,
	2);

y_refine= (int *) NclGetArgValue(
	3,
	26,
	&ndims_y_refine,
	dsizes_y_refine,
	&missing_y_refine,
	&has_missing_y_refine,
	&type_y_refine,
	2);

basin_depth= (double *) NclGetArgValue(
	4,
	26,
	&ndims_basin_depth,
	dsizes_basin_depth,
	&missing_basin_depth,
	&has_missing_basin_depth,
	&type_basin_depth,
	2);

topog_file= (string *) NclGetArgValue(
	5,
	26,
	&ndims_topog_file,
	dsizes_topog_file,
	&missing_topog_file,
	&has_missing_topog_file,
	&type_topog_file,
	2);

char *topog_file_charray = NrmQuarkToString(topog_file[0]);

if (!strncmp(topog_file_charray, "null", strlen(topog_file_charray)))
	*topog_file_charray = (char)NULL;

topog_field= (string *) NclGetArgValue(
	6,
	26,
	&ndims_topog_field,
	dsizes_topog_field,
	&missing_topog_field,
	&has_missing_topog_field,
	&type_topog_field,
	2);

char *topog_field_charray = NrmQuarkToString(topog_field[0]);

if (!strncmp(topog_field_charray, "null", strlen(topog_field_charray)))
	*topog_field_charray = '\0';

bottom_depth= (double *) NclGetArgValue(
	7,
	26,
	&ndims_bottom_depth,
	dsizes_bottom_depth,
	&missing_bottom_depth,
	&has_missing_bottom_depth,
	&type_bottom_depth,
	2);

min_depth= (double *) NclGetArgValue(
	8,
	26,
	&ndims_min_depth,
	dsizes_min_depth,
	&missing_min_depth,
	&has_missing_min_depth,
	&type_min_depth,
	2);

scale_factor= (double *) NclGetArgValue(
	9,
	26,
	&ndims_scale_factor,
	dsizes_scale_factor,
	&missing_scale_factor,
	&has_missing_scale_factor,
	&type_scale_factor,
	2);

num_filter_pass= (int *) NclGetArgValue(
	10,
	26,
	&ndims_num_filter_pass,
	dsizes_num_filter_pass,
	&missing_num_filter_pass,
	&has_missing_num_filter_pass,
	&type_num_filter_pass,
	2);

gauss_amp= (double *) NclGetArgValue(
	11,
	26,
	&ndims_gauss_amp,
	dsizes_gauss_amp,
	&missing_gauss_amp,
	&has_missing_gauss_amp,
	&type_gauss_amp,
	2);

gauss_scale= (double *) NclGetArgValue(
	12,
	26,
	&ndims_gauss_scale,
	dsizes_gauss_scale,
	&missing_gauss_scale,
	&has_missing_gauss_scale,
	&type_gauss_scale,
	2);

slope_x= (double *) NclGetArgValue(
	13,
	26,
	&ndims_slope_x,
	dsizes_slope_x,
	&missing_slope_x,
	&has_missing_slope_x,
	&type_slope_x,
	2);

slope_y= (double *) NclGetArgValue(
	14,
	26,
	&ndims_slope_y,
	dsizes_slope_y,
	&missing_slope_y,
	&has_missing_slope_y,
	&type_slope_y,
	2);

bowl_south= (double *) NclGetArgValue(
	15,
	26,
	&ndims_bowl_south,
	dsizes_bowl_south,
	&missing_bowl_south,
	&has_missing_bowl_south,
	&type_bowl_south,
	2);

bowl_north= (double *) NclGetArgValue(
	16,
	26,
	&ndims_bowl_north,
	dsizes_bowl_north,
	&missing_bowl_north,
	&has_missing_bowl_north,
	&type_bowl_north,
	2);

bowl_west= (double *) NclGetArgValue(
	17,
	26,
	&ndims_bowl_west,
	dsizes_bowl_west,
	&missing_bowl_west,
	&has_missing_bowl_west,
	&type_bowl_west,
	2);

bowl_east= (double *) NclGetArgValue(
	18,
	26,
	&ndims_bowl_east,
	dsizes_bowl_east,
	&missing_bowl_east,
	&has_missing_bowl_east,
	&type_bowl_east,
	2);

fill_first_row= (int *)NclGetArgValue(
	19,
	26,
	&ndims_fill_first_row,
	dsizes_fill_first_row,
	&missing_fill_first_row,
	&has_missing_fill_first_row,
	&type_fill_first_row,
	2);

filter_topog= (int *)NclGetArgValue(
	20,
	26,
	&ndims_filter_topog,
	dsizes_filter_topog,
	&missing_filter_topog,
	&has_missing_filter_topog,
	&type_filter_topog,
	2);

round_shallow= (int *)NclGetArgValue(
	21,
	26,
	&ndims_round_shallow,
	dsizes_round_shallow,
	&missing_round_shallow,
	&has_missing_round_shallow,
	&type_round_shallow,
	2);

fill_shallow= (int *)NclGetArgValue(
	22,
	26,
	&ndims_fill_shallow,
	dsizes_fill_shallow,
	&missing_fill_shallow,
	&has_missing_fill_shallow,
	&type_fill_shallow,
	2);

deepen_shallow= (int *)NclGetArgValue(
	23,
	26,
	&ndims_deepen_shallow,
	dsizes_deepen_shallow,
	&missing_deepen_shallow,
	&has_missing_deepen_shallow,
	&type_deepen_shallow,
	2);

smooth_topo_allow_deepening= (int *)NclGetArgValue(
	24,
	26,
	&ndims_smooth_topo_allow_deepening,
	dsizes_smooth_topo_allow_deepening,
	&missing_smooth_topo_allow_deepening,
	&has_missing_smooth_topo_allow_deepening,
	&type_smooth_topo_allow_deepening,
	2);

topog_mosaic= (string *)NclGetArgValue(
	25,
	26,
	&ndims_topog_mosaic,
	dsizes_topog_mosaic,
	&missing_topog_mosaic,
	&has_missing_topog_mosaic,
	&type_topog_mosaic,
	2);

char *topog_mosaic_charray = NrmQuarkToString(topog_mosaic[0]);

if (!strncmp(topog_mosaic_charray, "null", strlen(topog_mosaic_charray)))
	*topog_mosaic_charray = '\0';
/*
* call C function from gridspec: make_topog()
*/

int argc = 0;
char **argv=NULL; 
mpp_init(&argc, &argv);
mpp_domain_init();

nccf_make_topog(history, mosaic_file_charray, topog_type_charray,
		*x_refine, *y_refine, *basin_depth, topog_file_charray,
		topog_field_charray, *bottom_depth, *min_depth, *scale_factor,
		*num_filter_pass, *gauss_amp, *gauss_scale, *slope_x, *slope_y,
		*bowl_south, *bowl_north, *bowl_west,
		*bowl_east, *fill_first_row, *filter_topog, *round_shallow,
		*fill_shallow, *deepen_shallow, *smooth_topo_allow_deepening,
		topog_mosaic_charray); 

return ret;

}
