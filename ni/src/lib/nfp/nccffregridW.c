#include <stdio.h>
#include "wrapper.h"
/*#include "tool_util.h"*/
#define STRING 255
#include "mpp.h"
#include "mpp_io.h"
#include "mpp_domain.h"

extern void nccf_fregrid(char *history, char *mosaic_in, char *mosaic_out, char *dir_in,
                       char *dir_out, char input_file[][STRING], int nfiles, char output_file[][STRING],
                       int nfiles_out, char *remap_file, char scalar_name[][STRING], int nscalar,
                       char u_name[][STRING], int nvector, char v_name[][STRING], int nvector2,
                       char *interp_method, char *test_case, double test_param,
                       unsigned int opcode, int grid_type, unsigned int finer_step,
                       int fill_missing, int nlon, int nlat, int check_conserve,
                       int y_at_center, double lonbegin, double lonend, double latbegin,
                       double latend, int kbegin, int kend, int lbegin, int lend);

NhlErrorTypes nccffregridW(void)
{

NhlErrorTypes ret = NhlNOERROR;

/*
* NCL Input arguments
*/

string *mosaic_in, *mosaic_out, *dir_in, *dir_out, *remap_file, *interp_method, *test_case;
string *input_file, *output_file, *scalar_name, *u_name, *v_name;
int *nfiles, *nfiles_out, *nscalar, *nvector, *nvector2, *grid_type, *fill_missing, *nlon, *nlat;
int *check_conserve, *y_at_center, *kbegin, *kend, *lbegin, *lend;
unsigned int *opcode, *finer_step;
double *test_param, *lonbegin, *lonend, *latbegin, *latend;
int argc = 0;
char **argv = NULL;
char *history = "history!";
/*
 * Input Array Variables
 */
 /*int ndims_history, dsizes_history[NCL_MAX_DIMENSIONS];*/ 
 int ndims_mosaic_in;
 ng_size_t dsizes_mosaic_in[NCL_MAX_DIMENSIONS];
 int ndims_mosaic_out;
 ng_size_t dsizes_mosaic_out[NCL_MAX_DIMENSIONS];
 int ndims_dir_in;
 ng_size_t dsizes_dir_in[NCL_MAX_DIMENSIONS];
 int ndims_dir_out;
 ng_size_t dsizes_dir_out[NCL_MAX_DIMENSIONS];
 int ndims_input_file;
 ng_size_t dsizes_input_file[NCL_MAX_DIMENSIONS];
 int ndims_nfiles;
 ng_size_t dsizes_nfiles[NCL_MAX_DIMENSIONS];
 int ndims_output_file;
 ng_size_t dsizes_output_file[NCL_MAX_DIMENSIONS];
 int ndims_nfiles_out;
 ng_size_t dsizes_nfiles_out[NCL_MAX_DIMENSIONS];
 int ndims_remap_file;
 ng_size_t dsizes_remap_file[NCL_MAX_DIMENSIONS];
 int ndims_scalar_name;
 ng_size_t dsizes_scalar_name[NCL_MAX_DIMENSIONS];
 int ndims_nscalar;
 ng_size_t dsizes_nscalar[NCL_MAX_DIMENSIONS];
 int ndims_u_name;
 ng_size_t dsizes_u_name[NCL_MAX_DIMENSIONS];
 int ndims_nvector;
 ng_size_t dsizes_nvector[NCL_MAX_DIMENSIONS];
 int ndims_v_name;
 ng_size_t dsizes_v_name[NCL_MAX_DIMENSIONS];
 int ndims_nvector2;
 ng_size_t dsizes_nvector2[NCL_MAX_DIMENSIONS];
 int ndims_interp_method;
 ng_size_t dsizes_interp_method[NCL_MAX_DIMENSIONS];
 int ndims_test_case;
 ng_size_t dsizes_test_case[NCL_MAX_DIMENSIONS];
 int ndims_test_param;
 ng_size_t dsizes_test_param[NCL_MAX_DIMENSIONS];
 int ndims_opcode;
 ng_size_t dsizes_opcode[NCL_MAX_DIMENSIONS];
 int ndims_grid_type;
 ng_size_t dsizes_grid_type[NCL_MAX_DIMENSIONS];
 int ndims_finer_step;
 ng_size_t dsizes_finer_step[NCL_MAX_DIMENSIONS];
 int ndims_fill_missing;
 ng_size_t dsizes_fill_missing[NCL_MAX_DIMENSIONS];
 int ndims_nlon;
 ng_size_t dsizes_nlon[NCL_MAX_DIMENSIONS];
 int ndims_nlat;
 ng_size_t dsizes_nlat[NCL_MAX_DIMENSIONS];
 int ndims_check_conserve;
 ng_size_t dsizes_check_conserve[NCL_MAX_DIMENSIONS];
 int ndims_y_at_center;
 ng_size_t dsizes_y_at_center[NCL_MAX_DIMENSIONS];
 int ndims_lonbegin;
 ng_size_t dsizes_lonbegin[NCL_MAX_DIMENSIONS];
 int ndims_lonend;
 ng_size_t dsizes_lonend[NCL_MAX_DIMENSIONS];
 int ndims_latbegin;
 ng_size_t dsizes_latbegin[NCL_MAX_DIMENSIONS];
 int ndims_latend;
 ng_size_t dsizes_latend[NCL_MAX_DIMENSIONS];
 int ndims_kbegin;
 ng_size_t dsizes_kbegin[NCL_MAX_DIMENSIONS];
 int ndims_kend;
 ng_size_t dsizes_kend[NCL_MAX_DIMENSIONS];
 int ndims_lbegin;
 ng_size_t dsizes_lbegin[NCL_MAX_DIMENSIONS];
 int ndims_lend;
 ng_size_t dsizes_lend[NCL_MAX_DIMENSIONS];
 
 NclScalar missing_mosaic_in, missing_mosaic_out, missing_dir_in;
 NclScalar missing_dir_out, missing_input_file, missing_nfiles, missing_output_file;
 NclScalar missing_nfiles_out, missing_remap_file, missing_scalar_name, missing_nscalar;
 NclScalar missing_u_name, missing_nvector, missing_v_name, missing_nvector2;
 NclScalar missing_interp_method, missing_test_case, missing_test_param, missing_opcode;
 NclScalar missing_grid_type, missing_finer_step, missing_fill_missing, missing_nlon;
 NclScalar missing_nlat, missing_check_conserve, missing_y_at_center, missing_lonbegin;
 NclScalar missing_lonend, missing_latbegin, missing_latend, missing_kbegin;
 NclScalar missing_kend, missing_lbegin, missing_lend;
 
 int has_missing_mosaic_in, has_missing_mosaic_out, has_missing_dir_in;
 int has_missing_dir_out, has_missing_input_file, has_missing_nfiles, has_missing_output_file;
 int has_missing_nfiles_out, has_missing_remap_file, has_missing_scalar_name, has_missing_nscalar;
 int has_missing_u_name, has_missing_nvector, has_missing_v_name, has_missing_nvector2;
 int has_missing_interp_method, has_missing_test_case, has_missing_test_param, has_missing_opcode;
 int has_missing_grid_type, has_missing_finer_step, has_missing_fill_missing, has_missing_nlon;
 int has_missing_nlat, has_missing_check_conserve, has_missing_y_at_center, has_missing_lonbegin;
 int has_missing_lonend, has_missing_latbegin, has_missing_latend, has_missing_kbegin;
 int has_missing_kend, has_missing_lbegin, has_missing_lend;
 
 NclBasicDataTypes type_mosaic_in, type_mosaic_out, type_dir_in;
 NclBasicDataTypes type_dir_out, type_input_file, type_nfiles, type_output_file;
 NclBasicDataTypes type_nfiles_out, type_remap_file, type_scalar_name, type_nscalar;
 NclBasicDataTypes type_u_name, type_nvector, type_v_name, type_nvector2;
 NclBasicDataTypes type_interp_method, type_test_case, type_test_param, type_opcode;
 NclBasicDataTypes type_grid_type, type_finer_step, type_fill_missing, type_nlon;
 NclBasicDataTypes type_nlat, type_check_conserve, type_y_at_center, type_lonbegin;
 NclBasicDataTypes type_lonend, type_latbegin, type_latend, type_kbegin;
 NclBasicDataTypes type_kend, type_lbegin, type_lend;
 
/*
 * Retrieve parameters
 */

/*
 history = (string *)NclGetArgValue(
	0,
	35,
	&ndims_history,
	dsizes_history,
	&missing_history,
	&has_missing_history,
	&type_history,
	2);

 * convert quark to a string to pass into c function
 

 char *history_charray = NrmQuarkToString(history[0]);
 */
 mosaic_in = (string *)NclGetArgValue(
	0,
	34,
	&ndims_mosaic_in,
	dsizes_mosaic_in,
	&missing_mosaic_in,
	&has_missing_mosaic_in,
	&type_mosaic_in,
	2);

 /*
 * convert quark to a string to pass into c function
 */
 char *mosaic_in_charray = NrmQuarkToString(mosaic_in[0]);

 if (!strncmp(mosaic_in_charray, "null", strlen(mosaic_in_charray)))
	*mosaic_in_charray = '\0';

 mosaic_out = (string *)NclGetArgValue(
	1,
	34,
	&ndims_mosaic_out,
	dsizes_mosaic_out, 
	&missing_mosaic_out,
	&has_missing_mosaic_out,
	&type_mosaic_out,
	2); 
 
 /*
 * convert quark to a string to pass into c function
 */
 char *mosaic_out_charray = NrmQuarkToString(mosaic_out[0]);

 if (!strncmp(mosaic_out_charray, "null", strlen(mosaic_out_charray)))
	*mosaic_out_charray = '\0';

 dir_in = (string *)NclGetArgValue(
	2,
	34,
	&ndims_dir_in,
	dsizes_dir_in,
	&missing_dir_in,
	&has_missing_dir_in,
	&type_dir_in,
	2);

 char *dir_in_charray = NrmQuarkToString(dir_in[0]);

 if (!strncmp(dir_in_charray, "null", strlen(dir_in_charray)))
	*dir_in_charray = '\0';

 dir_out = (string *)NclGetArgValue(
	3,
	34,
	&ndims_dir_out,
	dsizes_dir_out,
	&missing_dir_out,
	&has_missing_dir_out,
	&type_dir_out,
	2); 

 char *dir_out_charray = NrmQuarkToString(dir_out[0]);

 if (!strncmp(dir_out_charray, "null", strlen(dir_out_charray)))
	*dir_out_charray = '\0';
 
 input_file = (string *)NclGetArgValue(
	4,
	34,
	&ndims_input_file,
	dsizes_input_file,
	&missing_input_file,
	&has_missing_input_file,
	&type_input_file,
	2); 

 /*
 * Create an array of strings to pass into c function
 */
 char input_file_array[dsizes_input_file[0]][STRING];
 ng_size_t i; 

 for (i=0; i<dsizes_input_file[0]; i++)
 {
	strncpy(input_file_array[i], NrmQuarkToString(input_file[i]), 254);
	input_file_array[i][254] = '\0';
        if (!strncmp(input_file_array[i], "null", strlen(input_file_array[i])))
		input_file_array[i][0] = 0;
 }

 nfiles = (int *)NclGetArgValue(
	5,
	34,
	&ndims_nfiles,
	dsizes_nfiles,
	&missing_nfiles,
	&has_missing_nfiles,
	&type_nfiles,
	2); 

 output_file = (string *)NclGetArgValue(
	6,
	34,
	&ndims_output_file,
	dsizes_output_file,
	&missing_output_file,
	&has_missing_output_file,
	&type_output_file,
	2); 

 /*
 * Create an array of strings to pass into c function
 */
 char output_file_array[dsizes_output_file[0]][STRING];
 for (i=0; i<dsizes_output_file[0]; i++)
 {
	strncpy(output_file_array[i], NrmQuarkToString(output_file[i]), 254);
	output_file_array[i][254] = '\0'; 
        if (!strncmp(output_file_array[i], "null", strlen(output_file_array[i])))
		output_file_array[i][0] = 0;
 }

 nfiles_out = (int *)NclGetArgValue(
	7,
	34,
	&ndims_nfiles_out,
	dsizes_nfiles_out,
	&missing_nfiles_out,
	&has_missing_nfiles_out,
	&type_nfiles_out,
	2); 

 remap_file = (string *)NclGetArgValue(
	8,
	34,
	&ndims_remap_file,
	dsizes_remap_file,
	&missing_remap_file,
	&has_missing_remap_file,
	&type_remap_file,
	2); 

 /*
 * convert quark to a string to pass into c function
 */
 char *remap_file_charray = NrmQuarkToString(remap_file[0]);

 if (!strncmp(remap_file_charray, "null", strlen(remap_file_charray)))
	*remap_file_charray = '\0';

 scalar_name = (string *)NclGetArgValue(
	9,
	34,
	&ndims_scalar_name,
	dsizes_scalar_name,
	&missing_scalar_name,
	&has_missing_scalar_name,
	&type_scalar_name,
	2); 

 /*
 * Create an array of strings to pass into c function
 */
 char scalar_name_array[dsizes_scalar_name[0]][STRING];
 for (i=0; i<dsizes_scalar_name[0]; i++)
 {
	strncpy(scalar_name_array[i], NrmQuarkToString(scalar_name[i]), 254);
	scalar_name_array[i][254] = '\0'; 
        if (!strncmp(scalar_name_array[i], "null", strlen(scalar_name_array[i])))
		scalar_name_array[i][0] = 0;
 }

 nscalar = (int *)NclGetArgValue(
	10,
	34,
	&ndims_nscalar,
	dsizes_nscalar,
	&missing_nscalar,
	&has_missing_nscalar,
	&type_nscalar,
	2); 

 u_name = (string *)NclGetArgValue(
	11,
	34,
	&ndims_u_name,
	dsizes_u_name,
	&missing_u_name,
	&has_missing_u_name,
	&type_u_name,
	2); 

 /*
 * Create an array of strings to pass into c function
 */
 char u_name_array[dsizes_u_name[0]][STRING];
 for (i=0; i<dsizes_u_name[0]; i++)
 {
	strncpy(u_name_array[i], NrmQuarkToString(u_name[i]), 254);
	u_name_array[i][254] = '\0'; 
        if (!strncmp(u_name_array[i], "null", strlen(u_name_array[i])))
		u_name_array[i][0] = 0;
 }

 nvector = (int *)NclGetArgValue(
	12,
	34,
	&ndims_nvector,
	dsizes_nvector,
	&missing_nvector,
	&has_missing_nvector,
	&type_nvector,
	2); 

 v_name = (string *)NclGetArgValue(
	13,
	34,
	&ndims_v_name,
	dsizes_v_name,
	&missing_v_name,
	&has_missing_v_name,
	&type_v_name,
	2); 

 /*
 * Create an array of strings to pass into c function
 */
 char v_name_array[dsizes_v_name[0]][STRING];
 for (i=0; i<dsizes_v_name[0]; i++)
 {
	strncpy(v_name_array[i], NrmQuarkToString(v_name[i]), 254);
	v_name_array[i][254] = '\0'; 
        if (!strncmp(v_name_array[i], "null", strlen(v_name_array[i])))
		v_name_array[i][0] = 0;
 }

 nvector2 = (int *)NclGetArgValue(
	14,
	34,
	&ndims_nvector2,
	dsizes_nvector2,
	&missing_nvector2,
	&has_missing_nvector2,
	&type_nvector2,
	2); 
 
 interp_method = (string *)NclGetArgValue(
	15,
	34,
	&ndims_interp_method,
	dsizes_interp_method,
	&missing_interp_method,
	&has_missing_interp_method,
	&type_interp_method,
	2);

 /*
 * convert quark to a string to pass into c function
 */
 char *interp_method_charray = NrmQuarkToString(interp_method[0]);

 if (!strncmp(interp_method_charray, "null", strlen(interp_method_charray)))
	*interp_method_charray = '\0';

 test_case = (string *)NclGetArgValue(
	16,
	34,
	&ndims_test_case,
	dsizes_test_case, 
	&missing_test_case,
	&has_missing_test_case,
	&type_test_case,
	2);

 char *test_case_charray = NrmQuarkToString(test_case[0]);
 
 if (!strncmp(test_case_charray, "null", strlen(test_case_charray)))
	*test_case_charray = '\0';
	
 test_param = (double *)NclGetArgValue(
	17,
	34,
	&ndims_test_param,
	dsizes_test_param,
	&missing_test_param,
	&has_missing_test_param,
	&type_test_param,
	2);

 opcode = (unsigned int *)NclGetArgValue(
	18,
	34,
	&ndims_opcode,
	dsizes_opcode,
	&missing_opcode,
	&has_missing_opcode,
	&type_opcode,
	2);

 grid_type = (int *)NclGetArgValue(
	19,
	34,
	&ndims_grid_type,
	dsizes_grid_type,
	&missing_grid_type,
	&has_missing_grid_type,
	&type_grid_type,
	2);

 finer_step = (unsigned int *)NclGetArgValue(
	20,
	34,
	&ndims_finer_step,
	dsizes_finer_step,
	&missing_finer_step,
	&has_missing_finer_step,
	&type_finer_step,
	2);

 fill_missing = (int *)NclGetArgValue(
	21,
	34,
	&ndims_fill_missing,
	dsizes_fill_missing,
	&missing_fill_missing,
	&has_missing_fill_missing,
	&type_fill_missing,
	2);

 nlon = (int *)NclGetArgValue(
	22,
	34,
	&ndims_nlon,
	dsizes_nlon,
	&missing_nlon,
	&has_missing_nlon,
	&type_nlon,
	2);

 nlat = (int *)NclGetArgValue(
	23,
	34,
	&ndims_nlat,
	dsizes_nlat,
	&missing_nlat,
	&has_missing_nlat,
	&type_nlat,
	2);

 check_conserve = (int *)NclGetArgValue(
	24,
	34,
	&ndims_check_conserve,
	dsizes_check_conserve,
	&missing_check_conserve,
	&has_missing_check_conserve,
	&type_check_conserve,
	2);

 y_at_center = (int *)NclGetArgValue(
	25,
	34,
	&ndims_y_at_center,
	dsizes_y_at_center,
	&missing_y_at_center,
	&has_missing_y_at_center,
	&type_y_at_center,
	2);

 lonbegin = (double *)NclGetArgValue(
	26,
	34,
	&ndims_lonbegin,
	dsizes_lonbegin,
	&missing_lonbegin,
	&has_missing_lonbegin,
	&type_lonbegin,
	2);

 lonend = (double *)NclGetArgValue(
	27,
	34,
	&ndims_lonend,
	dsizes_lonend,
	&missing_lonend,
	&has_missing_lonend,
	&type_lonend,
	2);

 latbegin = (double *)NclGetArgValue(
	28,
	34,
	&ndims_latbegin,
	dsizes_latbegin,
	&missing_latbegin,
	&has_missing_latbegin,
	&type_latbegin,
	2);

 latend = (double *)NclGetArgValue(
	29,
	34,
	&ndims_latend,
	dsizes_latend,
	&missing_latend,
	&has_missing_latend,
	&type_latend,
	2);

 kbegin = (int *)NclGetArgValue(
	30,
	34,
	&ndims_kbegin,
	dsizes_kbegin,
	&missing_kbegin,
	&has_missing_kbegin,
	&type_kbegin,
	2);

 kend = (int *)NclGetArgValue(
	31,
	34,
	&ndims_kend,
	dsizes_kend,
	&missing_kend,
	&has_missing_kend,
	&type_kend,
	2);

 lbegin = (int *)NclGetArgValue(
	32,
	34,
	&ndims_lbegin,
	dsizes_lbegin,
	&missing_lbegin,
	&has_missing_lbegin,
	&type_lbegin,
	2);

 lend = (int *)NclGetArgValue(
	33,
	34,
	&ndims_lend,
	dsizes_lend,
	&missing_lend,
	&has_missing_lend,
	&type_lend,
	2);
/*
* Call C function from gridspec: fregrid()
*/

mpp_init(&argc, &argv);
mpp_domain_init();
nccf_fregrid(history, mosaic_in_charray, mosaic_out_charray, dir_in_charray, dir_out_charray,
		input_file_array, *nfiles, output_file_array, *nfiles_out, remap_file_charray,
		scalar_name_array, *nscalar, u_name_array, *nvector, v_name_array, *nvector2,
		interp_method_charray, test_case_charray, *test_param, *opcode, *grid_type,
		*finer_step, *fill_missing, *nlon, *nlat, *check_conserve, *y_at_center, *lonbegin, 
		*lonend, *latbegin, *latend, *kbegin, *kend, *lbegin, *lend);

return ret;
}
