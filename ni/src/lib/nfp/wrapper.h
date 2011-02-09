#include <ncarg/ncargC.h>
#include <ncarg/gks.h>
#include <ncarg/ngmath.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/ncl/defs.h>
#include "Symbol.h"
#include "NclDataDefs.h"
#include "Machine.h"
#include "NclMdInc.h"
#include "TypeSupport.h"
#include "NclData.h"
#include "AttSupport.h"
#include "NclAtt.h"
#include "NclCoordVar.h"
#include "NclVar.h"
#include "VarSupport.h"
#include "DataSupport.h"
#include "NclBuiltInSupport.h"
#include "NclBuiltIns.h"
#include "NclHLUObj.h"

#define min(x,y)   ((x) < (y) ? (x) : (y))
#define max(x,y)   ((x) > (y) ? (x) : (y))

extern void coerce_missing(NclBasicDataTypes,int,NclScalar *,
                           NclScalar *,NclScalar *);

extern double *coerce_input_double(void*,NclBasicDataTypes,ng_size_t,int,
                                   NclScalar*,NclScalar*);

extern void coerce_subset_input_double(void *,double *,ng_size_t,NclBasicDataTypes,
                                       ng_size_t,int,NclScalar*,NclScalar*);

extern void coerce_subset_input_double_step(void *,double *,ng_size_t,ng_size_t,
					    NclBasicDataTypes,ng_size_t,int,
					    NclScalar*,NclScalar*);

extern double *copy_scalar_to_array(double *, int, ng_size_t *, ng_size_t);

extern float *coerce_output_float(double *, void *, ng_size_t, int);

extern void coerce_output_float_only(void *,double *,ng_size_t, ng_size_t);

extern void coerce_output_int_only(void *,double *,ng_size_t, ng_size_t);

extern void coerce_output_float_or_double(void *,double *,
					   NclBasicDataTypes,ng_size_t,ng_size_t);

extern void coerce_output_float_or_double_step(void *,double *,
					       NclBasicDataTypes,ng_size_t,ng_size_t,ng_size_t);

extern float *coerce_input_float(void*,NclBasicDataTypes,ng_size_t,int,
                                   NclScalar*,NclScalar*);

extern void coerce_subset_input_float(void *,float *,ng_size_t,NclBasicDataTypes,
                                       ng_size_t,int,NclScalar*,NclScalar*);

extern double *coerce_output_double(void*,NclBasicDataTypes,ng_size_t);

extern int contains_missing(double *,ng_size_t,int,double);
extern int contains_missing_float(float *,ng_size_t,int,float);

extern void set_subset_output_missing(void *,ng_size_t,NclBasicDataTypes,ng_size_t,
				      double);
extern void set_subset_output_missing_step(void *,ng_size_t,ng_size_t,NclBasicDataTypes,
					   ng_size_t,double);

extern int is_scalar(int,ng_size_t*);


extern void compute_nlatnlon(ng_size_t *, int, ng_size_t *, ng_size_t *,
                             ng_size_t *, ng_size_t *, ng_size_t *);

extern void compute_nlatanlona(ng_size_t *,ng_size_t *,int,int,
                               ng_size_t *,ng_size_t *,ng_size_t *,ng_size_t *,
                               ng_size_t *,ng_size_t *,ng_size_t *,ng_size_t *,ng_size_t *);

extern void print_minmax(void *,ng_size_t,NclBasicDataTypes);


extern NclDimRec *get_dim_info(int, int);

extern ng_size_t *get_dimensions(void *tmp_dimensions,ng_size_t n_dimensions,
				 NclBasicDataTypes type_dimensions,
				 const char *);
