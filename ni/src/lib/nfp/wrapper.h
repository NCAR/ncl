#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/ncl/defs.h>
#include "Symbol.h"
#include "NclMdInc.h"
#include "Machine.h"
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/NclBuiltInSupport.h>
#include <ncarg/gks.h>

#define min(x,y)   ((x) < (y) ? (x) : (y))
#define max(x,y)   ((x) > (y) ? (x) : (y))

extern void coerce_missing(NclBasicDataTypes,int,NclScalar *,
                           NclScalar *,NclScalar *);

extern double *coerce_input_double(void*,NclBasicDataTypes,int,int,
                                   NclScalar*,NclScalar*);

extern void coerce_subset_input_double(void *,double *,int,NclBasicDataTypes,
                                       int,int,NclScalar*,NclScalar*);

extern double *copy_scalar_to_array(double *, int, int *, int);

extern float *coerce_output_float(double *, void *, int, int);

extern void *coerce_output_float_only(void *,double *,int, int);

extern void *coerce_output_int_only(void *,double *,int, int);

extern void *coerce_output_float_or_double(void *,double *,
					   NclBasicDataTypes,int,int);

extern float *coerce_input_float(void*,NclBasicDataTypes,int,int,
                                   NclScalar*,NclScalar*);

extern void coerce_subset_input_float(void *,float *,int,NclBasicDataTypes,
                                       int,int,NclScalar*,NclScalar*);

extern double *coerce_output_double(void*,NclBasicDataTypes,int);

extern int contains_missing(double *,int,int,double);
extern int contains_missing_float(float *,int,int,float);

extern void set_subset_output_missing(void *,int,NclBasicDataTypes,int,double);

extern int is_scalar(int,int*);


extern void compute_nlatnlon(int *, int, int *, int *, int *, int *, int *);

extern void compute_nlatanlona(int *,int *,int,int,int *,int *,int *,int *,
                               int *,int *,int *,int *,int *);

extern void print_minmax(void *,int,NclBasicDataTypes);


