#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/ncl/defs.h>
#include "Symbol.h"
#include "NclMdInc.h"
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/NclBuiltInSupport.h>
#include <ncarg/gks.h>

#define min(x,y)   ((x) < (y) ? (x) : (y))
#define max(x,y)   ((x) > (y) ? (x) : (y))

extern double *coerce_input_double(void*,NclBasicDataTypes,int,int,
				   NclScalar*,NclScalar*,NclScalar*);

extern float *coerce_output_float(double *, void *, int, int);

extern double *coerce_output_double(void*,NclBasicDataTypes,int);

extern float *coerce_output_float_missing(double*,int,double);

extern void compute_nlatnlon(int *, int, int *, int *, int *, int *, int *);

extern void compute_nlatanlona(int *,int *,int,int,int *,int *,int *,int *,
			       int *,int *,int *,int *,int *);
extern void compute_jlatilon(int *,int,int *,int *,int *,int *,int *,int *,
			     int *, int *, int);

extern int contains_missing(double *,int,int,double);
