/*
 *      $Id: openclAddFuncs.c,v 1.30 2010-02-07 01:34:26 haley Exp $
 */
/************************************************************************
*                                                                       *
*                   Copyright (C)  2009                                 *
*           University Corporation for Atmospheric Research             *
*                   All Rights Reserved                                 *
*                                                                       *
************************************************************************/
/*
 *    File:		openclAddFuncs.c
 *
 *    Author:		Wei Huang
 *    		National Center for Atmospheric Research
 *    		PO 3000, Boulder, Colorado
 *
 *    Date:		Fri Aug 31 10:07:50 MDT 2012
 *
 *    Description:	
 */
#ifdef __cplusplus
extern "C" {
#endif

#include <OpenCL/opencl.h>

#include <stdio.h>
#include <stdlib.h>
#include <ncarg/c.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/Error.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/PlotManager.h>
#include <ncarg/hlu/Workstation.h>
#include <ncarg/hlu/Workspace.h>
#include <ncarg/hlu/Callbacks.h>
#include <ncarg/ncargC.h>
#include <ncarg/c.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <math.h>
#include <limits.h>
#include <float.h>

#include "defs.h"
#include <errno.h>
#include "Symbol.h"
#include "NclDataDefs.h"
#include "Machine.h"
#include "NclFile.h"
#include "NclVar.h"
#include "NclCoordVar.h"
#include "VarSupport.h"
#include "DataSupport.h"
#include "NclMdInc.h"
#include "NclHLUObj.h"
#include "parser.h"
#include "OpsList.h"
#include "ApiRecords.h"
#include "TypeSupport.h"
#include "NclBuiltInSupport.h"
#include "FileSupport.h"
#include "NclAtt.h"
#include "NclList.h"
#include "ListSupport.h"
#include "NclFileInterfaces.h"
#include <signal.h>
#include <regex.h>
#include <ctype.h>

#define FAILURE    	-1
#define SUCCESS    	0
#define MAXOPENCLDIM    3

extern int *get_dims_for_n_funcs(int arg_num,  int num_args,
                                 NclStackEntry tmpdata,
                                 const char *name, int *ndims);

const char *C_row_priv_KernelSource = "\n" \
"__kernel void mmul(                                        \n" \
"   const int Mdim,                                         \n" \
"   const int Ndim,                                         \n" \
"   const int Pdim,                                         \n" \
"   __global float* A,                                      \n" \
"   __global float* B,                                      \n" \
"   __global float* C)                                      \n" \
"{                                                          \n" \
"   int k,j;                                                \n" \
"   int i = get_global_id(0);                               \n" \
"   float Awrk[1000];                                       \n" \
"   float tmp;                                              \n" \
"   if( (i < Ndim) )                                        \n" \
"   {                                                       \n" \
"       for(k=0;k<Pdim;k++)                                 \n" \
"          Awrk[k] = A[i*Ndim+k];                           \n" \
"                                                           \n" \
"       for(j=0;j<Mdim;j++){                                \n" \
"          tmp = 0.0;                                       \n" \
"          for(k=0;k<Pdim;k++)                              \n" \
"              tmp += Awrk[k] * B[k*Pdim+j];                \n" \
"          C[i*Ndim+j] = tmp;                               \n" \
"       }                                                   \n" \
"   }                                                       \n" \
"}                                                          \n" \
"\n";

const char *C_general_dimavg_KernelSource = "\n" \
"__kernel void dimavg(                                       \n" \
"   const int nr,                                            \n" \
"   const int m,                                             \n" \
"   __global float* vin,                                     \n" \
"   __global float* vout)                                    \n" \
"{                                                           \n" \
"   int k;                                                   \n" \
"   int i = get_global_id(0);                                \n" \
"   int j = get_global_id(1);                                \n" \
"   float sum_val;                                           \n" \
"   sum_val = 0.0;                                           \n" \
"   for(k = 0; k < m; ++k)                                   \n" \
"       sum_val += vin[i*(nr*m)+(k*nr)+j];                   \n" \
"   vout[i*nr+j] = sum_val / (float)m;                       \n" \
"}                                                           \n" \
"\n";

const char *C_lastdim_dimavg_KernelSource = "\n" \
"__kernel void dimavg(                                       \n" \
"   const int m,                                             \n" \
"   __global float* vin,                                     \n" \
"   __global float* vout)                                    \n" \
"{                                                           \n" \
"   int k;                                                   \n" \
"   int i = get_global_id(0);                                \n" \
"   float sum_val;                                           \n" \
"   sum_val = 0.0;                                           \n" \
"   for(k = 0; k < m; ++k)                                   \n" \
"       sum_val += vin[i + k];                               \n" \
"   vout[i] = sum_val / (float)m;                            \n" \
"}                                                           \n" \
"\n";

NhlErrorTypes _Ncldim_avg_n_cl(void)
{
    NclStackEntry data;
    NhlErrorTypes ret = NhlNOERROR;
    NclMultiDValData tmp_md = NULL;
    void *out_val = NULL;
    int *dims;
    int ndims;
    float *val = NULL;
    ng_size_t *dimsizes = NULL;
    ng_size_t i;
    ng_size_t m,n,nr,nl;
    int nd;
    NclBasicDataTypes data_type;
    NclBasicDataTypes out_data_type;
    NclScalar missing;

    int              err;               /* error code from OpenCL */
    size_t           global[DIM];       /* global domain size */
    cl_device_id     device_id;         /* compute device id */
    cl_context       context;           /* compute context */
    cl_command_queue commands;          /* compute command queue */
    cl_program       program;           /* compute program */
    cl_kernel        kernel;            /* compute kernel */
    cl_uint          ndrange;           /* Number of dims in NDRange */
    cl_mem           v_in;              /* Memory object for in matrix */
    cl_mem           v_out;             /* Memory Object for out matrix */

    cl_event prof_event;
    cl_ulong ev_start_time = (cl_ulong)0;
    cl_ulong ev_end_time   = (cl_ulong)0;
    cl_ulong run_time      = (cl_ulong)0;

    cl_uint        numPlatforms;
    cl_platform_id firstPlatformId;

    cl_context_properties properties [] = { CL_CONTEXT_PLATFORM, 0, 0, };

/*
 *-------------------------------------------------------------------
 * Set up the OpenCL platform using whichever platform is "first"
 *-------------------------------------------------------------------
 */
    err = clGetPlatformIDs(1, &firstPlatformId, &numPlatforms);

    err = clGetDeviceIDs(firstPlatformId, CL_DEVICE_TYPE_GPU, 1,
                         &device_id, NULL);

    printf("firstPlatformId = %ld\n", (long)firstPlatformId);

    properties[1] = (cl_context_properties)firstPlatformId;

    context = clCreateContext(properties, 1, &device_id, NULL, NULL, &err);

    commands = clCreateCommandQueue(context, device_id,
                                    CL_QUEUE_PROFILING_ENABLE, &err);

/*
 * Read data values off stack (or not)
 */
    data = _NclGetArg(0,2,DONT_CARE);
    switch(data.kind) {
    case NclStk_VAR:
    	tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
    	break;
    case NclStk_VAL:
    	tmp_md = (NclMultiDValData)data.u.data_obj;
    	break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
        }
    if(tmp_md == NULL)
    	return(NhlFATAL);

/*
 * Get dimension(s) to do average across. These can be dimension 
 * indexes or dimension names.
 */
    dims = get_dims_for_n_funcs(1,2,data,"dim_avg_n",&ndims);
    if(dims == NULL) { 
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_avg_n: Invalid input dimensions specified");
      return(NhlFATAL);
    }
/*
 * Some error checking. Make sure input dimensions are valid.
 */
    for(i = 0; i < ndims; i++ ) {
      if(dims[i] < 0 || dims[i] >= tmp_md->multidval.n_dims) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_avg_n: Invalid dimension sizes to do average across, can't continue");
        return(NhlFATAL);
      }
      if(i > 0 && dims[i] != (dims[i-1]+1)) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_avg_n: Input dimension sizes must be monotonically increasing, can't continue");
        return(NhlFATAL);
      }
    }
/*
 * Calculate size of leftmost dimensions (nl) up to the dims[0]-th
 *   dimensions.
 * Calculate size of rightmost dimensions (nr) from the
 *   ndims[ndims-1]-th dimension
 *
 * The dimension(s) to do the average across are "dims".
 */
    nl = nr = m = 1;
    if(tmp_md->multidval.n_dims > 1) {
      nd       = tmp_md->multidval.n_dims-ndims;
      dimsizes = NclMalloc(nd * sizeof(ng_size_t));
      for(i = 0; i < dims[0] ; i++) {
        nl = nl*tmp_md->multidval.dim_sizes[i];
        dimsizes[i] = tmp_md->multidval.dim_sizes[i];
      }
      for(i = 0; i < ndims ; i++) {
        m = m*tmp_md->multidval.dim_sizes[dims[i]];
      }
      for(i = dims[ndims-1]+1; i < tmp_md->multidval.n_dims; i++) {
        nr = nr*tmp_md->multidval.dim_sizes[i];
        dimsizes[i-ndims] = tmp_md->multidval.dim_sizes[i];
      }
    } else {
      dimsizes = NclMalloc(sizeof(ng_size_t));
      *dimsizes = 1;
      nd = 1;
      m  = tmp_md->multidval.dim_sizes[dims[0]];
    }
    n = nr * nl;

    printf("nl = %d, nr = %d, m = %d\n", nl, nr, m);

/*
 * Determine output type, which will either be float or double.
 */
    data_type = NCL_double;
    if(tmp_md->multidval.data_type == NCL_double)
    {
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,"dim_avg_n_cl: Could not handle double, can't continue"));
        return(NhlFATAL);
    }
    else
    {
        out_val = (void*)NclMalloc(sizeof(float)* n);
        out_data_type = NCL_float;
        if(tmp_md->multidval.missing_value.has_missing)
        {
            missing = tmp_md->multidval.missing_value.value;
        }
    }

    val = (float*)tmp_md->multidval.val;

  /*
   *if(tmp_md->multidval.missing_value.has_missing)
   *{
   *    NHLPERROR((NhlFATAL,NhlEUNKNOWN,"dim_avg_n_cl: Could not handle data with missing value, can't continue"));
   *    return(NhlFATAL);
   *}
   */

  /*
   *-------------------------------------------------------------------
   * Set up the buffers, initialize matrices, and write them
   * into global memory
   *-------------------------------------------------------------------
   */
    v_in  = clCreateBuffer(context,  CL_MEM_READ_ONLY,
                           sizeof(float) * n * m, NULL, NULL);
    v_out = clCreateBuffer(context,  CL_MEM_WRITE_ONLY,
                           sizeof(float) * n, NULL, NULL);
  
    if(1 < nr)
    {
        printf("\nTest Program: C_general_dimavg_KernelSource\n\n");
  
      /*Create the compute program from the source buffer*/
        program = clCreateProgramWithSource(context, 1,
                                            (const char **) & C_general_dimavg_KernelSource,
                                            NULL, &err);
    }
    else
    {
        printf("\nTest Program: C_lastdim_dimavg_KernelSource\n\n");
 
      /*Create the compute program from the source buffer*/
        program = clCreateProgramWithSource(context, 1,
                                            (const char **) & C_lastdim_dimavg_KernelSource,
                                            NULL, &err);
    }

  /*Build the program*/
    err = clBuildProgram(program, 0, NULL, NULL, NULL, NULL);
    if(err != CL_SUCCESS)
    {
        size_t len;
        char buffer[2048];

        printf("Error: Failed to build program executable!\n");
        clGetProgramBuildInfo(program, device_id,
                            CL_PROGRAM_BUILD_LOG,
                            sizeof(buffer), buffer, &len);
        printf("%s\n", buffer);
        return FAILURE;
    }

  /*Create the compute kernel from the program*/
    kernel = clCreateKernel(program, "dimavg", &err);

  /*Set the arguments to our compute kernel*/
    err  = 0;

    if(1 < nr)
    {
        err |= clSetKernelArg(kernel, 0, sizeof(int),     &nr);
        err |= clSetKernelArg(kernel, 1, sizeof(int),     &m);

        err |= clSetKernelArg(kernel, 2, sizeof(cl_mem), &v_in);
        err |= clSetKernelArg(kernel, 3, sizeof(cl_mem), &v_out);
    }
    else
    {
        err |= clSetKernelArg(kernel, 0, sizeof(int),     &m);

        err |= clSetKernelArg(kernel, 1, sizeof(cl_mem), &v_in);
        err |= clSetKernelArg(kernel, 2, sizeof(cl_mem), &v_out);
    }

  /*Write the A and B matrices into compute device memory*/
    err = clEnqueueWriteBuffer(commands, v_in, CL_TRUE, 0,
                               sizeof(float) * n * m, val, 0, NULL, NULL);

   /*Execute the kernel over the entire range of C matrix elements*/
    global[0] =(size_t) nl;

    if(1 < nr)
    {
        global[1] =(size_t) nr;
        ndrange = 2;
    }
    else
    {
        ndrange = 1;
    }

    err = clEnqueueNDRangeKernel(commands, kernel, ndrange, NULL,
                                 global, NULL, 0, NULL, &prof_event);

  /*Wait for the commands to complete before reading back results*/
    clFinish(commands);

    err = clGetEventProfilingInfo(prof_event,
                      CL_PROFILING_COMMAND_START,
                      sizeof(cl_ulong),
                      &ev_start_time,
                      NULL);

    err = clGetEventProfilingInfo(prof_event,
                      CL_PROFILING_COMMAND_END,
                      sizeof(cl_ulong),
                      &ev_end_time,
                      NULL);
  /*Read back the results from the compute device*/
    err = clEnqueueReadBuffer( commands, v_out, CL_TRUE, 0,
                         sizeof(float) * n, out_val, 0, NULL, NULL );

    run_time  = ev_end_time - ev_start_time;

    printf("  ev_end_time = %ld\n", (long)ev_end_time);
    printf("ev_start_time = %ld\n", (long)ev_start_time);
    printf("     run_time = %ld\n", (long)run_time);

    clReleaseProgram(program);
    clReleaseKernel(kernel);
    clReleaseMemObject(v_in);
    clReleaseMemObject(v_out);
    clReleaseCommandQueue(commands);
    clReleaseContext(context);

    ret = NclReturnValue(
    		       out_val,
    		       nd,
    		       dimsizes,
    		       NULL,
    		       out_data_type,
    		       0);
    NclFree(dims);
    NclFree(dimsizes);
    return(ret);
}

#ifdef __cplusplus
}
#endif

