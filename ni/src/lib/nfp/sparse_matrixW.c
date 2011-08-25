#include <stdio.h>
#include "wrapper.h"

NhlErrorTypes sparse_matrix_mult_W
#if NhlNeedProto
(void)
#else
()
#endif
{
    /* Locally used variables */
    int nElement;
    int ncol, nrow, nrowcol;
    int i, j;
    int xInd, yInd;
    
    /* Defining the arguments */
    /* Argument # 0 */
    int *row;
    ng_size_t dsizes_row[1];
    
    /* Argument # 1 */
    int *col;
    ng_size_t dsizes_col[1];
    
    /* Argument # 2 */
    void   *S;
    double *dS;
    NclBasicDataTypes type_S;
    ng_size_t dsizes_S[1];
    
    /* Argument # 3 */
    void   *x;
    double *dx;
    NclScalar missing_x, missing_dx, missing_fx;
    int  has_missing_x;
    NclBasicDataTypes type_x;
    ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
    
    /* Output argument # 4 */
    void   *y;
    NclBasicDataTypes type_y;
    int ret;

    /* Getting Arguments values */

    /* Argument # 0 */
    row = (int*)NclGetArgValue(
                0,
                4,
                NULL,
                dsizes_row,
                NULL,
                NULL,
                NULL,
                DONT_CARE);    

    /* Argument # 1 */
    col = (int*)NclGetArgValue(
                1,
                4,
                NULL,
                dsizes_col,
                NULL,
                NULL,
                NULL,
                DONT_CARE);    

    /* Argument # 2 */
    S = (void*)NclGetArgValue(
                2,
                4,
                NULL,
                dsizes_S,
                NULL,
                NULL,
                &type_S,
                DONT_CARE); 

      /* Argument # 3 */
    x = (void*)NclGetArgValue(
                3,
                4,
                NULL,
                dsizes_x,
                &missing_x,
                &has_missing_x,
                &type_x,
                DONT_CARE); 

    if( dsizes_row[0] != dsizes_col[0] || dsizes_col[0] != dsizes_S[0] ) 
    {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"sparse_matrix_mult: row, col, non-zero must have same number of elements");
        return(NhlFATAL);
    }

    nElement = dsizes_S[0];
    nrow     = dsizes_x[0];
    ncol     = dsizes_x[1];
    nrowcol  = nrow * ncol;

/*
 * Coerce missing values to double if necessary.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_fx);

/*
 * Coerce x and S to double if necessary.
 */
  dx = coerce_input_double(x,type_x,nrowcol,has_missing_x,&missing_x,
                           &missing_dx);
  dS = coerce_input_double(S,type_S,dsizes_S[0],0,NULL,NULL);

/*
 * Create output variable.
 */
    if(type_x == NCL_double || type_S == NCL_double) {
      type_y = NCL_double;
      y      = (void*)calloc(nrowcol,sizeof(double));
      if(y == NULL) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"sparse_matrix_mult: Unable to allocate memory for output array");
        return(NhlFATAL);
      }
    }
    else {
      type_y = NCL_float;
      y      = (void*)calloc(nrowcol,sizeof(float));
      if(y == NULL) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"sparse_matrix_mult: Unable to allocate memory for output array");
        return(NhlFATAL);
      }
    }

    /* performing the real multiplication */
    if (has_missing_x)
    {
        for(i=0;i<nElement;i++)
        {
            for (j=0;j<ncol;j++)
            {
                xInd=(col[i]-1)*ncol+j;
                yInd=(row[i]-1)*ncol+j;
                if ( dx[xInd]==missing_dx.doubleval ) 
                {
                  if(type_y == NCL_double) {
                    ((double*)y)[yInd] = missing_dx.doubleval;
                  }
                  else {
                    ((float*)y)[yInd] = missing_fx.floatval;
                  }
                }
                else
                {
                  if(type_y == NCL_double) {
                    ((double*)y)[yInd] += dx[xInd]*dS[i];
                  }
                  else {
                    ((float*)y)[yInd] += (float)(dx[xInd]*dS[i]);
                  }
                }
            }
        }
    }
    else
    {
        for(i=0;i<nElement;i++)
        {
            for (j=0;j<ncol;j++)
            {
                xInd=(col[i]-1)*ncol+j;
                yInd=(row[i]-1)*ncol+j;
                if(type_y == NCL_double) {
                  ((double*)y)[yInd] += dx[xInd]*dS[i];
                }
                else {
                  ((float*)y)[yInd] += (float)(dx[xInd]*dS[i]);
                }
            }
        }    
    }
    
/*
 * Return value back to NCL script.
 */
    if(has_missing_x) {
      if(type_y == NCL_double) {
        ret = NclReturnValue(y,2,dsizes_x,&missing_dx,type_y,0);
      }
      else {
        ret = NclReturnValue(y,2,dsizes_x,&missing_fx,type_y,0);
      }
    }
    else {
      ret = NclReturnValue(y,2,dsizes_x,NULL,type_y,0);
    }
    return(ret);
}
