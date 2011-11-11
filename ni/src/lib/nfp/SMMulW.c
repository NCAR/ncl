#include <stdio.h>
#include "wrapper.h"

NhlErrorTypes SMMul_W
#if	NhlNeedProto
(void)
#else
()
#endif
{
    /* Locally used variables */
    int nElement;
    int nData;
    int i,j;
    int xInd,yInd;
    
    /* Defining the arguments */
    /* Argument # 0 */
    int * row;
    int ndims_row;
    ng_size_t dsizes_row[NCL_MAX_DIMENSIONS];
    
    /* Argument # 1 */
    int * col;
    int ndims_col;
    ng_size_t dsizes_col[NCL_MAX_DIMENSIONS];
    
    /* Argument # 2 */
    double * S;
    int ndims_S;
    ng_size_t dsizes_S[NCL_MAX_DIMENSIONS];
    
    /* Argument # 3 */
    double * x;
    int ndims_x;
    NclScalar missing_x;
    int  xhasmissing;
    NclBasicDataTypes xtype;
    ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
    
    /* Argument # 4 */
    double * y;
    int ndims_y;
    NclScalar missing_y;
    int  yhasmissing;
    NclBasicDataTypes ytype;
    ng_size_t dsizes_y[NCL_MAX_DIMENSIONS];

    /* Getting Arguments values */

    /* Argument # 0 */
    row = (int*)NclGetArgValue(
                0,
                5,
                &ndims_row, 
                dsizes_row,
                NULL,
                NULL,
                NULL,
                DONT_CARE);    

    /* Argument # 1 */
    col = (int*)NclGetArgValue(
                1,
                5,
                &ndims_col, 
                dsizes_col,
                NULL,
                NULL,
                NULL,
                DONT_CARE);    

    /* Argument # 2 */
    S = (double*)NclGetArgValue(
                2,
                5,
                &ndims_S, 
                dsizes_S,
                NULL,
                NULL,
                NULL,
                DONT_CARE); 
      /* Argument # 3 */
    x = (double*)NclGetArgValue(
                3,
                5,
                &ndims_x, 
                dsizes_x,
                &missing_x,
                &xhasmissing,
                &xtype,
                DONT_CARE); 
      /* Argument # 4 */
    y = (double*)NclGetArgValue(
                4,
                5,
                &ndims_y, 
                dsizes_y,
                &missing_y,
                &yhasmissing,
                &ytype,
                DONT_CARE); 

    /* checking some of the dimensions */

    if( ndims_row != 1 || ndims_col != 1 || ndims_S != 1 ) 
    {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"row, col, non-zero elements must be a vector.");
        return(NhlFATAL);
    }
    
    if( *dsizes_row != *dsizes_col || *dsizes_col != *dsizes_S ) 
    {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"row, col, non-zero must have same number of elements");
        return(NhlFATAL);
    }

    if( ndims_x != 2 || ndims_y != 2 ) 
    {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"x and y must have two dimensions");
        return(NhlFATAL);
    }

    if( ndims_x != 2 || ndims_y != 2 ) 
    {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"x and y must have two dimensions");
        return(NhlFATAL);
    }

    if( dsizes_x[1] !=  dsizes_y[1] ) 
    {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"The second dimension of x and y must be the same.");
        return(NhlFATAL);
    }
    
    nElement=*dsizes_S;
    nData=dsizes_x[1];
    
    /* performing the real multiplication */
    if (xhasmissing)
    {
        for(i=0;i<nElement;i++)
        {
            for (j=0;j<nData;j++)
            {
                xInd=(col[i]-1)*nData+j;
                yInd=(row[i]-1)*nData+j;
                if ( x[xInd]==missing_x.doubleval || y[yInd]==missing_y.doubleval )
                {
                    y[yInd]=missing_y.doubleval;
                }
                else
                {
                    y[yInd]+=x[xInd]*S[i];
                }
            }
        }
    }
    else
    {
        for(i=0;i<nElement;i++)
        {
            for (j=0;j<nData;j++)
            {
                xInd=(col[i]-1)*nData+j;
                yInd=(row[i]-1)*nData+j;
                y[yInd]+=x[xInd]*S[i];
            }
        }    
    }
    
    return(NhlNOERROR);                                                                  
                

}