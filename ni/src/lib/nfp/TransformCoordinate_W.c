#include <stdio.h>
#include <stdbool.h>
#include "TransformCoordinate.h"
#include "wrapper.h"

NhlErrorTypes TransformCoordinate_W
#if NhlNeedProto
(void)
#else
()
#endif
{
    /* Argument # 0 */
    NrmQuark * SrcProjStr;
    char * c_SrcProjStr;
    SrcProjStr = (NrmQuark *) NclGetArgValue(
            0,
            5,
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);
    c_SrcProjStr = NrmQuarkToString(*SrcProjStr);

    /* Argument # 1 */
    NrmQuark * DstProjStr;
    char * c_DstProjStr;
    DstProjStr = (NrmQuark *) NclGetArgValue(
            1,
            5,
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);
    c_DstProjStr = NrmQuarkToString(*DstProjStr);

    /* Argument # 2 */
    double * xCoord;
    NclScalar missing_xCoord;
    int ndims_xCoord, has_missing_xCoord;
    ng_size_t dsizes_xCoord[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_xCoord;
    xCoord = (double *) NclGetArgValue(
            2,
            5,
            &ndims_xCoord,
            dsizes_xCoord,
            &missing_xCoord,
            &has_missing_xCoord,
            &type_xCoord,
            DONT_CARE);

    /* Argument # 3 */
    double *yCoord;
    NclScalar missing_yCoord;
    int ndims_yCoord, has_missing_yCoord;
    ng_size_t dsizes_yCoord[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_yCoord;
    yCoord = (double *) NclGetArgValue(
            3,
            5,
            &ndims_yCoord,
            dsizes_yCoord,
            &missing_yCoord,
            &has_missing_yCoord,
            &type_yCoord,
            DONT_CARE);


    /* Argument # 4 */
    double *zCoord;
    NclScalar missing_zCoord;
    int ndims_zCoord, has_missing_zCoord;
    ng_size_t dsizes_zCoord[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_zCoord;
    zCoord = (double *) NclGetArgValue(
            4,
            5,
            &ndims_zCoord,
            dsizes_zCoord,
            &missing_zCoord,
            &has_missing_zCoord,
            &type_zCoord,
            DONT_CARE);


    /* doing some tests */
    if (ndims_xCoord != ndims_yCoord ||
            ndims_xCoord != ndims_zCoord) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "TransformCoordinate: x/y/zCoord must have same dimensions");
        return (NhlFATAL);
    }

    int i;
    unsigned int nPoint = 1;
    for (i = 0; i < ndims_xCoord; i++) {
        if (dsizes_xCoord[i] != dsizes_yCoord[i] ||
                dsizes_xCoord[i] != dsizes_zCoord[i]) {
            NhlPError(NhlFATAL, NhlEUNKNOWN, "TransformCoordinate: x/y/zCoord must have same size");
            return (NhlFATAL);
        }
        nPoint *= dsizes_xCoord[i];
    }

    if (TransformCoordinate(c_SrcProjStr, c_DstProjStr, xCoord, yCoord, zCoord, nPoint) != 0) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "TransformCoordinate: Couldn't transform coordinates.");
        return (NhlFATAL);
    }

    return NhlNOERROR;
}

