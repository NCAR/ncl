#include <png.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include "rgba2png.h"
#include "wrapper.h"

NhlErrorTypes rgba2png_W(void)
{
    /* Defining the arguments and Retrieving their values*/
    /* Argument # 0 */
    NrmQuark * BaseFileName;
    char * c_BaseFileName;
    BaseFileName = (NrmQuark *) NclGetArgValue(
            0,
            8,
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);
    c_BaseFileName = NrmQuarkToString(*BaseFileName);
#ifdef DEBUG
    printf("PNG File Base Name: %s\n", c_BaseFileName);
#endif

    /* Argument # 1 */
    unsigned int *Red;
    NclScalar missing_Red;
    int ndims_Red, has_missing_Red;
    ng_size_t dsizes_Red[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_Red;
    Red = (unsigned int *) NclGetArgValue(
            1,
            8,
            &ndims_Red,
            dsizes_Red,
            &missing_Red,
            &has_missing_Red,
            &type_Red,
            DONT_CARE);

    /* Argument # 2 */
    unsigned int *Green;
    NclScalar missing_Green;
    int ndims_Green, has_missing_Green;
    ng_size_t dsizes_Green[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_Green;
    Green = (unsigned int *) NclGetArgValue(
            2,
            8,
            &ndims_Green,
            dsizes_Green,
            &missing_Green,
            &has_missing_Green,
            &type_Green,
            DONT_CARE);

    /* Argument # 3 */
    unsigned int *Blue;
    NclScalar missing_Blue;
    int ndims_Blue, has_missing_Blue;
    ng_size_t dsizes_Blue[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_Blue;
    Blue = (unsigned int *) NclGetArgValue(
            3,
            8,
            &ndims_Blue,
            dsizes_Blue,
            &missing_Blue,
            &has_missing_Blue,
            &type_Blue,
            DONT_CARE);

    /* Argument # 4 */
    unsigned int *Alpha;
    NclScalar missing_Alpha;
    int ndims_Alpha, has_missing_Alpha;
    ng_size_t dsizes_Alpha[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_Alpha;
    Alpha = (unsigned int *) NclGetArgValue(
            4,
            8,
            &ndims_Alpha,
            dsizes_Alpha,
            &missing_Alpha,
            &has_missing_Alpha,
            &type_Alpha,
            DONT_CARE);

    /* Argument # 5 */
    unsigned int *width;
    NclScalar missing_width;
    int ndims_width, has_missing_width;
    ng_size_t dsizes_width[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_width;
    width = (unsigned int *) NclGetArgValue(
            5,
            8,
            &ndims_width,
            dsizes_width,
            &missing_width,
            &has_missing_width,
            &type_width,
            DONT_CARE);

    /* Argument # 6 */
    unsigned int *height;
    NclScalar missing_height;
    int ndims_height, has_missing_height;
    ng_size_t dsizes_height[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_height;
    height = (unsigned int *) NclGetArgValue(
            6,
            8,
            &ndims_height,
            dsizes_height,
            &missing_height,
            &has_missing_height,
            &type_height,
            DONT_CARE);

    /* Argument # 7 */
    unsigned int *nFrames;
    NclScalar missing_nFrames;
    int ndims_nFrames, has_missing_nFrames;
    ng_size_t dsizes_nFrames[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_nFrames;
    nFrames = (unsigned int *) NclGetArgValue(
            7,
            8,
            &ndims_nFrames,
            dsizes_nFrames,
            &missing_nFrames,
            &has_missing_nFrames,
            &type_nFrames,
            DONT_CARE);

    /* Checking the sizes */
    if (!(ndims_Red == ndims_Green &&
            ndims_Red == ndims_Blue &&
            ndims_Red == ndims_Alpha)) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "rgba2png: Red, Green, Blue, and Alpha must have the same number of elements");
        return (NhlFATAL);
    }

    /* Checking the dimensions. They should all match. */
    int i;
    for (i = 0; i < ndims_Red; i++) {
        if (!(dsizes_Red[i] == dsizes_Green[i] &&
                dsizes_Red[i] == dsizes_Blue[i] &&
                dsizes_Red[i] == dsizes_Alpha[i])) {
            NhlPError(NhlFATAL, NhlEUNKNOWN, "rgba2png: Red, Green, Blue, and Alpha must have the same number of elements");
            return (NhlFATAL);
        }
    }

    /* actually converting to PNG */
    if (rgba2png(Red, Green, Blue, Alpha, *width, *height, *nFrames, c_BaseFileName) != 0) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "rgba2png: Something Went wrong while storing the PNG Files.");
        return (NhlFATAL);
    }

    return NhlNOERROR;
}

