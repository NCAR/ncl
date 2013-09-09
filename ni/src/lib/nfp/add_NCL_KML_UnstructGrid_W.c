#include <stdio.h>
#include <stdbool.h>
#include "KML_Funcs.h"
#include "wrapper.h"

NhlErrorTypes add_NCL_KML_UnstructGrid_W(void) {
    /* Argument # 0 */
    NrmQuark * FileName;
    char * c_FileName;
    FileName = (NrmQuark *) NclGetArgValue(
            0,
            10,
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);
    c_FileName = NrmQuarkToString(*FileName);
#ifdef DEBUG
    printf("KML File Name: %s\n", c_FileName);
#endif

    /* Argument # 1 */
    unsigned int *verticesOnEdge;
    NclScalar missing_verticesOnEdge;
    int ndims_verticesOnEdge, has_missing_verticesOnEdge;
    ng_size_t dsizes_verticesOnEdge[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_verticesOnEdge;
    verticesOnEdge = (unsigned int *) NclGetArgValue(
            1,
            10,
            &ndims_verticesOnEdge,
            dsizes_verticesOnEdge,
            &missing_verticesOnEdge,
            &has_missing_verticesOnEdge,
            &type_verticesOnEdge,
            DONT_CARE);

    /* Argument # 2 */
    float *latVertex;
    NclScalar missing_latVertex;
    int ndims_latVertex, has_missing_latVertex;
    ng_size_t dsizes_latVertex[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_latVertex;
    latVertex = (float *) NclGetArgValue(
            2,
            10,
            &ndims_latVertex,
            dsizes_latVertex,
            &missing_latVertex,
            &has_missing_latVertex,
            &type_latVertex,
            DONT_CARE);

    /* Argument # 3 */
    float *lonVertex;
    NclScalar missing_lonVertex;
    int ndims_lonVertex, has_missing_lonVertex;
    ng_size_t dsizes_lonVertex[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_lonVertex;
    lonVertex = (float *) NclGetArgValue(
            3,
            10,
            &ndims_lonVertex,
            dsizes_lonVertex,
            &missing_lonVertex,
            &has_missing_lonVertex,
            &type_lonVertex,
            DONT_CARE);

    /* Argument # 4 */
    float *hVertex;
    NclScalar missing_hVertex;
    int ndims_hVertex, has_missing_hVertex;
    ng_size_t dsizes_hVertex[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_hVertex;
    hVertex = (float *) NclGetArgValue(
            4,
            10,
            &ndims_hVertex,
            dsizes_hVertex,
            &missing_hVertex,
            &has_missing_hVertex,
            &type_hVertex,
            DONT_CARE);

    /* Argument # 5 */
    NrmQuark * StyleIDStr;
    char * c_StyleIDStr;
    StyleIDStr = (NrmQuark *) NclGetArgValue(
            5,
            10,
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);
    c_StyleIDStr = NrmQuarkToString(*StyleIDStr);

    /* Argument # 6 */
    unsigned int *AltMode;
    NclScalar missing_AltMode;
    int ndims_AltMode, has_missing_AltMode;
    ng_size_t dsizes_AltMode[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type_AltMode;
    AltMode = (unsigned int *) NclGetArgValue(
            6,
            10,
            &ndims_AltMode,
            dsizes_AltMode,
            &missing_AltMode,
            &has_missing_AltMode,
            &type_AltMode,
            DONT_CARE);

    /* Argument # 7 */
    logical * Extrude;
    bool c_Extrude;
    Extrude = (logical *) NclGetArgValue(7, 10, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
    c_Extrude = (*Extrude) ? true : false;


    /* Argument # 8 */
    logical * Tessellate;
    bool c_Tessellate;
    Tessellate = (logical *) NclGetArgValue(8, 10, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
    c_Tessellate = (*Tessellate) ? true : false;

    /* Argument # 9 */
    logical * Visible;
    bool c_Visible;
    Visible = (logical *) NclGetArgValue(9, 10, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
    c_Visible = (*Visible) ? true : false;

    /* Doing some tests */
    if (ndims_latVertex != ndims_lonVertex ||
            ndims_latVertex != ndims_hVertex) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "add_NCL_KML_UnstructGrid: latVertex, lonVertex, and hVertex must have same dimension");
        return (NhlFATAL);
    }

    if (dsizes_latVertex[0] != dsizes_lonVertex[0] ||
            dsizes_latVertex[0] != dsizes_hVertex[0]) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "add_NCL_KML_UnstructGrid: latVertex, lonVertex, and hVertex must have same dimension");
        return (NhlFATAL);
    }

    if (dsizes_verticesOnEdge[1] != 2) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "add_NCL_KML_UnstructGrid: verticesOnEdge must be of size [*][2]");
        return (NhlFATAL);
    }

    unsigned int nEdges = dsizes_verticesOnEdge[0];

    if (NCL_OpenKMLFile(c_FileName) != 0) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "add_NCL_KML_2DGrid_W: Cannot access KML file.");
        return (NhlFATAL);
    }

    add_NCL_KML_UnstructGrid(nEdges, verticesOnEdge,
            latVertex, lonVertex, hVertex,
            c_StyleIDStr,
            *AltMode,
            c_Extrude, c_Tessellate, c_Visible);
    NCL_CloseKMLFile();

    return NhlNOERROR;
}