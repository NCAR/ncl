#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "wrapper.h"
#include "KML_UTILS.h"

NhlErrorTypes Unstruct2KML_W(void) {
    /* Defining the Arguments */
    /* Argument # 0 */
    string * filename;
    char * c_filename;
    FILE * fid;

    /* Argument # 1 */
    string * gridname;
    char * c_gridname;

    /* Argument # 2 */
    double * lat;
    int ndims_lat;
    ng_size_t dsizes_lat[NCL_MAX_DIMENSIONS];

    /* Argument # 3*/
    double * lon;
    int ndims_lon;
    ng_size_t dsizes_lon[NCL_MAX_DIMENSIONS];

    /* Argument # 4 */
    int * elCon;
    int ndims_elCon;
    NclScalar missing_elCon;
    int elConhasmissing;
    NclBasicDataTypes elContype;
    ng_size_t dsizes_elCon[NCL_MAX_DIMENSIONS];


    /* Argument # 5*/
    int * nCon;
    int ndims_nCon;
    ng_size_t dsizes_nCon[NCL_MAX_DIMENSIONS];
    /* End of Defining the Arguments */

    /* Local Variables */
    int Base = 1;
    double depth = 9000.0;
    char *GridFolder;
    char *Altmode;
    GridFolder = "Grid Cells";
    Altmode = "relativeToGround";

    /* Getting Arguments values */
    /* Argument # 0 */
    filename = (string *) NclGetArgValue(
            0,
            6,
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);

    c_filename = NrmQuarkToString(*filename);

    /* Argument # 1 */
    gridname = (string *) NclGetArgValue(
            1,
            6,
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);

    c_gridname = NrmQuarkToString(*gridname);

    /* Argument # 2 */
    lat = (double*) NclGetArgValue(
            2,
            6,
            &ndims_lat,
            dsizes_lat,
            NULL,
            NULL,
            NULL,
            DONT_CARE);

    /* Argument # 3 */
    lon = (double*) NclGetArgValue(
            3,
            6,
            &ndims_lon,
            dsizes_lon,
            NULL,
            NULL,
            NULL,
            DONT_CARE);

    /* Argument # 4 */
    elCon = (int*) NclGetArgValue(
            4,
            6,
            &ndims_elCon,
            dsizes_elCon,
            &missing_elCon,
            &elConhasmissing,
            &elContype,
            DONT_CARE);

    /* Argument # 5 */
    nCon = (int*) NclGetArgValue(
            5,
            6,
            &ndims_nCon,
            dsizes_nCon,
            NULL,
            NULL,
            NULL,
            DONT_CARE);
    /* End of getting argument values */

    /* opening the output file */
    printf("Opening %s file ...\n", c_filename);
    if ((fid = fopen(c_filename, "w")) == NULL) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "Cannot open the file!");
        return (NhlFATAL);
    }

    /* writing the file */

    KML_AddKMLHeader(fid);
    KML_OpenDocument(fid);
    KML_AddNameRecord(fid, c_gridname);
    KML_AddStyle(fid, 0);
    KML_OpenFolder(fid);
    KML_AddNameRecord(fid, c_gridname);
    KML_OpenFolder(fid);
    KML_AddNameRecord(fid, GridFolder);

    KML_AddPolygons(0,
            1,
            0,
            depth,
            Base,
            Altmode,
            lat,
            lon,
            elCon,
            dsizes_elCon,
            nCon,
            fid);


    KML_CloseFolder(fid);
    KML_CloseFolder(fid);
    KML_CloseDocument(fid);
    KML_CloseKML(fid);

    fclose(fid);
    return (NhlNOERROR);
}