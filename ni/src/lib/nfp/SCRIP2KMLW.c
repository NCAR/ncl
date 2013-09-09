#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <netcdf.h>
#include "wrapper.h"
#include "KML_UTILS.h"

NhlErrorTypes SCRIP2KML_W(void) {
    /* Defining the Arguments */
    /* Argument # 0 */
    string * scrip_filename;
    char * c_scrip_filename;
    int ncid;

    /* Argument # 1 */
    string * kml_filename;
    char * c_kml_filename;
    FILE * fid;

    /* Argument # 2 */
    string * gridname;
    char * c_gridname;

    /* End of Defining the Arguments */

    /* Local Variables */
    size_t grid_size;
    int grid_size_id;

    size_t grid_corners;
    int grid_corners_id;

    int grid_corner_lat_id;
    double * grid_corner_lat;

    int grid_corner_lon_id;
    double * grid_corner_lon;

    int Base = 1;
    int nodeInd;
    int i, j;
    double depth = 9000.0;
    char *GridFolder;
    char *Altmode;
    GridFolder = "Grid Cells";
    Altmode = "relativeToGround";
    char Name[30];

    /* Getting Arguments values */
    /* Argument # 0 */
    scrip_filename = (string *) NclGetArgValue(
            0,
            3,
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);

    c_scrip_filename = NrmQuarkToString(*scrip_filename);

    /* Argument # 1 */
    kml_filename = (string *) NclGetArgValue(
            1,
            3,
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);

    c_kml_filename = NrmQuarkToString(*kml_filename);

    /* Argument # 1 */
    gridname = (string *) NclGetArgValue(
            2,
            3,
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);

    c_gridname = NrmQuarkToString(*gridname);

    /* End of getting argument values */

    /* Opening the SCRIP NetCDF File */
    printf("Opening %s file ...\n", c_scrip_filename);
    if (nc_open(c_scrip_filename, NC_NOWRITE, &ncid) != NC_NOERR) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "Cannot open the SCRIP file!");
        return (NhlFATAL);
    }

    /* getting NetCDF Dimension IDs */
    if (nc_inq_dimid(ncid, "grid_size", &grid_size_id) != NC_NOERR) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "Cannot get grid size ID!");
        return (NhlFATAL);
    }

    if (nc_inq_dimid(ncid, "grid_corners", &grid_corners_id) != NC_NOERR) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "Cannot get grid corners ID!");
        return (NhlFATAL);
    }

    /* getting NetCDF Dimension Len */

    if (nc_inq_dimlen(ncid, grid_size_id, &grid_size) != NC_NOERR) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "Cannot read grid size!");
        return (NhlFATAL);
    }
    if (nc_inq_dimlen(ncid, grid_corners_id, &grid_corners) != NC_NOERR) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "Cannot read grid corners!");
        return (NhlFATAL);
    }

    /* allocating Memory */

    grid_corner_lat = malloc((int) grid_size * (int) grid_corners * sizeof (double));
    if (!grid_corner_lat) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "Cannot assign memory to store grid corner lat!");
        return (NhlFATAL);
    }
    grid_corner_lon = malloc((int) grid_size * (int) grid_corners * sizeof (double));
    if (!grid_corner_lon) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "Cannot assign memory to store grid corner lon!");
        return (NhlFATAL);
    }

    /* getting some Var IDs */
    if (nc_inq_varid(ncid, "grid_corner_lat", &grid_corner_lat_id) != NC_NOERR) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "Cannot get grid_corner_lat ID!");
        return (NhlFATAL);
    }

    if (nc_inq_varid(ncid, "grid_corner_lon", &grid_corner_lon_id) != NC_NOERR) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "Cannot get grid_corner_lon ID!");
        return (NhlFATAL);
    }

    /* Finally reading the values */
    if (nc_get_var_double(ncid, grid_corner_lat_id, grid_corner_lat) != NC_NOERR) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "Cannot read grid_corner_lat!");
        return (NhlFATAL);
    }
    if (nc_get_var_double(ncid, grid_corner_lon_id, grid_corner_lon) != NC_NOERR) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "Cannot read grid_corner_lon!");
        return (NhlFATAL);
    }

    /* opening the output file */
    printf("Opening %s file ...\n", c_kml_filename);
    if ((fid = fopen(c_kml_filename, "w")) == NULL) {
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

    printf("Number of elements=%d\n", grid_size);
    for (i = 0; i < grid_size; i++) {
        KML_OpenPlaceMark(fid);
        sprintf(Name, "Cell #%d", i);
        KML_AddNameRecord(fid, Name);
        KML_UseStyle(fid, 0);
        KML_OpenPolygon(fid);

        KML_SetTesselateMode(fid, 1);
        KML_SetExtrude(fid, 0);
        KML_SetAltitudeMode(fid, Altmode);
        KML_OpenouterBoundaryIs(fid);
        KML_OpenLinearRing(fid);
        KML_Opencoordinates(fid);
        for (j = 0; j < grid_corners; j++) {
            nodeInd = i * grid_corners + j;
            fprintf(fid, "%f,%f,%f\n", grid_corner_lon[nodeInd]
                    , grid_corner_lat[nodeInd]
                    , depth);
        }
        nodeInd = i * grid_corners + 0;
        fprintf(fid, "%f,%f,%f\n", grid_corner_lon[nodeInd]
                , grid_corner_lat[nodeInd]
                , depth);

        KML_Closecoordinates(fid);
        KML_CloseLinearRing(fid);
        KML_CloseouterBoundaryIs(fid);
        KML_ClosePolygon(fid);
        KML_ClosePlaceMark(fid);
    }



    KML_CloseFolder(fid);
    KML_CloseFolder(fid);
    KML_CloseDocument(fid);
    KML_CloseKML(fid);

    free(grid_corner_lat);
    free(grid_corner_lon);
    fclose(fid);
    return (NhlNOERROR);
}