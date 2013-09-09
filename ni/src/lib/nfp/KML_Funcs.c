#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "KML_Funcs.h"
#include "DirectVincenty.h"

int NCL_OpenKMLFile(char * FileName) {
    if ((KML_fp = fopen(FileName, "a")) == NULL) {
        return (1);
    }
    return (0);
}

void NCL_CloseKMLFile(void) {
    fclose(KML_fp);
}

void Open_NCL_KML_Placemark(char * Placemark_Name) {
    fprintf(KML_fp, "<Placemark>\n");
    add_NCL_KML_Name(Placemark_Name);
}

void Close_NCL_KML_Placemark(void) {
    fprintf(KML_fp, "</Placemark>\n");
}

void add_NCL_KML_Name(char * NameStr) {
    fprintf(KML_fp, "<name>%s</name>\n", NameStr);
}

void add_NCL_KML_Visibility(bool YesNo) {
    if (YesNo) {
        fprintf(KML_fp, "<visibility>1</visibility>\n");
    } else {
        fprintf(KML_fp, "<visibility>0</visibility>\n");
    }
}

void add_NCL_KML_Extrude(bool YesNo) {
    if (YesNo) {
        fprintf(KML_fp, "<extrude>1</extrude>\n");
    } else {
        fprintf(KML_fp, "<extrude>0</extrude>\n");
    }
}

void add_NCL_KML_Tessellate(bool YesNo) {
    if (YesNo) {
        fprintf(KML_fp, "<tessellate>1</tessellate>\n");
    } else {
        fprintf(KML_fp, "<tessellate>0</tessellate>\n");
    }
}

void add_NCL_KML_StyleUrl(char * StyleStr) {
    fprintf(KML_fp, "<styleUrl>%s</styleUrl>\n", StyleStr);
}

void add_NCL_KML_TimeSpan(char * BeginTimeStr, char * EndTimeStr) {
    fprintf(KML_fp, "<TimeSpan><begin>%s</begin><end>%s</end></TimeSpan>\n",
            BeginTimeStr,
            EndTimeStr);
}

int add_NCL_KML_AltMode(enum AltitudeMode_t AltMode) {
    switch (AltMode) {
        case KML_absolute:
            fprintf(KML_fp, "<altitudeMode>absolute</altitudeMode>\n");
            break;
        case KML_clampToGround:
            fprintf(KML_fp, "<altitudeMode>clampToGround</altitudeMode>\n");
            break;
        case KML_clampToSeaFloor:
            fprintf(KML_fp, "<altitudeMode>clampToSeaFloor</altitudeMode>\n");
            break;
        case KML_relativeToGround:
            fprintf(KML_fp, "<altitudeMode>relativeToGround</altitudeMode>\n");
            break;
        case KML_relativeToSeaFloor:
            fprintf(KML_fp, "<altitudeMode>relativeToSeaFloor</altitudeMode>\n");
            break;
        default:
            printf("FATAL ERROR: Altitude mode not supported.\n");
            return (1);
    }
    return (0);
}

void add_NCL_KML_Coordinates(float * lat, float * lon, float * h, int nPoint) {
    int i;
    fprintf(KML_fp, "<coordinates>\n");
    for (i = 0; i < nPoint; i++) {
        fprintf(KML_fp, "%f,%f,%f\n", lon[i], lat[i], h[i]);
    }
    fprintf(KML_fp, "</coordinates>\n");
}

void add_NCL_KML_LineString(int nPoint, float * lat, float * lon, float * h,
        enum AltitudeMode_t AltMode, bool Extrude, bool Tessellate) {
    fprintf(KML_fp, "<LineString>\n");
    add_NCL_KML_Extrude(Extrude);
    add_NCL_KML_Tessellate(Tessellate);
    add_NCL_KML_AltMode(AltMode);
    add_NCL_KML_Coordinates(lat, lon, h, nPoint);
    fprintf(KML_fp, "</LineString>\n");
}

void add_NCL_KML_Arrow_Kernel(unsigned int nlat, unsigned int nlon, unsigned int lat_stride, unsigned int lon_stride,
        float * lat, float * lon,
        float * u, float * v, float * h,
        unsigned int * cIndex,
        char * BeginTimeStr, char * EndTimeStr,
        float ArrowScale,
        float ArrowTipAngle, float ArrowTipScale,
        enum AltitudeMode_t AltMode,
        bool Extrude, bool Tessellate, bool Visible) {
    double r2d = 45.0 / atan(1.0);
    double length;
    double azimuth;
    double ArrowCoords[4][2];
    float tmpLat[3], tmpLon[3], tmpH[3];
    char PlacemarkName[30];
    char StyleIDStr[30];
    if (ArrowScale == 0.0) ArrowScale = Default_ArrowScale;

    int i, j, idx;
    for (i = 0; i < nlat; i += lat_stride) {
        for (j = 0; j < nlon; j += lon_stride) {
            /* printf("Point %d,%d\n",i,j); */
            idx = i * nlon + j;
            sprintf(PlacemarkName, "Data Point (%d,%d)", i, j);
            Open_NCL_KML_Placemark(PlacemarkName);
            add_NCL_KML_Visibility(Visible);
            sprintf(StyleIDStr, "ColorMapStyleID%d", *(cIndex + idx));
            add_NCL_KML_StyleUrl(StyleIDStr);

            length = sqrt(*(u + idx) * *(u + idx) + *(v + idx) * *(v + idx)) * ArrowScale;
            azimuth = 90.0 - atan2(*(v + idx), *(u + idx)) * r2d;
            NCL_GetArrow_Coords(ArrowCoords, *(lat + idx), *(lon + idx),
                    length, azimuth,
                    ArrowTipAngle, ArrowTipScale, 0.0, 0.0);

            Open_NCL_KML_MultiGeometry();
            tmpLat[0] = (float) ArrowCoords[0][0];
            tmpLat[1] = (float) ArrowCoords[1][0];
            tmpLon[0] = (float) ArrowCoords[0][1];
            tmpLon[1] = (float) ArrowCoords[1][1];
            tmpH[0] = *(h + idx);
            tmpH[1] = *(h + idx);
            tmpH[2] = *(h + idx);
            add_NCL_KML_LineString(2, tmpLat, tmpLon, tmpH, AltMode, Extrude, Tessellate);
            tmpLat[0] = (float) ArrowCoords[2][0];
            tmpLat[1] = (float) ArrowCoords[1][0];
            tmpLat[2] = (float) ArrowCoords[3][0];
            tmpLon[0] = (float) ArrowCoords[2][1];
            tmpLon[1] = (float) ArrowCoords[1][1];
            tmpLon[2] = (float) ArrowCoords[3][1];
            add_NCL_KML_LineString(3, tmpLat, tmpLon, tmpH, AltMode, Extrude, Tessellate);
            Close_NCL_KML_MultiGeometry();
            add_NCL_KML_TimeSpan(BeginTimeStr, EndTimeStr);
            Close_NCL_KML_Placemark();
        }
    }
}

int add_NCL_KML_2DGrid(int nlat, int nlon,
        float * lat, float * lon, float * h,
        char * StyleIDStr,
        enum AltitudeMode_t AltMode,
        bool Extrude, bool Tessellate, bool Visible) {
    int i, j;
    char GridLineID[10];
    float * gridLine_lat;
    float * gridLine_lon;
    float * gridLine_h;

    /* Exporting the grid lines along i */
    if ((gridLine_lat = (float *) malloc(nlon * sizeof (float))) == NULL) {
        printf("Couldn't get enough memory.\n");
        return (1);
    }
    if ((gridLine_lon = (float *) malloc(nlon * sizeof (float))) == NULL) {
        printf("Couldn't get enough memory.\n");
        return (1);
    }
    if ((gridLine_h = (float *) malloc(nlon * sizeof (float))) == NULL) {
        printf("Couldn't get enough memory.\n");
        return (1);
    }
    for (i = 0; i < nlat; i++) {
        for (j = 0; j < nlon; j++) {
            gridLine_lat[j] = *(lat + i * nlon + j);
            gridLine_lon[j] = *(lon + i * nlon + j);
            gridLine_h[j] = *(h + i * nlon + j);
        }
        sprintf(GridLineID, "i = %d", i);
        Open_NCL_KML_Placemark(GridLineID);
        add_NCL_KML_Visibility(Visible);
        add_NCL_KML_StyleUrl(StyleIDStr);
        add_NCL_KML_LineString(nlon, gridLine_lat, gridLine_lon, gridLine_h, AltMode, Extrude, Tessellate);
        Close_NCL_KML_Placemark();
    }
    free(gridLine_lat);
    free(gridLine_lon);
    free(gridLine_h);

    /* Exporting the grid lines along j */
    if ((gridLine_lat = (float *) malloc(nlat * sizeof (float))) == NULL) {
        printf("Couldn't get enough memory.\n");
        return (1);
    }
    if ((gridLine_lon = (float *) malloc(nlat * sizeof (float))) == NULL) {
        printf("Couldn't get enough memory.\n");
        return (1);
    }
    if ((gridLine_h = (float *) malloc(nlat * sizeof (float))) == NULL) {
        printf("Couldn't get enough memory.\n");
        return (1);
    }
    for (j = 0; j < nlon; j++) {
        for (i = 0; i < nlat; i++) {
            gridLine_lat[i] = *(lat + i * nlon + j);
            gridLine_lon[i] = *(lon + i * nlon + j);
            gridLine_h[i] = *(h + i * nlon + j);
        }
        sprintf(GridLineID, "j = %d", i);
        Open_NCL_KML_Placemark(GridLineID);
        add_NCL_KML_Visibility(Visible);
        add_NCL_KML_StyleUrl(StyleIDStr);
        add_NCL_KML_LineString(nlat, gridLine_lat, gridLine_lon, gridLine_h, AltMode, Extrude, Tessellate);
        Close_NCL_KML_Placemark();
    }
    free(gridLine_lat);
    free(gridLine_lon);
    free(gridLine_h);

    return (0);
}

void add_NCL_KML_UnstructGrid(int nEdges, unsigned int * verticesOnEdge,
        float * latVertex, float * lonVertex, float * hVertex,
        char * StyleIDStr,
        enum AltitudeMode_t AltMode,
        bool Extrude, bool Tessellate, bool Visible) {
    float lat[2];
    float lon[2];
    float h[2];
    char * GridName = "Unstructured Grid";

    int i, vertexID;

    Open_NCL_KML_Placemark(GridName);
    add_NCL_KML_Visibility(Visible);
    add_NCL_KML_StyleUrl(StyleIDStr);
    Open_NCL_KML_MultiGeometry();
    for (i = 0; i < nEdges; i++) {
        /* First point of the Edge */
        vertexID = verticesOnEdge[i * 2 + 0];
        lat[0] = latVertex[vertexID];
        lon[0] = lonVertex[vertexID];
        h[0] = hVertex[vertexID];

        /* Second point of the edge */
        vertexID = verticesOnEdge[i * 2 + 1];
        lat[1] = latVertex[vertexID];
        lon[1] = lonVertex[vertexID];
        h[1] = hVertex[vertexID];

        /* exporting the edge to KML */
        add_NCL_KML_LineString(2, lat, lon, h, AltMode, Extrude, Tessellate);
    }
    Close_NCL_KML_MultiGeometry();
    Close_NCL_KML_Placemark();
}

void Open_NCL_KML_MultiGeometry(void) {
    fprintf(KML_fp, "<MultiGeometry>\n");
}

void Close_NCL_KML_MultiGeometry(void) {
    fprintf(KML_fp, "</MultiGeometry>\n");
}

void NCL_GetArrow_Coords(double ArrowCoords[4][2], double lat, double lon,
        double length, double azimuth,
        double ArrowTipAngle, double ArrowTipScale,
        double a, double f) {
    /* Here is how the arrow coordinate stored
               2
                 \
           0 =====1
                 /	                    	 
               3
            they are stored as lat/lon pairs in degree 
     */

    double d2r = atan(1.0) / 45.0;
    double r2d = 45.0 / atan(1.0);
    double tmpLat;
    double tmpLon;

    if (ArrowTipAngle == 0.0) ArrowTipAngle = Default_ArrowTipAngle;
    if (ArrowTipScale == 0.0) ArrowTipScale = Default_ArrowTipScale;

    ArrowCoords[0][0] = lat;
    ArrowCoords[0][1] = lon;

    DirectVincenty(lat*d2r, lon*d2r, azimuth*d2r, length, &tmpLat, &tmpLon, a, f);
    ArrowCoords[1][0] = tmpLat*r2d;
    ArrowCoords[1][1] = tmpLon*r2d;

    DirectVincenty(ArrowCoords[1][0] * d2r, ArrowCoords[1][1] * d2r,
            (azimuth + 180 - ArrowTipAngle) * d2r, length*ArrowTipScale, &tmpLat, &tmpLon, a, f);
    ArrowCoords[2][0] = tmpLat*r2d;
    ArrowCoords[2][1] = tmpLon*r2d;

    DirectVincenty(ArrowCoords[1][0] * d2r, ArrowCoords[1][1] * d2r,
            (azimuth - 180 + ArrowTipAngle) * d2r, length*ArrowTipScale, &tmpLat, &tmpLon, a, f);
    ArrowCoords[3][0] = tmpLat*r2d;
    ArrowCoords[3][1] = tmpLon*r2d;

}

