#include <stdio.h>
#include <stdlib.h>
#include "wrapper.h"
#include "KML_UTILS.h"

void KML_AddKMLHeader(FILE *fid) {
    fprintf(fid, "<\?xml version=\"1.0\" encoding=\"UTF-8\"\?>\n");
    fprintf(fid, "<kml xmlns=\"http://www.opengis.net/kml/2.2\">\n");
}

void KML_CloseKML(FILE *fid) {
    fprintf(fid, "</kml>\n");
}

void KML_OpenDocument(FILE *fid) {
    fprintf(fid, "<Document>\n");
}

void KML_CloseDocument(FILE *fid) {
    fprintf(fid, "</Document>\n");
}

void KML_AddStyle(FILE *fid, int StyleType) {
    if (StyleType == 0) {
        fprintf(fid, "<Style id=\"transYellowPoly\">\n");
        fprintf(fid, "<LineStyle>\n");
        fprintf(fid, "<width>0.1</width>\n");
        fprintf(fid, "</LineStyle>\n");
        fprintf(fid, "<PolyStyle>\n");
        fprintf(fid, "<color>0000ffff</color>\n");
        fprintf(fid, "</PolyStyle>\n");
        fprintf(fid, "</Style>\n");
    }
}

void KML_OpenFolder(FILE *fid) {
    fprintf(fid, "<Folder>\n");
}

void KML_CloseFolder(FILE *fid) {
    fprintf(fid, "</Folder>\n");
}

void KML_AddNameRecord(FILE *fid, char *nameString) {
    fprintf(fid, "<name>%s</name>\n", nameString);
}

void KML_OpenPlaceMark(FILE *fid) {
    fprintf(fid, "<Placemark>\n");
}

void KML_ClosePlaceMark(FILE *fid) {
    fprintf(fid, "</Placemark>\n");
}

void KML_OpenPolygon(FILE *fid) {
    fprintf(fid, "<Polygon>\n");
}

void KML_ClosePolygon(FILE *fid) {
    fprintf(fid, "</Polygon>\n");
}

void KML_UseStyle(FILE *fid, int StyleType) {
    if (StyleType == 0) {
        fprintf(fid, "<styleUrl>#transYellowPoly</styleUrl>\n");
    }
}

void KML_SetAltitudeMode(FILE *fid, char * Altmode) {
    fprintf(fid, "<altitudeMode>%s</altitudeMode>\n", Altmode);
}

void KML_SetTesselateMode(FILE *fid, int TesselateMode) {
    fprintf(fid, "<tessellate>%d</tessellate>\n", TesselateMode);
}

void KML_SetExtrude(FILE *fid, int ExtrudeMode) {
    fprintf(fid, "<extrude>%d</extrude>\n", ExtrudeMode);
}

void KML_OpenouterBoundaryIs(FILE *fid) {
    fprintf(fid, "<outerBoundaryIs>\n");
}

void KML_CloseouterBoundaryIs(FILE *fid) {
    fprintf(fid, "</outerBoundaryIs>\n");
}

void KML_OpenLinearRing(FILE *fid) {
    fprintf(fid, "<LinearRing>\n");
}

void KML_CloseLinearRing(FILE *fid) {
    fprintf(fid, "</LinearRing>\n");
}

void KML_Opencoordinates(FILE *fid) {
    fprintf(fid, "<coordinates>\n");
}

void KML_Closecoordinates(FILE *fid) {
    fprintf(fid, "</coordinates>\n");
}

void KML_AddPolygons(int StyleType,
        int TesselateMode,
        int ExtrudeMode,
        double depth,
        int Base,
        char *Altmode,
        double * lat,
        double *lon,
        int * elCon,
        ng_size_t * dsizes_elCon,
        int *nCon,
        FILE *fid) {
    int i, j, nodeInd;
    int nElement = dsizes_elCon[0];
    int MaxElCount = dsizes_elCon[1];
    char Name[30];

    printf("Number of elements=%d\n", nElement);
    for (i = 0; i < nElement; i++) {
        KML_OpenPlaceMark(fid);
        sprintf(Name, "Cell #%d", i);
        KML_AddNameRecord(fid, Name);
        KML_UseStyle(fid, 0);
        KML_OpenPolygon(fid);

        KML_SetTesselateMode(fid, TesselateMode);
        KML_SetExtrude(fid, ExtrudeMode);
        KML_SetAltitudeMode(fid, Altmode);
        KML_OpenouterBoundaryIs(fid);
        KML_OpenLinearRing(fid);
        KML_Opencoordinates(fid);
        for (j = 0; j < nCon[i]; j++) {
            nodeInd = elCon[i * MaxElCount + j] - Base;
            fprintf(fid, "%f,%f,%f\n", lon[nodeInd], lat[nodeInd], depth);
        }
        nodeInd = elCon[i * MaxElCount + 0] - Base;
        fprintf(fid, "%f,%f,%f\n", lon[nodeInd], lat[nodeInd], depth);

        KML_Closecoordinates(fid);
        KML_CloseLinearRing(fid);
        KML_CloseouterBoundaryIs(fid);
        KML_ClosePolygon(fid);
        KML_ClosePlaceMark(fid);
    }
}
