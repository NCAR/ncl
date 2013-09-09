void KML_AddKMLHeader(FILE *fid);
void KML_CloseKML(FILE *fid);
void KML_OpenDocument(FILE *fid);
void KML_CloseDocument(FILE *fid);
void KML_AddStyle(FILE *fid, int StyleType);
void KML_OpenFolder(FILE *fid);
void KML_CloseFolder(FILE *fid);
void KML_AddNameRecord(FILE *fid, char *nameString);
void KML_OpenPlaceMark(FILE *fid);
void KML_ClosePlaceMark(FILE *fid);
void KML_OpenPolygon(FILE *fid);
void KML_ClosePolygon(FILE *fid);
void KML_UseStyle(FILE *fid, int StyleType);
void KML_SetAltitudeMode(FILE *fid, char * Altmode);
void KML_SetTesselateMode(FILE *fid, int TesselateMode);
void KML_SetExtrude(FILE *fid, int ExtrudeMode);
void KML_OpenouterBoundaryIs(FILE *fid);
void KML_CloseouterBoundaryIs(FILE *fid);
void KML_OpenLinearRing(FILE *fid);
void KML_CloseLinearRing(FILE *fid);
void KML_Opencoordinates(FILE *fid);
void KML_Closecoordinates(FILE *fid);
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
        FILE *fid);


