
#ifndef _KML_Funcs_
#define _KML_Funcs_

#define Default_ArrowScale 10000.0
#define Default_ArrowTipScale 0.25
#define Default_ArrowTipAngle 30.0

static FILE * KML_fp;

enum AltitudeMode_t {
    KML_absolute, KML_clampToGround,
    KML_clampToSeaFloor, KML_relativeToGround,
    KML_relativeToSeaFloor
};

int NCL_OpenKMLFile(char * FileName);
void NCL_CloseKMLFile(void);

void Open_NCL_KML_Placemark(char * Placemark_Name);
void Close_NCL_KML_Placemark(void);
void add_NCL_KML_Name(char * NameStr);
void add_NCL_KML_Visibility(bool YesNo);
void add_NCL_KML_Extrude(bool YesNo);
void add_NCL_KML_Tessellate(bool YesNo);
void add_NCL_KML_StyleUrl(char * StyleStr);
void add_NCL_KML_TimeSpan(char * BeginTimeStr, char * EndTimeStr);
int add_NCL_KML_AltMode(enum AltitudeMode_t AltMode);
void add_NCL_KML_Coordinates(float * lat, float * lon, float * h, int nPoint);
void add_NCL_KML_LineString(int nPoint, float * lat, float * lon, float * h,
        enum AltitudeMode_t AltMode, bool Extrude, bool Tessellate);
void add_NCL_KML_Arrow_Kernel(unsigned int nlat, unsigned int nlon, unsigned int lat_stride, unsigned int lon_stride,
        float * lat, float * lon,
        float * u, float * v, float * h,
        unsigned int * cIndex,
        char * BeginTimeStr, char * EndTimeStr,
        float ArrowScale,
        float ArrowTipAngle, float ArrowTipScale,
        enum AltitudeMode_t AltMode,
        bool Extrude, bool Tessellate, bool Visible);

int add_NCL_KML_2DGrid(int nlat, int nlon,
        float * lat, float * lon, float * h,
        char * StyleIDStr,
        enum AltitudeMode_t AltMode,
        bool Extrude, bool Tessellate, bool Visible);

void add_NCL_KML_UnstructGrid(int nEdges, unsigned int * verticesOnEdge,
        float * latVertex, float * lonVertex, float * hVertex,
        char * StyleIDStr,
        enum AltitudeMode_t AltMode,
        bool Extrude, bool Tessellate, bool Visible);

void Open_NCL_KML_MultiGeometry(void);
void Close_NCL_KML_MultiGeometry(void);



void NCL_GetArrow_Coords(double ArrowCoords[4][2], double lat, double lon,
        double length, double azimuth,
        double ArrowTipAngle, double ArrowTipScale,
        double a, double f);

#endif