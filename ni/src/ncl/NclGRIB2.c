#include <stdlib.h>
#include <errno.h>
#include <stdio.h>
#include <dirent.h>
#include <math.h>
#include <ctype.h>
#include <unistd.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/Callbacks.h>
#include "defs.h"
#include <netcdf.h>
#include "NclDataDefs.h"
#include "NclFileInterfaces.h"
#include "NclMdInc.h"
#include "DataSupport.h"
#include "date.h"
#include "NclGRIB.h"
#include "NclFile.h"

# include "grib2.h"
# include "NclGRIB2.h"

# define    NCL_GRIB_CACHE_SIZE     150

static void *vbuf;



unsigned int
g2rightrot(unsigned int x, unsigned int n)
{
    while (n > 0) {
        if ((x & 1) == 1)
            x = (x >> 1) | ~(~0U >> 1);
        else
            x = (x >> 1);
        n--;
    }

    return x;
}

unsigned int
g2getbits(unsigned int x, int p, int n) {
    return (x >> (p + 1 - n)) & ~(~0 << n);
}

/***
static void g2GenAtts
#if     NhlNeedProto
(Grib2ParamList *thevarrec, Grib2AttInqRecList **lat_att_list, int *nlatatts, 
Grib2AttInqRecList **lon_att_list, int *lonatts, int do_rot, int grid_oriented,
Grib2AttInqRecList **rot_att_list, int *rotatts)
#endif
);
***/
void g2GenAtts
#if NhlNeedProto
(Grib2ParamList *thevarrec, Grib2AttInqRecList **lat_att_list, int *nlatatts, 
Grib2AttInqRecList **lon_att_list, int *lonatts, int do_rot, int grid_oriented,
Grib2AttInqRecList **rot_att_list, int *rotatts)
#endif
{
#ifdef NOTNOW
	Grib2AttInqRecList* tmp_att_list_ptr;
	NclQuark *tmp_string = NULL;
	int tmp_dimsizes = 1;

	tmp_att_list_ptr = (*lat_att_list_ptr);
	(*lat_att_list_ptr) = (Grib2AttInqRecList*)NclMalloc((unsigned)sizeof(Grib2AttInqRecList));
	(*lat_att_list_ptr)->next = tmp_att_list_ptr;
	(*lat_att_list_ptr)->att_inq = (Grib2AttInqRec*)NclMalloc((unsigned)sizeof(Grib2AttInqRec));
	(*lat_att_list_ptr)->att_inq->name = NrmStringToQuark("units");
	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark("degrees_north");
	(*lat_att_list_ptr)->att_inq->thevalue = (NclMultiDValData)_NclCreateVal( NULL, NULL, Ncl_MultiDValData, 0, (void*) tmp_string, NULL, 1 , &tmp_dimsizes, PERMANENT, NULL, nclTypestringClass);
	(*nlatatts)++;

	tmp_att_list_ptr = (*lat_att_list_ptr);
	(*lat_att_list_ptr) = (Grib2AttInqRecList*)NclMalloc((unsigned)sizeof(Grib2AttInqRecList));
	(*lat_att_list_ptr)->next = tmp_att_list_ptr;
	(*lat_att_list_ptr)->att_inq = (Grib2AttInqRec*)NclMalloc((unsigned)sizeof(Grib2AttInqRec));
	(*lat_att_list_ptr)->att_inq->name = NrmStringToQuark("grid_description");
	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark(grid[thevarrec->grid_tbl_index].grid_name);
	(*lat_att_list_ptr)->att_inq->thevalue = (NclMultiDValData)_NclCreateVal( NULL, NULL, Ncl_MultiDValData, 0, (void*) tmp_string, NULL, 1 , &tmp_dimsizes, PERMANENT, NULL, nclTypestringClass);
	(*nlatatts)++;

	tmp_att_list_ptr = (*lat_att_list_ptr);
	(*lat_att_list_ptr) = (Grib2AttInqRecList*)NclMalloc((unsigned)sizeof(Grib2AttInqRecList));
	(*lat_att_list_ptr)->next = tmp_att_list_ptr;
	(*lat_att_list_ptr)->att_inq = (Grib2AttInqRec*)NclMalloc((unsigned)sizeof(Grib2AttInqRec));
	(*lat_att_list_ptr)->att_inq->name = NrmStringToQuark("long_name");
	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark("latitude");
	(*lat_att_list_ptr)->att_inq->thevalue = (NclMultiDValData)_NclCreateVal( NULL, NULL, Ncl_MultiDValData, 0, (void*) tmp_string, NULL, 1 , &tmp_dimsizes, PERMANENT, NULL, nclTypestringClass);
	(*nlatatts)++;

	tmp_att_list_ptr = (*lon_att_list_ptr);
	(*lon_att_list_ptr) = (Grib2AttInqRecList*)NclMalloc((unsigned)sizeof(Grib2AttInqRecList));
	(*lon_att_list_ptr)->next = tmp_att_list_ptr;
	(*lon_att_list_ptr)->att_inq = (Grib2AttInqRec*)NclMalloc((unsigned)sizeof(Grib2AttInqRec));
	(*lon_att_list_ptr)->att_inq->name = NrmStringToQuark("units");
	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark("degrees_east");
	(*lon_att_list_ptr)->att_inq->thevalue = (NclMultiDValData)_NclCreateVal( NULL, NULL, Ncl_MultiDValData, 0, (void*) tmp_string, NULL, 1 , &tmp_dimsizes, PERMANENT, NULL, nclTypestringClass);
	(*nlonatts)++;

	tmp_att_list_ptr = (*lon_att_list_ptr);
	(*lon_att_list_ptr) = (Grib2AttInqRecList*)NclMalloc((unsigned)sizeof(Grib2AttInqRecList));
	(*lon_att_list_ptr)->next = tmp_att_list_ptr;
	(*lon_att_list_ptr)->att_inq = (Grib2AttInqRec*)NclMalloc((unsigned)sizeof(Grib2AttInqRec));
	(*lon_att_list_ptr)->att_inq->name = NrmStringToQuark("grid_description");
	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark(grid[thevarrec->grid_tbl_index].grid_name);
(*lon_att_list_ptr)->att_inq->thevalue = (NclMultiDValData)_NclCreateVal( NULL, NULL, Ncl_MultiDValData, 0, (void*) tmp_string, NULL, 1 , &tmp_dimsizes, PERMANENT, NULL, nclTypestringClass);
	(*nlonatts)++;

	tmp_att_list_ptr = (*lon_att_list_ptr);
	(*lon_att_list_ptr) = (Grib2AttInqRecList*)NclMalloc((unsigned)sizeof(Grib2AttInqRecList));
	(*lon_att_list_ptr)->next = tmp_att_list_ptr;
	(*lon_att_list_ptr)->att_inq = (Grib2AttInqRec*)NclMalloc((unsigned)sizeof(Grib2AttInqRec));
	(*lon_att_list_ptr)->att_inq->name = NrmStringToQuark("long_name");
	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark("longitude");
	(*lon_att_list_ptr)->att_inq->thevalue = (NclMultiDValData)_NclCreateVal( NULL, NULL, Ncl_MultiDValData, 0, (void*) tmp_string, NULL, 1 , &tmp_dimsizes, PERMANENT, NULL, nclTypestringClass);
	(*nlonatts)++;

	if (do_rot) {
		Do_Rotation_Atts(NrmNULLQUARK,rot_att_list_ptr,nrotatts,grid_oriented);
		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string = NrmStringToQuark(grid[thevarrec->grid_tbl_index].grid_name);
		GribPushAtt(rot_att_list_ptr,"grid_description",tmp_string,1,nclTypestringClass); (*nrotatts)++;
	}
#endif
    return;
}

void Grib2PushAtt
#if NhlNeedProto
(Grib2AttInqRecList **att_list_ptr,char* name,void *val,int dimsize,NclObjClass type) 
#else
(att_list_ptr,name,val,dimsize,type) 
Grib2AttInqRecList **att_list_ptr;
char* name;
void *val;
int dimsize;
NclObjClass type;
#endif
{
	Grib2AttInqRecList* tmp_att_list_ptr;

	tmp_att_list_ptr = (*att_list_ptr);
	(*att_list_ptr) = (Grib2AttInqRecList*)NclMalloc((unsigned)sizeof(Grib2AttInqRecList));
	(*att_list_ptr)->next = tmp_att_list_ptr;
	(*att_list_ptr)->att_inq = (Grib2AttInqRec*)NclMalloc((unsigned)sizeof(Grib2AttInqRec));
	(*att_list_ptr)->att_inq->name = NrmStringToQuark(name);
	(*att_list_ptr)->att_inq->thevalue = (NclMultiDValData)_NclCreateVal(NULL, NULL,
            Ncl_MultiDValData, 0, (void *) val, NULL, 1 , &dimsize, PERMANENT, NULL, type);
}


void g2GetThinnedLonParams
# if NhlNeedProto
(G2_GDS *gds,
 int nlat,
 int lo1,
 int lo2,
 int idir,
 int *nlon,
 double *di
)
# else
(gds, nlat, lo1, lo2, idir, nlon, di)
    G2_GDS *gds,
    int nlat;
    int lo1;
    int lo2;
    int idir;
    int *nlon;
    double *di;
# endif
{
    int pl_ix;
    int nmax = 0;
    int max_ix = 0;
    int i,
        n;
    int diff;

    *nlon = 0;
    pl_ix = gds->grid_list_num_oct_num;

    if (pl_ix <= 0) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
            "NclGRIB: Invalid thinned longitude grid");
        return;
    }

    for (i = 0; i < nlat; i++) {
        n = gds->grid_list_num_oct_opt[i];
        if (n > nmax) {
            nmax = n;
            max_ix = i;
        }
    }

    if (nmax == 0) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
            "NclGRIB: Invalid thinned longitude grid");
        return;
    }

    *nlon = nmax;
    if (idir == 1) {
        while (lo2 < lo1) {
/*            lo2 += 360000;*/
            lo2 += 360;
        }
        diff = lo2 - lo1;
    } else {
        while (lo1 < lo2) {
/*            lo1 += 360000;*/
            lo1 += 360;
        }
        diff = lo1 - lo2;
    }

    *di =  diff / (double) (*nlon - 1);

    return;
}

void g2GetThinnedLatParams
# if NhlNeedProto
/*(unsigned char *gds,*/
 (G2_GDS *gds,
 int nlon,
 int la1,
 int la2,
 int jdir,
 int *nlat,
 double *dj
)
# else
(gds, nlon, la1, la2, jdir, nlat, dj)
/*    unsigned char *gds;*/
    G2_GDS *gds,
    int nlon;
    int la1;
    int la2;
    int jdir;
    int *nlat;
    double *dj;
# endif
{
	
    int pl_ix;
    int nmax = 0;
    int max_ix = 0;
    int i,
        n;
    int diff;

    *nlat = 0;
/*    pl_ix = (gds[4] == 255) ? -1 : (int) gds[3] * 4 + (int) gds[4] - 1;*/
    pl_ix = gds->grid_list_num_oct_num;

    if (pl_ix == -1) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
            "NclGRIB: Invalid thinned latitude grid");
        return;
    }

    for (i = 0; i < nlon; i++) {
/*        n = CnvtToDecimal(2,&(gds[pl_ix + i * 2]));*/
        n = gds->grid_list_num_oct_opt[i];
        if (n > nmax) {
            nmax = n;
            max_ix = i;
        }
    }

    if (nmax == 0) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
            "NclGRIB: Invalid thinned latitude grid");
        return;
    }

    *nlat = nmax;
    if (jdir == 1) {
        diff = la2 - la1;
    } else {
        diff = la1 - la2;
    }

    *dj =  diff / (double)(*nlat - 1);
    
    return;
}

void g2GenLatLon 
#if NhlNeedProto
(Grib2ParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon,int xsize,int ysize, float lon_start,float lat_start, float lon_dir, float lat_dir)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon,xsize,ysize, lon_start,lat_start, lon_dir, lat_dir)
Grib2ParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
int xsize;
int ysize;
float lon_start;
float lat_start;
float lon_dir;
float lat_dir;
#endif
{
	int j;

*lat = (float*)NclMalloc(sizeof(float)*ysize);
*lon = (float*)NclMalloc(sizeof(float)*xsize);
    *dimsizes_lat = (int*)NclMalloc(sizeof(int));
    *dimsizes_lon = (int*)NclMalloc(sizeof(int));
    *n_dims_lat = 1;
    *n_dims_lon = 1;
    (*dimsizes_lat)[0] = ysize;
    (*dimsizes_lon)[0] = xsize;

    for (j = 0; j < ysize; j++) {
        (*lat)[j] = lat_start + lat_dir * j;
    }

    for (j = 0; j < xsize; j++) {
        (*lon)[j] = lon_start + lon_dir * j;
    }

	return;
}


void g2GetAtts_1
#if NhlNeedProto
(Grib2ParamList* thevarrec, Grib2AttInqRecList **lat_att_list_ptr, int *nlatatts,
 Grib2AttInqRecList **lon_att_list_ptr, int *nlonatts, int do_rot, int grid_oriented,
 Grib2AttInqRecList **rot_att_list_ptr, int *nrotatts)
#else
(thevarrec,lat_att_list_ptr, nlatatts, lon_att_list_ptr, nlonatts, do_rot,rot_att_list_ptr, nrotatts)
    Grib2ParamList *thevarrec;
    Grib2AttInqRecList **lat_att_list_ptr;
    int *nlatatts; 
    Grib2AttInqRecList **lon_att_list_ptr;
    int *nlonatts;
    int do_rot;
    int grid_oriented;
    Grib2AttInqRecList **rot_att_list_ptr;
    int *nrotatts;
#endif
{
	Grib2AttInqRecList* tmp_att_list_ptr;
	NclQuark *tmp_string = NULL;
	float *tmp_float = NULL;
	int tmp_dimsizes = 1;


	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark("MERCATOR");
	GribPushAtt(lat_att_list_ptr,"mpProjection",tmp_string,1,nclTypestringClass); (*nlatatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 0.0;
	GribPushAtt(lat_att_list_ptr,"mpCenterLatF",tmp_float,1,nclTypefloatClass); (*nlatatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 180.0;
	GribPushAtt(lat_att_list_ptr,"mpCenterLonF",tmp_float,1,nclTypefloatClass); (*nlatatts)++;

	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark("MERCATOR");
	GribPushAtt(lon_att_list_ptr,"mpProjection",tmp_string,1,nclTypestringClass); (*nlonatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 0.0;
	GribPushAtt(lon_att_list_ptr,"mpCenterLatF",tmp_float,1,nclTypefloatClass); (*nlonatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 180.0;
	GribPushAtt(lon_att_list_ptr,"mpCenterLonF",tmp_float,1,nclTypefloatClass); (*nlonatts)++;

    g2GenAtts(thevarrec, lat_att_list_ptr, nlatatts, lon_att_list_ptr, nlonatts,
            do_rot, grid_oriented, rot_att_list_ptr, nrotatts);

    return;
}


void g2GetGrid_0
#if NhlNeedProto
(
	Grib2ParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	int** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	int** dimsizes_lon, 
	float **rot,
	Grib2AttInqRecList** lat_att_list, 
	int* nlatatts, 
	Grib2AttInqRecList** lon_att_list, 
	int* nlonatts,
	Grib2AttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
    Grib2ParamList *thevarrec; 
    float **lat; 
    int *n_dims_lat;
    int **dimsizes_lat;
    float **lon;
    int *n_dims_lon;
    int **dimsizes_lon;
    float **rot;
    Grib2AttInqRecList **lat_att_list; 
    int* nlatatts; 
    Grib2AttInqRecList **lon_att_list; 
    int *nlonatts;
    Grib2AttInqRecList **rot_att_list;
    int *nrotatts;
#endif
{
	int kgds[3];

	kgds[0] = 0;
	kgds[1] = 144;
	kgds[2] = 73;

	g2GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon,
            144, 73, 0.0, 90.0, 2.5, -2.5);

	return;
}

void g2GDSCEGrid
# if NhlNeedProto
(Grib2ParamList *thevarrec, float **lat, int *n_dims_lat, int **dimsizes_lat,
 float **lon, int *n_dims_lon, int **dimsizes_lon, float **rot, int *n_dims_rot,
 int **dimsizes_rot, Grib2AttInqRecList **lat_att_list, int *nlatatts, 
 Grib2AttInqRecList **lon_att_list, int *nlonatts, Grib2AttInqRecList **rot_att_list,
 int *nrotatts
)
# else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon,
 rot, n_dims_rot, dimsizes_rot, lat_att_list, nlatatts, lon_att_list, nlonatts,
 rot_att_list, nrotatts)
    Grib2ParamList *thevarrec; 
    float **lat; 
    int *n_dims_lat;
    int **dimsizes_lat;
    float **lon;
    int *n_dims_lon;
    int **dimsizes_lon;
    float **rot;
    int *n_dims_rot;
    int **dimsizes_rot;
    Grib2AttInqRecList **lat_att_list; 
    int *nlatatts; 
    Grib2AttInqRecList **lon_att_list; 
    int *nlonatts;
    Grib2AttInqRecList **rot_att_list;
    int *nrotatts;
# endif
{
    G2_GDS *gds;
    int la1;
    int lo1;
    int la2;
    int lo2;
    double di;
    double dj;
/*    int latXlon;*/
    int idir;
    int jdir;
    int has_dir_inc;
    int vectors;
    int is_thinned_lat;
    int is_thinned_lon;
    unsigned char tmp[4];
    int sign;
    int i;
    float *tmp_float;
    NclQuark* tmp_string;
    int nlon, nlat;
    int itmp;
	
	
    *lat = NULL;
    *n_dims_lat = 0;
    *dimsizes_lat = NULL;
    *lon = NULL;
    *n_dims_lon= 0;
    *dimsizes_lon= NULL;
    if ((thevarrec->thelist == NULL)  ||  (thevarrec->thelist->rec_inq == NULL)) 
        return;

    gds = (G2_GDS *) thevarrec->thelist->rec_inq->gds;
    if (gds == NULL) {
        return;
    }

    nlon = gds->shape_of_earth->npts_along_meridian;
    nlat = gds->shape_of_earth->npts_along_parallel;

    /* all bits set indicates missing: missing means thinned */
    is_thinned_lon = (nlon == -1); 
    is_thinned_lat = (nlat == -1);
    if ((nlon <= 1 && !is_thinned_lon) || (nlat <= 1 && !is_thinned_lat)) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
            "GdsCEGrid: Invalid grid detected");
        *lat = NULL;
        *n_dims_lat = 0;
        *dimsizes_lat = NULL;
        *lon = NULL;
        *n_dims_lon = 0;
        *dimsizes_lon = NULL;
        return;
    }

    idir = gds->scan_mode->idir;
    jdir = gds->scan_mode->jdir;
    la1 = gds->lat_first_gridpt;
    lo1 = gds->lon_first_gridpt;

    la2 = gds->lat_last_gridpt;
    lo2 = gds->lon_last_gridpt;
    has_dir_inc = gds->scan_mode->scan_dir;

    if (is_thinned_lon) {
        g2GetThinnedLonParams(gds, nlat, lo1, lo2, idir, &nlon, &di);
    } else {
        /* 
         * This is more accurate in any case: do it this way
         * Not specified: must be calculated from the endpoints and number of steps
         */

        /*
         * Adapted from the NCEP code: it should account for all cases of
         * modular longitude values 
         */
        di = 1000 * ((fmod((lo2 - lo1) * 1e-3 - 1.0 + 3600.0, 360.0) +1.0) / (double) (nlon - 1));
        if (di < 0)
            di = -di;
    }

    if (is_thinned_lat) {
        g2GetThinnedLatParams(gds, nlon, la1, la2, jdir, &nlat, &dj);
    } else {
        /* Not specified: must be calculated from the endpoints and number of steps */
        dj = (la2 - la1) / (double) (nlat - 1);
        if (dj < 0)
            dj = -dj;
	}
			
    *dimsizes_lat = (int *) NclMalloc(sizeof(int));
    *dimsizes_lon = (int *) NclMalloc(sizeof(int));
    *(*dimsizes_lon) = nlon;
    *(*dimsizes_lat) = nlat;
    *n_dims_lat = 1;
    *n_dims_lon = 1;
    *lat = (float *) NclMalloc((unsigned)sizeof(float) * nlat);
    *lon = (float *) NclMalloc((unsigned)sizeof(float) * nlon);

    for (i = 0; i < *(*dimsizes_lat) ; i++)
        (*lat)[i] = (float) (la1 + jdir * i * dj) ;

    for (i = 0; i < *(*dimsizes_lon) ; i++)
        (*lon)[i] = (float)(lo1 + idir * i * di) ;
	
	if (lon_att_list != NULL) {
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = la1;
		Grib2PushAtt(lon_att_list,"La1",tmp_float,1,nclTypefloatClass); (*nlonatts)++;

		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = lo1;
		Grib2PushAtt(lon_att_list,"Lo1",tmp_float,1,nclTypefloatClass);
        (*nlonatts)++;

		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = la2;
		Grib2PushAtt(lon_att_list,"La2",tmp_float,1,nclTypefloatClass);
        (*nlonatts)++;

		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = lo2;
		Grib2PushAtt(lon_att_list,"Lo2",tmp_float,1,nclTypefloatClass);
        (*nlonatts)++;

		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = di;
		Grib2PushAtt(lon_att_list,"Di",tmp_float,1,nclTypefloatClass);
        (*nlonatts)++;

		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = dj;
		Grib2PushAtt(lon_att_list,"Dj",tmp_float,1,nclTypefloatClass);
        (*nlonatts)++;

		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string = NrmStringToQuark("degrees_east");
		Grib2PushAtt(lon_att_list,"units",tmp_string,1,nclTypestringClass);
        (*nlonatts)++;

		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		if (is_thinned_lat || is_thinned_lon)
			*tmp_string = NrmStringToQuark(
                    "Cylindrical Equidistant Projection Grid (Quasi-Regular)");
		else 
			*tmp_string = NrmStringToQuark("Cylindrical Equidistant Projection Grid");
		Grib2PushAtt(lon_att_list,"GridType",tmp_string,1,nclTypestringClass);
        (*nlonatts)++;

		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string = NrmStringToQuark("longitude");
		Grib2PushAtt(lon_att_list,"long_name",tmp_string,1,nclTypestringClass);
        (*nlonatts)++;
	}

	if (lat_att_list != NULL) {
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = la1;
		Grib2PushAtt(lat_att_list,"La1",tmp_float,1,nclTypefloatClass); (*nlatatts)++;

		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = lo1;
		Grib2PushAtt(lat_att_list,"Lo1",tmp_float,1,nclTypefloatClass); (*nlatatts)++;

		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = la2;
		Grib2PushAtt(lat_att_list,"La2",tmp_float,1,nclTypefloatClass); (*nlatatts)++;

		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = lo2;
		Grib2PushAtt(lat_att_list,"Lo2",tmp_float,1,nclTypefloatClass); (*nlatatts)++;

		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = di;
		Grib2PushAtt(lat_att_list,"Di",tmp_float,1,nclTypefloatClass); (*nlatatts)++;

		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = dj;
		Grib2PushAtt(lat_att_list,"Dj",tmp_float,1,nclTypefloatClass); (*nlatatts)++;

		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string = NrmStringToQuark("degrees_north");
		Grib2PushAtt(lat_att_list,"units",tmp_string,1,nclTypestringClass); (*nlatatts)++;

		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		if (is_thinned_lon || is_thinned_lat)
			*tmp_string = NrmStringToQuark(
                    "Cylindrical Equidistant Projection Grid (Quasi-Regular)");
		else 
			*tmp_string = NrmStringToQuark("Cylindrical Equidistant Projection Grid");
		Grib2PushAtt(lat_att_list,"GridType",tmp_string,1,nclTypestringClass); (*nlatatts)++;

		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string = NrmStringToQuark("latitude");
		Grib2PushAtt(lat_att_list,"long_name",tmp_string,1,nclTypestringClass); (*nlatatts)++;
	}

    return;
}


static void _g2NclNewGridCache
#if NhlNeedProto
(Grib2FileRecord *therec,int grid_number,int has_gds,int gds_tbl_index,int n_dims_lat,int *dimsizes_lat,int n_dims_lon,int *dimsizes_lon)
#else
(therec,grid_number,has_gds,gds_tbl_index,n_dims_lat,dimsizes_lat,n_dims_lon,dimsizes_lon)
Grib2FileRecord *therec;
int grid_number;
int has_gds;
int gds_tbl_index;
int n_dims_lat;
int *dimsizes_lat;
int n_dims_lon;
int *dimsizes_lon;
#endif
{
	NclGrib2CacheList *newlist;
	if(therec->grib_grid_cache == NULL) {
		therec->grib_grid_cache = NclMalloc(sizeof(NclGrib2CacheList));
		newlist = NULL;
	} else {
		newlist = therec->grib_grid_cache;
                therec->grib_grid_cache = NclMalloc(sizeof(NclGrib2CacheList));
	}
		
	therec->grib_grid_cache->grid_number = grid_number;
	therec->grib_grid_cache->has_gds = has_gds;
	therec->grib_grid_cache->grid_gds_tbl_index = gds_tbl_index;
	if((n_dims_lon == 1) &&(n_dims_lat ==1)) {
		therec->grib_grid_cache->n_dims = 2;
		therec->grib_grid_cache->dimsizes[1] = *dimsizes_lon;
		therec->grib_grid_cache->dimsizes[0] = *dimsizes_lat;
	} else if((n_dims_lon ==2) &&(n_dims_lat ==2)&&(dimsizes_lon[0] == dimsizes_lat[0])&&(dimsizes_lon[1] == dimsizes_lat[1])) {
		therec->grib_grid_cache->n_dims = 2;
		therec->grib_grid_cache->dimsizes[1] = dimsizes_lon[1];
		therec->grib_grid_cache->dimsizes[0] = dimsizes_lon[0];
	} else {
		if(n_dims_lat ==2) {
			therec->grib_grid_cache->n_dims = 2;
			therec->grib_grid_cache->dimsizes[1] = dimsizes_lat[1];
			therec->grib_grid_cache->dimsizes[0] = dimsizes_lat[0];
		} else if(n_dims_lon ==2) {
			therec->grib_grid_cache->n_dims = 2;
			therec->grib_grid_cache->dimsizes[1] = dimsizes_lon[1];
			therec->grib_grid_cache->dimsizes[0] = dimsizes_lon[0];
		}
	}
	therec->grib_grid_cache->n_entries = 0;
	therec->grib_grid_cache->thelist = NULL;
	therec->grib_grid_cache->next = newlist;

    return;
}

static void _g2NclNewSHGridCache
#if NhlNeedProto
(Grib2FileRecord *therec,int grid_number,int has_gds,int gds_tbl_index,int n_dims_lat,int *dimsizes_lat,int n_dims_lon,int *dimsizes_lon)
#else
(therec,grid_number,has_gds,gds_tbl_index,n_dims_lat,dimsizes_lat,n_dims_lon,dimsizes_lon)

Grib2FileRecord *therec;
int grid_number;
int has_gds;
int gds_tbl_index;
int n_dims_lat;
int *dimsizes_lat;
int n_dims_lon;
int *dimsizes_lon;
#endif
{
	NclGrib2CacheList *newlist;
	if(therec->grib_grid_cache == NULL) {
		therec->grib_grid_cache = NclMalloc(sizeof(NclGrib2CacheList));
		newlist = NULL;
	} else {
		newlist = therec->grib_grid_cache;
                therec->grib_grid_cache = NclMalloc(sizeof(NclGrib2CacheList));
	}
		
	therec->grib_grid_cache->grid_number = grid_number;
	therec->grib_grid_cache->has_gds = has_gds;
	therec->grib_grid_cache->grid_gds_tbl_index = gds_tbl_index;
	therec->grib_grid_cache->n_dims = 3;
	therec->grib_grid_cache->dimsizes[0] = 2;
	therec->grib_grid_cache->dimsizes[2] = *dimsizes_lon;
	therec->grib_grid_cache->dimsizes[1] = *dimsizes_lat;
	therec->grib_grid_cache->n_entries = 0;
	therec->grib_grid_cache->thelist = NULL;
	therec->grib_grid_cache->next = newlist;

    return;
}



static void g2Merge2
#if 	NhlNeedProto
(int *tmp_lvs,int *tmp_lvs1,int *tmp_n_lvs,int *lv_vals,int *lv_vals1,int n_lv,int** out_lvs0,int **out_lvs1)
#else
(tmp_lvs,tmp_lvs,tmp_n_lvs,lv_vals,lv_vals1,n_lv,out_lvs0,out_lvs1)
int *tmp_lvs;
int *tmp_lvs1;
int *tmp_n_lvs;
int *lv_vals;
int *lv_vals1;
int n_lv;
int **out_lvs0;
int **out_lvs1;
#endif
{
	int i,j,k;
	int *tmp_out_lvs = NULL;
	int *tmp_out_lvs1 = NULL;

	i = 0;	
	j = 0;
	k = 0;

	tmp_out_lvs = (int*)NclMalloc((unsigned)sizeof(int)*(*tmp_n_lvs + n_lv));
	tmp_out_lvs1 = (int*)NclMalloc((unsigned)sizeof(int)*(*tmp_n_lvs + n_lv));


		
	while((i < *tmp_n_lvs)&&(j< n_lv)) {
		if((tmp_lvs[i] == lv_vals[j])&&(tmp_lvs1[i] == lv_vals1[j])) {
			tmp_out_lvs[k] = tmp_lvs[i];
			tmp_out_lvs1[k] = tmp_lvs1[i];
			i++;
			j++;
			k++;
		} else if((tmp_lvs[i] < lv_vals[j])||((tmp_lvs[i] == lv_vals[j])&&(tmp_lvs1[i] != lv_vals1[j]))){
			tmp_out_lvs[k] = tmp_lvs[i];
			tmp_out_lvs1[k] = tmp_lvs1[i];
			k++;
			i++;
		} else {
			tmp_out_lvs[k] = lv_vals[j];
			tmp_out_lvs1[k] = lv_vals1[j];
			k++;
			j++;
		}
	}
	if(i< *tmp_n_lvs) {
		for( ; i < *tmp_n_lvs;i++) {
			tmp_out_lvs[k] = tmp_lvs[i];
			tmp_out_lvs1[k] = tmp_lvs1[i];
			k++;
		}	
	} else {
		for( ; j < n_lv ;j++) {
			tmp_out_lvs[k] = lv_vals[j];
			tmp_out_lvs1[k] = lv_vals1[j];
			k++;
		}	
	}
	

	NclFree(tmp_lvs);
	NclFree(tmp_lvs1);
	*tmp_n_lvs = k;	
	*out_lvs0 = tmp_out_lvs;
	*out_lvs1 = tmp_out_lvs1;

	return;
}

static int *g2Merge
#if 	NhlNeedProto
(int *tmp_lvs,int *tmp_n_lvs,int *lv_vals,int n_lv)
#else
(tmp_lvs,tmp_n_lvs,lv_vals,n_lv)
int *tmp_lvs;
int *tmp_n_lvs;
int *lv_vals;
int n_lv;
#endif
{
	int i,j,k;
	int *out_lvs = NULL;

	i = 0;	
	j = 0;
	k = 0;

	out_lvs = (int*)NclMalloc((unsigned)sizeof(int)*(*tmp_n_lvs + n_lv));

		
	while ((i < *tmp_n_lvs)&&(j< n_lv)) {
		if (tmp_lvs[i] == lv_vals[j]) {
			out_lvs[k] = tmp_lvs[i];
			i++;
			j++;
			k++;
		} else if(tmp_lvs[i] < lv_vals[j]) {
			out_lvs[k] = tmp_lvs[i];
			k++;
			i++;
		} else {
			out_lvs[k] = lv_vals[j];
			k++;
			j++;
		}
	}

	if (i< *tmp_n_lvs) {
		for( ; i < *tmp_n_lvs;i++) {
			out_lvs[k] = tmp_lvs[i];
			k++;
		}	
	} else {
		for( ; j < n_lv ;j++) {
			out_lvs[k] = lv_vals[j];
			k++;
		}	
	}
	

	NclFree(tmp_lvs);
	*tmp_n_lvs = k;	
	return(out_lvs);
}

static NrmQuark g2GetItQuark
#if NhlNeedProto
(G2_GIT *the_it)
#else
(the_it)
G2_GIT *the_it;
#endif
{
	int y = 0;
	unsigned short mn = 0;
	unsigned short d = 0;
	int h = 0;
	int mi = 0;
	char buffer[100];

	HeisDiffDate(1,1,the_it->year,the_it->days_from_jan1,&d,&mn,&y);

	if (mn < 10) {
		sprintf(buffer,"0%d/",mn);
	} else {
		sprintf(buffer,"%d/",mn);
	}

	if (d < 10) {
		sprintf(&(buffer[strlen(buffer)]),"0%d/",d);
	} else {
		sprintf(&(buffer[strlen(buffer)]),"%d/",d);
	}
	sprintf(&(buffer[strlen(buffer)]),"%d ",y);

	if (((int)the_it->minute_of_day / 60) < 10) {
		sprintf(&(buffer[strlen(buffer)]),"(0%d:",(int)the_it->minute_of_day / 60);
	} else {
		sprintf(&(buffer[strlen(buffer)]),"(%d:",(int)the_it->minute_of_day / 60);
	}

	if (((int)the_it->minute_of_day % 60) < 10 ) {
		sprintf(&(buffer[strlen(buffer)]),"0%d)",(int)the_it->minute_of_day % 60);
	} else {
		sprintf(&(buffer[strlen(buffer)]),"%d)",(int)the_it->minute_of_day % 60);
	}

	return(NrmStringToQuark(buffer));
}

static NrmQuark g2GetEnsQuark
#if NhlNeedProto
(G2_ENS *ens)
#else
(ens)
G2_ENS *ens;
#endif
{
	char buf[256];

	if (ens->extension_type == 0) { /* NCEP extension */
		if (ens->prod_id == 1) {
			switch (ens->type) {
			case 1:
				sprintf(buf,"%s resolution control forecast",(ens->id == 1 ? "high" : "low"));
				break;
			case 2:
				sprintf(buf,"negative pertubation # %d",ens->id);
				break;
			case 3:
				sprintf(buf,"positive pertubation # %d",ens->id);
				break;
			default:
				sprintf(buf,"type: %d, id: %d, prod_id: %d",ens->type,ens->id,ens->prod_id);

			}
		}
		else {
			sprintf(buf,"type: %d, id: %d, prod_id: %d",ens->type,ens->id,ens->prod_id);
		}
	}
	else if (ens->extension_type == 1) {  /* ECMWF local definition 1 */
		switch (ens->type) {
		case 10:
			sprintf(buf,"control forecast");
			break;
		case 11:
			if (ens->id % 2 == 0) 
				sprintf(buf,"negative pertubation # %d",ens->id / 2);
			else 
				sprintf(buf,"positive pertubation # %d",ens->id / 2 + 1);
			break;
		case 17:
			sprintf(buf,"ensemble mean");
			break;
		case 18:
			sprintf(buf,"ensemble standard deviation");
			break;
		default:
			sprintf(buf,"type: %d, id: %d",ens->type,ens->id);
		}

	}
	else {   /* other ECMWF local definitions */
		if (ens->id == 0) 
			sprintf(buf,"control forecast");
		else
			sprintf(buf,"perturbed forecast # %d",ens->id);
			
	}

	return NrmStringToQuark(buf);
}

static NclBasicDataTypes g2GribMapToNcl 
#if	NhlNeedProto
(void* the_type)
#else
(the_type)
	void *the_type;
#endif
{
	int int_or_float = *(int*)the_type;

	if(int_or_float) {
		return(NCL_int);
	} else {
		return(NCL_float);
	}
}

static void *g2GribMapFromNcl
#if	NhlNeedProto
(NclBasicDataTypes the_type)
#else
(the_type)
	NclBasicDataTypes the_type;
#endif
{
	int *tmp ;

	tmp = (int*)NclMalloc((unsigned)sizeof(int));
	
	switch(the_type) {
	case NCL_int:
		*tmp = 1;
		break;
	case NCL_float:
		*tmp = 0;
		break;
	default:
		*tmp = -1;
	}

	return ((void*)tmp);
}

static int g2LVNotEqual( Grib2RecordInqRecList *s_1, Grib2RecordInqRecList *s_2)
{

	if ((s_1->rec_inq->level0 != -1)&&(s_1->rec_inq->level1 != -1)) {
		if (s_1->rec_inq->level0 == s_2->rec_inq->level0) {
			if (s_1->rec_inq->level1 == s_2->rec_inq->level1) {
				return(0);
			} else {
				return(s_1->rec_inq->level1 - s_2->rec_inq->level1);
			}
		} else {
			return(s_1->rec_inq->level0 - s_2->rec_inq->level0);
		}
	} else {
		if (s_1->rec_inq->level0 == s_2->rec_inq->level0) {
			return(0);
		} else {
			return(s_1->rec_inq->level0 - s_2->rec_inq->level0);
		}
	} 
}


static int _g2GetTimeOffset 
# if    NhlNeedProto
(int time_indicator, unsigned char *offset)
# else
(time_indicator, offset)
int time_indicator;
unsigned char *offset;
# endif
{
    switch (time_indicator) {
        case 0: /* reference time + P1 */
        case 1: /* reference time + P1 */
            return (int)offset[0];
            break;

        case 2: /* reference time + P1 < t < reference time + P2 */
        case 3: /* Average from reference time + P1 to reference time + P2 */
        case 4: /* Accumulation from reference time + P1 to reference time + P2 */
        case 5: /* Difference from reference time + P1 to reference time + P2 */
            return (int)offset[1];
            break;

        case 6: /* Average from reference time - P1 to reference time - P2 */
            return -(int)offset[1];
            break;

        case 7: /* Average from reference time - P1 to reference time + P2 */
            return (int)offset[1];
            break;

        case 10:/* P1 occupies both bytes */
            return UnsignedCnvtToDecimal(2,offset);
            break;

        case 51:
        case 113:
        case 114:
        case 115:
        case 116:
        case 118:
        case 123:
        case 124:
            return 0;
            break;

        case 117:
            return (int)offset[0];
            break;

        default:
            NhlPError(NhlWARNING, NhlEUNKNOWN,
                "NclGRIB2: Unknown or unsupported time range indicator detected.");
            return -1;
            break;
    }
}


static int _g2GetLevels
# if NhlNeedProto
(int *l0, int *l1, int ind_l1, int l1_val)
# else
/*(l0, l1, indicator, lv)*/
(l0, l1, ind_l1, ind_l2)
int *l0;
int *l1;
int ind_l1;
int l1_val;
# endif
{
    /*
     * Special Levels.  See GRIB2 Table 4.5
     */
    if (ind_l1 < 100) {
        *l0 = -1;
        *l1 = -1;
    }

    switch (ind_l1) {
        case 1:
            /* Ground or Water Surface */
            *l0 = -1;
            *l1 = -1;
            return 1;
            break;

        case 100:
            /* Isobaric Surface */
            *l0 = l1_val;
            *l1 = -1;
            return 1;
            break;

        case 101:
            /* Mean Sea Level */
            *l0 = -1;
            *l1 = -1;
            return 1;
            break;

        case 102:
            /* Specific Altitude Above Mean Sea Level */
            *l0 = l1_val;
            *l1 = -1;
            break;

        case 103:
            /* Specified Height Level Above Ground */
            *l0 = l1_val;
            *l1 = -1;
            break;

        case 104:
            /* Sigma Level */
            *l0 = l1_val;
            *l1 = -1;
            break;

        case 105:
            /* Hybrid Level */
            *l0 = l1_val;
            *l1 = -1;
            break;

        case 106:
            /* Depth Below Land Surface */
            *l0 = l1_val;
            *l1 = -1;
            break;

        case 107:
            /* Isentropic (theta) Level
            *l0 = l1_val;
            *l1 = -1;
            break;

        case 108:
            /* Level at Specified Pressure Difference from Ground to Level */
            *l0 = l1_val;
            *l1 = -1;
            break;

        case 109:
            /* Potential Vorticity Surface */
            *l0 = l1_val;
            *l1 = -1;
            break;

        case 111:
            /* Eta Level */
            *l0 = l1_val;
            *l1 = -1;
            break;

        case 117:
            /* Mixed Layer Depth */
            *l0 = l1_val;
            *l1 = -1;
            break;

        case 160:
            /* Depth Below Sea Level */
            *l0 = l1_val;
            *l1 = -1;
            break;

        case 200:
            /* Entire Atmosphere (considered as a single layer */
            *l0 = l1_val;
            *l1 = -1;
            break;

        case 201:
            /* Entire Ocean (considered as a single layer */
            *l0 = l1_val;
            *l1 = -1;
            break;

        default: 
            *l0 = -1;
            *l1 = -1;
            break;
    }

    return 0;
}


static int g2level_comp
#if	NhlNeedProto
(Const void *s1, Const void *s2)
#else
(s1, s2)
void *s1;
void *s2;
#endif
{
	Grib2RecordInqRecList *s_1 = *(Grib2RecordInqRecList**)s1;
	Grib2RecordInqRecList *s_2 = *(Grib2RecordInqRecList**)s2;

	if((s_1->rec_inq->level0 != -1)&&(s_1->rec_inq->level1 != -1)) {
		if(s_1->rec_inq->level0 == s_2->rec_inq->level0) {
			if(s_1->rec_inq->level1 == s_2->rec_inq->level1) {
				return(s_1->rec_inq->level1 - s_2->rec_inq->level1);
			}
		} else {
			return(s_1->rec_inq->level0 - s_2->rec_inq->level0);
		}
	} else {
		if(s_1->rec_inq->level0 == s_2->rec_inq->level0) {
			return(s_1->rec_inq->level0 - s_2->rec_inq->level0);
		}
	} 
}

static int g2date_comp
#if 	NhlNeedProto
(Const void *s1, Const void *s2)
#else
(s1, s2)
void *s1;
void *s2;
#endif
{
	Grib2RecordInqRecList *s_1 = *(Grib2RecordInqRecList**)s1;
	Grib2RecordInqRecList *s_2 = *(Grib2RecordInqRecList**)s2;
	int d1,m1,year1,d2,m2,year2;
	short result = 0;

	result = s_1->rec_inq->initial_time.year - s_2->rec_inq->initial_time.year;
	if(!result) {
		result = s_1->rec_inq->initial_time.days_from_jan1 - s_2->rec_inq->initial_time.days_from_jan1;
		if(!result) {
			result = s_1->rec_inq->initial_time.minute_of_day - s_2->rec_inq->initial_time.minute_of_day;
			if(!result) {
				result = s_1->rec_inq->time_offset- s_2->rec_inq->time_offset;
			}
		}
		
	} 
	if (! result) {
		return(g2level_comp(s1,s2));
	}

	return result;
}

static int g2record_comp
#if 	NhlNeedProto
(Const void *s1, Const void *s2)
#else
(s1, s2)
void *s1;
void *s2;
#endif
{
	Grib2RecordInqRecList *s_1 = *(Grib2RecordInqRecList**)s1;
	Grib2RecordInqRecList *s_2 = *(Grib2RecordInqRecList**)s2;
	short result = 0;

	if (! s_1->rec_inq->is_ensemble) /* if one is an ensemble they both have to be */
		return g2date_comp(s1,s2);
	result = s_1->rec_inq->ens.extension_type - s_2->rec_inq->ens.extension_type;
	if (! result) {
		result = s_1->rec_inq->ens.prod_id - s_2->rec_inq->ens.prod_id;
	}
	if (! result) {
		result =  s_1->rec_inq->ens.type - s_2->rec_inq->ens.type;
	}
	if (! result) {
		result =  s_1->rec_inq->ens.id - s_2->rec_inq->ens.id;
	}

	if (! result) {
		return g2date_comp(s1,s2);
	}

	return result;
}

static void _g2SetAttributeLists
#if 	NhlNeedProto
(Grib2FileRecord *therec)
#else
(therec)
Grib2FileRecord *therec;
#endif
{
	Grib2ParamList *step = NULL;
	NclQuark *tmp_string = NULL;
	int *tmp_int = NULL;
	int tmp_dimsizes = 1;
	Grib2RecordInqRec *grib_rec = NULL;
	Grib2AttInqRecList *att_list_ptr= NULL;
	Grib2AttInqRec 	*att_ptr= NULL;
	int i;
	int *tmp_level = NULL;
	void *tmp_fill = NULL;


	step = therec->var_list;
	
	while(step != NULL) {
        /* Handle long_name, units, center, sub_center, model and _FillValue */
        for (i = 0; i < step->n_entries; i++) {
            if (step->thelist[i].rec_inq != NULL) {
                grib_rec = step->thelist[i].rec_inq;
                break;
            }
        }

        /* Handle coordinate attributes,  level, initial_time, forecast_time */
        if (step->yymmddhh_isatt) {
            att_list_ptr = (Grib2AttInqRecList *) NclMalloc((unsigned)sizeof(Grib2AttInqRecList));
            att_list_ptr->next = step->theatts;
            att_list_ptr->att_inq = (Grib2AttInqRec *) NclMalloc((unsigned)sizeof(Grib2AttInqRec));
            att_list_ptr->att_inq->name = NrmStringToQuark("initial_time");
            att_list_ptr->att_inq->thevalue = (NclMultiDValData)step->yymmddhh;

            /* Don't want two references */
            step->yymmddhh = NULL;
            step->theatts = att_list_ptr;
            step->n_atts++;
        }

        if (step->forecast_time_isatt) {
            att_list_ptr = (Grib2AttInqRecList *) NclMalloc((unsigned)sizeof(Grib2AttInqRecList));
            att_list_ptr->next = step->theatts;
            att_list_ptr->att_inq = (Grib2AttInqRec *) NclMalloc((unsigned)sizeof(Grib2AttInqRec));
            att_list_ptr->att_inq->name = NrmStringToQuark("forecast_time");
            att_list_ptr->att_inq->thevalue = (NclMultiDValData)step->forecast_time;

            /* Don't want two references */
            step->forecast_time= NULL;
            step->theatts = att_list_ptr;
            step->n_atts++;
        }

        if ((step->levels_isatt)&&(!step->levels_has_two)) {
            att_list_ptr = (Grib2AttInqRecList*)NclMalloc((unsigned)sizeof(Grib2AttInqRecList));
            att_list_ptr->next = step->theatts;
            att_list_ptr->att_inq = (Grib2AttInqRec*)NclMalloc((unsigned)sizeof(Grib2AttInqRec));
            att_list_ptr->att_inq->name = NrmStringToQuark("level");
            att_list_ptr->att_inq->thevalue = (NclMultiDValData)step->levels;

            /* Don't want two references */
            step->levels= NULL;
            step->theatts = att_list_ptr;
            step->n_atts++;
        } else if ((step->levels_isatt) && (step->levels_has_two)) {
            tmp_level = (int*)NclMalloc(sizeof(int) * 2);
            att_list_ptr = (Grib2AttInqRecList*)NclMalloc((unsigned)sizeof(Grib2AttInqRecList));
            att_list_ptr->next = step->theatts;
            att_list_ptr->att_inq = (Grib2AttInqRec*)NclMalloc((unsigned)sizeof(Grib2AttInqRec));
            att_list_ptr->att_inq->name = NrmStringToQuark("level");
            /* att_list_ptr->att_inq->thevalue = (NclMultiDValData)step->levels0; */
            tmp_level[0] = *(int*)step->levels0->multidval.val;
            tmp_level[1] = *(int*)step->levels1->multidval.val;
            tmp_dimsizes = 2;
            att_list_ptr->att_inq->thevalue = (NclMultiDValData)_NclCreateVal(
                    NULL, NULL, Ncl_MultiDValData, 0, (void *) tmp_level, NULL, 1, 
                    &tmp_dimsizes, PERMANENT, NULL, nclTypeintClass);
            tmp_dimsizes = 1;

            /* Don't want two references */
            _NclDestroyObj((NclObj)step->levels0);
            _NclDestroyObj((NclObj)step->levels1);
            step->levels0= NULL;
            step->levels1= NULL;
            step->theatts = att_list_ptr;
            step->n_atts++;
		}

        att_list_ptr = (Grib2AttInqRecList*)NclMalloc((unsigned)sizeof(Grib2AttInqRecList));
        att_list_ptr->next = step->theatts;
        att_list_ptr->att_inq = (Grib2AttInqRec*)NclMalloc((unsigned)sizeof(Grib2AttInqRec));
        att_list_ptr->att_inq->name = NrmStringToQuark("parameter_number");
        tmp_int = (int*)NclMalloc(sizeof(int));
        *tmp_int= grib_rec->param_number;
        att_list_ptr->att_inq->thevalue = (NclMultiDValData)_NclCreateVal(NULL, NULL,
                Ncl_MultiDValData, 0, (void *) tmp_int, NULL, 1, &tmp_dimsizes, 
                PERMANENT, NULL, nclTypeintClass);
        step->theatts = att_list_ptr;
        step->n_atts++;

/*
        att_list_ptr = (Grib2AttInqRecList*)NclMalloc((unsigned)sizeof(Grib2AttInqRecList));
        att_list_ptr->next = step->theatts;
        att_list_ptr->att_inq = (Grib2AttInqRec*)NclMalloc((unsigned)sizeof(Grib2AttInqRec));
        att_list_ptr->att_inq->name = NrmStringToQuark("gds_type");
        tmp_int = (int *) NclMalloc(sizeof(int));
        *tmp_int = grib_rec->gds_type;
        att_list_ptr->att_inq->thevalue = (NclMultiDValData)_NclCreateVal(NULL, NULL,
                Ncl_MultiDValData, 0, (void *) tmp_int, NULL, 1, &tmp_dimsizes,
                PERMANENT, NULL, nclTypeintClass);
        step->theatts = att_list_ptr;
        step->n_atts++;
*/

        att_list_ptr = (Grib2AttInqRecList*)NclMalloc((unsigned)sizeof(Grib2AttInqRecList));
        att_list_ptr->next = step->theatts;
        att_list_ptr->att_inq = (Grib2AttInqRec*)NclMalloc((unsigned)sizeof(Grib2AttInqRec));
        att_list_ptr->att_inq->name = NrmStringToQuark("grid_number");
        tmp_int = (int*)NclMalloc(sizeof(int));
        *tmp_int = grib_rec->grid_number;
        att_list_ptr->att_inq->thevalue = (NclMultiDValData)_NclCreateVal(NULL, NULL,
                Ncl_MultiDValData, 0, (void *) tmp_int, NULL, 1, &tmp_dimsizes, 
                PERMANENT, NULL, nclTypeintClass);
        step->theatts = att_list_ptr;
        step->n_atts++;

        att_list_ptr = (Grib2AttInqRecList*)NclMalloc((unsigned)sizeof(Grib2AttInqRecList));
        att_list_ptr->next = step->theatts;
        att_list_ptr->att_inq = (Grib2AttInqRec*)NclMalloc((unsigned)sizeof(Grib2AttInqRec));
        att_list_ptr->att_inq->name = NrmStringToQuark("level_indicator");
        tmp_int = (int*)NclMalloc(sizeof(int));
        *tmp_int= grib_rec->level_indicator;
        att_list_ptr->att_inq->thevalue = (NclMultiDValData)_NclCreateVal(NULL, NULL,
                Ncl_MultiDValData, 0, (void*)tmp_int, NULL, 1, &tmp_dimsizes,
                PERMANENT, NULL, nclTypeintClass);
        step->theatts = att_list_ptr;
        step->n_atts++;

        /*
         * if 2D coordinates, this adds the CF compliant attribute "coordinates", to point to the
         * auxiliary coordinate variables
         */

        if (step->aux_coords[0] != NrmNULLQUARK) {
            char buffer[80];

            att_list_ptr = (Grib2AttInqRecList*)NclMalloc((unsigned)sizeof(Grib2AttInqRecList));
            att_list_ptr->next = step->theatts;
            att_list_ptr->att_inq = (Grib2AttInqRec*)NclMalloc((unsigned)sizeof(Grib2AttInqRec));
            att_list_ptr->att_inq->name = NrmStringToQuark("coordinates");
            tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
            sprintf(buffer,"%s %s",NrmQuarkToString(step->aux_coords[0]),
                NrmQuarkToString(step->aux_coords[1]));
            *tmp_string = NrmStringToQuark(buffer);		
            att_list_ptr->att_inq->thevalue = (NclMultiDValData)_NclCreateVal(NULL, NULL,
                    Ncl_MultiDValData, 0, (void *) tmp_string, NULL, 1, &tmp_dimsizes,
                    PERMANENT, NULL, nclTypestringClass);
            step->theatts = att_list_ptr;
            step->n_atts++;
        }

        att_list_ptr = (Grib2AttInqRecList*)NclMalloc((unsigned)sizeof(Grib2AttInqRecList));
        att_list_ptr->next = step->theatts;
        att_list_ptr->att_inq = (Grib2AttInqRec*)NclMalloc((unsigned)sizeof(Grib2AttInqRec));
        att_list_ptr->att_inq->name = NrmStringToQuark(NCL_MISSING_VALUE_ATT);
        if (step->var_info.data_type == NCL_int) {
            tmp_fill = NclMalloc(sizeof(int));
            *(int *) tmp_fill = G2_DEFAULT_MISSING_INT;
            att_list_ptr->att_inq->thevalue = (NclMultiDValData)_NclCreateVal(NULL, NULL,
                    Ncl_MultiDValData, 0, (void *) tmp_fill, NULL, 1, &tmp_dimsizes,
                    PERMANENT, NULL, nclTypeintClass);
        } else {
            tmp_fill = NclMalloc(sizeof(float));
            *(float *) tmp_fill = G2_DEFAULT_MISSING_FLOAT;
            att_list_ptr->att_inq->thevalue = (NclMultiDValData)_NclCreateVal(NULL, NULL,
                    Ncl_MultiDValData, 0, (void *) tmp_fill, NULL, 1, &tmp_dimsizes, 
                    PERMANENT, NULL, nclTypefloatClass);
        }

        step->theatts = att_list_ptr;
        step->n_atts++;
		
        if (grib_rec->ptable_rec !=NULL) {
            /* units */
            att_list_ptr = (Grib2AttInqRecList*)NclMalloc((unsigned)sizeof(Grib2AttInqRecList));
            att_list_ptr->next = step->theatts;
            att_list_ptr->att_inq = (Grib2AttInqRec*)NclMalloc((unsigned)sizeof(Grib2AttInqRec));
            att_list_ptr->att_inq->name = NrmStringToQuark("units");
            tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
            *tmp_string = NrmStringToQuark(grib_rec->ptable_rec->units);		
            att_list_ptr->att_inq->thevalue = (NclMultiDValData)_NclCreateVal(NULL, NULL, 
                    Ncl_MultiDValData, 0, (void *) tmp_string, NULL, 1, &tmp_dimsizes,
                    PERMANENT, NULL, nclTypestringClass);
            step->theatts = att_list_ptr;
            step->n_atts++;

            /* long_name */
            att_list_ptr = (Grib2AttInqRecList*)NclMalloc((unsigned)sizeof(Grib2AttInqRecList));
            att_list_ptr->next = step->theatts;
            att_list_ptr->att_inq = (Grib2AttInqRec*)NclMalloc((unsigned)sizeof(Grib2AttInqRec));
            att_list_ptr->att_inq->name = NrmStringToQuark("long_name");
            tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
            *tmp_string = NrmStringToQuark(grib_rec->ptable_rec->long_name);		
            att_list_ptr->att_inq->thevalue = (NclMultiDValData)_NclCreateVal(NULL, NULL, 
                    Ncl_MultiDValData, 0, (void *) tmp_string, NULL, 1, &tmp_dimsizes,
                    PERMANENT, NULL, nclTypestringClass);
            step->theatts = att_list_ptr;
            step->n_atts++;
        } else {
            /* units */
            att_list_ptr = (Grib2AttInqRecList*)NclMalloc((unsigned)sizeof(Grib2AttInqRecList));
            att_list_ptr->next = step->theatts;
            att_list_ptr->att_inq = (Grib2AttInqRec*)NclMalloc((unsigned)sizeof(Grib2AttInqRec));
            att_list_ptr->att_inq->name = NrmStringToQuark("units");
            tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
            *tmp_string = grib_rec->units_q;		
            att_list_ptr->att_inq->thevalue = (NclMultiDValData)_NclCreateVal( NULL, NULL, 
                    Ncl_MultiDValData, 0, (void*)tmp_string, NULL, 1, &tmp_dimsizes, 
                    PERMANENT, NULL, nclTypestringClass);
            step->theatts = att_list_ptr;
            step->n_atts++;
            att_list_ptr = (Grib2AttInqRecList*)NclMalloc((unsigned)sizeof(Grib2AttInqRecList));
            att_list_ptr->next = step->theatts;
            att_list_ptr->att_inq = (Grib2AttInqRec*)NclMalloc((unsigned)sizeof(Grib2AttInqRec));
            att_list_ptr->att_inq->name = NrmStringToQuark("long_name");
            tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
            *tmp_string = grib_rec->long_name_q;		
            att_list_ptr->att_inq->thevalue = (NclMultiDValData)_NclCreateVal(NULL, NULL, 
                    Ncl_MultiDValData, 0, (void*)tmp_string, NULL, 1, &tmp_dimsizes, 
                    PERMANENT, NULL, nclTypestringClass);
            step->theatts = att_list_ptr;
            step->n_atts++;
        }

        /* center */
        att_list_ptr = (Grib2AttInqRecList*)NclMalloc((unsigned)sizeof(Grib2AttInqRecList));
        att_list_ptr->next = step->theatts;
        att_list_ptr->att_inq = (Grib2AttInqRec*)NclMalloc((unsigned)sizeof(Grib2AttInqRec));
        att_list_ptr->att_inq->name = NrmStringToQuark("center");
        tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
        *tmp_string = NrmStringToQuark(grib_rec->center);		
        att_list_ptr->att_inq->thevalue = (NclMultiDValData)_NclCreateVal(NULL, NULL,
                Ncl_MultiDValData, 0, (void *) tmp_string, NULL, 1, &tmp_dimsizes,
                PERMANENT, NULL, nclTypestringClass);
        step->theatts = att_list_ptr;
        step->n_atts++;

        /* subcenter */
        if (grib_rec->sub_center != NULL) {
            att_list_ptr = (Grib2AttInqRecList*)NclMalloc((unsigned)sizeof(Grib2AttInqRecList));
            att_list_ptr->next = step->theatts;
            att_list_ptr->att_inq = (Grib2AttInqRec*)NclMalloc((unsigned)sizeof(Grib2AttInqRec));
            att_list_ptr->att_inq->name = NrmStringToQuark("subcenter");
            tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
            *tmp_string = NrmStringToQuark(grib_rec->sub_center);
            att_list_ptr->att_inq->thevalue = (NclMultiDValData)_NclCreateVal(NULL, NULL,
                    Ncl_MultiDValData, 0, (void *) tmp_string, NULL, 1, &tmp_dimsizes,
                    PERMANENT, NULL, nclTypestringClass);
            step->theatts = att_list_ptr;
            step->n_atts++;
        }

        step = step->next;

    }

    return;
}



static void _Grib2AddInternalVar
#if	NhlNeedProto
(Grib2FileRecord *therec,NclQuark name_q,int *dim_numbers, NclMultiDValData tmp_md, Grib2AttInqRecList *attlist,int natts)
#else
(therec,dim_name_q,tmp_md)
Grib2FileRecord *therec;
NclQuark dim_name_q;
NclMultiDValData *tmp_md;
Grib2AttInqRecList *attlist;
int natts;
#endif
{
	Grib2InternalVarList *vstep; 
	Grib2InternalVarRec *tmp;
	int i;

	tmp = (Grib2InternalVarRec*)NclMalloc(sizeof(Grib2InternalVarRec));
	tmp->var_info.data_type = tmp_md->multidval.data_type;
	tmp->var_info.var_name_quark = name_q;
	tmp->var_info.num_dimensions = tmp_md->multidval.n_dims;
	for (i = 0; i < tmp_md->multidval.n_dims; i++) {
/*
		tmp->var_info.dim_sizes[i] = tmp_md->multidval.dim_sizes[i];
*/
		tmp->var_info.file_dim_num[i] = dim_numbers[i];
	}

	tmp->value = tmp_md;
	vstep = (Grib2InternalVarList*)NclMalloc(sizeof(Grib2InternalVarList));
	vstep->next = therec->internal_var_list;
	vstep->int_var = tmp;
	vstep->int_var->n_atts = natts;
	vstep->int_var->theatts = attlist;
	therec->internal_var_list = vstep;
	therec->n_internal_vars++;

	return;
}


static void g2SetInitialTimeCoordinates
#if 	NhlNeedProto
(Grib2FileRecord *therec)
#else
(therec)
Grib2FileRecord *therec;
#endif
{
	Grib2DimInqRecList *step,*ptr;
	Grib2InternalVarList *vstep,*nvstep;
	Grib2DimInqRec *tmp;
	int i,j,k;

	step = therec->it_dims;
	for (i = 0; i < therec->n_it_dims; i++) {
		int dimsize;
		char buffer[128];
		char *cp;
		NrmQuark dimq,newdimq;
			

		dimq = step->dim_inq->dim_name;
		vstep = therec->internal_var_list;
		for (j = 0; j < therec->n_internal_vars; j++) {
			if (vstep->int_var->var_info.var_name_quark == dimq) {
				break;
			}
			vstep = vstep->next;
		}
		if (j == therec->n_internal_vars) {
			printf("var %s not found\n",NrmQuarkToString(dimq));
			continue;
		}
		cp = strrchr(NrmQuarkToString(dimq),'_');
		if (cp && ! strcmp(cp,"_hours")) {
			if ((NrmQuark)therec->options[GRIB2_INITIAL_TIME_COORDINATE_TYPE_OPT].values == NrmStringToQuark("hours"))
				continue;
			sprintf(buffer,NrmQuarkToString(dimq));
			cp = strrchr(buffer,'_');
			*cp = '\0';
			newdimq = NrmStringToQuark(buffer);
			nvstep = therec->internal_var_list;
			for (k = 0; k < therec->n_internal_vars; k++) {
				if (nvstep->int_var->var_info.var_name_quark == newdimq) {
					break;
				}
				nvstep = nvstep->next;
			}
			if (k == therec->n_internal_vars) {
				printf("var %s not found\n",NrmQuarkToString(newdimq));
				continue;
			}
			step->dim_inq->dim_name = newdimq;
		}
		else {
			if ((NrmQuark)therec->options[GRIB2_INITIAL_TIME_COORDINATE_TYPE_OPT].values == NrmStringToQuark("string"))
				continue;
			sprintf(buffer,"%s_hours",NrmQuarkToString(dimq));
			newdimq = NrmStringToQuark(buffer);
			nvstep = therec->internal_var_list;
			for (k = 0; k < therec->n_internal_vars; k++) {
				if (nvstep->int_var->var_info.var_name_quark == newdimq) {
					break;
				}
				nvstep = nvstep->next;
			}
			if (k == therec->n_internal_vars) {
				printf("var %s not found\n",NrmQuarkToString(newdimq));
				continue;
			}
			step->dim_inq->dim_name = newdimq;
		}
		step = step->next;
	}

	return;
}

static double  *_g2DateStringsToEncodedDoubles
#if 	NhlNeedProto
(
NrmQuark *vals,
int dimsize
	)
#else
(vals,dimsize)
NrmQuark *vals;
int dimsize;
#endif
{
	int i;
	char *str;
	double *ddates;

	ddates = NclMalloc(dimsize * sizeof(double));
	if (!ddates) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NULL;
	}

	for (i = 0; i < dimsize; i++) {
		int y,m,d,h;
		float min;
		str = NrmQuarkToString(vals[i]);
		sscanf(str,"%2d/%2d/%4d (%2d:%2d)",&m,&d,&y,&h,&min);
		ddates[i] = y * 1e6 + m * 1e4 + d * 1e2 + h + min / 60.;
	}
				       
	return ddates;
}

static double  *_g2DateStringsToHours
#if 	NhlNeedProto
(
NrmQuark *vals,
int dimsize
	)
#else
(vals,dimsize)
NrmQuark *vals;
int dimsize;
#endif
{
	int i;
	char *str;
	double *dhours;

	dhours = NclMalloc(dimsize * sizeof(double));
	if (!dhours) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NULL;
	}

	for (i = 0; i < dimsize; i++) {
		int y,m,d,h,min;
		long jddiff;
		int iyear;
		str = NrmQuarkToString(vals[i]);
		sscanf(str,"%2d/%2d/%4d (%2d:%2d)",&m,&d,&y,&h,&min);
		jddiff = HeisDayDiff(1,1,1800,d,m,y);
		dhours[i] = jddiff * 24 + h + min / 60.0;
	}
				       
	return dhours;
}



static void _g2CreateSupplementaryTimeVariables
#if 	NhlNeedProto
(Grib2FileRecord *therec)
#else
(therec)
Grib2FileRecord *therec;
#endif
{
	Grib2DimInqRecList *step,*ptr;
	Grib2InternalVarList *vstep;
	Grib2DimInqRec *tmp;
	int i,j;

	step = therec->it_dims;
	for (i = 0; i < therec->n_it_dims; i++) {
		int dimsize;
		NrmQuark *vals;
		double *dates;
		double *hours;
		char buffer[128];
		NrmQuark  *qstr;
		Grib2AttInqRecList *tmp_att_list_ptr= NULL;
		NclMultiDValData mdval;
			

		NrmQuark dimq = step->dim_inq->dim_name;
		vstep = therec->internal_var_list;
		for (j = 0; j < therec->n_internal_vars; j++) {
			if (vstep->int_var->var_info.var_name_quark == dimq) {
				break;
			}
			vstep = vstep->next;
		}
		if (j == therec->n_internal_vars) {
			printf("var %s no found\n",NrmQuarkToString(dimq));
			continue;
		}
		dimsize = vstep->int_var->value->multidval.totalelements;
		vals = (NrmQuark *)vstep->int_var->value->multidval.val;
		
		dates = _g2DateStringsToEncodedDoubles(vals,dimsize);
		hours = _g2DateStringsToHours(vals,dimsize);
		if (! (dates && hours) )
			continue;
		mdval = _NclCreateVal(NULL,NULL,Ncl_MultiDValData,0,(void*)dates,
			              NULL,1,&dimsize,TEMPORARY,NULL,nclTypedoubleClass),
		sprintf(buffer,"%s_encoded",NrmQuarkToString(dimq));
		qstr = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*qstr = NrmStringToQuark
			("yyyymmddhh.hh_frac");
		Grib2PushAtt(&tmp_att_list_ptr,"units",qstr,1,nclTypestringClass); 
		qstr = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*qstr =  NrmStringToQuark("initial time encoded as double");
		Grib2PushAtt(&tmp_att_list_ptr,"long_name",qstr,1,nclTypestringClass); 
		_Grib2AddInternalVar(therec,NrmStringToQuark(buffer),
				    &step->dim_inq->dim_number,mdval,tmp_att_list_ptr,2);
		tmp_att_list_ptr = NULL;

		mdval = _NclCreateVal(NULL,NULL,Ncl_MultiDValData,0,(void*)hours,
			              NULL,1,&dimsize,TEMPORARY,NULL,nclTypedoubleClass),
		sprintf(buffer,"%s_hours",NrmQuarkToString(dimq));
		qstr = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*qstr = NrmStringToQuark
			("hours since 1800-01-01 00:00");
		Grib2PushAtt(&tmp_att_list_ptr,"units",qstr,1,nclTypestringClass); 
		qstr = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*qstr =  NrmStringToQuark("initial time");
		Grib2PushAtt(&tmp_att_list_ptr,"long_name",qstr,1,nclTypestringClass); 
		_Grib2AddInternalVar(therec,NrmStringToQuark(buffer),
				    &step->dim_inq->dim_number,mdval,tmp_att_list_ptr,2);
		tmp_att_list_ptr = NULL;
		step = step->next;
	}

	g2SetInitialTimeCoordinates(therec);
	return;
}


static void _Grib2FreeGribRec
#if	NhlNeedProto
(Grib2RecordInqRec *grib_rec)
#else
(grib_rec)
Grib2RecordInqRec *grib_rec;
#endif
{
    if (grib_rec->var_name != NULL) {
        NclFree(grib_rec->var_name);
    }

    if (grib_rec->gds != NULL) {
        NclFree(grib_rec->gds);
    }

    if (grib_rec->pds != NULL) {
        NclFree(grib_rec->pds);
    }

    if (grib_rec->the_dat != NULL) {
        _NclDestroyObj((NclObj)grib_rec->the_dat);
    }

    NclFree(grib_rec);

    return;
}



static void _Grib2FreeParamRec
#if	NhlNeedProto
(Grib2ParamList *vstep)
#else
(vstep)
Grib2ParamList *vstep;
#endif
{
	int i;
	Grib2AttInqRecList *astep= NULL,*tmp =NULL;
	if(vstep != NULL){
		if(vstep->it_vals != NULL) {
			NclFree(vstep->it_vals);
		}

		for (i = 0; i< vstep->n_entries; i++) {
			if (vstep->thelist[i].rec_inq != NULL) {
				_Grib2FreeGribRec(vstep->thelist[i].rec_inq);
			}
		}

		if (vstep->forecast_time != NULL) {
			_NclDestroyObj((NclObj)vstep->forecast_time);
		}

		if (vstep->yymmddhh!= NULL) {
			_NclDestroyObj((NclObj)vstep->yymmddhh);
		}

		if (vstep->levels!= NULL) {
			_NclDestroyObj((NclObj)vstep->levels);
		}

		if (vstep->levels0!= NULL) {
			_NclDestroyObj((NclObj)vstep->levels0);
		}

		if (vstep->levels1!= NULL) {
			_NclDestroyObj((NclObj)vstep->levels1);
		}

		astep = vstep->theatts;
		for (i = 0; i < vstep->n_atts; i++) {
			_NclDestroyObj((NclObj)astep->att_inq->thevalue);
			NclFree(astep->att_inq);	
			tmp = astep;
			astep = astep->next;
			NclFree(tmp);
		}

		NclFree(vstep->thelist);
		NclFree(vstep);
	}

	return;
}


static Grib2ParamList *_g2NewListNode
# if    NhlNeedProto
(Grib2RecordInqRec *grib_rec)
# else
(grib_rec)
    Grib2RecordInqRec* grib_rec;
# endif
{
	Grib2ParamList *tmp = NULL;
	Grib2RecordInqRecList *list = NULL;

	tmp = (Grib2ParamList *)NclMalloc((unsigned) sizeof(Grib2ParamList));
	tmp->next = NULL;
	list = (Grib2RecordInqRecList *) NclMalloc((unsigned) sizeof(Grib2RecordInqRecList));
	list->rec_inq = grib_rec;
	list->next = NULL;
	tmp->thelist = list;
	tmp->var_info.var_name_quark = grib_rec->var_name_q;
	tmp->var_info.data_type = g2GribMapToNcl((void *) &(grib_rec->int_or_float));
	tmp->param_number = grib_rec->param_number;
	tmp->grid_number = grib_rec->grid_number;
	tmp->has_gds= grib_rec->has_gds;
	tmp->gds_type = grib_rec->gds_type;
	tmp->level_indicator = grib_rec->level_indicator;
	tmp->n_entries = 1;
	tmp->minimum_it = grib_rec->initial_time;
	tmp->time_range_indicator = grib_rec->time_range_indicator;
	tmp->time_period = grib_rec->time_period;
	tmp->time_unit_indicator = grib_rec->time_unit_indicator;
	
	tmp->levels = NULL;
	tmp->levels0 = NULL;
	tmp->levels1 = NULL;
	tmp->levels_has_two = 0;
	tmp->yymmddhh = NULL;
	tmp->forecast_time = NULL;
	tmp->n_atts = 0;

	return tmp;
}

static void _g2InsertNodeAfter
#if NhlNeedProto
(Grib2ParamList *node, Grib2ParamList *new_node)
#else
(node, new_node)
Grib2ParamList *node; 
Grib2ParamList *new_node;
#endif
{
	Grib2ParamList * tmp;

	tmp = node->next;
	node->next = new_node;
	new_node->next = tmp;

	return;
}

static Grib2RecordInqRec* _g2MakeMissingRec
#if NhlNeedProto
(void)
#else
()
#endif
{
	Grib2RecordInqRec* grib_rec = (Grib2RecordInqRec*)NclMalloc(sizeof(Grib2RecordInqRec));

	grib_rec->var_name_q = -1;
	grib_rec->param_number = -1;
	grib_rec->ptable_rec = NULL;
	grib_rec->grid_number = -1;
	grib_rec->time_offset = -1;
	grib_rec->time_period = -1;
	grib_rec->level0 = -1;
	grib_rec->level1 = -1;
	grib_rec->var_name = NULL;
	grib_rec->long_name_q = -1;
	grib_rec->units_q = -1;
/*	grib_rec->bds_off = 0;*/
	grib_rec->bds_size = 0;
	grib_rec->pds = NULL;
/*	grib_rec->pds_size = 0; */
    grib_rec->time_range_indicator = 0;
    grib_rec->time_unit_indicator = 0;
    grib_rec->per1 = 0;
    grib_rec->per2 = 0;
	grib_rec->gds = NULL;
	grib_rec->has_gds = 0;
/*	grib_rec->gds_off = 0; */
/*	grib_rec->gds_size = 0; */
	grib_rec->has_bms = 0; 
/*	grib_rec->bms_off = 0;*/
	grib_rec->bms_size = 0;
	grib_rec->the_dat = NULL;
	grib_rec->interp_method = 0;
	return(grib_rec);
	
}

static void _g2AddRecordToNode
#if NhlNeedProto
(Grib2ParamList *node, Grib2RecordInqRec* grib_rec)
#else
(node, grib_rec)
Grib2ParamList *node;
Grib2RecordInqRec* grib_rec;
#endif
{
    Grib2RecordInqRecList * grib_rec_list = (Grib2RecordInqRecList *)
        NclMalloc((unsigned) sizeof(Grib2RecordInqRecList));

    if ((grib_rec->initial_time.year < node->minimum_it.year)
            || ((grib_rec->initial_time.year == node->minimum_it.year)	
            && (grib_rec->initial_time.days_from_jan1 < node->minimum_it.days_from_jan1))
            || ((grib_rec->initial_time.year == node->minimum_it.year)
            && (grib_rec->initial_time.days_from_jan1 == node->minimum_it.days_from_jan1)
            && (grib_rec->initial_time.minute_of_day < node->minimum_it.minute_of_day))) {
		node->minimum_it = grib_rec->initial_time;
	}

    if (node->time_unit_indicator != grib_rec->time_unit_indicator) {
        NhlPError(NhlWARNING, NhlEUNKNOWN,
            "NclGRIB: Time unit indicator varies for parameter (%s), continuing anyway.",
            NrmQuarkToString(grib_rec->var_name_q));
	}

    grib_rec_list->rec_inq = grib_rec;
    grib_rec_list->next = node->thelist;
    node->thelist = grib_rec_list;
    node->n_entries++;

    return;
}

static int _g2IsDef
#if NhlNeedProto
(Grib2FileRecord *therec, int param_num)
#else
(therec, param_num)
Grib2FileRecord *therec;
int param_num;
#endif
{
    Grib2ParamList *step;

    if (therec != NULL) {
        step = therec->var_list;
        while(step != NULL) {
            if (step->param_number == param_num)
                return 1;
            step = step->next;
		}
	}

    return 0;
}

static int g2GridCompare
#if NhlNeedProto
(Grib2ParamList *step, Grib2RecordInqRec *grib_rec)
#else
(step, grib_rec)
Grib2ParamList *step;
Grib2RecordInqRec *grib_rec;
#endif
{
	Grib2RecordInqRec *compare_rec = step->thelist->rec_inq;
	int r1;

	if (step->grid_number != grib_rec->grid_number)
		return step->grid_number - grib_rec->grid_number;

	if (grib_rec->grid_number < 255)
		return 0;

	if (compare_rec->has_gds != grib_rec->has_gds) 
		return compare_rec->has_gds - grib_rec->has_gds;

	if (! grib_rec->has_gds)
		return 0;

	if (compare_rec->gds_type != grib_rec->gds_type)
		return compare_rec->gds_type - grib_rec->gds_type;

/***
	if (compare_rec->gds_size != grib_rec->gds_size)
		return compare_rec->gds_size - grib_rec->gds_size;
***/

	if (grib_rec->gds_type >= 50 && grib_rec->gds_type < 90)
		return 0;
	/* 
	 * Compare La1 Lo1 La2 Lo2 - hopefully this will give us a definitive answer
	 * NOTE: GRIB2 is "big endian" a simple memcmp() should sort by La1, Lo1, La2, Lo2 
	 */
	r1 = memcmp(&(compare_rec->La1), &(grib_rec->La2), 6);
	if (r1 != 0)
		return r1;

	return memcmp(&(compare_rec->Lo1), &(grib_rec->Lo2), 6);
}

#if 0
static int g2InsertGribRec
#if NhlNeedProto
(Grib2FileRecord *therec, Grib2ParamList *step, Grib2ParamList *prev, Grib2RecordInqRec *grib_rec)
#else
(therec, step, prev,grib_rec)
Grib2FileRecord *therec;
Grib2ParamList *step;
Grib2ParamList *prev;
Grib2RecordInqRec *grib_rec;
#endif
{
	Grib2ParamList *new;
	int gridcomp;

	if (step->param_number <  grib_rec->param_number)
		return 0;

	if (step->param_number > grib_rec->param_number) {
		new = _g2NewListNode(grib_rec);
		new->next = step;
		if (prev)
			prev->next = new;
		else
			therec->var_list = new;
		therec->n_vars++;
		return 1;
	}

	gridcomp = g2GridCompare(step,grib_rec);
	if (gridcomp < 0)
		return 0;
	if (gridcomp > 0) {
		new = _g2NewListNode(grib_rec);
		new->next = step;
		if (prev)
			prev->next = new;
		else
			therec->var_list = new;
		therec->n_vars++;
		return 1;
	}

	if (step->time_range_indicator < (int)grib_rec->pds[20])
		return 0;

	if (step->time_range_indicator > (int)grib_rec->pds[20]) {
		new = _g2NewListNode(grib_rec);
		new->next = step;
		if (prev)
			prev->next = new;
		else
			therec->var_list = new;
		therec->n_vars++;
		return 1;
	}

	if (step->time_period < (int)grib_rec->time_period)
		return 0;

	if (step->time_period > (int)grib_rec->time_period) {
		new = _g2NewListNode(grib_rec);
		new->next = step;
		if (prev)
			prev->next = new;
		else
			therec->var_list = new;
		therec->n_vars++;
		return 1;
	}

	if (step->level_indicator < grib_rec->level_indicator)
		return 0;

	if (step->level_indicator > grib_rec->level_indicator) {
		new = _g2NewListNode(grib_rec);
		new->next = step;
		if (prev)
			prev->next = new;
		else
			therec->var_list = new;
		therec->n_vars++;
		return 1;
	}

	_g2AddRecordToNode(step,grib_rec);
	return 1;
}
#endif

static int _g2FirstCheck
#if NhlNeedProto
(Grib2FileRecord *therec, Grib2ParamList *step, Grib2RecordInqRec *grib_rec)
#else
(therec, step, grib_rec)
    Grib2FileRecord *therec;
    Grib2ParamList *step;
    Grib2RecordInqRec *grib_rec;
#endif
{
    int gridcomp;

    if (step->param_number <  grib_rec->param_number)
        return 0;

    if (step->param_number > grib_rec->param_number) {
        therec->var_list = _g2NewListNode(grib_rec);
        therec->var_list->next = step;
        therec->n_vars++;
        return 1;
    }

    gridcomp = g2GridCompare(step,grib_rec);
    if (gridcomp < 0)
        return 0;

    if (gridcomp > 0) {
        therec->var_list = _g2NewListNode(grib_rec);
        therec->var_list->next = step;
        therec->n_vars++;
        return 1;
    }

    if (step->time_range_indicator < grib_rec->time_range_indicator)
        return 0;

    if (step->time_range_indicator > grib_rec->time_range_indicator) {
        therec->var_list = _g2NewListNode(grib_rec);
        therec->var_list->next = step;
        therec->n_vars++;
        return 1;
    }

    if (step->time_period < (int)grib_rec->time_period)
        return 0;

    if (step->time_period > (int)grib_rec->time_period) {
        therec->var_list = _g2NewListNode(grib_rec);
        therec->var_list->next = step;
        therec->n_vars++;
        return 1;
    }

    if (step->level_indicator < grib_rec->level_indicator)
        return 0;

    if (step->level_indicator > grib_rec->level_indicator) {
        therec->var_list = _g2NewListNode(grib_rec);
        therec->var_list->next = step;
        therec->n_vars++;
        return 1;
    }

    /* this record is belongs to existing variable */

    _g2AddRecordToNode(step,grib_rec);

    return 1;
}


static int g2AdjustedTimePeriod
#if	NhlNeedProto
(Grib2RecordInqRec *grec, int time_period, int unit_code,char *buf)
#else
(grec, time_period, unit_code, buf)
    Grib2RecordInqRec *grec;
    int time_period;
    int unit_code;
    char *buf;
#endif
{
	int days_per_month[12] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
	int month, month_days;
	int is_leap = 0;
	int ix;

    /*
     * Negative time periods are considered to be modular values, and converted to
     * positive values depending on the units. This is difficult for days, as well
     * as for any units involving years, where there is no obvious modular value.
     */
    switch (unit_code) {
        case 0: /* Minute */
            while (time_period < 0)
            time_period = 60 + time_period;
            (void) sprintf(buf, "%dmin", time_period);
            break;

        case 1: /*Hour*/
            while (time_period < 0)
                time_period = 24 + time_period;
            sprintf(buf, "%dh", time_period);
            break;

    	case 2: /* Day */
            /*
             * This oversimplifies and may need attention when there are users
             * of such time periods
             */
            /* if time period in days == a month, then switch to months */
            month = (int) grec->mon;
            month_days = days_per_month[month - 1];
            if (month == 2 && HeisLeapYear(grec->initial_time.year)) {
                month_days++;
                is_leap = 1;
            }

            if (grec->per2 == 7) {
                /*
                 * Special processing for GODAS -- although maybe it should be
                 * universal -- see if we're really talking about a monthly average
                 */
                if (grec->day - grec->per1 == 1 && grec->day + grec->per2 == month_days) {
                    sprintf(buf, "1m");
                    time_period = 1;
                }
            } else {
                ix = month - 1;
                while (time_period < 0) {
                    if (ix == 1 && is_leap) {
                        time_period = days_per_month[ix] + 1 + time_period;
                    } else {
                        time_period = days_per_month[ix] + time_period;
                    }

                    ix = (ix + 1) % 12;
                    if (ix == 0) {
                        is_leap =  HeisLeapYear(grec->initial_time.year + 1);
                    }
                }

                sprintf(buf, "%dd", time_period);
            }
		
            break;

	case 3: /*Month*/
		while (time_period < 0)
			time_period = 12 + time_period;
		sprintf(buf,"%dm",time_period);
		break;
	case 4: /*Year*/
		time_period = abs(time_period);
		sprintf(buf,"%dy",time_period);
		break;
	case 5: /*Decade (10 years)*/
		time_period = abs(time_period);
		sprintf(buf,"%dy",time_period * 10);
		break;
	case 6: /*Normal (30 years)*/
		time_period = abs(time_period);
		sprintf(buf,"%dy",time_period * 30);
		break;
	case 7: /*Century*/
		time_period = abs(time_period);
		sprintf(buf,"%dy",time_period * 100);
		break;
	case 10: /*3 hours*/
		time_period *= 3;
		while (time_period < 0)
			time_period = 24 + time_period;
		sprintf(buf,"%dh",time_period);
		time_period /= 3;
		break;
	case 11: /*6 hours*/
		time_period *= 6;
		while (time_period < 0)
			time_period = 24 + time_period;
		sprintf(buf,"%dh",time_period);
		time_period /= 6;
		break;
	case 12: /*12 hours*/
		time_period *= 12;
		while (time_period < 0)
			time_period = 24 + time_period;
		sprintf(buf,"%dh",time_period);
		time_period /= 12;
		break;
	case 254: /*Second*/
		while (time_period < 0)
			time_period = 60 + time_period;
		sprintf(buf,"%dsec",time_period);
		break;
	default: /*unknown*/
		time_period = abs(time_period);
		sprintf(buf,"%d",time_period);
		break;
	}

	return time_period;
}


static NclMultiDValData  _Grib2GetInternalVar
#if	NhlNeedProto
(Grib2FileRecord * therec,NclQuark name_q, NclGrib2FVarRec **vrec)
#else
(therec,name_q)
Grib2FileRecord * therec;
NclQuark name_q;
NclGrib2FVarRec **vrec
#endif
{
	Grib2InternalVarList *vstep; 

	vstep = therec->internal_var_list;
	while(vstep != NULL ) {
		if(vstep->int_var->var_info.var_name_quark == name_q) {
			*vrec = &vstep->int_var->var_info;
			return(vstep->int_var->value);
		} else {
			vstep = vstep->next;
		}
	}

	*vrec = NULL;
	return(NULL);
}

int g2it_equal(G2_GIT *it1, G2_GIT* it2)
{
    if ((it1->year == it2->year) &&
        (it1->days_from_jan1 == it2->days_from_jan1)
            && (it1->minute_of_day == it2->minute_of_day))
    return 1;

    return 0;
}

int g2ens_equal(G2_ENS *ens1, G2_ENS *ens2)
{
    if ((ens1->prod_id == ens2->prod_id)
            && (ens1->type == ens2->type)
            && (ens1->id == ens2->id)
            && (ens1->extension_type == ens2->extension_type))
        return 1;

    return 0;
}

int g2it_comp (G2_GIT *it1, G2_GIT* it2)
{
    int return_val;

    return_val = it1->year - it2->year;

    if (! return_val) {
        return_val = it1->days_from_jan1 - it2->days_from_jan1;
        if (! return_val) {
            return_val = it1->minute_of_day - it2->minute_of_day;
        }
    }
	
    return return_val;
}


static int g2GetLVList
# if    NhlNeedProto
(Grib2ParamList *thevar, Grib2RecordInqRecList *lstep, int** lv_vals, int** lv_vals1) 
# else
(thevar, lstep, lv_vals, lv_vals1) 
 Grib2ParamList *thevar;
 Grib2RecordInqRecList *lstep;
 int** lv_vals; 
 int** lv_vals1; 
# endif
{
    int n_lvs = 1;
    int i;
    Grib2RecordInqRecList   *strt,
                            *tmp;

    strt = lstep;
    while(strt->next != NULL) {
        if (!g2LVNotEqual(strt, strt->next)) {
            tmp = strt->next;
            strt->next = strt->next->next;
            thevar->n_entries--;
             /* dib note 2002-12-13: doesn't free the_dat -- why not?? */
            if (tmp->rec_inq->var_name != NULL)
                NclFree(tmp->rec_inq->var_name);
            NclFree(tmp->rec_inq);
            NclFree(tmp);
        } else {
            n_lvs++;
            strt = strt->next;
        }
    }

    strt = lstep;
    *lv_vals = (int *) NclMalloc((unsigned)sizeof(int) * n_lvs);
    if (strt->rec_inq->level1 != -1) {
        *lv_vals1 = (int *) NclMalloc((unsigned)sizeof(int) * n_lvs);
    }

    for (i = 0; i < n_lvs; i++) {
        (*lv_vals)[i] = strt->rec_inq->level0;
        if (strt->rec_inq->level1 != -1) {
            (*lv_vals1)[i] = strt->rec_inq->level1;
        }

        strt = strt->next;
    }
	
    return n_lvs;
}

static G2_GIT *g2MergeIT
#if 	NhlNeedProto
(G2_GIT *tmp_it_vals, int *tmp_n_it_vals, G2_GIT *it_vals,int n_it)
#else
(tmp_it_vals,tmp_n_it_vals,it_vals,n_it)
G2_GIT *tmp_it_vals;
int *tmp_n_it_vals;
G2_GIT *it_vals;
int n_it;
#endif
{
	int i,j,k;
	G2_GIT *out_it_vals = NULL;

	i = 0;	
	j = 0;
	k = 0;

	out_it_vals = (G2_GIT *) NclMalloc((unsigned) sizeof(G2_GIT) * (*tmp_n_it_vals + n_it));
		
	while((i < *tmp_n_it_vals)&&(j< n_it)) {
		if(! g2it_comp(&(tmp_it_vals[i]),&(it_vals[j]))) {
			out_it_vals[k] = tmp_it_vals[i];
			i++;
			j++;
			k++;
		} else if(g2it_comp(&(tmp_it_vals[i]),&(it_vals[j])) < 0) {
			out_it_vals[k] = tmp_it_vals[i];
			k++;
			i++;
		} else {
			out_it_vals[k] = it_vals[j];
			k++;
			j++;
		}
	}
	if(i< *tmp_n_it_vals) {
		for( ; i < *tmp_n_it_vals;i++) {
			out_it_vals[k] = tmp_it_vals[i];
			k++;
		}	
	} else {
		for( ; j < n_it ;j++) {
			out_it_vals[k] = it_vals[j];
			k++;
		}	
	}

	NclFree(tmp_it_vals);
	*tmp_n_it_vals = k;	

	return out_it_vals;
}


static G2_FTLIST *g2GetFTList
# if    NhlNeedProto
(Grib2ParamList *thevar, Grib2RecordInqRecList *step, int* n_ft, int **ft_vals,
 int* total_valid_lv, int** valid_lv_vals, int** valid_lv_vals1)
# else
(thevar, step, n_ft, ft_vals, total_valid_lv, valid_lv_vals, valid_lv_vals1)
 Grib2ParamList *thevar;
 Grib2RecordInqRecList *fstep;
 int* n_ft;
 int **ft_vals;
 int* total_valid_lv;
 int** valid_lv_vals;
 int** valid_lv_vals1;
# endif
{
    int i;
    Grib2RecordInqRecList   *strt, 
                            *fnsh,
                            *fstep,
                            *last;
    int n_fts = 0;
    int n_nodes;
    int current_offset;
    G2_FTLIST header;
    G2_FTLIST   *the_end,
                *tmp;
    int *tmp_lvs = NULL;
    int *tmp_lvs1 = NULL;
    int tmp_n_lvs = 0;


    the_end = &header;
    the_end->next = NULL;
    strt = fstep = step;
    last = fstep;
    current_offset = strt->rec_inq->time_offset;

    while (fstep->next != NULL) {
        if (fstep->next->rec_inq->time_offset != current_offset) {
            fnsh = fstep;
            last = fstep;
            fstep = fstep->next;
            fnsh->next = NULL;
            the_end->next = (G2_FTLIST *) NclMalloc((unsigned)sizeof(G2_FTLIST));
            the_end = the_end->next;
            the_end->ft = current_offset;
            the_end->thelist = strt;
            the_end->next = NULL;
            the_end->lv_vals = NULL;
            the_end->lv_vals1 = NULL;
            the_end->n_lv = 0;
            the_end->n_lv = g2GetLVList(thevar, strt, &the_end->lv_vals, &the_end->lv_vals1);

            if ((strt->rec_inq->level0 != -1) && (strt->rec_inq->level1 == -1)) {
                if (tmp_lvs == NULL) {
                    tmp_lvs = (int *) NclMalloc((unsigned)sizeof(int) * the_end->n_lv);
                    tmp_n_lvs = the_end->n_lv;
                    memcpy((void *) tmp_lvs, the_end->lv_vals, the_end->n_lv * sizeof(int));
                } else {
                    tmp_lvs = g2Merge(tmp_lvs, &tmp_n_lvs, the_end->lv_vals, the_end->n_lv);
                }
            } else if ((strt->rec_inq->level0 != -1) && (strt->rec_inq->level1 != -1)){
                /* Handle multiple value coordinate levels */
                if (tmp_lvs == NULL) {
                    tmp_lvs = (int *) NclMalloc((unsigned)sizeof(int) * the_end->n_lv);
                    tmp_lvs1 = (int *) NclMalloc((unsigned)sizeof(int) * the_end->n_lv);
                    tmp_n_lvs = the_end->n_lv;
                    memcpy((void*) tmp_lvs, the_end->lv_vals, the_end->n_lv * sizeof(int));
                    memcpy((void* )tmp_lvs1, the_end->lv_vals1, the_end->n_lv * sizeof(int));
                } else {
                    g2Merge2(tmp_lvs, tmp_lvs1, &tmp_n_lvs, the_end->lv_vals, the_end->lv_vals1,
                            the_end->n_lv, &tmp_lvs, &tmp_lvs1);
                }
            }

            strt = fstep;
            current_offset = strt->rec_inq->time_offset;
            n_fts++;
        } else {
			fstep = fstep->next;
            /***
            if (last != NULL) {
                last->next = fstep->next;
                fstep = last->next;
                thevar->n_entries--;
            }
            ***/
        }
    }

    the_end->next = (G2_FTLIST *) NclMalloc((unsigned)sizeof(G2_FTLIST));
    the_end = the_end->next;
    the_end->ft = current_offset;
    the_end->thelist = strt;
    the_end->next = NULL;
    the_end->lv_vals = NULL;
    the_end->lv_vals1 = NULL;
    the_end->n_lv = 0;
    the_end->n_lv = g2GetLVList(thevar, strt, &the_end->lv_vals, &the_end->lv_vals1);

    if ((strt->rec_inq->level0 != -1) && (strt->rec_inq->level1 == -1)) {
        if (tmp_lvs != NULL) {
            tmp_lvs = g2Merge(tmp_lvs, &tmp_n_lvs, the_end->lv_vals, the_end->n_lv);
        } else {
            tmp_lvs = (int *) NclMalloc((unsigned)sizeof(int) * the_end->n_lv);
            tmp_n_lvs = the_end->n_lv;
            memcpy((void*) tmp_lvs, the_end->lv_vals, the_end->n_lv * sizeof(int));
        }
    } else if ((strt->rec_inq->level0 != -1) && (strt->rec_inq->level1 != -1)) {
        /* Handle multiple value coordinate levels */
        if (tmp_lvs == NULL) {
            tmp_lvs = (int *) NclMalloc((unsigned)sizeof(int) * the_end->n_lv);
            tmp_lvs1 = (int *) NclMalloc((unsigned)sizeof(int) * the_end->n_lv);
            tmp_n_lvs = the_end->n_lv;
            memcpy((void *) tmp_lvs, the_end->lv_vals, the_end->n_lv * sizeof(int));
            memcpy((void *) tmp_lvs1, the_end->lv_vals1, the_end->n_lv * sizeof(int));
        } else {
            g2Merge2(tmp_lvs, tmp_lvs1, &tmp_n_lvs, the_end->lv_vals, the_end->lv_vals1,
                    the_end->n_lv, &tmp_lvs, &tmp_lvs1);
        }
    }

    n_fts++;
    *n_ft = n_fts;
    *ft_vals = NclMalloc((unsigned)sizeof(int) * n_fts);
    the_end = header.next;
    for (i = 0; i < n_fts; i++) {
        (*ft_vals)[i] = the_end->ft;
        the_end = the_end->next;
    }

    *total_valid_lv = tmp_n_lvs;
    *valid_lv_vals = tmp_lvs;
    *valid_lv_vals1 = tmp_lvs1;
    return header.next;
}


static G2_ITLIST *g2GetITList
#if 	NhlNeedProto
(Grib2ParamList *thevar, 
 Grib2RecordInqRecList *step,
 int* n_it, 
 G2_GIT **it_vals,
 int* n_ft,
 int **ft_vals,
 int* total_valid_lv,
 int** valid_lv_vals, 
 int** valid_lv_vals1)
#else
(thevar, step, n_it, it_vals, n_ft, ft_vals, total_valid_lv, valid_lv_vals, valid_lv_vals1)
 Grib2ParamList *thevar;
 Grib2RecordInqRecList *step;
 int* n_it;
 G2_GIT **it_vals;
 int* n_ft;
 int **ft_vals;
 int* total_valid_lv;
 int** valid_lv_vals;
 int** valid_lv_vals1;
#endif
{
    int i;
    Grib2RecordInqRecList   *strt,
                            *fnsh,
                            *istep,
                            *last;
    int n_its = 0;
    int n_nodes;
    G2_ITLIST header;
    G2_ITLIST   *the_end,
                *tmp;
    int tmp_n_ft;
    int *tmp_ft_vals = NULL;
    int *tmp_lvs = NULL;
    int *tmp_lvs1 = NULL;
    int tmp_n_lvs = 0;
    G2_GIT current_it;


    the_end = &header;
    the_end->next = NULL;
	
    strt = istep = step;

    last = istep;
    current_it = strt->rec_inq->initial_time;
    while (istep->next != NULL) {
        if ((istep->next->rec_inq->initial_time.year == current_it.year)
          &&(istep->next->rec_inq->initial_time.days_from_jan1 == current_it.days_from_jan1)
          &&(istep->next->rec_inq->initial_time.minute_of_day == current_it.minute_of_day)) {
            istep = istep->next;
            continue;
        }

        fnsh = istep;
        last = istep;
        istep = istep->next;
        fnsh->next = NULL;
        the_end->next = (G2_ITLIST *) NclMalloc((unsigned) sizeof(G2_ITLIST));
        the_end = the_end->next;
        the_end->it = current_it;
        the_end->next = NULL;
        the_end->ft_vals = NULL;
        the_end->lv_vals = NULL;
        the_end->lv_vals1 = NULL;
        the_end->thelist = g2GetFTList(thevar, strt, &the_end->n_ft, &the_end->ft_vals,
                                &the_end->n_lv, &the_end->lv_vals, &the_end->lv_vals1);
        if (the_end->n_ft > 0) {
            if (tmp_ft_vals == NULL) {
                tmp_ft_vals = NclMalloc((unsigned) sizeof(int) * the_end->n_ft);
                tmp_n_ft = the_end->n_ft;
                memcpy((void*) tmp_ft_vals, the_end->ft_vals, the_end->n_ft * sizeof(int));
            } else {
                tmp_ft_vals = g2Merge(tmp_ft_vals, &tmp_n_ft, the_end->ft_vals, the_end->n_ft);
            }
        }

        if ((strt->rec_inq->level0 != -1) && (strt->rec_inq->level1 == -1)) {
            if (tmp_lvs == NULL) {
                tmp_lvs = (int *) NclMalloc((unsigned)sizeof(int) * the_end->n_lv);
                tmp_n_lvs = the_end->n_lv;
                memcpy((void*) tmp_lvs, the_end->lv_vals, the_end->n_lv * sizeof(int));
            } else {
                tmp_lvs = g2Merge(tmp_lvs, &tmp_n_lvs, the_end->lv_vals, the_end->n_lv);
            }
        } else if ((strt->rec_inq->level0 != -1) && (strt->rec_inq->level1 != -1)){
            /* Handle multiple value coordinate levels */
            if (tmp_lvs == NULL) {
                tmp_lvs = (int*) NclMalloc((unsigned)sizeof(int) * the_end->n_lv);
                tmp_lvs1 = (int*) NclMalloc((unsigned)sizeof(int) * the_end->n_lv);
                tmp_n_lvs = the_end->n_lv;
                memcpy((void*) tmp_lvs, the_end->lv_vals, the_end->n_lv * sizeof(int));
                memcpy((void*) tmp_lvs1,the_end->lv_vals1, the_end->n_lv * sizeof(int));
            } else {
                g2Merge2(tmp_lvs, tmp_lvs1, &tmp_n_lvs, the_end->lv_vals, the_end->lv_vals1,
                        the_end->n_lv, &tmp_lvs, &tmp_lvs1);
            }
        }

        strt = istep;
        current_it = strt->rec_inq->initial_time;
        n_its++;
	}

    the_end->next =(G2_ITLIST *) NclMalloc((unsigned)sizeof(G2_ITLIST));
    the_end = the_end->next;
    the_end->it = current_it;
    the_end->next = NULL;
    the_end->lv_vals = NULL;
    the_end->lv_vals1 = NULL;
    the_end->n_lv = 0;
    the_end->thelist = g2GetFTList(thevar, strt, &the_end->n_ft, &the_end->ft_vals,
                        &the_end->n_lv, &the_end->lv_vals, &the_end->lv_vals1);

    if (the_end->n_ft > 0) {
        if (tmp_ft_vals == NULL) {
            tmp_ft_vals = NclMalloc((unsigned)sizeof(int) * the_end->n_ft);
            tmp_n_ft = the_end->n_ft;
            memcpy((void*) tmp_ft_vals, the_end->ft_vals, the_end->n_ft * sizeof(int));
        } else {
            tmp_ft_vals = g2Merge(tmp_ft_vals, &tmp_n_ft, the_end->ft_vals, the_end->n_ft);
        }
    }

    if ((strt->rec_inq->level0 != -1) && (strt->rec_inq->level1 == -1)) {
        if (tmp_lvs != NULL) {
            tmp_lvs = g2Merge(tmp_lvs, &tmp_n_lvs, the_end->lv_vals, the_end->n_lv);
        } else {
            tmp_lvs = (int *) NclMalloc((unsigned)sizeof(int) * the_end->n_lv);
            tmp_n_lvs = the_end->n_lv;
            memcpy((void *) tmp_lvs, the_end->lv_vals, the_end->n_lv * sizeof(int));
        }
    } else if ((strt->rec_inq->level0 != -1) && (strt->rec_inq->level1 != -1)) {
        /* Handle multiple value coordinate levels */
        if (tmp_lvs == NULL) {
            tmp_lvs = (int *) NclMalloc((unsigned)sizeof(int) * the_end->n_lv);
            tmp_lvs1 = (int *) NclMalloc((unsigned)sizeof(int) * the_end->n_lv);
            tmp_n_lvs = the_end->n_lv;
            memcpy((void*) tmp_lvs, the_end->lv_vals, the_end->n_lv * sizeof(int));
            memcpy((void*) tmp_lvs1, the_end->lv_vals1, the_end->n_lv * sizeof(int));
        } else {
            g2Merge2(tmp_lvs, tmp_lvs1, &tmp_n_lvs, the_end->lv_vals, the_end->lv_vals1,
                    the_end->n_lv, &tmp_lvs, &tmp_lvs1);
        }
    }

    n_its++;
    *n_it = n_its;
    *it_vals = NclMalloc((unsigned)sizeof(G2_GIT) * n_its);
    the_end = header.next;
    for (i = 0; i < n_its; i++) {
        (*it_vals)[i] = the_end->it;
        the_end = the_end->next;
    }

    *ft_vals = tmp_ft_vals;
    *n_ft = tmp_n_ft;
    *total_valid_lv = tmp_n_lvs;
    *valid_lv_vals = tmp_lvs;
    *valid_lv_vals1 = tmp_lvs1;
    return header.next;
}



static NhlErrorTypes _g2DetermineDimensionAndGridInfo
# if NhlNeedProto
(Grib2FileRecord *therec, Grib2ParamList* step)
# else
(therec, step)
Grib2FileRecord *therec;
Grib2ParamList* step;
# endif
{
    Grib2RecordInqRecList   *rstep,
                            *strt,
                            *fnsh,
                            *free_rec;
    int current_ens_ix;
    int n_ens = 0,
        i, j, k, m,
        icount = 0;
    G2_ENS  current_ens;
    G2_ENSLIST  header;
    G2_ENSLIST  *the_end = NULL,
                *free_ens = NULL;
    G2_ITLIST   *itstep = NULL,
                *free_it = NULL;
    G2_FTLIST   *ftstep = NULL,
                *free_ft = NULL;
    int *tmp_lv_vals = NULL;
    int *tmp_lv_vals1 = NULL;
    int n_tmp_lv_vals = 0;
    int *tmp_ft_vals = NULL;
    int n_tmp_ft_vals = 0;
    G2_GIT *tmp_it_vals = NULL;
    int n_tmp_it_vals = 0;
    NclQuark *it_vals_q = NULL;
    NclQuark *ens_vals_q = NULL;
    int *ens_indexes = NULL;
    int n_tmp_ens_vals = 0;
    G2_ENS *tmp_ens_vals = NULL;
    int total;
    int doff;
    char *name;
    NhlErrorTypes returnval = NhlNOERROR;


    doff = step->var_info.doff;
    the_end = &header;
    memset(&header, 0, sizeof(G2_ENSLIST));

    if (step->n_entries > 1) {
        strt = rstep  = step->thelist;
        current_ens = rstep->rec_inq->ens;

        while (rstep->next != NULL) {
            if (g2ens_equal(&rstep->next->rec_inq->ens, &current_ens)) {
                rstep = rstep->next;
                continue;
            }

            current_ens = rstep->next->rec_inq->ens;
            fnsh = rstep;
            rstep = rstep->next;
            fnsh->next = NULL;
		
            the_end->next = (G2_ENSLIST *) NclMalloc((unsigned int) sizeof(G2_ENSLIST));
            the_end = the_end->next;
            the_end->next = NULL;
            the_end->ens = strt->rec_inq->ens;
            the_end->ens_ix = n_ens;
            the_end->thelist = g2GetITList(step, strt, &the_end->n_it, &the_end->it_vals,
                &the_end->n_ft, &the_end->ft_vals, &the_end->n_lv, &the_end->lv_vals,
                &the_end->lv_vals1);
            strt = rstep;
            n_ens++;
        }

        the_end->next = (G2_ENSLIST *) NclMalloc((unsigned)sizeof(G2_ENSLIST));
        the_end = the_end->next;
        the_end->next = NULL;
        the_end->ens = strt->rec_inq->ens;
        the_end->ens_ix = n_ens;
        the_end->thelist = g2GetITList(step, strt, &the_end->n_it, &the_end->it_vals,
            &the_end->n_ft, &the_end->ft_vals, &the_end->n_lv, &the_end->lv_vals,
            &the_end->lv_vals1);
        n_ens++;
        name = NrmQuarkToString(strt->rec_inq->var_name_q);
        the_end = header.next;

        n_tmp_ens_vals = n_ens;	
        tmp_ens_vals = (G2_ENS *) NclMalloc(sizeof(G2_ENS) * n_ens);
        i = 0;
        while (the_end != NULL) {
            tmp_ens_vals[i] = the_end->ens;
            if ((the_end->n_lv > 0)&&(the_end->lv_vals1 == NULL) ) {
				if(tmp_lv_vals == NULL) {
					tmp_lv_vals = NclMalloc((unsigned)sizeof(int)*the_end->n_lv);
					n_tmp_lv_vals = the_end->n_lv;
					memcpy((void*)tmp_lv_vals,the_end->lv_vals,the_end->n_lv*sizeof(int));
				} else 	{
					tmp_lv_vals  = g2Merge(tmp_lv_vals,&n_tmp_lv_vals,the_end->lv_vals,the_end->n_lv);
				}
			} else {
				if(tmp_lv_vals == NULL) {
					tmp_lv_vals = NclMalloc((unsigned)sizeof(int)*the_end->n_lv);
					tmp_lv_vals1 = NclMalloc((unsigned)sizeof(int)*the_end->n_lv);
					n_tmp_lv_vals = the_end->n_lv;
					memcpy((void*)tmp_lv_vals,the_end->lv_vals,the_end->n_lv*sizeof(int));
					memcpy((void*)tmp_lv_vals1,the_end->lv_vals1,the_end->n_lv*sizeof(int));
				} else 	{
					g2Merge2(tmp_lv_vals, tmp_lv_vals1, &n_tmp_lv_vals,
                        the_end->lv_vals, the_end->lv_vals1, the_end->n_lv, &tmp_lv_vals,
                        &tmp_lv_vals1);
				}
			}

			if (the_end->n_ft > 0) {
				if(tmp_ft_vals == NULL) {
					tmp_ft_vals = NclMalloc((unsigned)sizeof(int)*the_end->n_ft);
					n_tmp_ft_vals = the_end->n_ft;
					memcpy((void *) tmp_ft_vals, the_end->ft_vals, the_end->n_ft * sizeof(int));
				} else {
                    tmp_ft_vals = g2Merge(tmp_ft_vals, &n_tmp_ft_vals, the_end->ft_vals,
                                            the_end->n_ft);
				}
			}

			if (the_end->n_it > 0) {
				if(tmp_it_vals == NULL) {
					tmp_it_vals = (G2_GIT *)NclMalloc((unsigned)sizeof(G2_GIT)*the_end->n_it);
					n_tmp_it_vals = the_end->n_it;
					memcpy((void*)tmp_it_vals,the_end->it_vals,the_end->n_it*sizeof(G2_GIT));

				} 
				else {
					tmp_it_vals = g2MergeIT(tmp_it_vals, &n_tmp_it_vals, the_end->it_vals,
                            the_end->n_it);
				}
			}

			the_end = the_end->next;
/*            (void) fprintf(stdout,"%s\n",NrmQuarkToString(it_vals_q[i]));*/
			i++;
		}
#if 0
		if (n_tmp_lv_vals > 0) {
			fprintf(stdout,"(");
			for(j = 0; j< n_tmp_lv_vals-1; j++) {	
				fprintf(stdout,"%d, ",tmp_lv_vals[j]);
			}

			fprintf(stdout,"%d)\n",tmp_lv_vals[j]);
		}

		if (n_tmp_ft_vals > 0) {
			fprintf(stdout,"(");
			for(j = 0; j< n_tmp_ft_vals-1; j++) {	
				fprintf(stdout,"%d, ",tmp_ft_vals[j]);
			}
			fprintf(stdout,"%d)\n",tmp_ft_vals[j]);
		}
#endif
		
    } else {
        n_tmp_ens_vals = 1;
        tmp_ens_vals = (G2_ENS *) NclMalloc((unsigned) sizeof(G2_ENS));
        memset(tmp_ens_vals, 0, sizeof(G2_ENS));
        header.next = (G2_ENSLIST *) NclMalloc((unsigned) sizeof(G2_ENSLIST));
        memset(header.next, 0, sizeof(G2_ENSLIST));
        memset(&(header.next->ens), 0, sizeof(G2_ENS));
        the_end->thelist = g2GetITList(step, step->thelist, &n_tmp_it_vals, &tmp_it_vals,
                &n_tmp_ft_vals, &tmp_ft_vals, &n_tmp_lv_vals, &tmp_lv_vals, &tmp_lv_vals1);
	}

    i = 0;
    step->var_info.num_dimensions = 0;
    if (n_tmp_ens_vals > 0) {
        ens_vals_q = (NclQuark *) NclMalloc(sizeof(NclQuark) * n_tmp_ens_vals);
        ens_indexes = (int *) NclMalloc(sizeof(int) * n_tmp_ens_vals);
        for (j = 0; j < n_tmp_ens_vals; j++) {
            ens_vals_q[j] = g2GetEnsQuark(&(tmp_ens_vals[j]));
            ens_indexes[j] = j;
        }

        step->ensemble = (NclMultiDValData)_NclCreateVal(
                            NULL,
                            NULL,
                            Ncl_MultiDValData,
                            0,
                            (void *) ens_vals_q,
                            NULL,
                            1,
                            (void *) &n_tmp_ens_vals,
                            TEMPORARY,
                            NULL,
                            nclTypestringClass);

        step->ens_indexes = (NclOneDValCoordData)_NclCreateVal(
                            NULL,
                            NULL,
                            Ncl_OneDValCoordData,
                            0,
                            (void *) ens_indexes,
                            NULL,
                            1,
                            (void *) &n_tmp_ens_vals,
                            TEMPORARY,
                            NULL,
                            nclTypeintClass);

    }
	
    if (n_tmp_ens_vals > 1) {
        step->var_info.dim_sizes[i] = n_tmp_ens_vals;
        step->ensemble_isatt = 0;
        i++;

    } else if (n_tmp_ens_vals == 1) {
        step->ensemble_isatt = 1;
    } else {
        step->ensemble_isatt = 0;
    }

	NclFree(tmp_ens_vals);
	if (n_tmp_it_vals > 0) {
		it_vals_q = (NclQuark *) NclMalloc(sizeof(NclQuark) * n_tmp_it_vals);
		for (j = 0; j < n_tmp_it_vals; j++)
			it_vals_q[j] = g2GetItQuark(&(tmp_it_vals[j]));
	}
	if(n_tmp_it_vals > 1 ) {
		step->var_info.dim_sizes[i] = n_tmp_it_vals;
		step->yymmddhh = (NclOneDValCoordData)_NclCreateVal(
					NULL,
					NULL,
					Ncl_OneDValCoordData,
					0,
					(void*)it_vals_q,
					NULL,
					1,
					(void*)&n_tmp_it_vals,
					TEMPORARY,
					NULL,
					nclTypestringClass);
		step->it_vals = tmp_it_vals;
			
		step->yymmddhh_isatt = 0;
		i++;
	} else if(n_tmp_it_vals == 1) {
		step->yymmddhh_isatt = 1;
		step->yymmddhh = (NclOneDValCoordData)_NclCreateVal(
					NULL,
					NULL,
					Ncl_OneDValCoordData,
					0,
					(void*)it_vals_q,
					NULL,
					1,
					(void*)&n_tmp_it_vals,
					TEMPORARY,
					NULL,
					nclTypestringClass);
		step->it_vals = tmp_it_vals;
	} else {
		step->yymmddhh_isatt = 0;
		step->yymmddhh = NULL;
		step->it_vals = NULL;
/*
		fprintf(stdout,"n_it: %d\n",n_tmp_it_vals);
*/
	}
	if(n_tmp_ft_vals > 1 ) {
		step->var_info.dim_sizes[i] = n_tmp_ft_vals;
		step->forecast_time = (NclOneDValCoordData)_NclCreateVal(
					NULL,
					NULL,
					Ncl_OneDValCoordData,
					0,
					(void*)tmp_ft_vals,
					NULL,
					1,
					(void*)&n_tmp_ft_vals,
					TEMPORARY,
					NULL,
					nclTypeintClass);
		step->forecast_time_isatt = 0;
		i++;
	} else if(n_tmp_ft_vals == 1) {
		step->forecast_time = (NclOneDValCoordData)_NclCreateVal(
					NULL,
					NULL,
					Ncl_OneDValCoordData,
					0,
					(void*)tmp_ft_vals,
					NULL,
					1,
					(void*)&n_tmp_ft_vals,
					TEMPORARY,
					NULL,
					nclTypeintClass);
		step->forecast_time_isatt = 1;
	} else {
		step->forecast_time_isatt = 0;
/*
		fprintf(stdout,"n_ft: %d\n",n_tmp_ft_vals);
*/
	}
	if((tmp_lv_vals != NULL)&&(tmp_lv_vals1 == NULL)) {
		if(n_tmp_lv_vals > 1 ) {
			step->var_info.dim_sizes[i] = n_tmp_lv_vals;
			step->levels = (NclOneDValCoordData)_NclCreateVal(
						NULL,
						NULL,
						Ncl_OneDValCoordData,
						0,
						(void*)tmp_lv_vals,
						NULL,
						1,
						(void*)&n_tmp_lv_vals,
						TEMPORARY,
						NULL,
						nclTypeintClass);
			step->levels0 = NULL;
			step->levels1 = NULL;
			i++;
				step->levels_isatt = 0;
		} else if (n_tmp_lv_vals == 1) {
			step->levels_isatt = 1;
			step->levels = (NclOneDValCoordData)_NclCreateVal(
						NULL,
						NULL,
						Ncl_OneDValCoordData,
						0,
						(void*)tmp_lv_vals,
						NULL,
						1,
						(void*)&n_tmp_lv_vals,
						TEMPORARY,
						NULL,
						nclTypeintClass);
			step->levels0 = NULL;
			step->levels1 = NULL;
		} else {
			step->levels_isatt = 0;
/*
			fprintf(stdout,"n_lv: %d\n",n_tmp_lv_vals);
*/
		}
	} else if((tmp_lv_vals != NULL)&&(tmp_lv_vals1 != NULL)) { 
		if(n_tmp_lv_vals > 1 ) {
			step->var_info.dim_sizes[i] = n_tmp_lv_vals;
			step->levels = NULL;
			step->levels0 = (NclMultiDValData)_NclCreateVal(
						NULL,
						NULL,
						Ncl_MultiDValData,
						0,
						(void*)tmp_lv_vals,
						NULL,
						1,
						(void*)&n_tmp_lv_vals,
						TEMPORARY,
						NULL,
						nclTypeintClass);
			step->levels1 = (NclMultiDValData)_NclCreateVal(
						NULL,
						NULL,
						Ncl_MultiDValData,
						0,
						(void*)tmp_lv_vals1,
						NULL,
						1,
						(void*)&n_tmp_lv_vals,
						TEMPORARY,
						NULL,
						nclTypeintClass);
			step->levels_has_two = 1;
			i++;
				step->levels_isatt = 0;
		} else if (n_tmp_lv_vals == 1) {
			step->levels_isatt = 1;
			step->levels = NULL;
			step->levels0 = (NclMultiDValData)_NclCreateVal(
						NULL,
						NULL,
						Ncl_MultiDValData,
						0,
						(void*)tmp_lv_vals,
						NULL,
						1,
						(void*)&n_tmp_lv_vals,
						TEMPORARY,
						NULL,
						nclTypeintClass);
			step->levels1 = (NclMultiDValData)_NclCreateVal(
						NULL,
						NULL,
						Ncl_MultiDValData,
						0,
						(void*)tmp_lv_vals1,
						NULL,
						1,
						(void*)&n_tmp_lv_vals,
						TEMPORARY,
						NULL,
						nclTypeintClass);
			step->levels_has_two = 1;
		} else {
			step->levels_isatt = 0;
/*
			fprintf(stdout,"n_lv: %d\n",n_tmp_lv_vals);
*/
		}
	} else {
		step->levels_isatt = 0;
	}
	step->var_info.num_dimensions = i + (doff+1);
	for (i = 0; i < step->var_info.num_dimensions; i++) {
		/* initialize the file dim number to something out of range */
		step->var_info.file_dim_num[i] = -1;
	}

    /*
     * Now call grid code to get coordinates
     *
     * Build single array of GribRecordInqRecList *'s
    */
    if (step->var_info.num_dimensions - (doff + 1) <= 0) {
        if (header.next != NULL) {
            free_ens = header.next;

        if (free_ens->lv_vals != NULL) 
            NclFree(free_ens->lv_vals);

        if (free_ens->lv_vals1 != NULL) 
            NclFree(free_ens->lv_vals1);

        if (free_ens->ft_vals != NULL) 
            NclFree(free_ens->ft_vals);

        if (free_ens->it_vals != NULL) 
            NclFree(free_ens->it_vals);

        if (free_ens->thelist != NULL) {
            if (free_ens->thelist->lv_vals != NULL)
                NclFree(free_ens->thelist->lv_vals);

            if (free_ens->thelist->lv_vals1 != NULL)
                NclFree(free_ens->thelist->lv_vals1);

            if (free_ens->thelist->ft_vals != NULL)
                NclFree(free_ens->thelist->ft_vals);

            NclFree(free_ens->thelist);
        }

        NclFree(free_ens);
    }

    return returnval;
}

	total = 1;
	for(i = 0; i < step->var_info.num_dimensions - (doff +1); i++) {
		total *= step->var_info.dim_sizes[i];
	}
	strt = (Grib2RecordInqRecList*)NclMalloc((unsigned)sizeof(Grib2RecordInqRecList)*total);
	for (i = 0; i < total; i++) {
		strt[i].rec_inq = (Grib2RecordInqRec*)10;
		strt[i].next = NULL;
	}
	the_end = header.next;
	i = 0;
	icount = 0;

#define PRINT_MISSING(ens_ix,it_ix,ft_ix,lv_ix) \
	sprintf(buf,"%s->%s is missing",NrmQuarkToString(therec->file_path_q),name); \
	if (n_tmp_ens_vals > 1) \
		sprintf(&(buf[strlen(buf)])," ens: %d",ens_ix); \
	if (n_tmp_it_vals > 1) \
		sprintf(&(buf[strlen(buf)])," it: %s",NrmQuarkToString(it_vals_q[it_ix])); \
	if (n_tmp_ft_vals > 1) \
		sprintf(&(buf[strlen(buf)])," ft: %d",tmp_ft_vals[ft_ix]); \
	if (n_tmp_lv_vals > 1) \
		if (! step->levels_has_two) \
			sprintf(&(buf[strlen(buf)])," lv: %d",tmp_lv_vals[lv_ix]); \
		else \
			sprintf(&(buf[strlen(buf)])," lv: (%d, %d)",tmp_lv_vals[lv_ix],tmp_lv_vals1[lv_ix]); \
	NhlPError(NhlWARNING,NhlEUNKNOWN,buf)
			
	while(the_end != NULL) {
		char buf[256];
		itstep = the_end->thelist;
		j = 0;
		while(itstep != NULL) {
			ftstep = itstep->thelist;
			if ((tmp_it_vals != NULL) && (! g2it_equal(&itstep->it,&(tmp_it_vals[j])))) {
				for (k = 0; k < n_tmp_ft_vals; k++) {
					for( m = 0 /* i already set */; m < n_tmp_lv_vals; i++,m++) {
						strt[i].rec_inq = NULL;
						PRINT_MISSING(the_end->ens_ix,j,k,m);
					}
				}
				j++;
				continue;
			}
			k = 0;
			while (ftstep != NULL) {
				rstep = ftstep->thelist;
				if((tmp_ft_vals != NULL)&&(ftstep->ft != tmp_ft_vals[k])){
					for( m = 0 /* i already set */; m < n_tmp_lv_vals; i++,m++) {
						strt[i].rec_inq = NULL;
						PRINT_MISSING(the_end->ens_ix,j,k,m);
					}
					k++;
					continue;
				}
				m = 0;
				if(!step->levels_has_two) {
					while(rstep != NULL) {
						if((tmp_lv_vals == NULL) ||(rstep->rec_inq->level0 == tmp_lv_vals[m])) {
							strt[i].rec_inq = rstep->rec_inq;	
							icount +=1;
							free_rec = rstep;
							rstep = rstep->next;
							NclFree(free_rec);
							m++;
						} else {
							strt[i].rec_inq = NULL;
							fprintf(stdout,"%s is missing ft: %d lv: %d\n",name,
								tmp_ft_vals[k],
								tmp_lv_vals[m]);
							m++;
						}
						i++;
					}
					if((rstep == NULL)&&(m < n_tmp_lv_vals)) {
						for( ;m < n_tmp_lv_vals; m++) {
							strt[i].rec_inq = NULL;
							PRINT_MISSING(the_end->ens_ix,j,k,m);
							i++;
						}
					}
				} else {
					while(rstep != NULL) {
						if((rstep->rec_inq->level0 == tmp_lv_vals[m])
						   &&(rstep->rec_inq->level1 == tmp_lv_vals1[m])) {
							strt[i].rec_inq = rstep->rec_inq;	
							icount +=1;
							free_rec = rstep;
							rstep = rstep->next;
							NclFree(free_rec);
							m++;
						} else {
							strt[i].rec_inq = NULL;
							PRINT_MISSING(the_end->ens_ix,j,k,m);
							m++;
						}
						i++;
					}
					if((rstep == NULL)&&(m < n_tmp_lv_vals)) {
						for( ;m < n_tmp_lv_vals; m++) {
							strt[i].rec_inq = NULL;
							PRINT_MISSING(the_end->ens_ix,j,k,m);
							i++;
						}
					}
				}
				free_ft = ftstep;
				ftstep = ftstep->next;
				if(free_ft->lv_vals != NULL) 
					NclFree(free_ft->lv_vals);
				if(free_ft->lv_vals1 != NULL) 
					NclFree(free_ft->lv_vals1);
				NclFree(free_ft);
				k++;
			}
			while(k < n_tmp_ft_vals) {
				for( m = 0 /* i already set */; m < n_tmp_lv_vals; i++,m++) {
					strt[i].rec_inq = NULL;
					PRINT_MISSING(the_end->ens_ix,j,k,m);
				}
				k++;
			}
			free_it = itstep;
			itstep = itstep->next;
			if (free_it->lv_vals != NULL)
				NclFree(free_it->lv_vals);
			if(free_it->lv_vals1 != NULL) 
				NclFree(free_it->lv_vals1);
			if(free_it->ft_vals != NULL) 
				NclFree(free_it->ft_vals);
			NclFree(free_it);
			j++;
		}
		while (j < n_tmp_it_vals) {
			for (k = 0; k < n_tmp_ft_vals; k++) {
				for( m = 0 /* i already set */; m < n_tmp_lv_vals; i++,m++) {
					strt[i].rec_inq = NULL;
					PRINT_MISSING(the_end->ens_ix,j,k,m);
				}
			}
			j++;
		}
		free_ens = the_end;
		the_end = the_end->next;
		if(free_ens->lv_vals != NULL) 
			NclFree(free_ens->lv_vals);
		if(free_ens->lv_vals1 != NULL) 
			NclFree(free_ens->lv_vals1);
		if(free_ens->ft_vals != NULL) 
			NclFree(free_ens->ft_vals);
		if(free_ens->it_vals != NULL) 
			NclFree(free_ens->it_vals);
		NclFree(free_ens);
	}
	while(i<total) strt[i++].rec_inq = NULL;
	step->thelist = strt;
	step->n_entries = total;
	
	return(returnval);
}




static void _g2SetFileDimsAndCoordVars
# if 	NhlNeedProto
(Grib2FileRecord *therec)
# else
(therec)
    Grib2FileRecord *therec;
# endif
{
    Grib2ParamList  *step,
                    *last,
                    *tmpstep;
    char buffer[80];

    NclQuark    gridx_q,
                lat_q;
    Grib2DimInqRecList  *dstep,
                        *ptr;
    Grib2DimInqRec  *tmp;
    NclQuark    *it_rhs,
                *it_lhs;
    int *rhs,
        *lhs;
    int *rhs1,
        *lhs1;

    float   *rhs_f,
            *lhs_f;
    int i,
        j,
        m;
    int current_dim = 0;
    int do_rot;

    NclMultiDValData    tmp_md;
    NclMultiDValData    tmp_md1;

    NclGrib2FVarRec *test;

    int n_dims_lat = 0;
    int n_dims_lon = 0;
    int n_dims_rot = 0;
    int n_dims_level = 0;

    int *dimsizes_lat = NULL;
    int *dimsizes_lon = NULL;
    int *dimsizes_rot = NULL;

    float   *tmp_lat = NULL;
    float   *tmp_lon = NULL;
    float   *tmp_rot = NULL;

    NhlErrorTypes is_err = NhlNOERROR;

    int tmp_file_dim_numbers[2];
    char name_buffer[80];

    Grib2AttInqRecList  *att_list_ptr = NULL;
    Grib2AttInqRecList  *tmp_att_list_ptr = NULL;
    Grib2AttInqRec  *att_ptr= NULL;

    int natts = 0;
    NclQuark    *tmp_string = NULL;

    float   *tmp_float = NULL;

    int tmp_dimsizes = 1;
    int dimsizes_level = 1;
    int nlonatts = 0;
    int nlatatts = 0;
    int nrotatts = 0;

    Grib2AttInqRecList  *lat_att_list_ptr = NULL;
    Grib2AttInqRecList  *lon_att_list_ptr = NULL;
    Grib2AttInqRecList  *rot_att_list_ptr = NULL;


    therec->total_dims = 0;
    therec->n_scalar_dims = 0;
    therec->scalar_dims = NULL;
    therec->n_it_dims = 0;
    therec->it_dims = NULL;
    therec->n_ft_dims = 0;
    therec->ft_dims = NULL;
    therec->n_lv_dims = 0;
    therec->lv_dims = NULL;
    therec->n_grid_dims = 0;
    therec->grid_dims = NULL;
    step = therec->var_list;

    while (step != NULL) {
        current_dim = 0;
        step->aux_coords[0] = step->aux_coords[1] = NrmNULLQUARK;
        if (!step->ensemble_isatt) {
            dstep = therec->ensemble_dims;
            for (i = 0; i < therec->n_ensemble_dims; i++) {
                if (dstep->dim_inq->size == step->ensemble->multidval.dim_sizes[0]) {
	                tmp_md = _Grib2GetInternalVar(therec, dstep->dim_inq->dim_name, &test);
                    if (tmp_md != NULL) {
                        lhs = (int *) tmp_md->multidval.val;
                        rhs = (int *) step->ens_indexes->multidval.val;
                        j = 0;
                        while (j < dstep->dim_inq->size) {
                            if (lhs[j] != rhs[j]) {
                                break;
                            } else {
                                j++;
                            }
                        }

                        if (j == dstep->dim_inq->size) {
                            break;
                        } else {
                            dstep= dstep->next;
                        }
                    } else {
                        dstep  = dstep->next;
                    }
                } else {
                    dstep = dstep->next;
                }
            }

            if (dstep == NULL) {
                /* Need a new dimension entry w name and number */
                tmp = (Grib2DimInqRec *) NclMalloc((unsigned) sizeof(Grib2DimInqRec));
                tmp->gds = (G2_GDS *) NclMalloc(sizeof(G2_GDS));
                memcpy(tmp->gds, therec->var_list->thelist->rec_inq->gds, sizeof(G2_GDS));
                tmp->dim_number = therec->total_dims;
                tmp->size = step->ensemble->multidval.dim_sizes[0];
                sprintf(buffer, "ensemble%d", therec->total_dims);
                tmp->dim_name = NrmStringToQuark(buffer);
                tmp->is_gds = -1;
                therec->total_dims++;
                ptr = (Grib2DimInqRecList *) NclMalloc((unsigned) sizeof(Grib2DimInqRecList));
                ptr->dim_inq = tmp;
                ptr->next = therec->ensemble_dims;
                therec->ensemble_dims = ptr;
                therec->n_ensemble_dims++;
                step->var_info.file_dim_num[current_dim] = tmp->dim_number;

                tmp_string = (NclQuark *) NclMalloc(sizeof(NclQuark));
                *tmp_string = NrmStringToQuark("non-dim");
                Grib2PushAtt(&tmp_att_list_ptr, "units", tmp_string, 1, nclTypestringClass); 

                tmp_string = (NclQuark *) NclMalloc(sizeof(NclQuark));
                *tmp_string = NrmStringToQuark("ensemble indexes");
                Grib2PushAtt(&tmp_att_list_ptr, "long_name", tmp_string, 1, nclTypestringClass); 

                _Grib2AddInternalVar(therec,tmp->dim_name,&tmp->dim_number,
                        (NclMultiDValData)step->ens_indexes, tmp_att_list_ptr, 2);
                tmp_att_list_ptr = NULL;
                step->ens_indexes = NULL;

                tmp_string = (NclQuark *) NclMalloc(sizeof(NclQuark));
                *tmp_string = NrmStringToQuark("ensemble elements description");
                Grib2PushAtt(&tmp_att_list_ptr, "long_name", tmp_string, 1, nclTypestringClass); 

                sprintf(&(buffer[strlen(buffer)]), "_info", therec->total_dims);
                _Grib2AddInternalVar(therec, NrmStringToQuark(buffer), &tmp->dim_number,
                        (NclMultiDValData) step->ensemble, tmp_att_list_ptr, 1);
                tmp_att_list_ptr = NULL;
                step->ensemble = NULL;
            } else {
                step->var_info.file_dim_num[current_dim] = dstep->dim_inq->dim_number;
                _NclDestroyObj((NclObj) step->ens_indexes);
                step->ens_indexes = NULL;
                _NclDestroyObj((NclObj) step->ensemble);
                step->ensemble = NULL;
            }
            current_dim++;
        }

		if (!step->yymmddhh_isatt) {
			dstep = therec->it_dims;
			for (i = 0; i < therec->n_it_dims; i++) {
				if (dstep->dim_inq->size == step->yymmddhh->multidval.dim_sizes[0]) {
					tmp_md = _Grib2GetInternalVar(therec, dstep->dim_inq->dim_name, &test);
					if (tmp_md != NULL) {
						it_lhs = (NclQuark *) tmp_md->multidval.val;
	
						it_rhs = (NclQuark *) step->yymmddhh->multidval.val;
						j = 0;
						while (j < dstep->dim_inq->size) {
							if (it_lhs[j] != it_rhs[j])
								break;
							else
								j++;
						}

						if (j == dstep->dim_inq->size)
							break;
						else
							dstep= dstep->next;
					} else {
						dstep = dstep->next;
                    }
				} else {
					dstep = dstep->next;
				}
			}

            /* All pointers to coordate will end up in dim list not in param list */
			if (dstep == NULL) {
                /* Need a new dimension entry w name and number */
				tmp = (Grib2DimInqRec *) NclMalloc((unsigned) sizeof(Grib2DimInqRec));
				tmp->dim_number = therec->total_dims;
				tmp->size = step->yymmddhh->multidval.dim_sizes[0];
				sprintf(buffer, "initial_time%d", therec->total_dims);
				tmp->dim_name = NrmStringToQuark(buffer);
				tmp->is_gds = -1;
				therec->total_dims++;
				ptr = (Grib2DimInqRecList *) NclMalloc((unsigned) sizeof(Grib2DimInqRecList));
				ptr->dim_inq = tmp;
				ptr->next = therec->it_dims;
				therec->it_dims = ptr;
				therec->n_it_dims++;
				step->var_info.file_dim_num[current_dim] = tmp->dim_number;
				
				tmp_string = (NclQuark *) NclMalloc(sizeof(NclQuark));
				*tmp_string = NrmStringToQuark("mm/dd/yyyy (hh:mm)");
				Grib2PushAtt(&tmp_att_list_ptr, "units", tmp_string, 1, nclTypestringClass); 

				tmp_string = (NclQuark *) NclMalloc(sizeof(NclQuark));
				*tmp_string = NrmStringToQuark("Initial time of first record");
				Grib2PushAtt(&tmp_att_list_ptr, "long_name", tmp_string, 1, nclTypestringClass); 

				_Grib2AddInternalVar(therec,tmp->dim_name, &tmp->dim_number,
                        (NclMultiDValData) step->yymmddhh, tmp_att_list_ptr, 2);
				tmp_att_list_ptr = NULL;
				step->yymmddhh = NULL;
			} else {
				step->var_info.file_dim_num[current_dim] = dstep->dim_inq->dim_number;
				_NclDestroyObj((NclObj) step->yymmddhh);
				step->yymmddhh = NULL;
			}
			current_dim++;
		}

        if (!step->forecast_time_isatt) {
			dstep = therec->ft_dims;
			for (i = 0; i < therec->n_ft_dims; i++) {
				if (dstep->dim_inq->size == step->forecast_time->multidval.dim_sizes[0]) {
					tmp_md = _Grib2GetInternalVar(therec, dstep->dim_inq->dim_name, &test);
					if (tmp_md != NULL) {
						lhs = (int *) tmp_md->multidval.val;
						rhs = (int *) step->forecast_time->multidval.val;
						j = 0;
						while (j < dstep->dim_inq->size) {
							if (lhs[j] != rhs[j]) {
								break;
							} else {
								j++;
							}
						}
						if (j == dstep->dim_inq->size) {
							break;
						} else {
							dstep= dstep->next;
						}
					} else {
						dstep  = dstep->next;
					}
				} else {
					dstep = dstep->next;
				}
			}

			if (dstep == NULL) {
                /* Need a new dimension entry name and number */
				tmp = (Grib2DimInqRec *) NclMalloc((unsigned) sizeof(Grib2DimInqRec));
				tmp->dim_number = therec->total_dims;
				tmp->size = step->forecast_time->multidval.dim_sizes[0];
				sprintf(buffer, "forecast_time%d", therec->total_dims);
				tmp->dim_name = NrmStringToQuark(buffer);
				tmp->is_gds = -1;
				therec->total_dims++;
				ptr = (Grib2DimInqRecList *) NclMalloc((unsigned) sizeof(Grib2DimInqRecList));
				ptr->dim_inq = tmp;
				ptr->next = therec->ft_dims;
				therec->ft_dims = ptr;
				therec->n_ft_dims++;
				step->var_info.file_dim_num[current_dim] = tmp->dim_number;

				tmp_string = (NclQuark *) NclMalloc(sizeof(NclQuark));
				*tmp_string = NrmStringToQuark("hours");
				Grib2PushAtt(&tmp_att_list_ptr, "units", tmp_string, 1, nclTypestringClass); 

				tmp_string = (NclQuark *) NclMalloc(sizeof(NclQuark));
				*tmp_string = NrmStringToQuark("Forecast offset from initial time");
				Grib2PushAtt(&tmp_att_list_ptr, "long_name", tmp_string, 1, nclTypestringClass); 

				_Grib2AddInternalVar(therec, tmp->dim_name, &tmp->dim_number,
                        (NclMultiDValData) step->forecast_time, tmp_att_list_ptr, 2);
				tmp_att_list_ptr = NULL;
				step->forecast_time = NULL;
			} else {
				step->var_info.file_dim_num[current_dim] = dstep->dim_inq->dim_number;
				_NclDestroyObj((NclObj) step->forecast_time);
				step->forecast_time = NULL;
			}
			current_dim++;
        }

		if ((!step->levels_isatt) && (step->levels != NULL)) {
			dstep = therec->lv_dims;
			for (i = 0; i < therec->n_lv_dims; i++) {
				if (dstep->dim_inq->size == step->levels->multidval.dim_sizes[0]) {
					tmp_md = _Grib2GetInternalVar(therec, dstep->dim_inq->dim_name, &test);
					if (tmp_md != NULL ) {
						lhs = (int *) tmp_md->multidval.val;
						rhs = (int *) step->levels->multidval.val;
						j = 0;
						while (j < dstep->dim_inq->size) {
							if (lhs[j] != rhs[j]) {
								break;
							} else {
								j++;
							}
						}
						if (j == dstep->dim_inq->size) {
							break;
						} else {
							dstep= dstep->next;
						}
					} else {
						dstep= dstep->next;
					}
				} else {
					dstep = dstep->next;
				}
			}

			if (dstep == NULL) {
                /* Need a new dimension entry name and number */
				tmp = (Grib2DimInqRec *) NclMalloc((unsigned)sizeof(Grib2DimInqRec));
				tmp->dim_number = therec->total_dims;
				tmp->is_gds = -1;
				tmp->size = step->levels->multidval.dim_sizes[0];
    			sprintf(buffer, "levels%d", therec->total_dims);
				tmp->dim_name = NrmStringToQuark(buffer);
				therec->total_dims++;
				ptr = (Grib2DimInqRecList *) NclMalloc((unsigned) sizeof(Grib2DimInqRecList));
				ptr->dim_inq = tmp;
				ptr->next = therec->lv_dims;
				therec->lv_dims = ptr;
				therec->n_lv_dims++;
				step->var_info.file_dim_num[current_dim] = tmp->dim_number;

/********
				tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
    			*tmp_string = NrmStringToQuark(level_str_long_name[i]);
********/

				_Grib2AddInternalVar(therec,tmp->dim_name, &tmp->dim_number,
                        (NclMultiDValData) step->levels, att_list_ptr, 2);
				att_list_ptr = NULL;
				step->levels = NULL;
			} else {
				step->var_info.file_dim_num[current_dim] = dstep->dim_inq->dim_number;
				_NclDestroyObj((NclObj)step->levels);
				step->levels = NULL;
			}
			current_dim++;
		} else if ((!step->levels_isatt) && (step->levels0 != NULL) && (step->levels1 != NULL)) {
			dstep = therec->lv_dims;
			for (i = 0; i < therec->n_lv_dims; i++) {
				if (dstep->dim_inq->size == step->levels0->multidval.dim_sizes[0]) {
					sprintf(name_buffer,"%s%s",NrmQuarkToString(dstep->dim_inq->dim_name),"_l0");
					tmp_md = _Grib2GetInternalVar(therec,NrmStringToQuark(name_buffer),&test);
					sprintf(name_buffer,"%s%s",NrmQuarkToString(dstep->dim_inq->dim_name),"_l1");
					tmp_md1 = _Grib2GetInternalVar(therec,NrmStringToQuark(name_buffer),&test);
					if ((tmp_md != NULL )&&(tmp_md1 != NULL) ) {
						lhs = (int*)tmp_md->multidval.val;
						rhs = (int*)step->levels0->multidval.val;
						lhs1 = (int*)tmp_md1->multidval.val;
						rhs1 = (int*)step->levels1->multidval.val;
						j = 0;
						while (j<dstep->dim_inq->size) {
							if ((lhs[j] != rhs[j])||(lhs1[j] != rhs1[j])) {
								break;
							} else {
								j++;
							}
						}
						if (j == dstep->dim_inq->size) {
							break;
						} else {
							dstep= dstep->next;
						}
					} else {
						dstep= dstep->next;
					}
				} else {
					dstep = dstep->next;
				}
			}
			if (dstep == NULL) {
                /* Need a new dimension entry w name and number */
				tmp = (Grib2DimInqRec*)NclMalloc((unsigned)sizeof(Grib2DimInqRec));
				tmp->dim_number = therec->total_dims;
				tmp->is_gds = -1;
				tmp->size = step->levels0->multidval.dim_sizes[0];
/***
				for(i = 0; i < sizeof(level_index)/sizeof(int); i++) {
					if(level_index[i] == step->level_indicator) {
						break;
					}
				}
				if(i < sizeof(level_index)/sizeof(int)) {
					sprintf(buffer,"lv_%s%d",level_str[i],therec->total_dims);
				} else {
					sprintf(buffer,"levels%d",therec->total_dims);
				}
			
				tmp->dim_name = NrmStringToQuark(buffer);
				therec->total_dims++;
				ptr = (Grib2DimInqRecList*)NclMalloc((unsigned)sizeof(Grib2DimInqRecList));
				ptr->dim_inq = tmp;
				ptr->next = therec->lv_dims;
				therec->lv_dims = ptr;
				therec->n_lv_dims++;
				step->var_info.file_dim_num[current_dim] = tmp->dim_number;
				sprintf(name_buffer,"%s%s",buffer,"_l0");

				tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
				if(i < sizeof(level_index)/sizeof(int)) {
					*tmp_string = NrmStringToQuark(level_units_str[i]);
				} else {
					*tmp_string = NrmStringToQuark("unknown");
				}
				Grib2PushAtt(&att_list_ptr,"units",tmp_string,1,nclTypestringClass); 

				tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
				if(i < sizeof(level_index)/sizeof(int)) {
					*tmp_string = NrmStringToQuark(level_str_long_name[i]);
				} else {
					*tmp_string = NrmStringToQuark("unknown");
				}
				Grib2PushAtt(&att_list_ptr,"long_name",tmp_string,1,nclTypestringClass); 
***/
				_Grib2AddInternalVar(therec,NrmStringToQuark(name_buffer),&tmp->dim_number,(NclMultiDValData)step->levels0,att_list_ptr,2);

				att_list_ptr = NULL;

				sprintf(name_buffer,"%s%s",buffer,"_l1");
				tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
/***
				if(i < sizeof(level_index)/sizeof(int)) {
					*tmp_string = NrmStringToQuark(level_units_str[i]);
				} else {
					*tmp_string = NrmStringToQuark("unknown");
				}
				Grib2PushAtt(&att_list_ptr,"units",tmp_string,1,nclTypestringClass); 

				tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
				if(i < sizeof(level_index)/sizeof(int)) {
					*tmp_string = NrmStringToQuark(level_str_long_name[i]);
				} else {
					*tmp_string = NrmStringToQuark("unknown");
				}
***/
				Grib2PushAtt(&att_list_ptr,"long_name",tmp_string,1,nclTypestringClass); 
				_Grib2AddInternalVar(therec,NrmStringToQuark(name_buffer),&tmp->dim_number,(NclMultiDValData)step->levels1,att_list_ptr,2);


				att_list_ptr = NULL;
				step->levels0 = NULL;
				step->levels1 = NULL;
			} else {
				step->var_info.file_dim_num[current_dim] = dstep->dim_inq->dim_number;
				_NclDestroyObj((NclObj)step->levels0);
				_NclDestroyObj((NclObj)step->levels1);
				step->levels0 = NULL;
				step->levels1 = NULL;
			}
			current_dim++;
        }

        /*
         * Now its time to get the grid coordinates  and define grid variables
         * First switch on whether record has GDS or not
         * if not :
         *  check to see if dimensions are defined
         *  if not: check get_grid field
         *      then get_grid.
         *      if its more that 1D then
         *          define gridx_### and gridy_### and then add variables gridlat_###
         *          and gridlon_### if gridx and gridy are 
         *      else
         *          define lat_### and lon_### and add them as variables of the same
         *          name (real coordinate variables)
         */
        (void) sprintf(buffer,"gridx_%d",step->grid_number);
        gridx_q = NrmStringToQuark(buffer);
        (void) sprintf(buffer,"lat_%d",step->grid_number);
        lat_q = NrmStringToQuark(buffer);

    	if (step->has_gds && step->grid_number == 255) {
            NhlPError(NhlWARNING, NhlEUNKNOWN,
                "NclGRIB: Unsupported GDS grid type (%d) can't decode",step->gds_type);
            is_err = NhlWARNING;
        }

        /* All GRIB v2 records have a GDS */
        if (is_err == NhlNOERROR) {
            if ((step->has_gds)) {
                /*
                 * For gds grid it must be decoded every time since several different
                 * grid could be defined by the smae grid_type number.
                 */
                dstep = therec->grid_dims;
                while (dstep != NULL) {
                    if (dstep->dim_inq->is_gds == step->gds_type) {
                        if ((step->gds_type == 203)
                                && (dstep->dim_inq->is_uv != Is_UV(step->param_number))) {
                            dstep = dstep->next;
                            continue;
                        }

                        break;
                    } else
                        dstep = dstep->next;
				}
            }  else {
				dstep = therec->grid_dims;
                while (dstep != NULL) {
                    if ((dstep->dim_inq->dim_name == gridx_q)
                            ||(dstep->dim_inq->dim_name == lat_q)) {
                        break;
                    } else {
                        dstep = dstep->next;
                    }
                }
            }

            if (dstep == NULL) {
                nlonatts = 0;
                nlatatts = 0;
                nrotatts = 0;
                lat_att_list_ptr = NULL;
                lon_att_list_ptr = NULL;
                rot_att_list_ptr = NULL;
                tmp_rot = NULL;
                tmp_lon = NULL;
                tmp_lat = NULL;
                n_dims_lat = 0;
                n_dims_lon = 0;
                dimsizes_lat = NULL;
                dimsizes_lon = NULL;

                /*
                 * Grid has not been defined
                 */
                if (step->has_gds) {
                    switch (step->grid_number) {
                        case 0:
                            /* Latitude/Longitude (Template 3.0) */
                            g2GetGrid_0(step, &tmp_lat, &n_dims_lat, &dimsizes_lat,
                                &tmp_lon, &n_dims_lon, &dimsizes_lon, &tmp_rot,
                                &lat_att_list_ptr, &nlatatts, &lon_att_list_ptr, &nlonatts,
                                &rot_att_list_ptr, &nrotatts);
                            break;

                        case 1:
                            /* Rotated Latitude/Longitude (Template 3.1) */
                            break;

                        case 2:
                            /* Stretched Latitude/Longitude (Template 3.2) */
                            break;

                        case 3:
                            /* Rotated and Stretched Latitude/Longitude (Template 3.3) */
                            break;

                        case 4: case 5: case 6: case 7: case 8: case 9:
                            /* Reserved */
                            break;

                        case 10:
                            /* Mercator (Template 3.10) */
                            break;

                        case 11: case 12: case 13: case 14: case 15: case 16:
                        case 17: case 18: case 19:
                            /* Reserved */
                            break;

                        case 20:
                            /* Polar Stereographic Projection (North or South) */
                            /* Template 3.20 */
                            break;

                        case 21: case 22: case 23: case 24: case 25: case 26:
                        case 27: case 28: case 29:
                            /* Reserved */
                            break;

                        case 30:
                            /* Lambert Conformal (secant, tangent, conical or biploar */
                            /*  Template 3.30 */
                            break;

                        default:
                            break;
                    }


					/*
                     * Get the atts if a grid has been set up but the atts have not
                     * been defined yet -- the new way is to set them in the 'grid' function
                     */
                    if (!tmp_lat) 
                        ;   /* do nothing */
                    else if (nlatatts == 0) {
                        int grid_oriented = 0;
                        /*
						 * if there's a gds, gds[16] determines whether the uv rotation is 
						 * grid or earth based; otherwise assume that if a rotation variable was
						 * created the rotation is grid-based
						 */

						if (step->has_gds)
							grid_oriented
                                = (step->thelist->rec_inq->gds->res_comp->idir_given > 0 ) 
                                        ? 1 : 0;
						else
							grid_oriented = do_rot;

					}
				}

                /*
                 * If a pre-defined grid has not been set up and there is a gds
                 * grid type that applies do this
                 */
                g2GDSCEGrid(step, &tmp_lat, &n_dims_lat, &dimsizes_lat, &tmp_lon,
                        &n_dims_lon,&dimsizes_lon, &tmp_rot, &n_dims_rot, &dimsizes_rot,
                        &lat_att_list_ptr, &nlatatts, &lon_att_list_ptr, &nlonatts,
                        &rot_att_list_ptr, &nrotatts);

	
                _g2NclNewGridCache(therec,step->grid_number, step->has_gds, 1,
                        /*step->grid_gds_tbl_index,*/ n_dims_lat, dimsizes_lat, n_dims_lon,
                        dimsizes_lon);

                /*
                 * Grids always need to be inserted into the grid_dim list in the
                 * right order. First lon is pushed then lat so that dstep->dim_inq
                 * always points to lat and dstep->next->dim_inq point to lon
                 */
                m = 0;
                while ((m < step->n_entries) && (step->thelist[m].rec_inq == NULL))
                    m++;

                if ((n_dims_lon == 1) && (n_dims_lat == 1)) {
                    if ((step->has_gds )&& (step->gds_type == 50)) {
                        step->var_info.dim_sizes[current_dim] = 2;
                        step->var_info.dim_sizes[current_dim + 1] = dimsizes_lat[0];
                        step->var_info.dim_sizes[current_dim + 2] = dimsizes_lon[0];
                        step->var_info.file_dim_num[current_dim] = therec->total_dims;
                        step->var_info.file_dim_num[current_dim + 1] = therec->total_dims + 1;
                        step->var_info.file_dim_num[current_dim + 2] = therec->total_dims + 2;
                        sprintf(buffer, "real_imaginary");
                        tmp = (Grib2DimInqRec*)NclMalloc((unsigned) sizeof(Grib2DimInqRec));
                        tmp->dim_number = therec->total_dims;
                        tmp->size = 2;
                        tmp->dim_name = NrmStringToQuark(buffer);
                        tmp->is_gds = step->gds_type;

                        tmp->gds = (G2_GDS *) NclMalloc(sizeof(G2_GDS));
                        memcpy(tmp->gds, step->thelist[m].rec_inq->gds, sizeof(G2_GDS));

                        ptr = (Grib2DimInqRecList*)NclMalloc((unsigned)sizeof(Grib2DimInqRecList));
                        ptr->dim_inq= tmp;
                        ptr->next = therec->grid_dims;
                        therec->grid_dims = ptr;
                        therec->n_grid_dims++;
						step->var_info.doff = 2;
                    } else {
                        step->var_info.dim_sizes[current_dim] = dimsizes_lat[0];
                        step->var_info.dim_sizes[current_dim+1] = dimsizes_lon[0];
                        step->var_info.file_dim_num[current_dim] = therec->total_dims;
                        step->var_info.file_dim_num[current_dim+1] = therec->total_dims + 1;
                        step->var_info.doff = 1;
                    }

                    /*
                     * y (longitude) first
                     */
                    if ((step->has_gds) && (step->grid_number == 255)) {
                        (void) sprintf(buffer, "g%d_lon_%d",
                            step->gds_type, therec->total_dims + step->var_info.doff);
                    } else {
                        (void) sprintf(buffer,"lon_%d",step->grid_number);
                    }

                    tmp = (Grib2DimInqRec*)NclMalloc((unsigned)sizeof(Grib2DimInqRec));
                    tmp->dim_number = therec->total_dims + step->var_info.doff;
                    tmp->size = dimsizes_lon[0];
                    tmp->dim_name = NrmStringToQuark(buffer);
                    tmp->is_gds = step->gds_type;

                    tmp->gds = (G2_GDS *) NclMalloc(sizeof(G2_GDS));
                    memcpy(tmp->gds, step->thelist[m].rec_inq->gds, sizeof(G2_GDS));

                    ptr = (Grib2DimInqRecList *) NclMalloc((unsigned) sizeof(Grib2DimInqRecList));
                    ptr->dim_inq= tmp;
                    ptr->next = therec->grid_dims;
                    therec->grid_dims = ptr;
                    therec->n_grid_dims++;

                    if (tmp_lon != NULL) {	
                        _Grib2AddInternalVar(therec, tmp->dim_name, &tmp->dim_number,
                                (NclMultiDValData)_NclCreateVal(
                                    NULL,
                                    NULL,
                                    Ncl_MultiDValData,
                                    0,
                                    (void *) tmp_lon,
                                    NULL,
                                    n_dims_lon,
                                    dimsizes_lon,
                                    TEMPORARY,
                                    NULL,
                                    nclTypefloatClass),
                            lon_att_list_ptr,nlonatts);
                    }

                    NclFree(dimsizes_lon);

					if((step->has_gds)&&(step->grid_number == 255)) {
						sprintf(buffer,"g%d_lat_%d",step->gds_type,therec->total_dims + (step->var_info.doff - 1));
					} else {
						sprintf(buffer,"lat_%d",step->grid_number);
					}
					tmp = (Grib2DimInqRec*)NclMalloc((unsigned)sizeof(Grib2DimInqRec));
					tmp->dim_number = therec->total_dims + (step->var_info.doff - 1);
					tmp->size = dimsizes_lat[0];
					tmp->dim_name = NrmStringToQuark(buffer);
					tmp->is_gds = step->gds_type;

                    tmp->gds = (G2_GDS *) NclMalloc(sizeof(G2_GDS));
                    memcpy(tmp->gds, step->thelist[m].rec_inq->gds, sizeof(G2_GDS));

					ptr = (Grib2DimInqRecList*)NclMalloc((unsigned)sizeof(Grib2DimInqRecList));
					ptr->dim_inq= tmp;
					ptr->next = therec->grid_dims;
					therec->grid_dims = ptr;
					therec->n_grid_dims++;

					if(tmp_lat != NULL) {
						_Grib2AddInternalVar(therec,tmp->dim_name,&tmp->dim_number,
								    (NclMultiDValData)_NclCreateVal(
									    NULL,
									    NULL,
									    Ncl_MultiDValData,
									    0,
									    (void*)tmp_lat,
									    NULL,
									    n_dims_lat,
									    dimsizes_lat,
									    TEMPORARY,
									    NULL,
									    nclTypefloatClass),
								    lat_att_list_ptr,nlatatts);
					}
					NclFree(dimsizes_lat);
					therec->total_dims += step->var_info.doff+1;


				} else if ((n_dims_lon == 2) && (n_dims_lat == 2)
                        && (dimsizes_lat[0] == dimsizes_lon[0])
                        && (dimsizes_lat[1] == dimsizes_lon[1])) {
					char *uv_m = "m";
					step->var_info.dim_sizes[current_dim] = dimsizes_lat[0];
					step->var_info.dim_sizes[current_dim + 1] = dimsizes_lon[1];
					step->var_info.file_dim_num[current_dim] = therec->total_dims;
					step->var_info.file_dim_num[current_dim + 1] = therec->total_dims + 1;
					step->var_info.doff=1;

					if ((step->has_gds) && (step->grid_number == 255)) {
						if (step->gds_type != 203) {
							sprintf(buffer,"g%d_y_%d",step->gds_type,therec->total_dims + 1);
						}
						else {
                        if (Is_UV(step->param_number))
                            uv_m = "v";
							(void) sprintf(buffer, "g%d%s_y_%d",
                                step->gds_type, uv_m, therec->total_dims + 1);
						}
					} else {
						sprintf(buffer, "gridy_%d", step->grid_number);
					}

					tmp = (Grib2DimInqRec*)NclMalloc((unsigned)sizeof(Grib2DimInqRec));
					tmp->dim_number = therec->total_dims + 1;
					tmp->size = dimsizes_lon[1];
					tmp->dim_name = NrmStringToQuark(buffer);
					tmp->is_gds = step->gds_type;
					tmp->is_uv = step->gds_type == 203 && Is_UV(step->param_number);

                    tmp->gds = (G2_GDS *) NclMalloc(sizeof(G2_GDS));
                    memcpy(tmp->gds, step->thelist[m].rec_inq->gds, sizeof(G2_GDS));

					ptr = (Grib2DimInqRecList*)NclMalloc((unsigned)sizeof(Grib2DimInqRecList));
					ptr->dim_inq= tmp;
					ptr->next = therec->grid_dims;
					therec->grid_dims = ptr;
					therec->n_grid_dims++;
					if((step->has_gds)&&(step->grid_number == 255)) {
						if (step->gds_type != 203) {
							sprintf(buffer,"g%d_lon_%d",step->gds_type,therec->total_dims + 1);
						}
						else {
							sprintf(buffer,"g%d%s_lon_%d",step->gds_type,uv_m,therec->total_dims + 1);
						}
					} else {
						sprintf(buffer,"gridlon_%d",step->grid_number);
					}
					step->aux_coords[1] = NrmStringToQuark(buffer);
					tmp_file_dim_numbers[0] = therec->total_dims;
					tmp_file_dim_numbers[1] = therec->total_dims+ 1;

					tmp_float = NclMalloc((unsigned)sizeof(float)*4);
					tmp_float[0] = tmp_lon[0];
					tmp_float[1] = tmp_lon[dimsizes_lon[1]-1];
					tmp_float[3] = tmp_lon[(dimsizes_lon[0]-1) * dimsizes_lon[1]];
					tmp_float[2] = tmp_lon[(dimsizes_lon[0] * dimsizes_lon[1])-1];
					Grib2PushAtt(&lon_att_list_ptr,"corners",tmp_float,4,nclTypefloatClass); nlonatts++;

					_Grib2AddInternalVar(therec,NrmStringToQuark(buffer),tmp_file_dim_numbers,
							    (NclMultiDValData)_NclCreateVal(
								    NULL,
								    NULL,
								    Ncl_MultiDValData,
								    0,
								    (void*)tmp_lon,
								    NULL,
								    n_dims_lon,
								    dimsizes_lon,
								    TEMPORARY,
								    NULL,
								    nclTypefloatClass),
							    lon_att_list_ptr,nlonatts);
					NclFree(dimsizes_lon);
						
					if((step->has_gds)&&(step->grid_number == 255)) {
						if (step->gds_type != 203) {
							sprintf(buffer,"g%d_x_%d",step->gds_type,therec->total_dims);
						}
						else {
							sprintf(buffer,"g%d%s_x_%d",step->gds_type,uv_m,therec->total_dims);
						}
					} else {
						sprintf(buffer,"gridx_%d",step->grid_number);
					}
					tmp = (Grib2DimInqRec*)NclMalloc((unsigned)sizeof(Grib2DimInqRec));
					tmp->dim_number = therec->total_dims;
					tmp->size = dimsizes_lat[0];
					tmp->dim_name = NrmStringToQuark(buffer);
					tmp->is_gds = step->gds_type;
					tmp->is_uv = step->gds_type == 203 && Is_UV(step->param_number);

                    tmp->gds = (G2_GDS *) NclMalloc(sizeof(G2_GDS));
                    memcpy(tmp->gds, step->thelist[m].rec_inq->gds, sizeof(G2_GDS));

					ptr = (Grib2DimInqRecList*)NclMalloc((unsigned)sizeof(Grib2DimInqRecList));
					ptr->dim_inq= tmp;
					ptr->next = therec->grid_dims;
					therec->grid_dims = ptr;
					therec->n_grid_dims++;
					if((step->has_gds)&&(step->grid_number == 255)) {
						if (step->gds_type != 203) {
							sprintf(buffer,"g%d_lat_%d",step->gds_type,therec->total_dims);
						}
						else {
							sprintf(buffer,"g%d%s_lat_%d",step->gds_type,uv_m,therec->total_dims);
						}
					} else {
						sprintf(buffer,"gridlat_%d",step->grid_number);
					}
					step->aux_coords[0] = NrmStringToQuark(buffer);
					tmp_float = NclMalloc((unsigned)sizeof(float)*4);
					tmp_float[0] = tmp_lat[0];
					tmp_float[1] = tmp_lat[dimsizes_lat[1]-1];
					tmp_float[3] = tmp_lat[(dimsizes_lat[0]-1) * dimsizes_lat[1]];
					tmp_float[2] = tmp_lat[(dimsizes_lat[0] * dimsizes_lat[1])-1];
					Grib2PushAtt(&lat_att_list_ptr,"corners",tmp_float,4,nclTypefloatClass); nlatatts++;

					_Grib2AddInternalVar(therec,NrmStringToQuark(buffer),tmp_file_dim_numbers,(NclMultiDValData)_NclCreateVal(
								    NULL,
								    NULL,
								    Ncl_MultiDValData,
								    0,
								    (void*)tmp_lat,
								    NULL,
								    n_dims_lat,
								    dimsizes_lat,
								    TEMPORARY,
								    NULL,
								    nclTypefloatClass),lat_att_list_ptr,nlatatts);

					therec->total_dims += 2;
					if (tmp_rot != NULL) {
						/* the rotation array is assumed to be the same size as the lat and lon arrays */
						if((step->has_gds)&&(step->grid_number == 255)) {
							if (step->gds_type != 203) {
								sprintf(buffer,"g%d_rot_%d",step->gds_type,therec->total_dims);
							}
							else {
								sprintf(buffer,"g%d%s_rot_%d",step->gds_type,uv_m,therec->total_dims);
							}
						} else {
							sprintf(buffer,"gridrot_%d",step->grid_number);
						}
						_Grib2AddInternalVar(therec,NrmStringToQuark(buffer),tmp_file_dim_numbers,
								    (NclMultiDValData)_NclCreateVal(NULL,
												    NULL,
												    Ncl_MultiDValData,
												    0,
												    (void*)tmp_rot,
												    NULL,
												    n_dims_lat,
												    dimsizes_lat,
												    TEMPORARY,
												    NULL,
												    nclTypefloatClass),rot_att_list_ptr,nrotatts);
					}
					NclFree(dimsizes_lat);
				} else {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"NclGRIB: Couldn't handle dimension information returned by grid decoding");
					is_err = NhlFATAL;
				}
			} else {
				Grib2InternalVarList	*iv;
				int dnum1, dnum2;
				int count = 0;
				if(dstep->dim_inq->is_gds==50) {
					step->var_info.dim_sizes[current_dim+1] = dstep->dim_inq->size;
					dnum1 = step->var_info.file_dim_num[current_dim+1] = dstep->dim_inq->dim_number;
					step->var_info.dim_sizes[current_dim+2] = dstep->next->dim_inq->size;
					dnum2 = step->var_info.file_dim_num[current_dim+2] = dstep->next->dim_inq->dim_number;
					step->var_info.dim_sizes[current_dim] = 2;
					step->var_info.file_dim_num[current_dim] = dstep->dim_inq->dim_number-1;
					step->var_info.doff = 2;
				} else {
					step->var_info.dim_sizes[current_dim] = dstep->dim_inq->size;
					dnum1 = step->var_info.file_dim_num[current_dim] = dstep->dim_inq->dim_number;
					step->var_info.dim_sizes[current_dim+1] = dstep->next->dim_inq->size;
					dnum2 = step->var_info.file_dim_num[current_dim+1] = dstep->next->dim_inq->dim_number;
					step->var_info.doff = 1;
				}
				/* find the auxiliary coordinate variables if they exist */
				for (iv = therec->internal_var_list; iv != NULL; iv = iv->next) {
					if (iv->int_var->var_info.num_dimensions != 2)
						continue;
					if ( !(iv->int_var->var_info.file_dim_num[0] == dnum1 &&
					       iv->int_var->var_info.file_dim_num[1] == dnum2)) 
						continue;
					if (strstr(NrmQuarkToString(iv->int_var->var_info.var_name_quark),"rot"))
						continue;
					step->aux_coords[count] = iv->int_var->var_info.var_name_quark;
					count++;
					if (count == 2) {
						break;
					}
				}
			}
		}
		if(is_err > NhlFATAL) {

			last = step;	
			step = step->next;
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"NclGRIB: Deleting reference to parameter because of decoding error");
			is_err = NhlNOERROR;
			if(last != NULL) {
				last->next = step->next;
			} else {
				therec->var_list = step->next;
			}
			tmpstep = step;
			step = step->next;
			_Grib2FreeParamRec(tmpstep);
			therec->n_vars--;
		}
	}

    _g2CreateSupplementaryTimeVariables(therec);

	return;
}



static void Grib2PrintRecords
# if    NhlNeedProto
(G2Rec **g2rec)
# else
(g2rec)
    G2Rec   **g2rec;
# endif
{
    int i = 0,
        j = 0,
        nr = 0;

    nr = g2rec[0]->numrecs;

    fprintf(stdout, "\nGRIB2 Records (total: %d)\n\n", nr);
    
    for (i = 0; i < nr; i++) {
        fprintf(stdout, "GRIB v2 record # %d (of %d)\n\n", i + 1, nr);

        fprintf(stdout, "Offset: %d\n", g2rec[i]->offset);
        fprintf(stdout, "Record Size: %d\n\n", g2rec[i]->rec_size);

        fprintf(stdout, "Section 0\n");
        fprintf(stdout, "\t section ID: %d\n", g2rec[i]->sec0.secid);
        fprintf(stdout, "\t discipline: %d\n", g2rec[i]->sec0.discipline);
        fprintf(stdout, "\t edition: %d\n", g2rec[i]->sec0.edition);
        fprintf(stdout, "\t msglen: %d\n", g2rec[i]->sec0.msglen);
        fprintf(stdout, "\n");
    
        fprintf(stdout, "Section 1\n");
        fprintf(stdout, "\t section ID: %d\n", g2rec[i]->sec1.secid);
        fprintf(stdout, "\t center ID: %d\n", g2rec[i]->sec1.centerID);
        fprintf(stdout, "\t center name: %s\n", g2rec[i]->sec1.center_name);
        if (g2rec[i]->sec1.subcenterID >= 0)
            fprintf(stdout, "\t subcenter ID: %d\n", g2rec[i]->sec1.subcenterID);
        if (g2rec[i]->sec1.subcenter_name != NULL)
            fprintf(stdout, "\t subcenter name: %s\n", g2rec[i]->sec1.subcenter_name);
        fprintf(stdout, "\t master table version: %d\n", g2rec[i]->sec1.master_table_ver);
        fprintf(stdout, "\t local table version: %d\n", g2rec[i]->sec1.local_table_ver);
        fprintf(stdout, "\t ref time: %d\n", g2rec[i]->sec1.ref_time);
        fprintf(stdout, "\t sig ref time: %s\n", g2rec[i]->sec1.sig_ref_time);

        fprintf(stdout, "\t\t date: %d %d %d : %d %d %d\n",
            g2rec[i]->sec1.date_time.year, g2rec[i]->sec1.date_time.mon,
            g2rec[i]->sec1.date_time.day, g2rec[i]->sec1.date_time.hour,
            g2rec[i]->sec1.date_time.min, g2rec[i]->sec1.date_time.sec);
        fprintf(stdout, "\t\t data date: %ld\n", g2rec[i]->sec1.date_time.dataDate);
        fprintf(stdout, "\t\t data time: %ld\n", g2rec[i]->sec1.date_time.dataTime);

        fprintf(stdout, "\t production status: %d\n", g2rec[i]->sec1.prod_status);
        fprintf(stdout, "\t proc production status: %s\n", g2rec[i]->sec1.proc_prod_status);
        fprintf(stdout, "\t data type: %d\n", g2rec[i]->sec1.data_type);
        if (g2rec[i]->sec1.proc_data_type != NULL)
            fprintf(stdout, "\t proc data type: %s\n", g2rec[i]->sec1.proc_data_type);
        fprintf(stdout, "\n");

        fprintf(stdout, "Number of repeated fields: %d\n", g2rec[i]->num_rptd);
        for (j = 0; j < g2rec[i]->num_rptd; j++) {
            fprintf(stdout, "Section 2\n");
            fprintf(stdout, "\t section ID: %d\n", g2rec[i]->sec2[j]->secid);
            fprintf(stdout, "\t local len: %d\n", g2rec[i]->sec2[j]->locallen);
            fprintf(stdout, "\n");

            fprintf(stdout, "Section 3\n");
            fprintf(stdout, "\t section ID: %d\n", g2rec[i]->sec3[j]->secid);
            fprintf(stdout, "\t grid def src: %d\n", g2rec[i]->sec3[j]->grid_def_src);
            fprintf(stdout, "\t grid def name: %s\n", g2rec[i]->sec3[j]->grid_def_name);
            fprintf(stdout, "\t num grid data pts: %d\n", g2rec[i]->sec3[j]->num_grid_data_pts);
            fprintf(stdout, "\t num oct opt: %d\n", g2rec[i]->sec3[j]->num_oct_opt);
            fprintf(stdout, "\t interp opt num pts: %d\n", g2rec[i]->sec3[j]->interp_opt_num_pts);
            fprintf(stdout, "\t interp opt name: %s\n", g2rec[i]->sec3[j]->interp_opt_name);
            fprintf(stdout, "\t grid def templ num: %d\n", g2rec[i]->sec3[j]->grid_def_templ_num);

            fprintf(stdout, "\t Shape of Earth info:\n");
            fprintf(stdout, "\t\t shape of earth: %d\n",
                g2rec[i]->sec3[j]->shape_of_earth->shapeOfEarth);
            fprintf(stdout, "\t\t earth shape: %s\n",
                g2rec[i]->sec3[j]->shape_of_earth->earthShape);
            fprintf(stdout, "\t\t scale factor rad sph earth: %d\n",
                g2rec[i]->sec3[j]->shape_of_earth->scale_factor_rad_sph_earth);
            fprintf(stdout, "\t\t scaled val rad sph earth: %d\n",
                g2rec[i]->sec3[j]->shape_of_earth->scaled_val_rad_sph_earth);
            fprintf(stdout, "\t\t scale factor major exis obl sph earth: %d\n",
                g2rec[i]->sec3[j]->shape_of_earth->scale_factor_maj_axis_obl_sph_earth);
            fprintf(stdout, "\t\t scaled val maj axis obl sph earth: %d\n",
                g2rec[i]->sec3[j]->shape_of_earth->scaled_val_maj_axis_obl_sph_earth);
            fprintf(stdout, "\t\t scale factor min axis obl sph earth: %d\n",
                g2rec[i]->sec3[j]->shape_of_earth->scale_factor_min_axis_obl_sph_earth);
            fprintf(stdout, "\t\t scaled val min axis obl sph earth: %d\n",
                g2rec[i]->sec3[j]->shape_of_earth->scaled_val_min_axis_obl_sph_earth);
            fprintf(stdout, "\t\t npts along parallel: %d\n",
                g2rec[i]->sec3[j]->shape_of_earth->npts_along_parallel);
            fprintf(stdout, "\t\t npts along meridian: %d\n",
                g2rec[i]->sec3[j]->shape_of_earth->npts_along_meridian);
            fprintf(stdout, "\t\t anl init prod domain: %d\n",
                g2rec[i]->sec3[j]->shape_of_earth->angl_init_prod_domain);
            fprintf(stdout, "\t\t subdiv basic angle: %d\n",
                g2rec[i]->sec3[j]->shape_of_earth->subdiv_basic_angle);
            fprintf(stdout, "\t\t lat first gridpt: %d\n",
                g2rec[i]->sec3[j]->shape_of_earth->lat_first_gridpt);
            fprintf(stdout, "\t\t lat last gridpt: %d\n",
                g2rec[i]->sec3[j]->shape_of_earth->lat_last_gridpt);
            fprintf(stdout, "\t\t lon first gridpt: %d\n",
                g2rec[i]->sec3[j]->shape_of_earth->lon_first_gridpt);
            fprintf(stdout, "\t\t lon last gridpt: %d\n",
                g2rec[i]->sec3[j]->shape_of_earth->lon_last_gridpt);
            fprintf(stdout, "\t\t idir incr: %d\n",
                g2rec[i]->sec3[j]->shape_of_earth->idir_incr);
            fprintf(stdout, "\t\t jdir incr: %d\n",
                g2rec[i]->sec3[j]->shape_of_earth->jdir_incr);
            fprintf(stdout, "\t\t idir incr scaled: %.3f\n",
                g2rec[i]->sec3[j]->shape_of_earth->idir_incr_scaled);
            fprintf(stdout, "\t\t jdir incr scaled: %.3f\n",
                g2rec[i]->sec3[j]->shape_of_earth->jdir_incr_scaled);

            fprintf(stdout, "\t Resolution Component Flags info:\n");
            fprintf(stdout, "\t\t i dir: %hd\n", g2rec[i]->sec3[j]->res_comp->idir_given);
            fprintf(stdout, "\t\t j dir: %hd\n", g2rec[i]->sec3[j]->res_comp->jdir_given);
            fprintf(stdout, "\t\t uv vectors: %hd\n", g2rec[i]->sec3[j]->res_comp->uv_vectors);

            fprintf(stdout, "\t Scan Mode Flags\n");
            fprintf(stdout, "\t\t first rowcol idir: %hd\n",
                g2rec[i]->sec3[j]->scan_mode->idir);
            fprintf(stdout, "\t\t first rowcol jdir: %hd\n",
                g2rec[i]->sec3[j]->scan_mode->jdir);
            fprintf(stdout, "\t\t adj ijdir consec: %hd\n",
                g2rec[i]->sec3[j]->scan_mode->adj_ijdir_consec);
            fprintf(stdout, "\t\t scan dir: %hd\n",
                g2rec[i]->sec3[j]->scan_mode->scan_dir);

            fprintf(stdout, "\t lat first gridpt: %3.0f\n", g2rec[i]->sec3[j]->lat_first_gridpt);
            fprintf(stdout, "\t lon first gridpt: %3.0f\n", g2rec[i]->sec3[j]->lon_first_gridpt);
            fprintf(stdout, "\t lat last gridpt: %3.0f\n", g2rec[i]->sec3[j]->lat_last_gridpt);
            fprintf(stdout, "\t lon last gridpt: %3.0f\n", g2rec[i]->sec3[j]->lon_last_gridpt);

            fprintf(stdout, "\t grid list num oct num: %d\n",
                g2rec[i]->sec3[j]->grid_list_num_oct_num);
            fprintf(stdout, "\n");

            fprintf(stdout, "Section 4:\n");
            fprintf(stdout, "\t section ID: %d\n", g2rec[i]->sec4[j]->secid);
            fprintf(stdout, "\t pds num: %d\n", g2rec[i]->sec4[j]->pds_num);
            fprintf(stdout, "\t prod def name: %s\n", g2rec[i]->sec4[j]->prod_def_name);

            fprintf(stdout, "\t Product Parameters:\n");
            fprintf(stdout, "\t\t param cat: %d\n",
                g2rec[i]->sec4[j]->prod_params->param_cat);
            fprintf(stdout, "\t\t param cat name: %s\n",
                g2rec[i]->sec4[j]->prod_params->param_cat_name);
            fprintf(stdout, "\t\t param num: %d\n",
                g2rec[i]->sec4[j]->prod_params->param_num);
            fprintf(stdout, "\t\t param name: %s\n",
                g2rec[i]->sec4[j]->prod_params->param_name);
            if (g2rec[i]->sec4[j]->prod_params->short_name != NULL)
                fprintf(stdout, "\t\t param short name: %s\n",
                    g2rec[i]->sec4[j]->prod_params->short_name);
            if (g2rec[i]->sec4[j]->prod_params->units != NULL)
                fprintf(stdout, "\t\t param units: %s\n",
                    g2rec[i]->sec4[j]->prod_params->units);
            fprintf(stdout, "\t\t gen process: %d\n",
                g2rec[i]->sec4[j]->prod_params->gen_process);
            fprintf(stdout, "\t\t gen process name: %s\n",
                g2rec[i]->sec4[j]->prod_params->gen_proc_name);
            fprintf(stdout, "\t\t bkgd gen process: %d\n",
                g2rec[i]->sec4[j]->prod_params->bkgd_gen_process);
            fprintf(stdout, "\t\t gen process ID: %d\n",
                g2rec[i]->sec4[j]->prod_params->gen_processID);
            fprintf(stdout, "\t\t hrs after reftime cutoff: %d\n",
                g2rec[i]->sec4[j]->prod_params->hrs_after_reftime_cutoff);
            fprintf(stdout, "\t\t min after reftime cutoff: %d\n",
                g2rec[i]->sec4[j]->prod_params->min_after_reftime_cutoff);
            fprintf(stdout, "\t\t time range: %d\n",
                g2rec[i]->sec4[j]->prod_params->time_range);
            fprintf(stdout, "\t\t time range unit: %s\n",
                g2rec[i]->sec4[j]->prod_params->time_range_unit);
            fprintf(stdout, "\t\t forecast time: %d\n",
                g2rec[i]->sec4[j]->prod_params->forecast_time);
            fprintf(stdout, "\t\t typeof first fixed sfc: %d\n",
                g2rec[i]->sec4[j]->prod_params->typeof_first_fixed_sfc);
            fprintf(stdout, "\t\t first fixed sfc: %s\n",
                g2rec[i]->sec4[j]->prod_params->first_fixed_sfc);
            fprintf(stdout, "\t\t first fixed sfc units: %s\n",
                g2rec[i]->sec4[j]->prod_params->units_first_fixed_sfc);
            fprintf(stdout, "\t\t scale factor first fixed sfc: %d\n",
                g2rec[i]->sec4[j]->prod_params->scale_factor_first_fixed_sfc);
            fprintf(stdout, "\t\t scaled val first fixed sfc: %d\n",
                g2rec[i]->sec4[j]->prod_params->scaled_val_first_fixed_sfc);

            fprintf(stdout, "\t\t typeof second fixed sfc: %d\n",
                g2rec[i]->sec4[j]->prod_params->typeof_second_fixed_sfc);
            fprintf(stdout, "\t\t second fixed sfc: %s\n",
                g2rec[i]->sec4[j]->prod_params->second_fixed_sfc);
            if (g2rec[i]->sec4[j]->prod_params->typeof_second_fixed_sfc != 255) {
                fprintf(stdout, "\t\t second fixed sfc units: %s\n",
                        g2rec[i]->sec4[j]->prod_params->units_second_fixed_sfc);
                fprintf(stdout, "\t\t scale factor second fixed sfc: %d\n",
                        g2rec[i]->sec4[j]->prod_params->scale_factor_second_fixed_sfc);
                fprintf(stdout, "\t\t scaled val second fixed sfc: %d\n",
                        g2rec[i]->sec4[j]->prod_params->scaled_val_second_fixed_sfc);
            }


            switch (g2rec[i]->sec4[j]->pds_num) {
                case 0:
                    /*
                     * Analysis or forecast at a horizontal level or in a
                     * horizontal layer at a point in time.
                     */
                    break;

                case 1:
                    /*
                     * Individual ensemble forecast, control and perturbed, at a
                     * horizontal level or in a horizontal layer at a point in time.
                     */
                    break;

                case 2:
                    /*
                     * Derived forecasts based on all ensemble members at a
                     * horizontal level or in a horizontal layer at a point
                     * in time.
                     */
                    break;

                case 3:
                    /*
                     * Derived forecasts based on a cluster of ensemble members
                     * over a rectangular area at a horizontal level or in a
                     * horizontal layer at a point in time.
                     */
                    break;

                case 4:
                    /*
                     * Derived forecasts based on a cluster of ensemble members
                     * over a circular area at a horizontal level or i
                     *  a horizontal layer at a point in time.
                     */
                    break;

                case 5:
                    /*
                     * Probability forecasts at a horizontal level or in a
                     * horizontal layer at a point in time.
                     */
                    break;

                case 6:
                    /*
                     * Percentile forecasts at a horizontal level or in a
                     * horizontal layer at a point in time.
                     */
                    break;

                case 7:
                    /*
                     * Analysis or forecast error at a horizontal level or
                     * in a horizontal layer at a point in time.
                     */
                    break;

                case 8:
                    /*
                     * Average, accumulation, extreme values or other
                     * statistically processed values at a horizontal level
                     * or in a horizontal layer in a continuous or
                     * non-continuous time interval.
                     */
                    break;

                case 9:
                    /*
                     * Probability forecasts at a horizontal level or in a
                     * horizontal layer in a continuous or non-continuous
                     * time interval.
                     */
                    break;

                case 10:
                    /*
                     * Percentile forecasts at a horizontal level or in a
                     * horizontal layer in a continuous or non-continuous time 
                     * interval.
                     */
                    break;

                case 11:
                    /*
                     * Individual ensemble forecast, control and perturbed, at a
                     * horizontal level or in a horizontal layer, in a continuous
                     * or non-continuous time interval.
                     */

                    /* EPS info */
                    fprintf(stdout, "\t\t level: %d\n",
                        g2rec[i]->sec4[j]->prod_params->level);
                    fprintf(stdout, "\t\t typeof ensemblefx: %d\n",
                        g2rec[i]->sec4[j]->prod_params->typeof_ensemble_fx);
                    fprintf(stdout, "\t\t ensemble fx type: %s\n",
                        g2rec[i]->sec4[j]->prod_params->ensemble_fx_type);
                    fprintf(stdout, "\t\t perturb num: %d\n",
                        g2rec[i]->sec4[j]->prod_params->perturb_num);
                    fprintf(stdout, "\t\t num fx ensemble: %d\n",
                        g2rec[i]->sec4[j]->prod_params->num_fx_ensemble);
                    fprintf(stdout, "\t\t year end overall time interval: %d\n",
                        g2rec[i]->sec4[j]->prod_params->year_end_overall_time_interval);
                    fprintf(stdout, "\t\t mon end overall time interval: %d\n",
                        g2rec[i]->sec4[j]->prod_params->mon_end_overall_time_interval);
                    fprintf(stdout, "\t\t day end overall time interval: %d\n",
                        g2rec[i]->sec4[j]->prod_params->day_end_overall_time_interval);
                    fprintf(stdout, "\t\t hour end overall time interval: %d\n",
                        g2rec[i]->sec4[j]->prod_params->hour_end_overall_time_interval);
                    fprintf(stdout, "\t\t min end overall time interval: %d\n",
                        g2rec[i]->sec4[j]->prod_params->min_end_overall_time_interval);
                    fprintf(stdout, "\t\t sec end overall time interval: %d\n",
                        g2rec[i]->sec4[j]->prod_params->sec_end_overall_time_interval);
                    fprintf(stdout, "\t\t num timerange spec time interval calc: %d\n",
                        g2rec[i]->sec4[j]->prod_params->num_timerange_spec_time_interval_calc);
                    fprintf(stdout, "\t\t total num missing data vals: %d\n",
                        g2rec[i]->sec4[j]->prod_params->total_num_missing_data_vals);

                    fprintf(stdout, "\t\t typeof stat proc: %d\n",
                        g2rec[i]->sec4[j]->prod_params->typeof_stat_proc);
                    fprintf(stdout, "\t\t stat proc: %s\n",
                        g2rec[i]->sec4[j]->prod_params->stat_proc);
                    fprintf(stdout, "\t\t typeof incr betw fields: %d\n",
                        g2rec[i]->sec4[j]->prod_params->typeof_incr_betw_fields);
                    fprintf(stdout, "\t\t incr betw fields: %s\n",
                        g2rec[i]->sec4[j]->prod_params->incr_betw_fields);
                    fprintf(stdout, "\t\t ind time range unit stat proc done: %d\n",
                        g2rec[i]->sec4[j]->prod_params->ind_time_range_unit_stat_proc_done);
                    fprintf(stdout, "\t\t itr unit: %s\n",
                        g2rec[i]->sec4[j]->prod_params->itr_unit);
                    fprintf(stdout, "\t\t len time range unit stat proc done: %d\n",
                        g2rec[i]->sec4[j]->prod_params->len_time_range_unit_stat_proc_done);
                    fprintf(stdout, "\t\t ind time unit incr succ fields: %d\n",
                        g2rec[i]->sec4[j]->prod_params->ind_time_unit_incr_succ_fields);
                    fprintf(stdout, "\t\t itr succ unit: %s\n",
                        g2rec[i]->sec4[j]->prod_params->itr_succ_unit);
                    fprintf(stdout, "\t\t time incr betw fields: %d\n",
                        g2rec[i]->sec4[j]->prod_params->time_incr_betw_fields);
                    break;

                case 12:
                    /*
                     * Derived forecasts based on all ensemble members at a
                     * horizontal level or in a horizontal layer, in a continuous
                     * or non-continuous time interval.
                     */
                    break;

                case 13:
                    /*
                     * Derived forecasts based on a cluster of ensemble members
                     * over a rectangular area at a horizontal level or in a
                     * horizontal layer, in a continuous or non-continuous time interval.
                     */
                    break;

                case 14:
                    /*
                     * Derived forecasts based on a cluster of ensemble members over
                     * a circular area at a horizontal level or in a horizontal
                     * layer, in a continuous or non-continuous time interval.
                     */
                    break;

                case 20:
                    /*
                     * Radar product.
                     */
                    break;

                case 30:
                    /*
                     * Satellite product.
                     */
                    break;

                case 254:
                    /*
                     * CCITT IA5 character string.
                     */
                    break;

                case 1000:
                    /*
                     * Cross-section of analysis and forecast at a point in time.
                     */
                    break;

                case 1001:
                    /*
                     * Cross-section of averaged or otherwise statistically
                     * processed analysis or forecast over a range of time.
                     */
                    break;

                case 1002:
                    /*
                     * Cross-section of analysis and forecast, averaged or
                     * otherwise statistically-processed over latitude or longitude.
                     */
                    break;

                case 1100:
                    /*
                     * Hovmoller-type grid with no averaging or other statistical
                     * processing.
                     */
                    break;

                case 1101:
                    /*
                     * Hovmoller-type grid with averaging or other statistical processing.
                     */
                    break;

                case 65535:
                    /* Missing */
                    break;

                default:
                    /* Reserved */
                    if (g2rec[i]->sec4[j]->pds_num >= 1102
                            || g2rec[i]->sec4[j]->pds_num <= 32767)
                        /* Reserved by WMO */
                        ;;
                        break;

                    if (g2rec[i]->sec4[j]->pds_num >= 32768
                            || g2rec[i]->sec4[j]->pds_num <= 65534)
                        /* Reserved for Local Use */
                        ;;
                        break;
            }





            fprintf(stdout, "\t num coord: %d\n", g2rec[i]->sec4[j]->num_coord);
            fprintf(stdout, "\n");

            fprintf(stdout, "Section 5:\n");
            fprintf(stdout, "\t section ID: %d\n", g2rec[i]->sec5[j]->secid);
            fprintf(stdout, "\t drt templ num: %d\n", g2rec[i]->sec5[j]->drt_templ_num);
            fprintf(stdout, "\t Data Representation:\n");
            fprintf(stdout, "\t\t ref val: %.3f\n",
                g2rec[i]->sec5[j]->data_repr->refVal);
            fprintf(stdout, "\t\t bin scale factor: %d\n",
                g2rec[i]->sec5[j]->data_repr->bin_scale_factor);
            fprintf(stdout, "\t\t dec scale factor: %d\n",
                g2rec[i]->sec5[j]->data_repr->dec_scale_factor);
            fprintf(stdout, "\t\t nbits packed val: %hd\n",
                g2rec[i]->sec5[j]->data_repr->nbits_packed_val);
            fprintf(stdout, "\t\t typeof field vals: %hd\n",
                g2rec[i]->sec5[j]->data_repr->typeof_field_vals);
            fprintf(stdout, "\t\t field vals: %s\n",
                g2rec[i]->sec5[j]->data_repr->field_vals);
            fprintf(stdout, "\t\t ndpts: %d\n",
                g2rec[i]->sec5[j]->ndpts);
            fprintf(stdout, "\n");

            fprintf(stdout, "Section 6:\n");
            fprintf(stdout, "\t section ID: %d\n", g2rec[i]->sec6[j]->secid);
            fprintf(stdout, "\t unpacked: %d\n", g2rec[i]->sec6[j]->unpacked);
            fprintf(stdout, "\t expanded: %d\n", g2rec[i]->sec6[j]->expanded);
            fprintf(stdout, "\t bmap ind: %d\n", g2rec[i]->sec6[j]->bmap_ind);
            fprintf(stdout, "\n");

            fprintf(stdout, "Section 7:\n");
            fprintf(stdout, "\t section ID: %d\n", g2rec[i]->sec7[j]->secid);
            fprintf(stdout, "\n\n\n");
        }
    }
}


static g2codeTable *Grib2ReadCodeTable
# if NhlNeedProto
(char *center, int secid, char *table, int oct)
# else
(center, secid, table, oct)
    char    *center;
    int secid;
    char    *table;
    int oct;
# endif
{
    FILE    *fp = NULL;
    char    *ctf = NULL;

    char    *s = NULL;

    char    *rol = NULL;
    char    *sep = ":";
/*    char    *sep = "\\/:;=+&^%$@!()?";*/

    int len;

    g2codeTable    *ct;
    int where = 0;


    /* table entries */
    s = NclMalloc(256 * sizeof(char));
    if (s == NULL) {
        NhlPError(NhlFATAL, NhlEUNKNOWN,
            "Code table: could not allocate entry memory.");
        return NULL;
    }


    ct = (g2codeTable *) NclMalloc(1 * sizeof(g2codeTable));
    if (ct == NULL) {
        NhlPError(NhlFATAL, NhlEUNKNOWN,
            "Could not allocate memory for code table.");
        return NULL;
    }

    ct->oct = -1;
    ct->cat = NULL;
    ct->descrip = NULL;
    ct->shname = NULL;
    ct->units = NULL;

    /*
     * construct pathname:
     *      codetable dir + center + section + table
     *      special case: reading the "centers" table -- don't know "center" yet
     */
    if (secid == -1) {
        /* reading table of centers */
        ctf = NclMalloc(strlen(NCL_DEF_GRIB2_CODETABLE_DIR) + strlen(table) + 2);
        if (ctf == NULL) {
            NhlPError(NhlFATAL, NhlEUNKNOWN, "Could not allocate memory for code table.");
            return NULL;
        }

        (void) sprintf(ctf, "%s/%s", NCL_DEF_GRIB2_CODETABLE_DIR, table);
    } else {
        ctf = NclMalloc(strlen(NCL_DEF_GRIB2_CODETABLE_DIR) + strlen(center) + 5 + strlen(table));
        if (ctf == NULL) {
            NhlPError(NhlFATAL, NhlEUNKNOWN, "Could not allocate memory for code table.");
            return NULL;
        }

        (void) sprintf(ctf, "%s/%s/%d/%s", NCL_DEF_GRIB2_CODETABLE_DIR, center, secid, table);
    }

    fp = fopen(ctf, "r");
    if (fp == (FILE *) NULL) {
        NhlPError(NhlFATAL, NhlEUNKNOWN,
            " NclGRIB2: codetable file \"%s/%s\" not a valid GRIB2 code table.\n",
                NCL_DEF_GRIB2_CODETABLE_DIR, ctf);
        return (g2codeTable *) NULL;
    } else {
        while (fgets(s, 256, fp)) {
            s[strlen(s) - 1] = '\0';
            if (s[0] != '#') {
                rol = strtok(s, sep);
                /* first field */
                ct->oct = (int) strtol(rol, (char **) NULL, 10);
                if (ct->oct == oct) {
                    while (rol != NULL) {
                        switch (where) {
                            case 0:
                                /* ct->oct already set */
                                ++where;
                                break;

                            case 1:
                                len = strlen(rol);
                                ct->cat = NclMalloc(len * sizeof(char) + 1);
                                if (ct->cat == NULL) {
                                    NhlPError(NhlFATAL, NhlEUNKNOWN,
                                        "Could not allocate memory for code table entry.");
                                    (void) fclose(fp);
                                    return NULL;
                                }

                                strncpy(ct->cat, rol, len);
                                ++where;
                                break;

                            case 2:
                                len = strlen(rol);
                                ct->descrip = NclMalloc(len * sizeof(char) + 1);
                                if (ct->descrip == NULL) {
                                    NhlPError(NhlFATAL, NhlEUNKNOWN,
                                        "Could not allocate memory for code table entry.");
                                    (void) fclose(fp);
                                    return NULL;
                                }

                                strncpy(ct->descrip, rol, len);
                                ct->descrip[len] = '\0';
                                ++where;
                                break;
                  
                            case 3:
                                len = strlen(rol);
                                ct->shname = NclMalloc(len * sizeof(char) + 1);
                                if (ct->shname == NULL) {
                                    NhlPError(NhlFATAL, NhlEUNKNOWN,
                                        "Could not allocate memory for code table entry.");
                                    (void) fclose(fp);
                                    return NULL;
                                }

                                strncpy(ct->shname, rol, len);
                                ct->shname[len] = '\0';
                                ++where;
                                break;

                            case 4:
                                 /* Some units are of length == 1 */
                                len = strlen(rol);
                                ct->units = NclMalloc(len * sizeof(char) + 1);
                                if (ct->units == NULL) {
                                    NhlPError(NhlFATAL, NhlEUNKNOWN,
                                        "Could not allocate memory for code table entry.");
                                    (void) fclose(fp);
                                    return NULL;
                                }

                                strncpy(ct->units, rol, len);
                                ct->units[len] = '\0';
                                ++where;
                                break;

                            default:
                                NhlPError(NhlFATAL, NhlEUNKNOWN,
                                    "Could not decode code table.");
                                (void) fclose(fp);
                                return NULL;
                                break;
                        }
    
                        rol = strtok(NULL, sep);
                    }

                    (void) fclose(fp);
                    return ct;
                } else {
                    continue;
                }
            }
        }

        /* didn't find entry in the table */
        ct->oct = -1;
        ct->cat = NULL;
        ct->descrip = NULL;
        ct->shname = NULL;
        ct->units = NULL;
        return ct;
    }

    (void) fclose(fp);
    return ct;
}



static int g2InitializeOptions 
# if    NhlNeedProto
(Grib2FileRecord *g2tmp)
# else
(tmp)
    Grib2FileRecord *g2tmp;
# endif /* NhlNeedProto */
{
    Grib2Options *g2options;
    int i;

    g2tmp->n_options = GRIB_NUM_OPTIONS;
	
    g2options = NclMalloc(g2tmp->n_options * sizeof(Grib2Options));
    if (! g2options) {
        NhlPError(NhlFATAL, ENOMEM, NULL);
        return 0;
    }

    g2options[GRIB_THINNED_GRID_INTERPOLATION_OPT].data_type = NCL_string;
    g2options[GRIB_THINNED_GRID_INTERPOLATION_OPT].n_values = 1;
    g2options[GRIB_THINNED_GRID_INTERPOLATION_OPT].values = (void *) NrmStringToQuark("linear");

    g2options[GRIB_INITIAL_TIME_COORDINATE_TYPE_OPT].data_type = NCL_string;
    g2options[GRIB_INITIAL_TIME_COORDINATE_TYPE_OPT].n_values = 1;
    g2options[GRIB_INITIAL_TIME_COORDINATE_TYPE_OPT].values = (void *) NrmStringToQuark("string");

    g2options[GRIB_DEFAULT_NCEP_PTABLE_OPT].data_type = NCL_string;
    g2options[GRIB_DEFAULT_NCEP_PTABLE_OPT].n_values = 1;
    g2options[GRIB_DEFAULT_NCEP_PTABLE_OPT].values = (void *) NrmStringToQuark("operational");

    g2tmp->options = g2options;
    return 1;
}



static void *Grib2InitializeFileRec
# if    NhlNeedProto
(void)
# else
(/* void */)
# endif /* NhlNeedProto */
{
    Grib2FileRecord *g2rec = NULL;

    g2rec = (Grib2FileRecord *) NclCalloc(1, sizeof(Grib2FileRecord));
    if (! g2rec) {
        NhlPError(NhlFATAL, ENOMEM, NULL);
        return NULL;
    }

    g2InitializeOptions(g2rec);
    return (void *) g2rec;
}

static void *Grib2CreateFile
# if    NhlNeedProto
(void *rec, NclQuark path)
# else
(rec, path)
    void    *rec;
    NclQuark path;
# endif /* NhlNeedProto */
{
    NhlPError(NhlFATAL, NhlEUNKNOWN,
        "GRIB v2 files can only be read, not created using NCL");
    return NULL;
}

static void *Grib2OpenFile
# if    NhlNeedProto
(void *rec, NclQuark path, int wr_status)
# else
(rec, path, wr_status)
    void    *rec;
    NclQuark path;
    int wr_status;
# endif /* NhlNeedProto */
{
# define GBUFSZ_T   1024
    FILE    *fd;
    unsigned char buf[4 * GBUFSZ_T];
    int len,
        err,
        i,
        j,
        k;

    /* g2clib API variables */
    unsigned char   *g2buf;
    gribfield   *g2fld;
    g2int   sec0[3],
            sec1[13];

    g2int   nfields,
            nlocal;
    g2int   expand = 0;

    int unpack = 0;
    long    lgrib,
            lskip,
            seek = 0;

    size_t  lengrib;
    int nrecs = 0,
        t_nrecs = 0;

    /* GRIB2 records */
    G2Rec   **g2rec,
            **tmp_g2rec;

    G2Sec2  *tmp_sec2;
    G2Sec3  *tmp_sec3;
    G2Sec4  *tmp_sec4;
    G2Sec5  *tmp_sec5;
    G2Sec6  *tmp_sec6;
    G2Sec7  *tmp_sec7;

    /* codetable variables */
    g2codeTable *ct = NULL;
    char    *ctfnam,
            *table;

    int ctflen = 0;
    char    *center = NULL,         /* center name */
            *sub_center = NULL;     /* subcenter name */
    int center_len = 0;
    int centerid,
        subcenterid;                /* center, subcenter IDs */

    int secid = 0;

    char    tmp_dataDate[16],
            tmp_dataTime[16];

    char    fnam[256];

    /* NCL GRIB2 records */
    Grib2FileRecord *g2frec;
    Grib2RecordInqRec   *g2inqrec = NULL;
    Grib2RecordInqRecList   *g2inqrec_list = NULL,
                            **g2sort = NULL;

    Grib2ParamList  *g2plist = NULL,
                    *g2plist_n = NULL,
                    *g2plist_tmp = NULL;
    G2_TBLE2    *g2name_rec = NULL,
                *g2tmp_name_rec = NULL;



    /* read-only access at this time */
    if (wr_status <= 0) {
        NhlPError(NhlWARNING, NhlEUNKNOWN,
            "GRIB v2 files are read only; opening file read only.");
	}

    /* GRIB v2 records */
    g2rec = (G2Rec **) NclMalloc((nrecs + 1) * sizeof(G2Rec *));
    if (g2rec == NULL) {
        NhlPError(NhlFATAL, NhlEUNKNOWN,
            "Could not allocate memory for GRIB v2 data.");
            NhlFree(g2rec);
            return NULL;
    }
    g2rec[nrecs] = NclMalloc(sizeof(G2Rec));
    if (g2rec[nrecs] == NULL) {
        NhlPError(NhlFATAL, NhlEUNKNOWN,
            "Could not allocate memory for GRIB v2 data entries.");
            NhlFree(g2rec[nrecs]);
            return NULL;
    }

    fd = fopen(NrmQuarkToString(path), "r");
    vbuf = (void *) NclMalloc(4 * getpagesize());
    setvbuf(fd, vbuf, _IOFBF, 4 * getpagesize());

    (void) fseek(fd, 0L, SEEK_SET);

    /*
     * Loop until end of data and/or EOF
     */
    t_nrecs = nrecs;
    for (;;) {
        seekgb(fd, seek, 32 * GBUFSZ_T, &lskip, &lgrib);
        /* EOF or other problem? */
        if (lgrib == 0)
            break;

        g2buf = (unsigned char *) NclMalloc((size_t) lgrib);
        if (g2buf == NULL) {
            NhlPError(NhlFATAL, NhlEUNKNOWN,
                "Could not allocate memory for GRIB v2 raw data.");
                NhlFree(g2rec);
                return NULL;
        }
        err = fseek(fd, lskip, SEEK_SET);
        lengrib = fread(g2buf, sizeof(unsigned char), lgrib, fd);
        seek = lskip + lgrib;

        err = g2_info(g2buf, sec0, sec1, &nfields, &nlocal);
        
        if (nrecs > t_nrecs) {
            ++t_nrecs;
            tmp_g2rec = NclRealloc(g2rec, (nrecs + 1) * sizeof(G2Rec *));
            if (tmp_g2rec == NULL) {
                NhlPError(NhlFATAL, NhlEUNKNOWN,
                "Could not extend memory for GRIB v2 data.");
                NhlFree(g2rec);
                return;
            }
            tmp_g2rec[nrecs] = NclMalloc(sizeof(G2Rec));
            if (tmp_g2rec[nrecs] == NULL) {
                NhlPError(NhlFATAL, NhlEUNKNOWN,
                    "Could not allocate temporary memory for GRIB v2 data.");
                    NhlFree(g2rec);
                    return NULL;
            }

            g2rec = tmp_g2rec;
        }

        g2rec[nrecs]->offset = lskip;
        g2rec[nrecs]->rec_size = lgrib;

        /* GRIB v2 Section 0 */
        secid = 0;
        (void) strcpy(g2rec[nrecs]->sec0.gid, "GRIB");
        g2rec[nrecs]->sec0.secid = 0;
        g2rec[nrecs]->sec0.discipline = sec0[0];
        g2rec[nrecs]->sec0.edition = sec0[1];
        g2rec[nrecs]->sec0.msglen = sec0[2];

        /* GRIB v2 Section 1 */
        secid = 1;
        g2rec[nrecs]->sec1.secid = 1;
        g2rec[nrecs]->sec1.centerID = sec1[0];
        g2rec[nrecs]->sec1.subcenterID = sec1[1];

        /* originating center */
        ct = Grib2ReadCodeTable("", -1, "centers.table", g2rec[nrecs]->sec1.centerID);
        if (ct == (g2codeTable *) NULL) {
            NhlPError(NhlFATAL, NhlEUNKNOWN,
            "Could not read GRIB v2 code table data.");
            NhlFree(g2rec);
            return;
        }

        g2rec[nrecs]->sec1.center_name = NclMalloc(strlen(ct->descrip) + 1);
        (void) strcpy(g2rec[nrecs]->sec1.center_name, ct->descrip);
        /* if center has a "short name" */
        if (ct->shname != NULL) {
            g2rec[nrecs]->sec1.subcenter_name = NclMalloc(strlen(ct->shname) + 1);
            if (g2rec[nrecs]->sec1.subcenter_name == NULL) {
                NhlPError(NhlFATAL, NhlEUNKNOWN,
                    "Could not allocate memory for GRIB v2 entry data.");
                    NhlFree(g2rec);
                    return NULL;
            }
        
            (void) strcpy(g2rec[nrecs]->sec1.subcenter_name, ct->shname);
        } else {
            g2rec[nrecs]->sec1.subcenter_name = NULL;
        }

        /* 
         * Set originating center info for codetable searches.
         * NOTE: not all known centers are represented, only the most commonly
         * used as per GRIB v1 usage in NCL.  Add as necessary.
         */
        if (! center) {
            switch (sec1[0]) {
                case 7:
                case 8:
                case 9:
                    /* NCEP */
                    center = "ncep";
                    center_len = strlen(center);
                    centerid = sec1[0];
                    subcenterid = sec1[1];
                    break;

                case 58:
                    /* US Navy FNMOC */
                    center = "fnmoc";
                    center_len = strlen(center);
                    centerid = sec1[0];
                    subcenterid = sec1[1];
                    break;

                case 59:
                    /* NOAA FSL */
                    center = "fsl";
                    center_len = strlen(center);
                    centerid = sec1[0];
                    subcenterid = sec1[1];
                    break;

                case 60:
                    /* NCAR */
                    center = "ncar";
                    center_len = strlen(center);
                    centerid = sec1[0];
                    subcenterid = sec1[1];
                    break;

                case 78:
                    /* DWD Offenbach */
                    center = "dwd";
                    center_len = strlen(center);
                    centerid = sec1[0];
                    subcenterid = sec1[1];
                    break;

                case 98:
                    /* ECMWF */
                    center = "ecmwf";
                    center_len = strlen(center);
                    centerid = sec1[0];
                    subcenterid = sec1[1];
                    break;

                default:
                    center = NULL;
                    center_len = 0;
                    centerid = subcenterid = -1;
                    break;
            }

            /* codetable filename base length: length of center name + 1 (section ID) */
            ctflen = center_len + 1;
        }

        g2rec[nrecs]->sec1.master_table_ver = sec1[2];
        g2rec[nrecs]->sec1.local_table_ver = sec1[3];

        /* table 1.2: Significance of Reference Time */
        g2rec[nrecs]->sec1.ref_time = sec1[4];
        table = "1.2.table";
        ct = Grib2ReadCodeTable(center, secid, table, g2rec[nrecs]->sec1.ref_time);
        if (ct == (g2codeTable *) NULL) {
            NhlPError(NhlFATAL, NhlEUNKNOWN,
            "Could not read GRIB v2 code table data.");
            NhlFree(g2rec);
            return;
        }

        g2rec[nrecs]->sec1.sig_ref_time = NclMalloc(strlen(ct->descrip) + 1);
        if (g2rec[nrecs]->sec1.sig_ref_time == NULL) {
            NhlPError(NhlFATAL, NhlEUNKNOWN,
                "Could not allocate memory for GRIB v2 data entry.");
                NhlFree(g2rec);
                return NULL;
        }

        (void) strcpy(g2rec[nrecs]->sec1.sig_ref_time, ct->descrip);

        g2rec[nrecs]->sec1.date_time.year = sec1[5];
        g2rec[nrecs]->sec1.date_time.mon = sec1[6];
        g2rec[nrecs]->sec1.date_time.day = sec1[7];
        g2rec[nrecs]->sec1.date_time.hour = sec1[8];
        g2rec[nrecs]->sec1.date_time.min = sec1[9];
        g2rec[nrecs]->sec1.date_time.sec = sec1[10];
        (void) sprintf(tmp_dataDate, "%d%02d%02d", sec1[5], sec1[6], sec1[7]);
        g2rec[nrecs]->sec1.date_time.dataDate = strtol(tmp_dataDate, NULL, 10);
        (void) sprintf(tmp_dataTime, "%02d%02d", sec1[8], sec1[9]);
        g2rec[nrecs]->sec1.date_time.dataTime = (int) strtol(tmp_dataTime, NULL, 10);

        /* table 1.3: Production Status of Data */
        g2rec[nrecs]->sec1.prod_status = sec1[11];
        table = "1.3.table";
        ct = Grib2ReadCodeTable(center, secid, table, g2rec[nrecs]->sec1.prod_status);
        if (ct == (g2codeTable *) NULL) {
            NhlPError(NhlFATAL, NhlEUNKNOWN,
            "Could not read GRIB v2 code table data.");
            NhlFree(g2rec);
            return;
        }

        g2rec[nrecs]->sec1.proc_prod_status = NclMalloc(strlen(ct->descrip) + 1);
        (void) strcpy(g2rec[nrecs]->sec1.proc_prod_status, ct->descrip);

        /* table 1.4: Type of Data */
        g2rec[nrecs]->sec1.data_type = sec1[12];
        table = "1.4.table";
        ct = Grib2ReadCodeTable(center,secid,table, g2rec[nrecs]->sec1.data_type);
        if (ct == (g2codeTable *) NULL) {
            NhlPError(NhlFATAL, NhlEUNKNOWN,
            "Could not read GRIB v2 code table data.");
            NhlFree(g2rec);
            return;
        }
	if (ct->descrip) {
            g2rec[nrecs]->sec1.proc_data_type = NclMalloc(strlen(ct->descrip) + 1);
	    (void) strcpy(g2rec[nrecs]->sec1.proc_data_type, ct->descrip);
	}
	else {
		g2rec[nrecs]->sec1.proc_data_type = NULL;
	}

        /*
         * Get GRIB v2 sections 2 thru 7.  These sections may be repeated
         * for each record.
         *
         * Variable 'nfields' is number of times sections are repeated.
         * Allocate space for sections 2 thru 7 now.
         */
        g2rec[nrecs]->sec2 = NclMalloc(nfields * sizeof(G2Sec2 *));
        if (g2rec[nrecs]->sec2 == NULL) {
            NhlPError(NhlFATAL, NhlEUNKNOWN,
            "Could not extend memory for GRIB v2 data.");
            NhlFree(g2rec);
            return NULL;
        }

        g2rec[nrecs]->sec3 = NclMalloc(nfields * sizeof(G2Sec3 *));
        if (g2rec[nrecs]->sec3 == NULL) {
            NhlPError(NhlFATAL, NhlEUNKNOWN,
            "Could not extend memory for GRIB v2 data.");
            NhlFree(g2rec);
            return NULL;
        }

        g2rec[nrecs]->sec4 = NclMalloc(nfields * sizeof(G2Sec4 *));
        if (g2rec[nrecs]->sec4 == NULL) {
            NhlPError(NhlFATAL, NhlEUNKNOWN,
            "Could not extend memory for GRIB v2 data.");
            NhlFree(g2rec);
            return NULL;
        }

        g2rec[nrecs]->sec5 = NclMalloc(nfields * sizeof(G2Sec5 *));
        if (g2rec[nrecs]->sec5 == NULL) {
            NhlPError(NhlFATAL, NhlEUNKNOWN,
            "Could not extend memory for GRIB v2 data.");
            NhlFree(g2rec);
            return NULL;
        }

        g2rec[nrecs]->sec6 = NclMalloc(nfields * sizeof(G2Sec6 *));
        if (g2rec[nrecs]->sec6 == NULL) {
            NhlPError(NhlFATAL, NhlEUNKNOWN,
            "Could not extend memory for GRIB v2 data.");
            NhlFree(g2rec);
            return NULL;
        }

        g2rec[nrecs]->sec7 = NclMalloc(nfields * sizeof(G2Sec7 *));
        if (g2rec[nrecs]->sec7 == NULL) {
            NhlPError(NhlFATAL, NhlEUNKNOWN,
            "Could not extend memory for GRIB v2 data.");
            NhlFree(g2rec);
            return NULL;
        }

        for (i = 0; i < nfields; i++) {
            g2rec[nrecs]->sec2[i] = NclMalloc(sizeof(G2Sec2));
            g2rec[nrecs]->sec3[i] = NclMalloc(sizeof(G2Sec3));
            g2rec[nrecs]->sec4[i] = NclMalloc(sizeof(G2Sec4));
            g2rec[nrecs]->sec5[i] = NclMalloc(sizeof(G2Sec5));
            g2rec[nrecs]->sec6[i] = NclMalloc(sizeof(G2Sec6));
            g2rec[nrecs]->sec7[i] = NclMalloc(sizeof(G2Sec7));
        }
        g2rec[nrecs]->num_rptd = nfields;

        /* loop over repeated sections, creating new records */
        for (i = 0; i < nfields; i++) {
            err = g2_getfld(g2buf, i + 1, unpack, expand, &g2fld);
            g2rec[nrecs]->version = g2fld->version;

            /* GRIB2 section 2 */
            secid = 2;
            g2rec[nrecs]->sec2[i]->secid = 2;
            g2rec[nrecs]->sec2[i]->locallen = g2fld->locallen;
            if (g2rec[nrecs]->sec2[i]->locallen > 0) {
                g2rec[nrecs]->sec2[i]->local = NclMalloc(g2fld->locallen + 1);
                memcpy(g2rec[nrecs]->sec2[i]->local, g2fld->local, g2fld->locallen);
            } else{
                g2rec[nrecs]->sec2[i]->local = NULL;
            }

            /* GRIB2 section 3   GDS */
            secid = 3;
            g2rec[nrecs]->sec3[i]->secid = 3;

            /* table 3.0: Source of Grid Defn */
            g2rec[nrecs]->sec3[i]->grid_def_src = g2fld->griddef;
            table = "3.0.table";
            ct = Grib2ReadCodeTable(center, secid, table, g2rec[nrecs]->sec3[i]->grid_def_src);
            if (ct == (g2codeTable *) NULL) {
                NhlPError(NhlFATAL, NhlEUNKNOWN,
                "Could not read GRIB v2 code table data.");
                NhlFree(g2rec);
                return;
            }

            switch (ct->oct) {
                case 0:
                    /* table 3.1: Grid Defn Template Num */
                    table = "3.1.table";
                    ct = Grib2ReadCodeTable(center, secid, table, ct->oct);
                    if (ct == (g2codeTable *) NULL) {
                        NhlPError(NhlFATAL, NhlEUNKNOWN,
                        "Could not read GRIB v2 code table data.");
                        NhlFree(g2rec);
                        return;
                    }

                    g2rec[nrecs]->sec3[i]->grid_num = ct->oct;
                    g2rec[nrecs]->sec3[i]->grid_def_name = NclMalloc(strlen(ct->descrip) + 1);
                    (void) strcpy(g2rec[nrecs]->sec3[i]->grid_def_name, ct->descrip);
                    break;

                case 1:
                    /* grid def det'd by originating center */
                    break;

                case 255:
                    /* grid defn doesn't apply */
                    g2rec[nrecs]->sec3[i]->grid_num = ct->oct;  /* 255 */
                    g2rec[nrecs]->sec3[i]->grid_def_name
                            = NclMalloc(strlen("A grid definition doesn't apply.") + 1);
                    (void) strcpy(g2rec[nrecs]->sec3[i]->grid_def_name,
                            "A grid definition doesn't apply.");

                    /* FALLTHROUGH */
                default:
                    /* grid def doesn't apply */
                    break;
            }

            g2rec[nrecs]->sec3[i]->num_grid_data_pts = g2fld->ngrdpts;
            g2rec[nrecs]->sec3[i]->num_oct_opt = g2fld->numoct_opt;

            /* table 3.11: Interpretation of List of Numbers Defining Number of Pts */
            g2rec[nrecs]->sec3[i]->interp_opt_num_pts = g2fld->interp_opt;
            table = "3.11.table";
            ct = Grib2ReadCodeTable(center, secid, table,
                    g2rec[nrecs]->sec3[i]->interp_opt_num_pts);
            if (ct == (g2codeTable *) NULL) {
                NhlPError(NhlFATAL, NhlEUNKNOWN,
                "Could not read GRIB v2 code table data.");
                NhlFree(g2rec);
                return;
            }

            g2rec[nrecs]->sec3[i]->interp_opt_name = NclMalloc(strlen(ct->descrip) + 1);
            (void) strcpy(g2rec[nrecs]->sec3[i]->interp_opt_name, ct->descrip);
            g2rec[nrecs]->sec3[i]->grid_def_templ_num = g2fld->igdtnum;

            if (g2fld->numoct_opt != 0) {
                /* non-regular grid */
                g2rec[nrecs]->sec3[i]->grid_list_num_oct_num = g2fld->num_opt;
                g2rec[nrecs]->sec3[i]->grid_list_num_oct_opt
                        = NclMalloc(g2fld->num_opt * sizeof(int *));
                memcpy(g2rec[nrecs]->sec3[i]->grid_list_num_oct_opt, g2fld->list_opt,
                        g2fld->num_opt * sizeof(int));
            }

            /* table 3.2: Shape of Earth */
            g2rec[nrecs]->sec3[i]->shape_of_earth = NclMalloc(sizeof(G2shapeOfEarth));
            g2rec[nrecs]->sec3[i]->res_comp = NclMalloc(sizeof(G2resComponentFlags));
            g2rec[nrecs]->sec3[i]->scan_mode = NclMalloc(sizeof(G2scanModeFlags));

            g2rec[nrecs]->sec3[i]->shape_of_earth->shapeOfEarth = g2fld->igdtmpl[0];
            table = "3.2.table";
            ct = Grib2ReadCodeTable(center, secid, table,
                    g2rec[nrecs]->sec3[i]->shape_of_earth->shapeOfEarth);
            if (ct == (g2codeTable *) NULL) {
                NhlPError(NhlFATAL, NhlEUNKNOWN,
                "Could not read GRIB v2 code table data.");
                NhlFree(g2rec);
                return;
            }

            g2rec[nrecs]->sec3[i]->shape_of_earth->earthShape = NclMalloc(strlen(ct->descrip) + 1);
            (void) strcpy(g2rec[nrecs]->sec3[i]->shape_of_earth->earthShape, ct->descrip);

            g2rec[nrecs]->sec3[i]->shape_of_earth->scale_factor_rad_sph_earth
                    = (int) g2fld->igdtmpl[1];
            g2rec[nrecs]->sec3[i]->shape_of_earth->scaled_val_rad_sph_earth
                    = (int) g2fld->igdtmpl[2];
            g2rec[nrecs]->sec3[i]->shape_of_earth->scale_factor_maj_axis_obl_sph_earth
                    = (int) g2fld->igdtmpl[3];
            g2rec[nrecs]->sec3[i]->shape_of_earth->scaled_val_maj_axis_obl_sph_earth
                    = (int) g2fld->igdtmpl[4];
            g2rec[nrecs]->sec3[i]->shape_of_earth->scale_factor_min_axis_obl_sph_earth
                    = (int) g2fld->igdtmpl[5];
            g2rec[nrecs]->sec3[i]->shape_of_earth->scaled_val_min_axis_obl_sph_earth
                    = (int) g2fld->igdtmpl[6];
            g2rec[nrecs]->sec3[i]->shape_of_earth->npts_along_meridian = (int) g2fld->igdtmpl[7];
            g2rec[nrecs]->sec3[i]->shape_of_earth->npts_along_parallel = (int) g2fld->igdtmpl[8];
            g2rec[nrecs]->sec3[i]->shape_of_earth->angl_init_prod_domain = (int) g2fld->igdtmpl[9];
            g2rec[nrecs]->sec3[i]->shape_of_earth->subdiv_basic_angle = (int) g2fld->igdtmpl[10];

            g2rec[nrecs]->sec3[i]->shape_of_earth->lat_first_gridpt = g2fld->igdtmpl[11];
            g2rec[nrecs]->sec3[i]->shape_of_earth->lon_first_gridpt = g2fld->igdtmpl[12];

            g2rec[nrecs]->sec3[i]->res_comp->idir_given = g2getbits(g2fld->igdtmpl[13], 4, 1);
            g2rec[nrecs]->sec3[i]->res_comp->jdir_given = g2getbits(g2fld->igdtmpl[13], 5, 1);
            g2rec[nrecs]->sec3[i]->res_comp->uv_vectors
                    = (short) g2getbits(g2fld->igdtmpl[13], 6, 1);

            g2rec[nrecs]->sec3[i]->shape_of_earth->lat_last_gridpt = g2fld->igdtmpl[14];
            g2rec[nrecs]->sec3[i]->shape_of_earth->lon_last_gridpt = g2fld->igdtmpl[15];

            g2rec[nrecs]->sec3[i]->lat_first_gridpt = g2fld->igdtmpl[11] / G2_SCALE_FACTOR;
            g2rec[nrecs]->sec3[i]->lon_first_gridpt = g2fld->igdtmpl[12] / G2_SCALE_FACTOR;
            g2rec[nrecs]->sec3[i]->lat_last_gridpt = g2fld->igdtmpl[14] / G2_SCALE_FACTOR;
            g2rec[nrecs]->sec3[i]->lon_last_gridpt = g2fld->igdtmpl[15] / G2_SCALE_FACTOR;



            g2rec[nrecs]->sec3[i]->shape_of_earth->idir_incr = (int) g2fld->igdtmpl[16];
            g2rec[nrecs]->sec3[i]->shape_of_earth->jdir_incr = (int) g2fld->igdtmpl[17];
            g2rec[nrecs]->sec3[i]->shape_of_earth->idir_incr_scaled
                    = (float) g2fld->igdtmpl[16] / G2_SCALE_FACTOR;
            g2rec[nrecs]->sec3[i]->shape_of_earth->jdir_incr_scaled
                    = (float) g2fld->igdtmpl[17] / G2_SCALE_FACTOR;

            g2rec[nrecs]->sec3[i]->scan_mode->idir
                    = g2getbits(g2fld->igdtmpl[18], 7, 1);
            g2rec[nrecs]->sec3[i]->scan_mode->jdir
                    = g2getbits(g2fld->igdtmpl[18], 6, 1);

            /*
             * Set idir/jdir directional values.  Need these later for
             * computing non-regular ("quasi") grid lat/lon increments.
             */
            if (g2rec[nrecs]->sec3[i]->scan_mode->idir == 0)
                g2rec[nrecs]->sec3[i]->scan_mode->idir = 1;
            else
                g2rec[nrecs]->sec3[i]->scan_mode->idir = -1;

            if (g2rec[nrecs]->sec3[i]->scan_mode->jdir == 0)
                g2rec[nrecs]->sec3[i]->scan_mode->jdir = -1;
            else
                g2rec[nrecs]->sec3[i]->scan_mode->jdir = 1;

            g2rec[nrecs]->sec3[i]->scan_mode->adj_ijdir_consec
                    = g2getbits(g2fld->igdtmpl[18], 5, 1);
            g2rec[nrecs]->sec3[i]->scan_mode->scan_dir = g2getbits(g2fld->igdtmpl[18], 4, 1);


            /* GRIB2 section 4   PDS */
            secid = 4;
            g2rec[nrecs]->sec4[i]->secid = 4;
            /* table 4.0: Product Defn Template Number */
            g2rec[nrecs]->sec4[i]->pds_num = g2fld->ipdtnum;
            table = "4.0.table";
            ct = Grib2ReadCodeTable(center, secid, table, g2rec[nrecs]->sec4[i]->pds_num);
            if (ct == (g2codeTable *) NULL) {
                NhlPError(NhlFATAL, NhlEUNKNOWN,
                "Could not read GRIB v2 code table data.");
                NhlFree(g2rec);
                return;
            }

            g2rec[nrecs]->sec4[i]->prod_def_name = NclMalloc(strlen(ct->descrip) + 1);
            (void) strcpy(g2rec[nrecs]->sec4[i]->prod_def_name, ct->descrip);

            /* table 4.1: Parameter Category by Product Discipline */
            g2rec[nrecs]->sec4[i]->prod_params = NclMalloc(sizeof(G2prodParams));
            g2rec[nrecs]->sec4[i]->prod_params->param_cat = g2fld->ipdtmpl[0];
            table = "4.1.table";
            ct = Grib2ReadCodeTable(center, secid, table,
                    g2rec[nrecs]->sec4[i]->prod_params->param_cat);
            if (ct == (g2codeTable *) NULL) {
                NhlPError(NhlFATAL, NhlEUNKNOWN,
                "Could not read GRIB v2 code table data.");
                NhlFree(g2rec);
                return;
            }

            g2rec[nrecs]->sec4[i]->prod_params->param_cat_name
                    = NclMalloc(strlen(ct->descrip) + 1);
            (void) strcpy(g2rec[nrecs]->sec4[i]->prod_params->param_cat_name, ct->descrip);

            /*
             * table 4.2.x.y: Product Discipline
             * use sec0.discipline to form table name
             */
            g2rec[nrecs]->sec4[i]->prod_params->param_num = g2fld->ipdtmpl[1];
            memset(fnam, '\0', 256);
            (void) sprintf(fnam, "4.2.%d.%d.table", g2rec[nrecs]->sec0.discipline,
                    g2rec[nrecs]->sec4[i]->prod_params->param_cat);
            ct = Grib2ReadCodeTable(center, secid, fnam,
                    g2rec[nrecs]->sec4[i]->prod_params->param_num);
            if (ct == (g2codeTable *) NULL) {
                NhlPError(NhlFATAL, NhlEUNKNOWN,
                "Could not read GRIB v2 code table data.");
                NhlFree(g2rec);
                return;
            }

            if (ct->oct != -1) {
                /* found parameter in table */
                g2rec[nrecs]->sec4[i]->prod_params->param_name
                        = NclMalloc(strlen(ct->descrip) + 1);
                (void) strcpy(g2rec[nrecs]->sec4[i]->prod_params->param_name, ct->descrip);

                if (ct->shname != NULL) {
                    g2rec[nrecs]->sec4[i]->prod_params->short_name = NclMalloc(
                            strlen(ct->shname) + 1);
                    (void) strcpy(g2rec[nrecs]->sec4[i]->prod_params->short_name, ct->shname);
                } else {
                    g2rec[nrecs]->sec4[i]->prod_params->short_name = NclMalloc(8 * sizeof(char));
                    (void) sprintf(g2rec[nrecs]->sec4[i]->prod_params->param_name, "VAR_%d",
                            g2rec[nrecs]->sec4[i]->prod_params->param_num);
                }

                if (ct->units != NULL) {
                    g2rec[nrecs]->sec4[i]->prod_params->units = NclMalloc(strlen(ct->units) + 1);
                    (void) strcpy(g2rec[nrecs]->sec4[i]->prod_params->units, ct->units);
                } else {
                    g2rec[nrecs]->sec4[i]->prod_params->units = NclMalloc(strlen("unknown") + 1);
                    (void) strcpy(g2rec[nrecs]->sec4[i]->prod_params->units, "unknown");
                }
            } else {
                /* parameter not found */
                g2rec[nrecs]->sec4[i]->prod_params->param_name
                        = NclMalloc(strlen("Unknown Variable Name") + 1);
                (void) strcpy(g2rec[nrecs]->sec4[i]->prod_params->param_name,
                        "Unknown Variable Name");

                g2rec[nrecs]->sec4[i]->prod_params->short_name = NclMalloc(8 * sizeof(char));
                (void) sprintf(g2rec[nrecs]->sec4[i]->prod_params->param_name, "VAR_%d",
                        g2rec[nrecs]->sec4[i]->prod_params->param_num);

                g2rec[nrecs]->sec4[i]->prod_params->units = NclMalloc(strlen("unknown") + 1);
                (void) strcpy(g2rec[nrecs]->sec4[i]->prod_params->units, "unknown");
            }

            /* table 4.3: Type of Generating Process */
            g2rec[nrecs]->sec4[i]->prod_params->gen_process = g2fld->ipdtmpl[2];
            table = "4.3.table";
            ct = Grib2ReadCodeTable(center, secid, table,
                    g2rec[nrecs]->sec4[i]->prod_params->gen_process);
            if (ct == (g2codeTable *) NULL) {
                NhlPError(NhlFATAL, NhlEUNKNOWN,
                "Could not read GRIB v2 code table data.");
                NhlFree(g2rec);
                return;
            }

            g2rec[nrecs]->sec4[i]->prod_params->gen_proc_name = NclMalloc(strlen(ct->descrip) + 1);
            (void) strcpy(g2rec[nrecs]->sec4[i]->prod_params->gen_proc_name, ct->descrip);
            g2rec[nrecs]->sec4[i]->prod_params->bkgd_gen_process = g2fld->ipdtmpl[3];
            g2rec[nrecs]->sec4[i]->prod_params->gen_processID = g2fld->ipdtmpl[4];
            g2rec[nrecs]->sec4[i]->prod_params->hrs_after_reftime_cutoff = g2fld->ipdtmpl[5];
            g2rec[nrecs]->sec4[i]->prod_params->min_after_reftime_cutoff = g2fld->ipdtmpl[6];

            /* table 4.4: Indicator of Unit of Time Range */
            g2rec[nrecs]->sec4[i]->prod_params->time_range = g2fld->ipdtmpl[7];
            table = "4.4.table";
            ct = Grib2ReadCodeTable(center, secid, table,
                    g2rec[nrecs]->sec4[i]->prod_params->time_range);
            if (ct == (g2codeTable *) NULL) {
                NhlPError(NhlFATAL, NhlEUNKNOWN,
                "Could not read GRIB v2 code table data.");
                NhlFree(g2rec);
                return;
            }

            g2rec[nrecs]->sec4[i]->prod_params->time_range_unit
                    = NclMalloc(strlen(ct->descrip) + 1);
            (void) strcpy(g2rec[nrecs]->sec4[i]->prod_params->time_range_unit, ct->descrip);
            g2rec[nrecs]->sec4[i]->prod_params->forecast_time = g2fld->ipdtmpl[8];

            /* table 4.5: Fixed Surface Types and Units */
            g2rec[nrecs]->sec4[i]->prod_params->typeof_first_fixed_sfc = g2fld->ipdtmpl[9];
            table = "4.5.table";
            ct = Grib2ReadCodeTable(center, secid, table,
                    g2rec[nrecs]->sec4[i]->prod_params->typeof_first_fixed_sfc);
            if (ct == (g2codeTable *) NULL) {
                NhlPError(NhlFATAL, NhlEUNKNOWN,
                "Could not read GRIB v2 code table data.");
                NhlFree(g2rec);
                return;
            }

            g2rec[nrecs]->sec4[i]->prod_params->first_fixed_sfc
                    = NclMalloc(strlen(ct->descrip) + 1);
            (void) strcpy(g2rec[nrecs]->sec4[i]->prod_params->first_fixed_sfc, ct->descrip);
            if (ct->units != NULL) {
                g2rec[nrecs]->sec4[i]->prod_params->units_first_fixed_sfc
                        = NclMalloc(strlen(ct->units) + 1);
                (void) strcpy(g2rec[nrecs]->sec4[i]->prod_params->units_first_fixed_sfc,
                        ct->units);
            }
            g2rec[nrecs]->sec4[i]->prod_params->scale_factor_first_fixed_sfc = g2fld->ipdtmpl[10];
            if (g2fld->ipdtmpl[11] < -127)
                g2rec[nrecs]->sec4[i]->prod_params->scaled_val_first_fixed_sfc = -127;
            else
                g2rec[nrecs]->sec4[i]->prod_params->scaled_val_first_fixed_sfc
                        = g2fld->ipdtmpl[11];


            g2rec[nrecs]->sec4[i]->prod_params->typeof_second_fixed_sfc = g2fld->ipdtmpl[12];
            ct = Grib2ReadCodeTable(center, secid, table,
                g2rec[nrecs]->sec4[i]->prod_params->typeof_second_fixed_sfc);
            if (ct == (g2codeTable *) NULL) {
                NhlPError(NhlFATAL, NhlEUNKNOWN,
                "Could not read GRIB v2 code table data.");
                NhlFree(g2rec);
                return;
            }

            g2rec[nrecs]->sec4[i]->prod_params->second_fixed_sfc
                    = NclMalloc(strlen(ct->descrip) + 1);
            (void) strcpy(g2rec[nrecs]->sec4[i]->prod_params->second_fixed_sfc, ct->descrip);
            if (ct->units != NULL) {
                g2rec[nrecs]->sec4[i]->prod_params->units_second_fixed_sfc
                        = NclMalloc(strlen(ct->units) + 1);
                (void) strcpy(g2rec[nrecs]->sec4[i]->prod_params->units_first_fixed_sfc,
                        ct->units);
            }
            g2rec[nrecs]->sec4[i]->prod_params->scale_factor_second_fixed_sfc = g2fld->ipdtmpl[13];
            if (g2fld->ipdtmpl[14] < -127)
                g2rec[nrecs]->sec4[i]->prod_params->scaled_val_second_fixed_sfc = -127;
            else
                g2rec[nrecs]->sec4[i]->prod_params->scaled_val_second_fixed_sfc
                        = g2fld->ipdtmpl[14];

            switch (g2rec[nrecs]->sec4[i]->prod_params->typeof_first_fixed_sfc) {
                case 1: /* ground or water surface */
                case 9: /* sea bottom */
                    g2rec[nrecs]->sec4[i]->prod_params->level = -1;
                    break;

                default:
                    if (g2rec[nrecs]->sec4[i]->prod_params->scale_factor_first_fixed_sfc < 100)
                        g2rec[nrecs]->sec4[i]->prod_params->level = g2fld->ipdtmpl[1];
                    else
                        g2rec[nrecs]->sec4[i]->prod_params->level = g2fld->ipdtmpl[1] / 100;

                    break;
            }

            /*
             * Depending on type of product, there may or may not be more info
             * available that what's been extracted to this point.  This is determined
             * by the Product Definition Templates (PDTs).
             */
	    g2rec[nrecs]->sec4[i]->prod_params->typeof_stat_proc = 0;
            switch (g2rec[nrecs]->sec4[i]->pds_num) {
                case 0:
                    /*
                     * Analysis or forecast at a horizontal level or in a
                     * horizontal layer at a point in time.
                     */
                    break;

                case 1:
                    /*
                     * Individual ensemble forecast, control and perturbed, at a
                     * horizontal level or in a horizontal layer at a point in time.
                     */
                    break;

                case 2:
                    /*
                     * Derived forecasts based on all ensemble members at a
                     * horizontal level or in a horizontal layer at a point
                     * in time.
                     */
                    break;

                case 3:
                    /*
                     * Derived forecasts based on a cluster of ensemble members
                     * over a rectangular area at a horizontal level or in a
                     * horizontal layer at a point in time.
                     */
                    break;

                case 4:
                    /*
                     * Derived forecasts based on a cluster of ensemble members
                     * over a circular area at a horizontal level or i
                     *  a horizontal layer at a point in time.
                     */
                    break;

                case 5:
                    /*
                     * Probability forecasts at a horizontal level or in a
                     * horizontal layer at a point in time.
                     */
                    break;

                case 6:
                    /*
                     * Percentile forecasts at a horizontal level or in a
                     * horizontal layer at a point in time.
                     */
                    break;

                case 7:
                    /*
                     * Analysis or forecast error at a horizontal level or
                     * in a horizontal layer at a point in time.
                     */
                    break;

                case 8:
                    /*
                     * Average, accumulation, extreme values or other
                     * statistically processed values at a horizontal level
                     * or in a horizontal layer in a continuous or
                     * non-continuous time interval.
                     */
                    break;

                case 9:
                    /*
                     * Probability forecasts at a horizontal level or in a
                     * horizontal layer in a continuous or non-continuous
                     * time interval.
                     */
                    break;

                case 10:
                    /*
                     * Percentile forecasts at a horizontal level or in a
                     * horizontal layer in a continuous or non-continuous time 
                     * interval.
                     */
                    break;

                case 11:
                    /*
                     * Individual ensemble forecast, control and perturbed, at a
                     * horizontal level or in a horizontal layer, in a continuous
                     * or non-continuous time interval.
                     */

                    /* table 4.6: Type of Ensemble Forecast */
                    g2rec[nrecs]->sec4[i]->prod_params->typeof_ensemble_fx = g2fld->ipdtmpl[15];
                    table = "4.6.table";
                    ct = Grib2ReadCodeTable(center, secid, table,
                            g2rec[nrecs]->sec4[i]->prod_params->typeof_ensemble_fx);
                    if (ct == (g2codeTable *) NULL) {
                        NhlPError(NhlFATAL, NhlEUNKNOWN,
                        "Could not read GRIB v2 code table data.");
                        NhlFree(g2rec);
                        return;
                    }

                    g2rec[nrecs]->sec4[i]->prod_params->ensemble_fx_type
                            = NclMalloc(strlen(ct->descrip) + 1);
                    (void) strcpy(g2rec[nrecs]->sec4[i]->prod_params->ensemble_fx_type,
                            ct->descrip);
                    
                    g2rec[nrecs]->sec4[i]->prod_params->perturb_num = g2fld->ipdtmpl[16];
                    g2rec[nrecs]->sec4[i]->prod_params->num_fx_ensemble = g2fld->ipdtmpl[17];

                    /* statistical processing */
                    g2rec[nrecs]->sec4[i]->prod_params->year_end_overall_time_interval
                            = g2fld->ipdtmpl[18];
                    g2rec[nrecs]->sec4[i]->prod_params->mon_end_overall_time_interval
                            = g2fld->ipdtmpl[19];
                    g2rec[nrecs]->sec4[i]->prod_params->day_end_overall_time_interval
                            = g2fld->ipdtmpl[20];
                    g2rec[nrecs]->sec4[i]->prod_params->hour_end_overall_time_interval
                            = g2fld->ipdtmpl[21];
                    g2rec[nrecs]->sec4[i]->prod_params->min_end_overall_time_interval
                            = g2fld->ipdtmpl[22];
                    g2rec[nrecs]->sec4[i]->prod_params->sec_end_overall_time_interval
                            = g2fld->ipdtmpl[23];
                    g2rec[nrecs]->sec4[i]->prod_params->num_timerange_spec_time_interval_calc
                            = g2fld->ipdtmpl[24];

                    g2rec[nrecs]->sec4[i]->prod_params->total_num_missing_data_vals
                            = g2fld->ipdtmpl[25];

                    /* table 4.10: Type of Statistical Processing */
                    g2rec[nrecs]->sec4[i]->prod_params->typeof_stat_proc
                            = g2fld->ipdtmpl[26];
                    table = "4.10.table";
                    ct = Grib2ReadCodeTable(center, secid, table,
                            g2rec[nrecs]->sec4[i]->prod_params->typeof_stat_proc);
                    if (ct == (g2codeTable *) NULL) {
                        NhlPError(NhlFATAL, NhlEUNKNOWN,
                        "Could not read GRIB v2 code table data.");
                        NhlFree(g2rec);
                        return;
                    }

                    g2rec[nrecs]->sec4[i]->prod_params->stat_proc
                            = NclMalloc(strlen(ct->descrip) + 1);
                    (void) strcpy(g2rec[nrecs]->sec4[i]->prod_params->stat_proc,
                            ct->descrip);
    
                    /* table 4.11: Type of Time Intervals */
                    g2rec[nrecs]->sec4[i]->prod_params->typeof_incr_betw_fields
                            = g2fld->ipdtmpl[27];
                    table = "4.11.table";
                    ct = Grib2ReadCodeTable(center, secid, table,
                            g2rec[nrecs]->sec4[i]->prod_params->typeof_incr_betw_fields);
                    if (ct == (g2codeTable *) NULL) {
                        NhlPError(NhlFATAL, NhlEUNKNOWN,
                        "Could not read GRIB v2 code table data.");
                        NhlFree(g2rec);
                        return;
                    }

                    g2rec[nrecs]->sec4[i]->prod_params->incr_betw_fields
                            = NclMalloc(strlen(ct->descrip) + 1);
                    (void) strcpy(g2rec[nrecs]->sec4[i]->prod_params->incr_betw_fields,
                            ct->descrip);
    
                    /* table 4.4: Indicator of Unit of Time Range */
                    g2rec[nrecs]->sec4[i]->prod_params->ind_time_range_unit_stat_proc_done
                            = g2fld->ipdtmpl[28];
                    table = "4.4.table";
                    ct = Grib2ReadCodeTable(center, secid, table,
                        g2rec[nrecs]->sec4[i]->prod_params->ind_time_range_unit_stat_proc_done);
                    if (ct == (g2codeTable *) NULL) {
                        NhlPError(NhlFATAL, NhlEUNKNOWN,
                        "Could not read GRIB v2 code table data.");
                        NhlFree(g2rec);
                        return;
                    }

                    g2rec[nrecs]->sec4[i]->prod_params->itr_unit
                            = NclMalloc(strlen(ct->descrip) + 1);
                    (void) strcpy(g2rec[nrecs]->sec4[i]->prod_params->itr_unit,
                            ct->descrip);
                    g2rec[nrecs]->sec4[i]->prod_params->len_time_range_unit_stat_proc_done
                            = g2fld->ipdtmpl[29];

                    g2rec[nrecs]->sec4[i]->prod_params->ind_time_unit_incr_succ_fields
                            = g2fld->ipdtmpl[30];
                    table = "4.4.table";
                    ct = Grib2ReadCodeTable(center, secid, table,
                            g2rec[nrecs]->sec4[i]->prod_params->ind_time_unit_incr_succ_fields);
                    if (ct == (g2codeTable *) NULL) {
                        NhlPError(NhlFATAL, NhlEUNKNOWN,
                        "Could not read GRIB v2 code table data.");
                        NhlFree(g2rec);
                        return;
                    }

                    g2rec[nrecs]->sec4[i]->prod_params->itr_succ_unit
                            = NclMalloc(strlen(ct->descrip) + 1);
                    (void) strcpy(g2rec[nrecs]->sec4[i]->prod_params->itr_succ_unit,
                            ct->descrip);
                    g2rec[nrecs]->sec4[i]->prod_params->time_incr_betw_fields
                            = g2fld->ipdtmpl[31];
                    break;

                case 12:
                    /*
                     * Derived forecasts based on all ensemble members at a
                     * horizontal level or in a horizontal layer, in a continuous
                     * or non-continuous time interval.
                     */
                    break;

                case 13:
                    /*
                     * Derived forecasts based on a cluster of ensemble members
                     * over a rectangular area at a horizontal level or in a
                     * horizontal layer, in a continuous or non-continuous time interval.
                     */
                    break;

                case 14:
                    /*
                     * Derived forecasts based on a cluster of ensemble members over
                     * a circular area at a horizontal level or in a horizontal
                     * layer, in a continuous or non-continuous time interval.
                     */
                    break;

                case 20:
                    /*
                     * Radar product.
                     */
                    break;

                case 30:
                    /*
                     * Satellite product.
                     */
                    break;

                case 254:
                    /*
                     * CCITT IA5 character string.
                     */
                    break;

                case 1000:
                    /*
                     * Cross-section of analysis and forecast at a point in time.
                     */
                    break;

                case 1001:
                    /*
                     * Cross-section of averaged or otherwise statistically
                     * processed analysis or forecast over a range of time.
                     */
                    break;

                case 1002:
                    /*
                     * Cross-section of analysis and forecast, averaged or
                     * otherwise statistically-processed over latitude or longitude.
                     */
                    break;

                case 1100:
                    /*
                     * Hovmoller-type grid with no averaging or other statistical
                     * processing.
                     */
                    break;

                case 1101:
                    /*
                     * Hovmoller-type grid with averaging or other statistical processing.
                     */
                    break;

                case 65535:
                    /* Missing */
                    break;

                default:
                    /* Reserved */
                    if (g2rec[nrecs]->sec4[i]->pds_num >= 1102
                            || g2rec[nrecs]->sec4[i]->pds_num <= 32767)
                        /* Reserved by WMO */
                        ;;
                        break;

                    if (g2rec[nrecs]->sec4[i]->pds_num >= 32768
                            || g2rec[nrecs]->sec4[i]->pds_num <= 65534)
                        /* Reserved for Local Use */
                        ;;
                        break;
            }


            g2rec[nrecs]->sec4[i]->num_coord = g2fld->num_coord;
            if (g2rec[nrecs]->sec4[i]->num_coord > 0) {
                g2rec[nrecs]->sec4[i]->coord_list = NclMalloc(g2fld->num_coord);
                memcpy(g2rec[nrecs]->sec4[i]->coord_list, g2fld->coord_list, g2fld->num_coord);
            }
        
            /* GRIB2 section 5   DRS */
            secid = 5;
            g2rec[nrecs]->sec5[i]->secid = 5;

            /* table 5.0: Data Representation Template Number */
            g2rec[nrecs]->sec5[i]->drt_templ_num = g2fld->idrtnum;
            table = "5.0.table";
            ct = Grib2ReadCodeTable(center, secid, table, g2rec[nrecs]->sec5[i]->drt_templ_num);
            if (ct == (g2codeTable *) NULL) {
                NhlPError(NhlFATAL, NhlEUNKNOWN,
                "Could not read GRIB v2 code table data.");
                NhlFree(g2rec);
                return;
            }

            g2rec[nrecs]->sec5[i]->drt_desc = NclMalloc(strlen(ct->descrip) + 1);
            (void) strcpy(g2rec[nrecs]->sec5[i]->drt_desc, ct->descrip);

            g2rec[nrecs]->sec5[i]->data_repr = NclMalloc(sizeof(G2dataRepr));
            g2rec[nrecs]->sec5[i]->data_repr->refVal = (double) g2fld->idrtmpl[0];
            g2rec[nrecs]->sec5[i]->data_repr->bin_scale_factor = g2fld->idrtmpl[1];
            g2rec[nrecs]->sec5[i]->data_repr->dec_scale_factor = g2fld->idrtmpl[2];
            g2rec[nrecs]->sec5[i]->data_repr->nbits_packed_val = g2fld->idrtmpl[3];

            /* table 5.1: Type of Original Field Values */
            g2rec[nrecs]->sec5[i]->data_repr->typeof_field_vals = g2fld->idrtmpl[4];
            table = "5.1.table";
            ct = Grib2ReadCodeTable(center, secid, table,
                    g2rec[nrecs]->sec5[i]->data_repr->typeof_field_vals);
            if (ct == (g2codeTable *) NULL) {
                NhlPError(NhlFATAL, NhlEUNKNOWN,
                "Could not read GRIB v2 code table data.");
                NhlFree(g2rec);
                return;
            }

            g2rec[nrecs]->sec5[i]->data_repr->field_vals = NclMalloc(strlen(ct->descrip) + 1);
            (void) strcpy(g2rec[nrecs]->sec5[i]->data_repr->field_vals, ct->descrip);

            /* type allocated is based on value read from table/template */
            g2rec[nrecs]->sec5[i]->ndpts = g2fld->ndpts;

            /* GRIB2 section 6  Bitmap section */
            secid = 6;
            g2rec[nrecs]->sec6[i]->secid = 6;
            g2rec[nrecs]->sec6[i]->unpacked = g2fld->unpacked;
            g2rec[nrecs]->sec6[i]->expanded = g2fld->expanded;

            /* table 6.0: Bitmap Indicator */
            g2rec[nrecs]->sec6[i]->bmap_ind = g2fld->ibmap;
            table = "6.0.table";
            ct = Grib2ReadCodeTable(center, secid, table, g2rec[nrecs]->sec6[i]->bmap_ind);
            if (ct == (g2codeTable *) NULL) {
                NhlPError(NhlFATAL, NhlEUNKNOWN,
                "Could not read GRIB v2 code table data.");
                NhlFree(g2rec);
                return;
            }

            g2rec[nrecs]->sec6[i]->bmap_desc = NclMalloc(strlen(ct->descrip) + 1);
            (void) strcpy(g2rec[nrecs]->sec6[i]->bmap_desc, ct->descrip);

            if (ct->oct == 255) {
                /* no bitmap specified */
                g2rec[nrecs]->sec6[i]->bmap = NULL;
            } else {
                g2rec[nrecs]->sec6[i]->bmap = NULL;
/***		
                g2rec[nrecs]->sec6[i]->bmap = NclMalloc(g2fld->ngrdpts);
                memcpy(g2rec[nrecs]->sec6[i]->bmap, g2fld->bmap, g2fld->ngrdpts);
***/
            }

            /* GRIB2 section 7 */
            secid = 7;
            g2rec[nrecs]->sec7[i]->secid = 7;
/***
            g2rec[nrecs]->sec7[i]->data = (float *) NclMalloc(g2fld->ndpts);
            memcpy(g2rec[nrecs]->sec7[i]->data, g2fld->fld, g2fld->ndpts);
***/

            ++nrecs;
            g2_free(g2fld);
        }

        NclFree(g2buf);
        NclFree(ct);
    }

    for (i = 0; i < nrecs; i++)
        g2rec[i]->numrecs = nrecs;

/* debug */
    Grib2PrintRecords(g2rec);
/* debug */

    g2frec = (Grib2FileRecord *) rec;
    g2frec->n_vars = 0;
    g2frec->var_list = NULL;
    g2frec->wr_status = wr_status;
    g2frec->file_path_q = path;
    g2frec->internal_var_list = NULL;
    g2frec->n_internal_vars = 0;


    for (i = 0; i < nrecs; i++) {
        for (j = 0; j < g2rec[i]->num_rptd; j++) {
            g2inqrec = NclMalloc(sizeof(Grib2RecordInqRec));
            g2inqrec->rec_num = i + 1;
            g2inqrec->the_dat = NULL;
            g2inqrec->version = g2rec[i]->version;
            g2inqrec->var_name = NULL;


            /* PDS */
            g2inqrec->pds = NclMalloc(sizeof(G2_PDS));
            g2inqrec->pds->prod_params = NclMalloc(sizeof(G2prodParams));
            if (g2rec[i]->sec4[j]->num_coord > 0) {
                g2inqrec->pds->coord_list
                    = NclMalloc(g2rec[i]->sec4[j]->num_coord * sizeof(float *));
                memcpy(g2inqrec->pds->coord_list, g2rec[i]->sec4[j]->coord_list,
                        g2rec[i]->sec4[j]->num_coord);
            }

            memcpy(g2inqrec->pds, g2rec[i]->sec4[j], sizeof(G2_PDS));
            g2inqrec->time_range_indicator = g2rec[i]->sec4[j]->prod_params->typeof_stat_proc;
	    /* this is temporary */
	    g2inqrec->time_unit_indicator = 1;

            /* GDS */
            g2inqrec->gds = NclMalloc(sizeof(G2_GDS));
            if (g2rec[i]->sec3[j]->num_oct_opt > 0) {
                g2inqrec->gds->grid_list_num_oct_opt = NclMalloc(
                        g2rec[i]->sec3[j]->num_oct_opt * sizeof(int *));
                memcpy(g2inqrec->gds->grid_list_num_oct_opt,
                        g2rec[i]->sec3[j]->grid_list_num_oct_opt, g2rec[i]->sec3[j]->num_oct_opt);
            }

            g2inqrec->gds->shape_of_earth = NclMalloc(sizeof(G2shapeOfEarth));
            g2inqrec->gds->res_comp = NclMalloc(sizeof(G2resComponentFlags));
            g2inqrec->gds->scan_mode = NclMalloc(sizeof(G2scanModeFlags));
            memcpy(g2inqrec->gds, g2rec[i]->sec3[j], sizeof(G2_GDS));

            /* Bitmap */
/*            g2inqrec->bms_off = NOT NEEDED ? */
/*            g2inqrec->bms_size = NOT NEEDED ? */

            /* Binary Data */
/*            g2inqrec->bds_off = NOT NEEDED ? */
/*            g2inqrec->bds_size = NOT NEEDED ? */

            /* Table 3.0: Source of Grid Defn */
            if (g2rec[i]->sec3[j]->grid_num == 255) {
                g2inqrec->has_gds = 0;
                g2inqrec->gds_type = -1;
            } else {
                g2inqrec->has_gds = 1;
                g2inqrec->gds_type = g2rec[i]->sec3[j]->grid_num;
            }

            if (g2rec[i]->sec6[j]->bmap != NULL)
                g2inqrec->has_bms = 1;
            else
                g2inqrec->has_bms = 0;

            g2inqrec->param_number = g2rec[i]->sec4[j]->prod_params->param_num;
            g2inqrec->grid_number = g2rec[i]->sec3[j]->grid_num;

            g2inqrec->center = NclMalloc(strlen(g2rec[i]->sec1.center_name) + 1);
            (void) strcpy(g2inqrec->center, g2rec[i]->sec1.center_name);

            if (g2rec[i]->sec1.subcenter_name != NULL) {
                g2inqrec->sub_center = NclMalloc(strlen(g2rec[i]->sec1.subcenter_name) + 1);
                (void) strcpy(g2inqrec->sub_center, g2rec[i]->sec1.subcenter_name);
            } else {
                g2inqrec->sub_center = NULL;
            }

            if ((NrmQuark) (g2frec->options[GRIB_THINNED_GRID_INTERPOLATION_OPT].values ==
                    NrmStringToQuark("cubic")))
                g2inqrec->interp_method = 1;
            else
                g2inqrec->interp_method =  0;

            g2inqrec->bds_flags = g2rec[i]->sec5[j]->drt_templ_num;
            g2inqrec->int_or_float = g2rec[i]->sec5[j]->data_repr->typeof_field_vals;

            /*
             * Variable info.  Fields will be populated per code above that reads
             * section 4 (PDS) and provides values read, or default values.
             */
            g2name_rec = NclMalloc(sizeof(G2_TBLE2));
            g2name_rec->num = g2rec[i]->sec4[j]->prod_params->param_num;

            if (g2rec[i]->sec4[j]->prod_params->param_name != NULL) {
                g2name_rec->long_name
                    = NclMalloc(strlen(g2rec[i]->sec4[j]->prod_params->param_name) + 1);
                (void) strcpy(g2name_rec->long_name, g2rec[i]->sec4[j]->prod_params->param_name);
            } else {
                g2name_rec->long_name = NULL;
            }

            if (g2rec[i]->sec4[j]->prod_params->short_name != NULL) {
                g2name_rec->abrev
                    = NclMalloc(strlen(g2rec[i]->sec4[j]->prod_params->short_name) + 1);
                (void) strcpy(g2name_rec->abrev, g2rec[i]->sec4[j]->prod_params->short_name);
            } else {
                g2name_rec->abrev = NULL;
            }

            if (g2rec[i]->sec4[j]->prod_params->units != NULL) {
                g2name_rec->units = NclMalloc(strlen(g2rec[i]->sec4[j]->prod_params->units) + 1);
                (void) strcpy(g2name_rec->units, g2rec[i]->sec4[j]->prod_params->units);
            } else {
                g2name_rec->units = NULL;
            }

            g2inqrec->ptable_rec = NclMalloc(sizeof(G2_TBLE2));
            memcpy(g2inqrec->ptable_rec, g2name_rec, sizeof(G2_TBLE2));

            g2inqrec->var_name = NclMalloc(strlen(g2rec[i]->sec4[j]->prod_params->short_name) + 1);
            (void) strcpy(g2inqrec->var_name, g2rec[i]->sec4[j]->prod_params->short_name);
            g2inqrec->var_name_q = NrmStringToQuark(g2inqrec->var_name);
            g2inqrec->long_name_q = NrmStringToQuark(g2name_rec->long_name);
            g2inqrec->units_q = NrmStringToQuark(g2name_rec->units);

            /* Time */
            g2inqrec->time_period = 0;
            g2inqrec->initial_time.year = g2rec[i]->sec1.date_time.year;
            g2inqrec->initial_time.days_from_jan1 = HeisDayDiff(1, 1,
                g2rec[i]->sec1.date_time.year, g2rec[i]->sec1.date_time.day,
                g2rec[i]->sec1.date_time.mon, g2rec[i]->sec1.date_time.year);
            g2inqrec->initial_time.minute_of_day = g2rec[i]->sec1.date_time.min;

            g2inqrec->year = g2rec[i]->sec1.date_time.year;
            g2inqrec->mon = g2rec[i]->sec1.date_time.mon;
            g2inqrec->day = g2rec[i]->sec1.date_time.day;
            g2inqrec->hour = g2rec[i]->sec1.date_time.hour;
            g2inqrec->min = g2rec[i]->sec1.date_time.min;
            g2inqrec->sec = g2rec[i]->sec1.date_time.sec;

            g2inqrec->time_offset = g2rec[i]->sec4[j]->prod_params->forecast_time
                    - g2rec[i]->sec4[j]->prod_params->hrs_after_reftime_cutoff;

            g2inqrec->per1 = 0;
            g2inqrec->per2 = 0;

            /* Levels */
            g2inqrec->level_indicator = g2rec[i]->sec4[j]->prod_params->typeof_first_fixed_sfc;
            _g2GetLevels(&g2inqrec->level0, &g2inqrec->level1,
                    (int) g2rec[i]->sec4[j]->prod_params->typeof_first_fixed_sfc,
                    (int) g2rec[i]->sec4[j]->prod_params->typeof_second_fixed_sfc);
/*                    (unsigned char *) g2rec[i]->sec4[j]->prod_params->scaled_val_first_fixed_sfc);*/

            /* Ensembles */
            g2inqrec->is_ensemble = 0;
            memset(&g2inqrec->ens, 0, sizeof(G2_ENS));
            /* Ensemble or not?  Determined by PDS number */
            switch (g2rec[i]->sec4[j]->pds_num) {
                case 0:
                    /*
                     * Analysis or forecast at a horizontal level or in a 
                     * horizontal layer at a point in time.
                     */
                    g2inqrec->is_ensemble = 0;
                    break;

                case 1:
                    /*
                     * Individual ensemble forecast, control and perturbed, at a
                     * horizontal level or in a horizontal layer at a point in time.
                     */
                    g2inqrec->is_ensemble = 1;
                    break;

                case 2:
                    /*
                     * Derived forecasts based on all ensemble members at a
                     * horizontal level or in a horizontal layer at a point
                     * in time.
                     */
                    g2inqrec->is_ensemble = 1;
                    break;

                case 3:
                    /*
                     * Derived forecasts based on a cluster of ensemble members
                     * over a rectangular area at a horizontal level or in a
                     * horizontal layer at a point in time.
                     */
                    g2inqrec->is_ensemble = 1;
                    break;

                case 4:
                    /*
                     * Derived forecasts based on a cluster of ensemble members
                     * over a circular area at a horizontal level or i
                     *  a horizontal layer at a point in time.
                     */
                    g2inqrec->is_ensemble = 1;
                    break;

                case 11:
                    /*
                     * Individual ensemble forecast, control and perturbed, at a
                     * horizontal level or in a horizontal layer, in a continuous
                     * or non-continuous time interval.
                     */
                    g2inqrec->is_ensemble = 1;
                    g2inqrec->ens.extension_type = 1;   /* ensemble */
                    g2inqrec->ens.type = g2rec[i]->sec4[j]->prod_params->typeof_ensemble_fx;
/*                    g2inqrec->ens.id = */
                    break;

                case 12:
                    /*
                     * Derived forecasts based on all ensemble members at a
                     * horizontal level or in a horizontal layer, in a continuous
                     * or non-continuous time interval.
                    g2inqrec->is_ensemble = 1;
                    break;
                     */

                case 13:
                    /*
                     * Derived forecasts based on a cluster of ensemble members
                     * over a rectangular area at a horizontal level or in a
                     * horizontal layer, in a continuous or non-continuous time interval.
                     */
                    g2inqrec->is_ensemble = 1;
                    break;

                case 14:
                    /*
                     * Derived forecasts based on a cluster of ensemble members over
                     * a circular area at a horizontal level or in a horizontal
                     * layer, in a continuous or non-continuous time interval.
                     */
                    g2inqrec->is_ensemble = 1;
                    break;

                default:
                    g2inqrec->is_ensemble = 0;
                    break;
            }

            if (g2frec->var_list == NULL) {
                g2frec->var_list = _g2NewListNode(g2inqrec);
                g2frec->n_vars = 1;
            } else {
                g2plist = g2frec->var_list;

                if (! _g2FirstCheck(g2frec, g2plist, g2inqrec)) {
                    /* keep an inorder list */
                    while ((g2plist->next != NULL)
                            && (g2plist->next->param_number < g2inqrec->param_number)) {
                        g2plist = g2plist->next;
                    }

                    if ((g2plist->next == NULL)
                            || (g2plist->next->param_number > g2inqrec->param_number)) {
                        /* no current instance of this param; insert */
                        g2plist_n = _g2NewListNode(g2inqrec);
                        _g2InsertNodeAfter(g2plist, g2plist_n);
                        g2frec->n_vars++;
                    } else if (g2GridCompare(g2plist->next, g2inqrec) <= 0) {
                        /* param number found, "->next" points to first occurrence of it */
                        while ((g2plist->next != NULL)
                                && (g2plist->next->param_number == g2inqrec->param_number)
                                && (g2GridCompare(g2plist->next, g2inqrec) < 0)) {
                            g2plist = g2plist->next;
                        }

                        if ((g2plist->next == NULL)
                                || (g2plist->next->param_number != g2inqrec->param_number)
                                || (g2GridCompare(g2plist->next, g2inqrec) > 0)) {
                            g2plist_n = _g2NewListNode(g2inqrec);
                            _g2InsertNodeAfter(g2plist, g2plist_n);
                            g2frec->n_vars++;
                        } else if (g2plist->next->time_range_indicator
                                <= g2inqrec->time_range_indicator) {
                            /* param_number, grid found */
                            while ((g2plist->next != NULL)
                                    && (g2plist->next->param_number == g2inqrec->param_number)
                                    && (g2GridCompare(g2plist->next, g2inqrec) == 0)
                                    && (g2plist->next->time_range_indicator
                                        < g2inqrec->time_range_indicator)) {
                                g2plist = g2plist->next;
                            }

                            if ((g2plist->next == NULL)
                                    || (g2plist->next->param_number != g2inqrec->param_number)
                                    || (g2plist->next->grid_number != g2inqrec->grid_number)
                                    || (g2GridCompare(g2plist->next, g2inqrec) != 0)
                                    || (g2plist->next->time_range_indicator
                                        > g2inqrec->time_range_indicator)) {
                                g2plist_n = _g2NewListNode(g2inqrec);
                                _g2InsertNodeAfter(g2plist, g2plist_n);
                                g2frec->n_vars++;
                            } else if (g2plist->next->time_period <= g2inqrec->time_period) {
                                while ((g2plist->next != NULL)
                                        && (g2plist->next->param_number == g2inqrec->param_number)
                                        && (g2GridCompare(g2plist->next, g2inqrec) == 0)
                                        && (g2plist->next->time_period < g2inqrec->time_period)) {
                                    g2plist = g2plist->next;
                                }

                                if ((g2plist->next == NULL)
                                        || (g2plist->next->param_number != g2inqrec->param_number)
                                        || (g2GridCompare(g2plist->next, g2inqrec) != 0)
                                        || (g2plist->next->time_range_indicator
                                            != g2inqrec->time_range_indicator)
                                        || (g2plist->next->time_period > g2inqrec->time_period)) {
                                    g2plist_n = _g2NewListNode(g2inqrec);
                                    _g2InsertNodeAfter(g2plist, g2plist_n);
                                    g2frec->n_vars++;
                                } else if (g2plist->next->level_indicator
                                        <= g2inqrec->level_indicator) {
                                    while ((g2plist->next != NULL)
                                            && (g2plist->next->param_number
                                                == g2inqrec->param_number)
                                            && (g2GridCompare(g2plist->next, g2inqrec) == 0)
                                            && (g2plist->next->time_range_indicator
                                                == g2inqrec->time_range_indicator)
                                            && (g2plist->next->time_period
                                                == g2inqrec->time_period)
                                            && (g2plist->next->level_indicator
                                                < g2inqrec->level_indicator)) {
                                        g2plist = g2plist->next;
                                    }

                                    if ((g2plist->next == NULL)
                                            || (g2plist->next->param_number
                                                != g2inqrec->param_number)
                                            || (g2GridCompare(g2plist->next, g2inqrec) != 0)
                                            || (g2plist->next->time_range_indicator
                                                != g2inqrec->time_range_indicator)
                                            || (g2plist->next->time_period
                                                != g2inqrec->time_period)
                                            || (g2plist->next->level_indicator
                                                > g2inqrec->level_indicator)) {
                                        g2plist = _g2NewListNode(g2inqrec);
                                        _g2InsertNodeAfter(g2plist, g2plist_n);
                                        g2frec->n_vars++;
                                    } else {
                                        /*
                                         * At this point, fall through param_number, grid_number,
                                         * time_range_indicator, time_period, and level_indicator
                                         * are all equal.  Add the record.
                                         */
                                        _g2AddRecordToNode(g2plist->next, g2inqrec);
                                    }
                                } else {
                                    g2plist_n = _g2NewListNode(g2inqrec);
                                    _g2InsertNodeAfter(g2plist, g2plist_n);
                                    g2frec->n_vars++;
                                }
                            } else {
                                g2plist_n = _g2NewListNode(g2inqrec);
                                _g2InsertNodeAfter(g2plist, g2plist_n);
                                g2frec->n_vars++;
                            }
                        } else {
                            g2plist_n = _g2NewListNode(g2inqrec);
                            _g2InsertNodeAfter(g2plist, g2plist_n);
                            g2frec->n_vars++;
                        }
                    } else {
                        g2plist_n = _g2NewListNode(g2inqrec);
                        _g2InsertNodeAfter(g2plist, g2plist_n);
                        g2frec->n_vars++;
                    }
                }
            }
        }
    }

    if (g2frec != NULL) {
        g2frec->grib_grid_cache = NULL;

        /* sort by time, then level, for each variable in the list */
        g2plist = g2frec->var_list;
        k = 0;

        while (g2plist != NULL) {
            g2inqrec_list = g2plist->thelist;
            g2sort = (Grib2RecordInqRecList **) NclMalloc(
                    (unsigned int) sizeof(Grib2RecordInqRecList *) * g2plist->n_entries);
            i = 0;
            /*
             * Scan thru records and compute time offset from top of GRIB record.
             * All offsets based on 'time_units_indicator' of the top of GRIB parameter record.
             * First: determine an offset in time units based on 'time_units_indicator' and
             * 'time_range_indicator.'  Then: determine offset in same units from top of
             * parameter list's time reference.
             */
            while (g2inqrec_list != NULL) {
                g2sort[i] = g2inqrec_list;
#ifdef NOTNOW
                g2inqrec_list->rec_inq->time_offset
                        = _g2GetTimeOffset(g2inqrec_list->rec_inq->time_offset,
                            (unsigned char *) &(g2inqrec_list->rec_inq->per1));
#endif
                g2inqrec_list = g2inqrec_list->next;
		i++;
            }

            qsort((void *) g2sort, i, sizeof(Grib2RecordInqRecList *), g2record_comp);

            g2plist->thelist = g2sort[0];
            for (i = 0; i < g2plist->n_entries - 1; i++) {
                g2sort[i]->next = g2sort[i + 1];
            }
            g2sort[g2plist->n_entries - 1]->next = NULL;

            /*
             * Determine dimensionality for each variable.  This is determined by:
             *     [yy:mm:dd:hh:mm] x [forecast offset] x [levels] x [grid x] x [grid y]
             * 
             * Below: k == variable number
             *        g2plist == Grib2 ParamList
             *        g2sort == all elements in order, connected.
             *
             * Missing entries inserted when it's determined that levels or forecast
             * times are missing.
             */

            /*
             * Determine grid/coord info as well as dimensionality for each record.
             * Fill in missing values.
             */
            if ((g2plist->has_gds) && (g2plist->gds_type == 50)) {
                /* grid num = 50 == spherical harmonic coefficients */
                g2plist->var_info.doff = 2;
                err = _g2DetermineDimensionAndGridInfo(g2frec, g2plist);
            } else {
                g2plist->var_info.doff = 1;
                err = _g2DetermineDimensionAndGridInfo(g2frec, g2plist);
            }

            if ((err < NhlNOERROR) && (g2plist_n == NULL)) {
                g2plist = g2plist->next;
                g2plist_n = g2frec->var_list;
                g2frec->var_list = g2plist;
                g2frec->n_vars--;
                _Grib2FreeParamRec(g2plist_n);
                g2plist_n = NULL;
            } else {
                if (err < NhlNOERROR) {
                    g2plist_tmp = g2plist;
                    g2plist_n->next = g2plist->next;
                    g2plist = g2plist->next;
                    g2frec->n_vars--;
                    _Grib2FreeParamRec(g2plist_tmp);
                } else {
                    g2plist_n = g2plist;
                    g2plist = g2plist->next;
                    ++k;
                }
            }

            NclFree(g2sort);
            g2sort = NULL;
        }

        /*
         * Scan variables and determine all dimensions; combine dimensions that are
         * equal.  The last two dimensions will always be the grid dimensions.
         *
         * Variables may contain between two and five dimensions (inclusive).  The
         * first three dimensions are:  [initial_time] x [forecast offset] x [levels]
         * Any (each) dimension could be == 1, in which case it's not a true dimension
         * but rather an attribute.
         */
        _g2SetFileDimsAndCoordVars(g2frec);
        _g2SetAttributeLists(g2frec);
/*        _MakeVarnamesUnique(g2frec);   NOT NEEDED ?? */

        fclose(fd);
        NclFree(vbuf);
        return g2frec;
    }
}


static void Grib2FreeFileRec
# if    NhlNeedProto
(void* therec)
# else
(therec)
    void*   therec;
# endif
{
	Grib2FileRecord *thefile = (Grib2FileRecord *) therec;
	Grib2ParamList *vstep,*vstep1;
	Grib2RecordInqRecList *rstep;
	Grib2DimInqRecList *dim,*dim1;
	Grib2InternalVarList *ivars,*itmp;
	Grib2AttInqRecList *theatts,*tmp;
	int i;
	NclGrib2CacheList *thelist,*thelist0;
	NclGrib2CacheRec *ctmp,*ctmp0;

	vstep = thefile->var_list;
	while(vstep != NULL){
		vstep1 = vstep->next;
		_Grib2FreeParamRec(vstep);
		vstep  = vstep1;
	}
	thelist = thefile->grib_grid_cache;
        while(thelist != NULL) {
		ctmp = thelist->thelist;
		while(ctmp!=NULL) {	
			ctmp0 = ctmp;
			ctmp = ctmp->next;
			NclFree(ctmp0);
		}
		thelist0 = thelist;
		thelist = thelist->next;
		NclFree(thelist0);
	}

	ivars = thefile->internal_var_list;
	while(ivars != NULL) {
		_NclDestroyObj((NclObj)ivars->int_var->value);
		theatts = ivars->int_var->theatts;
		while(theatts != NULL) {
			_NclDestroyObj((NclObj)theatts->att_inq->thevalue);
			NclFree(theatts->att_inq);
			tmp = theatts;
			theatts = theatts->next;
			NclFree(tmp);
		}	
		NclFree(ivars->int_var);
		itmp = ivars;	
		ivars = ivars->next;
		NclFree(itmp);
	}
	dim = thefile->it_dims;
	if(dim != NULL) {
		while(dim != NULL) {
			dim1 = dim->next;
			if(dim->dim_inq != NULL) {
				NclFree(dim->dim_inq);
			}
			NclFree(dim);
			dim = dim1;
		}
	}
	dim = thefile->ft_dims;
	if(dim != NULL) {
		while(dim != NULL) {
			dim1 = dim->next;
			if(dim->dim_inq != NULL) {
				NclFree(dim->dim_inq);
			}
			NclFree(dim);
			dim = dim1;
		}
	}
	dim = thefile->lv_dims;
	if(dim != NULL) {
		while(dim != NULL) {
			dim1 = dim->next;
			if(dim->dim_inq != NULL) {
				NclFree(dim->dim_inq);
			}
			NclFree(dim);
			dim = dim1;
		}
	}
	dim = thefile->grid_dims;
	if(dim != NULL) {
		while(dim != NULL) {
			dim1 = dim->next;
			if(dim->dim_inq != NULL) {
				if(dim->dim_inq->gds != NULL) {
					NclFree(dim->dim_inq->gds);
				}
				NclFree(dim->dim_inq);
			}
			NclFree(dim);
			dim = dim1;
		}
	}
	NclFree(therec);
}


static NclQuark *Grib2GetVarNames
# if    NhlNeedProto
(void* therec, int *num_vars)
# else
(therec, num_vars)
void*   therec;
int *num_vars;
# endif
{
	Grib2FileRecord *thefile = (Grib2FileRecord*)therec;
	Grib2ParamList *step;
	Grib2InternalVarList *vstep;
	int i;
	NclQuark *arout;

	*num_vars = thefile->n_vars + thefile->n_internal_vars;
	arout = (NclQuark*)NclMalloc((unsigned)sizeof(NclQuark)* *num_vars);


	step = thefile->var_list;	
	for(i = 0; i < thefile->n_vars; i++) {
		arout[i] = step->var_info.var_name_quark;
		step = step->next;
	}

	vstep = thefile->internal_var_list;
	for(; i < thefile->n_vars + thefile->n_internal_vars; i++) {
		arout[i] = vstep->int_var->var_info.var_name_quark;
		vstep = vstep->next;
	}
	return arout;
}


static NclFVarRec *Grib2GetVarInfo
# if    NhlNeedProto
(void *therec, NclQuark var_name)
# else
(therec, var_name)
    void*   therec;
    NclQuark    var_name;
# endif
{
    Grib2FileRecord *thefile = (Grib2FileRecord *) therec;
    Grib2ParamList *step;
    NclFVarRec *tmp;
    Grib2InternalVarList *vstep;
    int i;

    vstep = thefile->internal_var_list;
    while(vstep != NULL) {
        if (vstep->int_var->var_info.var_name_quark == var_name) {
            tmp = (NclFVarRec*)NclMalloc(sizeof(NclFVarRec));
            tmp->var_name_quark  = vstep->int_var->var_info.var_name_quark;
            tmp->data_type  = vstep->int_var->var_info.data_type;
            tmp->num_dimensions  = vstep->int_var->var_info.num_dimensions;
            for (i = 0; i < tmp->num_dimensions; i++) {
                tmp->file_dim_num[i]  = vstep->int_var->var_info.file_dim_num[i];
            }
		
            return tmp;
        } else {
		    vstep = vstep->next;
        }
    }	

    step = thefile->var_list;	
    while(step != NULL) {
        if (step->var_info.var_name_quark == var_name) {
            tmp = (NclFVarRec*)NclMalloc(sizeof(NclFVarRec));
            tmp->var_name_quark  = step->var_info.var_name_quark;
            tmp->data_type  = step->var_info.data_type;
            tmp->num_dimensions  = step->var_info.num_dimensions;
            for (i = 0; i < tmp->num_dimensions; i++) {
                tmp->file_dim_num[i]  = step->var_info.file_dim_num[i];
            }

            return tmp;
        } else {
            step = step->next;
        }
    }

    return NULL;
}


static NclQuark *Grib2GetDimNames
# if    NhlNeedProto
(void* therec, int* num_dims)
# else
(therec, num_dims)
    void*   therec;
    int*    num_dims;
# endif
{
    Grib2FileRecord *thefile = (Grib2FileRecord *) therec;
    Grib2DimInqRecList *dstep;
    NclQuark *dims;
    int i,
        j;

    dims = (NclQuark *) NclMalloc((unsigned int) sizeof(NclQuark) * thefile->total_dims);
    i = 0;
    *num_dims = thefile->total_dims;
    dstep = thefile->scalar_dims;
    for(j=0; j < thefile->n_scalar_dims; j++) {
	    dims[dstep->dim_inq->dim_number] = dstep->dim_inq->dim_name;	
    	dstep = dstep->next;
    }

    dstep = thefile->ensemble_dims;
    for(j=0; j < thefile->n_ensemble_dims; j++) {
	    dims[dstep->dim_inq->dim_number] = dstep->dim_inq->dim_name;	
    	dstep = dstep->next;
    }

    dstep = thefile->it_dims;
    for(j=0; j < thefile->n_it_dims; j++) {
	    dims[dstep->dim_inq->dim_number] = dstep->dim_inq->dim_name;	
    	dstep = dstep->next;
    }

    dstep = thefile->ft_dims;
    for(j=0; j < thefile->n_ft_dims; j++) {
    	dims[dstep->dim_inq->dim_number] = dstep->dim_inq->dim_name;	
	    dstep = dstep->next;
    }

    dstep = thefile->lv_dims;
    for(j=0; j < thefile->n_lv_dims; j++) {
	    dims[dstep->dim_inq->dim_number] = dstep->dim_inq->dim_name;	
    	dstep = dstep->next;
    }

    dstep = thefile->grid_dims;
    for(j=0; j < thefile->n_grid_dims; j++) {
	    dims[dstep->dim_inq->dim_number] = dstep->dim_inq->dim_name;	
    	dstep = dstep->next;
    }

    return dims;
}


static NclFDimRec *Grib2GetDimInfo
#if	NhlNeedProto
(void* therec, NclQuark dim_name_q)
#else
(therec,dim_name_q)
void* therec;
NclQuark dim_name_q;
#endif
{
Grib2FileRecord *thefile = (Grib2FileRecord*)therec;
Grib2DimInqRecList *dstep;
NclFDimRec *tmpd = NULL;
char *tmp;

tmp = NrmQuarkToString(dim_name_q);
/*
* first character is either i,f, g or l
*/
	dstep = thefile->scalar_dims;
	while(dstep != NULL) {
		if(dstep->dim_inq->dim_name == dim_name_q) {
			tmpd = (NclFDimRec*)NclMalloc(sizeof(NclFDimRec));
			tmpd->dim_name_quark = dim_name_q;
			tmpd->dim_size = dstep->dim_inq->size;
			tmpd->is_unlimited = 0;
			return tmpd;
		}
		dstep = dstep->next;
	}		
	dstep = thefile->ensemble_dims;
	while(dstep != NULL) {
		if(dstep->dim_inq->dim_name == dim_name_q) {
			tmpd = (NclFDimRec*)NclMalloc(sizeof(NclFDimRec));
			tmpd->dim_name_quark = dim_name_q;
			tmpd->dim_size = dstep->dim_inq->size;
			tmpd->is_unlimited = 0;
			return tmpd;
		}
		dstep = dstep->next;
	}		
	dstep = thefile->it_dims;
	while(dstep != NULL) {
		if(dstep->dim_inq->dim_name == dim_name_q) {
			tmpd = (NclFDimRec*)NclMalloc(sizeof(NclFDimRec));
			tmpd->dim_name_quark = dim_name_q;
			tmpd->dim_size = dstep->dim_inq->size;
			tmpd->is_unlimited = 0;
			return tmpd;
		}
		dstep = dstep->next;
	}		
	dstep = thefile->ft_dims;
	while(dstep != NULL) {
		if(dstep->dim_inq->dim_name == dim_name_q) {
			tmpd = (NclFDimRec*)NclMalloc(sizeof(NclFDimRec));
			tmpd->dim_name_quark = dim_name_q;
			tmpd->dim_size = dstep->dim_inq->size;
			tmpd->is_unlimited = 0;
			return tmpd;
		}
		dstep = dstep->next;
	}		
	dstep = thefile->grid_dims;
	while(dstep != NULL) {
		if(dstep->dim_inq->dim_name == dim_name_q) {
			tmpd = (NclFDimRec*)NclMalloc(sizeof(NclFDimRec));
			tmpd->dim_name_quark = dim_name_q;
			tmpd->dim_size = dstep->dim_inq->size;
			tmpd->is_unlimited = 0;
			return tmpd;
		}
		dstep = dstep->next;
	}		
	dstep = thefile->lv_dims;
	while(dstep != NULL) {
		if(dstep->dim_inq->dim_name == dim_name_q) {
			tmpd = (NclFDimRec*)NclMalloc(sizeof(NclFDimRec));
			tmpd->dim_name_quark = dim_name_q;
			tmpd->dim_size = dstep->dim_inq->size;
			tmpd->is_unlimited = 0;
			return tmpd;
		}
		dstep = dstep->next;
	}		
	return NULL;
}

static NclFAttRec *Grib2GetAttInfo
#if	NhlNeedProto
(void* therec, NclQuark att_name_q)
#else
(therec, att_name_q)
void* therec;
NclQuark att_name_q;
#endif
{
return NULL;
}

static NclQuark *Grib2GetAttNames
#if	NhlNeedProto
(void* therec,int *num_atts)
#else
(therec,num_atts)
void* therec;
int *num_atts;
#endif
{	
*num_atts = 0;
return NULL;
}

static NclQuark *Grib2GetVarAttNames
#if	NhlNeedProto
(void *therec , NclQuark thevar, int* num_atts)
#else
(therec , thevar, num_atts)
void *therec;
NclQuark thevar;
int* num_atts;
#endif
{
Grib2FileRecord *thefile = (Grib2FileRecord*)therec;
Grib2ParamList *step;
Grib2InternalVarList *vstep;
NclQuark *arout;
Grib2AttInqRecList *theatts;
int i;


vstep = thefile->internal_var_list;
while(vstep != NULL) {
	if(vstep->int_var->var_info.var_name_quark == thevar) {
		*num_atts = vstep->int_var->n_atts;
		arout = (NclQuark*)NclMalloc(sizeof(NclQuark)*vstep->int_var->n_atts);
		theatts = vstep->int_var->theatts;
		break;
	} else {
		vstep = vstep->next;
	}
}	

if(vstep == NULL ) {
	step = thefile->var_list;	
	while(step != NULL) {
		if(step->var_info.var_name_quark == thevar) {
			*num_atts = step->n_atts;
			arout = (NclQuark*)NclMalloc(sizeof(NclQuark)*step->n_atts);
			theatts = step->theatts;
			break;
		} else {
			step = step->next;
		}
	}
}
if((arout != NULL)&&(theatts!= NULL))  {
	for(i = 0; i < *num_atts; i++) {
		arout[i] = theatts->att_inq->name;
		theatts = theatts->next;
	}
	return arout;
} else {
	*num_atts = 0;	
	return NULL;
}
}


static NclFAttRec *Grib2GetVarAttInfo
#if	NhlNeedProto
(void *therec, NclQuark thevar, NclQuark theatt)
#else
(therec, thevar, theatt)
void *therec;
NclQuark thevar;
NclQuark theatt;
#endif
{
	Grib2FileRecord *thefile = (Grib2FileRecord*)therec;
	Grib2ParamList *step;
	Grib2InternalVarList *vstep;
	Grib2AttInqRecList *theatts;
	int i;
	NclFAttRec *tmp;


	vstep = thefile->internal_var_list;
	while(vstep != NULL) {
		if(vstep->int_var->var_info.var_name_quark == thevar) {
			theatts = vstep->int_var->theatts;
			break;
		} else {
			vstep = vstep->next;
		}
	}	
	if(vstep == NULL ) {
		step = thefile->var_list;	
		while(step != NULL) {
			if(step->var_info.var_name_quark == thevar) {
				theatts = step->theatts;
				break;
			} else {
				step = step->next;
			}
		}
	}
	if(theatts!= NULL)  {
		while(theatts != NULL) {
			if(theatts->att_inq->name == theatt) {
				tmp = (NclFAttRec*)NclMalloc(sizeof(NclFAttRec));
				tmp->att_name_quark = theatt;
				tmp->data_type = theatts->att_inq->thevalue->multidval.data_type;
				tmp->num_elements = theatts->att_inq->thevalue->multidval.totalelements;
				return tmp;
			}
			theatts = theatts->next;
		}
	} 
	return NULL;
}

static void _g2NclAdjustCacheTypeAndMissing
#if NhlNeedProto
(int int_or_float,NclMultiDValData the_dat,NclScalar *missingv)
#else
(int_or_float,the_dat,missingv)
(int int_or_float,NclMultiDValData the_dat,NclScalar *missingv)
#endif
{
	if(int_or_float) {
		the_dat->multidval.hlu_type_rep[0] = ((NclTypeintClass)nclTypeintClass)->type_class.hlu_type_rep[0];
		the_dat->multidval.hlu_type_rep[1] = ((NclTypeintClass)nclTypeintClass)->type_class.hlu_type_rep[1];
		the_dat->multidval.data_type = ((NclTypeintClass)nclTypeintClass)->type_class.data_type;
		the_dat->multidval.type = (NclTypeClass)nclTypeintClass;
		if(missingv != NULL) {
			the_dat->multidval.missing_value.has_missing = 1;
			the_dat->multidval.missing_value.value = *missingv;
		} else {
			the_dat->multidval.missing_value.has_missing = 0;
		}
	} else {
/*
* Type is float by defaul/
*/
		if(missingv != NULL) {
			the_dat->multidval.missing_value.has_missing = 1;
			the_dat->multidval.missing_value.value = *missingv;
		} else {
			the_dat->multidval.missing_value.has_missing = 0;
		}
	}
}

static NclMultiDValData  _g2NclGetCacheVal
#if	NhlNeedProto
(Grib2FileRecord *therec, Grib2ParamList *step, Grib2RecordInqRec *current_rec)
#else
(therec,step,current_rec)
Grib2FileRecord *therec;
Grib2ParamList *step;
Grib2RecordInqRec *current_rec;
#endif
{
/*
	NclGribCacheList *thelist;
	void *val;
	thelist = therec->grib_grid_cache;
				val = NclMalloc(sizeof(float)*thelist->dimsizes[0]*thelist->dimsizes[1]);

                                return(_NclCreateVal(NULL,
                                                                NULL,
                                                                Ncl_MultiDValData,
                                                                0,
                                                                val,
                                                                NULL,
                                                                thelist->n_dims,
                                                                thelist->dimsizes,
                                                                PERMANENT,
                                                                NULL,
                                                                nclTypefloatClass));
*/
	NclGrib2CacheList *thelist;
	NclGrib2CacheRec *tmp;
	int i;
	int tg;
	void *val;
	thelist = therec->grib_grid_cache;
	while(thelist != NULL) {
		if ((thelist->grid_number == step->grid_number) && (thelist->has_gds == step->has_gds)) {
			if(thelist->n_entries == NCL_GRIB_CACHE_SIZE) {
				tmp = thelist->tail;
				tmp->rec->the_dat = NULL;
				tmp->rec = current_rec;
				tmp->prev->next = NULL;
				thelist->tail = tmp->prev;
				tmp->prev = NULL;
				tmp->next = thelist->thelist;
				tmp->next->prev = tmp;
				thelist->thelist = tmp;
				return(tmp->thevalue);
			} 
			if(thelist->n_entries == 0) {
				thelist->thelist = NclMalloc(sizeof(NclGrib2CacheRec));
				thelist->thelist->prev = NULL;
				thelist->thelist->next = NULL;
				thelist->thelist->rec = current_rec;
				thelist->tail = thelist->thelist;
				tg = 1;
				for(i = 0; i< thelist->n_dims; i++) {
					tg*=thelist->dimsizes[i];
				}
				val = NclMalloc(sizeof(float)*tg);
				thelist->thelist->thevalue = _NclCreateVal(NULL,
								NULL,
								Ncl_MultiDValData,
								0,
								val,
								NULL,
								thelist->n_dims,
								thelist->dimsizes,
								PERMANENT,
								NULL,
								nclTypefloatClass);
				thelist->n_entries = 1;
				return(thelist->thelist->thevalue);
			} else {
				tmp = NclMalloc(sizeof(NclGrib2CacheRec));
				tmp->prev = NULL;
				tmp->next = thelist->thelist;
				tmp->next->prev = tmp;
				tmp->rec = current_rec;
				tg = 1;
				for(i = 0; i< thelist->n_dims; i++) {
					tg*=thelist->dimsizes[i];
				}
				val = NclMalloc(sizeof(float)*tg);
                                tmp->thevalue = _NclCreateVal(NULL,
                                                                NULL,
                                                                Ncl_MultiDValData,
                                                                0,
                                                                val,
                                                                NULL,
                                                                thelist->n_dims,
                                                                thelist->dimsizes,
                                                                PERMANENT,
                                                                NULL,
                                                                nclTypefloatClass);
				++thelist->n_entries;
				
				thelist->thelist = tmp;
				return(tmp->thevalue);
			}
		} else {
			thelist = thelist->next;
		}
	}
	return(NULL);
}



static void *Grib2ReadVar
#if	NhlNeedProto
(void* therec, NclQuark thevar, long* start, long* finish,long* stride,void* storage)
#else
(therec, thevar, start, finish,stride,storage)
void* therec;
NclQuark thevar;
long* start;
long* finish;
long* stride;
void* storage;
#endif
{
	Grib2FileRecord *rec = (Grib2FileRecord*)therec;
	Grib2ParamList *step;
	Grib2RecordInqRec *current_rec;
	void *out_data;
	long *grid_start;
	long *grid_finish;
	long *grid_stride;
	int n_other_dims;
	int current_index[3];
	int dim_sizes[3] = {-1,-1,-1};
	int i,tg;
	int offset;
	int done = 0,inc_done =0;
	int data_offset = 0;
	void *tmp;
	void *missing;
	NclScalar missingv;
	int int_or_float;
	FILE* fd;
	int grid_dim_sizes[3];
	int n_grid_dims;
	NclMultiDValData tmp_md;
	NclSelectionRecord  sel_ptr;
	Grib2InternalVarList *vstep;
	int current_interp_method;

	vstep = rec->internal_var_list;
	while(vstep != NULL ) {
		if(vstep->int_var->var_info.var_name_quark == thevar) {
			sel_ptr.n_entries = vstep->int_var->var_info.num_dimensions;
			out_data = storage;
			for(i = 0; i < vstep->int_var->var_info.num_dimensions; i++ ) {
				sel_ptr.selection[i].sel_type = Ncl_SUBSCR;
				sel_ptr.selection[i].dim_num = i;
				sel_ptr.selection[i].u.sub.start = start[i];
				sel_ptr.selection[i].u.sub.finish = finish[i];
				sel_ptr.selection[i].u.sub.stride = stride[i];
				sel_ptr.selection[i].u.sub.is_single = 0;
			}
			tmp_md = (NclMultiDValData)_NclReadSubSection((NclData)vstep->int_var->value,&sel_ptr,NULL);
			memcpy((void*)&((char*)out_data)[data_offset],tmp_md->multidval.val,tmp_md->multidval.totalsize);
			if(tmp_md->obj.status != PERMANENT) {
				_NclDestroyObj((NclObj)tmp_md);
			}
			return(out_data);
		}
		vstep = vstep->next;

	}

	if ((NrmQuark)(rec->options[GRIB_THINNED_GRID_INTERPOLATION_OPT].values) == NrmStringToQuark("cubic")) {
		current_interp_method = 1;
	}
	else {
		current_interp_method = 0;
	}



	step = rec->var_list;
	while(step != NULL) {
		if(step->var_info.var_name_quark == thevar) {
			fd = fopen(NrmQuarkToString(rec->file_path_q),"r");
			vbuf = (void*)NclMalloc(4*getpagesize());
			setvbuf(fd,vbuf,_IOFBF,4*getpagesize());

			out_data = storage;

			if(step->var_info.doff == 1) {
				grid_start = &(start[(step->var_info.num_dimensions - 2) ]);
				grid_finish = &(finish[(step->var_info.num_dimensions - 2) ]);
				grid_stride = &(stride[(step->var_info.num_dimensions - 2) ]);
				n_other_dims = step->var_info.num_dimensions - 2;
				
			
				for(i = 0; i < n_other_dims; i++) {
					current_index[i] = start[i];
					dim_sizes[i] = step->var_info.dim_sizes[i];
				}
				n_grid_dims = 2;
				grid_dim_sizes[0] = step->var_info.dim_sizes[step->var_info.num_dimensions - 2];
				grid_dim_sizes[1] = step->var_info.dim_sizes[step->var_info.num_dimensions - 1];
				sel_ptr.n_entries = 2;
				sel_ptr.selection[0].sel_type = Ncl_SUBSCR;
				sel_ptr.selection[0].dim_num = 0;
				sel_ptr.selection[0].u.sub.start = grid_start[0];
				sel_ptr.selection[0].u.sub.finish = grid_finish[0];
				sel_ptr.selection[0].u.sub.stride = grid_stride[0];
				sel_ptr.selection[0].u.sub.is_single = 0;
				sel_ptr.selection[1].sel_type = Ncl_SUBSCR;
				sel_ptr.selection[1].dim_num = 1;
				sel_ptr.selection[1].u.sub.start = grid_start[1];
				sel_ptr.selection[1].u.sub.finish = grid_finish[1];
				sel_ptr.selection[1].u.sub.stride = grid_stride[1];
				sel_ptr.selection[1].u.sub.is_single = 0;
			} else if(step->var_info.doff == 2) {
				grid_start = &(start[(step->var_info.num_dimensions - 3) ]);
				grid_finish = &(finish[(step->var_info.num_dimensions - 3) ]);
				grid_stride = &(stride[(step->var_info.num_dimensions - 3) ]);
				n_other_dims = step->var_info.num_dimensions - 3;
				
			
				for(i = 0; i < n_other_dims; i++) {
					current_index[i] = start[i];
					dim_sizes[i] = step->var_info.dim_sizes[i];
				}
				n_grid_dims = 3;
				grid_dim_sizes[0] = step->var_info.dim_sizes[step->var_info.num_dimensions - 3];
				grid_dim_sizes[1] = step->var_info.dim_sizes[step->var_info.num_dimensions - 2];
				grid_dim_sizes[2] = step->var_info.dim_sizes[step->var_info.num_dimensions - 1];
				sel_ptr.n_entries = 3;
				sel_ptr.selection[0].sel_type = Ncl_SUBSCR;
				sel_ptr.selection[0].dim_num = 0;
				sel_ptr.selection[0].u.sub.start = grid_start[0];
				sel_ptr.selection[0].u.sub.finish = grid_finish[0];
				sel_ptr.selection[0].u.sub.stride = grid_stride[0];
				sel_ptr.selection[0].u.sub.is_single = 0;
				sel_ptr.selection[1].sel_type = Ncl_SUBSCR;
				sel_ptr.selection[1].dim_num = 1;
				sel_ptr.selection[1].u.sub.start = grid_start[1];
				sel_ptr.selection[1].u.sub.finish = grid_finish[1];
				sel_ptr.selection[1].u.sub.stride = grid_stride[1];
				sel_ptr.selection[1].u.sub.is_single = 0;
				sel_ptr.selection[2].sel_type = Ncl_SUBSCR;
				sel_ptr.selection[2].dim_num = 2;
				sel_ptr.selection[2].u.sub.start = grid_start[2];
				sel_ptr.selection[2].u.sub.finish = grid_finish[2];
				sel_ptr.selection[2].u.sub.stride = grid_stride[2];
				sel_ptr.selection[2].u.sub.is_single = 0;
			}
			

			offset = 0;
			while(!done) {
				offset = 0;
				if(n_other_dims > 0 ) {
					for(i = 0; i < n_other_dims - 1; i++) {
						offset += dim_sizes[i+1] * current_index[i];
					}
					offset += current_index[n_other_dims-1];
				}
				current_rec = step->thelist[offset].rec_inq;
	/*
	* For now(4/27/98) missing records persist, Eventually I'll implement one missing record per grid type for
	* general use.
	*/
				if(current_rec == NULL) {
					if(step->var_info.data_type == NCL_int) {
						tg = 1;
						for(i = 0; i< n_grid_dims; i++) {
							tg *= grid_dim_sizes[i];	
						}
						tmp = NclMalloc(sizeof(int) * tg);
						for( i = 0; i < tg; i++){
							((int*)tmp)[i] = G2_DEFAULT_MISSING_INT;
							
						}
						missingv.intval = G2_DEFAULT_MISSING_INT;
						step->thelist[offset].rec_inq = _g2MakeMissingRec();
						current_rec = step->thelist[offset].rec_inq;
						current_rec->the_dat = _NclCreateVal(
									NULL,
									NULL,
									Ncl_MultiDValData,
									0,
									tmp,
									&missingv,
									n_grid_dims,
									grid_dim_sizes,
									PERMANENT,
									NULL,
									nclTypeintClass
								);
					} else {
						tg = 1;
                                                for(i = 0; i< n_grid_dims; i++) {
                                                        tg *= grid_dim_sizes[i];        
                                                }
						tmp = NclMalloc(sizeof(float) * tg);
						for( i = 0; i < tg; i++){
							((float*)tmp)[i] = G2_DEFAULT_MISSING_FLOAT;
						}
						missingv.floatval = G2_DEFAULT_MISSING_FLOAT;

						step->thelist[offset].rec_inq = _g2MakeMissingRec();
						current_rec = step->thelist[offset].rec_inq;
						current_rec->the_dat = _NclCreateVal(
									NULL,
									NULL,
									Ncl_MultiDValData,
									0,
									tmp,
									&missingv,
									n_grid_dims,
									grid_dim_sizes,
									PERMANENT,
									NULL,
									nclTypefloatClass
								);
					}
				}
				
				if((current_rec->the_dat == NULL) || 
				   (current_rec->interp_method != current_interp_method &&
				    current_rec->var_name_q > NrmNULLQUARK)) {
	/*
	* Retrieves LRU cache MultiDVal specific to this grid type
	*/
					if (current_rec->the_dat) {
						current_rec->interp_method = current_interp_method;
					}
					else {
						current_rec->the_dat = _g2NclGetCacheVal(therec,step,current_rec);
						if(current_rec->the_dat == NULL){
							NhlPError(NhlFATAL,NhlEUNKNOWN,
								  "NclGRIB: Unrecoverable caching error reading variable %s; can't continue",current_rec->var_name);
							fclose(fd);
							NclFree(vbuf);
							return(NULL);
						}
					}
	/*
	* grid and grid_gds will overwrite tmp
	*/
					tmp = current_rec->the_dat->multidval.val;

/***
                    if ((current_rec->has_gds) && (current_rec->grid_number == 255)) {
                        int_or_float = current_rec->int_or_float;


						if (grid_gds[current_rec->grid_gds_tbl_index].un_pack != NULL) {
							int_or_float = (*grid_gds[current_rec->grid_gds_tbl_index].un_pack)
								(fd,&tmp,&missing,current_rec,step);
						}
					} else if(current_rec->grid_tbl_index > -1 && (grid[current_rec->grid_tbl_index].un_pack != NULL)) {
						int_or_float = (*grid[current_rec->grid_tbl_index].un_pack)
							(fd,&tmp,&missing,current_rec,step);
					} else if((current_rec->has_gds)&&(current_rec->grid_gds_tbl_index > -1)) {
						if(grid_gds[current_rec->grid_gds_tbl_index].un_pack != NULL) {
							int_or_float = (*grid_gds[current_rec->grid_gds_tbl_index].un_pack)
								(fd,&tmp,&missing,current_rec,step);
						}
					}
***/

					if(tmp != NULL) {
						if(int_or_float) {
							if(missing != NULL) {
								missingv.intval = *(int*)missing;
							} else {
								missingv.intval = G2_DEFAULT_MISSING_INT;
							}
	/*
	* Needed to fix chicken/egg problem with respect to type and missing values
	*/
							_g2NclAdjustCacheTypeAndMissing(int_or_float,current_rec->the_dat,(missing == NULL) ? NULL : &missingv);

							NclFree(missing);
						} else {
							if(missing != NULL) {
								missingv.floatval = *(float*)missing;
							} else {
								missingv.floatval = G2_DEFAULT_MISSING_FLOAT;
							}
	/*
	* Needed to fix chicken/egg problem with respect to type and missing values
	*/
							_g2NclAdjustCacheTypeAndMissing(int_or_float,current_rec->the_dat,(missing == NULL) ? NULL : &missingv);

							NclFree(missing);
						}
					} else {
	/*
	* Need to figure out what to do here
	*/
					}
				} 
				if(current_rec->the_dat != NULL) {
					tmp_md = (NclMultiDValData)_NclReadSubSection((NclData)current_rec->the_dat,&sel_ptr,NULL);
					memcpy((void*)&((char*)out_data)[data_offset],tmp_md->multidval.val,tmp_md->multidval.totalsize);
					data_offset += tmp_md->multidval.totalsize;
					if(tmp_md->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)tmp_md);
					}
				} else {
					NhlPError(NhlFATAL,NhlEUNKNOWN,
						  "NclGRIB: Unrecoverable error reading variable %s; can't continue",
						  current_rec->var_name);
					fclose(fd);
					NclFree(vbuf);
					return(NULL);
				}

				if(n_other_dims > 0 ) {	
					current_index[n_other_dims-1] += stride[n_other_dims-1];
					for(i = n_other_dims-1; i > 0 ; i--) {
						if(current_index[i] > finish[i]) {
							current_index[i] = start[i];
							current_index[i-1] += stride[i-1];
						} else {
							inc_done = 1;
						}
						if(inc_done) {
							inc_done = 0;
							break;
						}
					}
					if(current_index[0] > finish[0]) {
						done = 1;
					}
				} else {
					done = 1;
				}
			}
			fclose(fd);
			NclFree(vbuf);
			return(out_data);
		} 
		step = step->next;
	}
	NhlPError(NhlFATAL,NhlEUNKNOWN,"NclGRIB: Variable (%s) is not an element of file (%s)",NrmQuarkToString(thevar),NrmQuarkToString(rec->file_path_q));

	return(NULL);
}



static NclFVarRec *Grib2GetCoordInfo
# if    NhlNeedProto
(void* therec, NclQuark thevar)
# else
(therec, thevar)
    void*   therec;
    NclQuark    thevar;
# endif
{
    return Grib2GetVarInfo(therec, thevar);
}


static void *Grib2ReadCoord
# if    NhlNeedProto
(void* therec, NclQuark thevar, long* start, long* finish, long* stride, void* storage)
# else
(therec, thevar, start, finish, stride, storage)
    void*   therec;
    NclQuark    thevar;
    long*   start;
    long*   finish;
    long*   stride;
    void*   storage;
# endif
{
    return Grib2ReadVar(therec, thevar, start, finish, stride, storage);
}

static void Grib2ReadAtt
#if	NhlNeedProto
(void *therec,NclQuark theatt,void* storage)
#else
(therec,theatt,storage)
void * therec;
NclQuark theatt;
void* storage;
#endif
{
    return;
/*    return(NULL);*/
}


static void *Grib2ReadVarAtt
#if	NhlNeedProto
(void * therec, NclQuark thevar, NclQuark theatt, void * storage)
#else
(therec, thevar, theatt, storage)
void * therec;
NclQuark thevar;
NclQuark theatt;
void* storage;
#endif
{
	Grib2FileRecord *thefile = (Grib2FileRecord*)therec;
	Grib2ParamList *step;
	Grib2InternalVarList *vstep;
	Grib2AttInqRecList *theatts;
	int i;
	void *out_dat;

	vstep = thefile->internal_var_list;
	while(vstep != NULL) {
		if(vstep->int_var->var_info.var_name_quark == thevar) {
			theatts = vstep->int_var->theatts;
			break;
		} else {
			vstep = vstep->next;
		}
	}	

	if(vstep == NULL ) {
		step = thefile->var_list;	
		while(step != NULL) {
			if(step->var_info.var_name_quark == thevar) {
				theatts = step->theatts;	
				break;
			} else {
				step = step->next;
			}
		}
	}
	if(theatts!= NULL)  {
		while(theatts != NULL) {
			if(theatts->att_inq->name == theatt) {
				if(storage != NULL) {
					memcpy(storage,theatts->att_inq->thevalue->multidval.val,theatts->att_inq->thevalue->multidval.totalsize);
					return(storage);
				} else {
					out_dat = (void*)NclMalloc(theatts->att_inq->thevalue->multidval.totalsize);
					memcpy(out_dat,theatts->att_inq->thevalue->multidval.val,theatts->att_inq->thevalue->multidval.totalsize);
					return(out_dat);
				}
			}
			theatts = theatts->next;
		}
	}
	return(NULL);
}

static int Grib2MapToNcl
# if    NhlNeedProto
(void* the_type)
# else
(the_type)
	void *the_type;
# endif
{
	int int_or_float = *(int *) the_type;

	if(int_or_float) {
		return(NCL_int);
	} else {
		return(NCL_float);
	}
}

static void *Grib2MapFromNcl
# if    NhlNeedProto
(NclBasicDataTypes the_type)
# else
(the_type)
	NclBasicDataTypes the_type;
# endif
{
    int *tmp ;

    tmp = (int *) NclMalloc((unsigned int) sizeof(int));
	
    switch (the_type) {
        case NCL_int:
            *tmp = 1;
            break;

        case NCL_float:
            *tmp = 0;
            break;

        default:
            *tmp = -1;
            break;
    }

    return((void *) tmp);
}


static int Grib2SetOption
#if	NhlNeedProto
(void *therec,NclQuark option, NclBasicDataTypes data_type, int n_items, void * values)
#else
(therec,theatt,data_type,n_items,values)
	void *therec;
	NclQuark theatt;
	NclBasicDataTypes data_type;
	int n_items;
	void * values;
#endif
{
	Grib2FileRecord *rec = (Grib2FileRecord*)therec;
	nc_type *the_data_type;
	int i,ret;
	int cdfid;

	if (option ==  NrmStringToQuark("thinnedgridinterpolation")) {
		rec->options[GRIB_THINNED_GRID_INTERPOLATION_OPT].values = (void*) *(NrmQuark *)values;
	}

	if (option ==  NrmStringToQuark("initialtimecoordinatetype")) {
		rec->options[GRIB_INITIAL_TIME_COORDINATE_TYPE_OPT].values = (void*) *(NrmQuark *)values;
		g2SetInitialTimeCoordinates(therec);
	}
	
	if (option ==  NrmStringToQuark("defaultncepptable")) {
		rec->options[GRIB_DEFAULT_NCEP_PTABLE_OPT].values = (void*) *(NrmQuark *)values;
	}
	
	return NhlNOERROR;
}


extern NclFormatFunctionRec Grib2Rec = {
/* NclInitializeFileRecFunc initialize_file_rec */      Grib2InitializeFileRec,
/* NclCreateFileFunc	   create_file; */		Grib2CreateFile,
/* NclOpenFileFunc         open_file; */		Grib2OpenFile,
/* NclFreeFileRecFunc      free_file_rec; */		Grib2FreeFileRec,
/* NclGetVarNamesFunc      get_var_names; */		Grib2GetVarNames,
/* NclGetVarInfoFunc       get_var_info; */		Grib2GetVarInfo,
/* NclGetDimNamesFunc      get_dim_names; */		Grib2GetDimNames,
/* NclGetDimInfoFunc       get_dim_info; */		Grib2GetDimInfo,
/* NclGetAttNamesFunc      get_att_names; */		Grib2GetAttNames,
/* NclGetAttInfoFunc       get_att_info; */		Grib2GetAttInfo,
/* NclGetVarAttNamesFunc   get_var_att_names; */	Grib2GetVarAttNames,
/* NclGetVarAttInfoFunc    get_var_att_info; */		Grib2GetVarAttInfo,
/* NclGetCoordInfoFunc     get_coord_info; */		Grib2GetCoordInfo,
/* NclReadCoordFunc        read_coord; */		Grib2ReadCoord,
/* NclReadCoordFunc        read_coord; */		NULL,
/* NclReadVarFunc          read_var; */			Grib2ReadVar,
/* NclReadVarFunc          read_var; */			NULL,
/* NclReadAttFunc          read_att; */			Grib2ReadAtt,
/* NclReadVarAttFunc       read_var_att; */		Grib2ReadVarAtt,
/* NclWriteCoordFunc       write_coord; */		NULL,
/* NclWriteCoordFunc       write_coord; */		NULL,
/* NclWriteVarFunc         write_var; */		NULL,
/* NclWriteVarFunc         write_var; */		NULL,
/* NclWriteAttFunc         write_att; */		NULL,
/* NclWriteVarAttFunc      write_var_att; */		NULL,
/* NclAddDimFunc           add_dim; */			NULL,
/* NclAddDimFunc           rename_dim; */		NULL,
/* NclAddVarFunc           add_var; */			NULL,
/* NclAddVarFunc           add_coord_var; */		NULL,
/* NclAddAttFunc           add_att; */			NULL,
/* NclAddVarAttFunc        add_var_att; */		NULL,
/* NclMapFormatTypeToNcl   map_format_type_to_ncl; */	Grib2MapToNcl,
/* NclMapNclTypeToFormat   map_ncl_type_to_format; */	Grib2MapFromNcl,
/* NclDelAttFunc           del_att; */			NULL,
/* NclDelVarAttFunc        del_var_att; */		NULL,
/* NclSetOptionFunc        set_option;  */              Grib2SetOption
};

#ifdef NOTNOW
NclFormatFunctionRecPtr GribAddFileFormat 
#if	NhlNeedProto
(char *path)
#else 
(path)
    char *path;
#endif
{
    switch (grib_version) {
        case 0:
            /* fallthrough */

        case 1:
            return(&GribRec);
            break;

        case 2:
            return(&Grib2Rec);
            break;

        case -1:
            /* fallthrough */

        default:
            return NULL;   
    }
}
#endif
