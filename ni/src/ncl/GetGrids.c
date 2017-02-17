#include <stdlib.h>
#include <errno.h>
#include <stdio.h>
#ifdef NIO_LIB_ONLY
#include "niohlu.h"
#include "nioNresDB.h"
#include "nioCallbacks.h"
#else
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/Callbacks.h>
#endif
#include "defs.h"
#include "netcdf.h"
#include "NclDataDefs.h"
#include "NclFileInterfaces.h"
#include "NclMdInc.h"
#include "DataSupport.h"
#include "date.h"
#include "NclGRIB.h"
#include <math.h>
#include <unistd.h>

static void GenAtts(
#if     NhlNeedProto
GribParamList* thevarrec, 
GribAttInqRecList **lat_att_list, 
int * nlatatts, 
GribAttInqRecList **lon_att_list, 
int *lonatts,
int do_rot,
int grid_oriented,
GribAttInqRecList **rot_att_list, 
int *rotatts
#endif
);

void _NclInitMapTrans
#if NhlNeedProto
(
	char *proj,
	double plat,
	double plon,
	double prot
)
#else
(proj,plat,plon,prot)
	char *proj;
	double plat;
	double plon;
	double prot;

#endif
{
	double rl[2] = {0,0};
	double fl = 0.1,fr = 0.99 ,fb = 0.1 ,ft = 0.99;
	int len;
	NGstring str;

	NGCALLF(mapbd,MAPBD)();
	NGCALLF(mdppos,MDPPOS)(&fl,&fr,&fb,&ft);
	len = NGSTRLEN(proj);
	str = NGCstrToFstr(proj,len);
	NGCALLF(mdproj,MDPROJ)(str,&plat,&plon,&prot,len);
	len = NGSTRLEN("MA");
	str = NGCstrToFstr("MA",len);
	NGCALLF(mdpset,MDPSET)(str,rl,rl,rl,rl,len);
	NGCALLF(mdpint,MDPINT)();
}

/* 
 * pds-defined grids that are still unsupported:
 * 8,53,94,95,96,97,110,127,145,146,147,148,170,171,172,173,175,185,186,190,192,194,198,215 - 254
 */

int grid_index[] = { 
	1, 2, 3, 4, 5, 6, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 33, 34, 37, 38, 39, 40, 
	41, 42, 43, 44, 45, 50, 55, 56, 61, 62, 63, 64, 75, 76, 77, 85, 86, 87, 88, 90, 
	91, 92, 93, 98, 100, 101, 103, 104, 105, 106, 107, 126, 130,
	160,163, 171, 172, 185, /* 186, */
	201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215,
        216, 217, 218, 219, 220, 221, 222, 223, 224, 226, 227, 236, 237, 240, 241, 242, 245, 246, 247, 249, 252}; 

int grid_tbl_len = sizeof(grid_index)/sizeof(int);

int grid_gds_index[] = { -1, 0, 1, /* 2,*/ 3, 4, 5, 10, /* 13,*/ 50, /*60, 70, 80, 90, */ 201, 202, 203, 205 };

int grid_gds_tbl_len = sizeof(grid_gds_index)/sizeof(int);
#define EAR 6371.2213
#define ear 6367.47
#define PI 3.14159265358979323846
#define PI4  (PI/4)
#define RADDEG PI / 180.0
#define EAST 1
#define WEST -1


float pi = 3.14159265358979323846;
float pi2 = 1.57079632679489661923;
float pi4 = 0.78539816339744830962;
float degtorad = 1.745329e-02;
float radtodeg = 5.729578e+01;
double dpi = (double)PI;
double rtod = (double) 180.0/ PI;
double dtor = (double) PI /(double)180.0;

typedef int (*Index_Func) (
#if NhlNeedProto
int index,
int nx,
int ny,
int ijswap,
int is_uv
#endif
);	

static int compute_index(int index,int nx, int ny, int ijswap, int is_uv) {
	int row_number;
	int col_number;

	if(!ijswap) return(index);
	
	row_number = (int)(index / nx);

	col_number = index % nx;

	return((col_number * ny) + row_number);

}

/* for staggered arakawa grids fill to a grid twice as big. Intemediate
 * elements will be interpolated later.
 */


static int compute_index_staggered(int index,int nx, int ny, int ijswap,int is_uv) {
	int row_number =  (2 * index) / nx;
	int col_number;
	int i;

	if (row_number % 2 == 0) {
		i = index * 2 + is_uv;
	}
	else {
		i = index * 2 + 1 - is_uv;
	}
	if(!(ijswap)) return(i);

	col_number = i % nx;

	return((col_number * ny) + row_number);
}


static int is_gpoint
#if NhlNeedProto
( unsigned char *bms, int index)
#else
(unsigned char *bms, int index)
#endif
{
	int i = 0;
	int off = 0;

	if(bms == NULL) {
		return(1);
	} else {
		i = index/8;
		off = index % 8;
		switch(off) {
		case 0:
			return(bms[i+6] & 0200);
		case 1:
			return(bms[i+6] & 0100);
		case 2:
			return(bms[i+6] & 0040);
		case 3:
			return(bms[i+6] & 0020);
		case 4:
			return(bms[i+6] & 0010);
		case 5:
			return(bms[i+6] & 0004);
		case 6:
			return(bms[i+6] & 0002);
		case 7:
			return(bms[i+6] & 0001);
		}
	}
	return 1;
}

#if 0
static int printbinary(int val) {

	static int count = 0;	
	(val & 020000000000) ? fprintf(stdout,"1"),count++ : fprintf(stdout,"0");
	(val & 010000000000) ? fprintf(stdout,"1"),count++ : fprintf(stdout,"0");
	(val & 004000000000) ? fprintf(stdout,"1"),count++ : fprintf(stdout,"0");
	(val & 002000000000) ? fprintf(stdout,"1"),count++ : fprintf(stdout,"0");
	(val & 001000000000) ? fprintf(stdout,"1") ,count++: fprintf(stdout,"0");
	(val & 000400000000) ? fprintf(stdout,"1") ,count++: fprintf(stdout,"0");
	(val & 000200000000) ? fprintf(stdout,"1") ,count++: fprintf(stdout,"0");
	(val & 000100000000) ? fprintf(stdout,"1") ,count++: fprintf(stdout,"0");
	(val & 000040000000) ? fprintf(stdout,"1") ,count++: fprintf(stdout,"0");
	(val & 000020000000) ? fprintf(stdout,"1") ,count++: fprintf(stdout,"0");
	(val & 000010000000) ? fprintf(stdout,"1") ,count++: fprintf(stdout,"0");
	(val & 000004000000) ? fprintf(stdout,"1") ,count++: fprintf(stdout,"0");
	(val & 000002000000) ? fprintf(stdout,"1") ,count++: fprintf(stdout,"0");
	(val & 000001000000) ? fprintf(stdout,"1") ,count++: fprintf(stdout,"0");
	(val & 000000400000) ? fprintf(stdout,"1") ,count++: fprintf(stdout,"0");
	(val & 000000200000) ? fprintf(stdout,"1") ,count++: fprintf(stdout,"0");
	(val & 000000100000) ? fprintf(stdout,"1") ,count++: fprintf(stdout,"0");
	(val & 000000040000) ? fprintf(stdout,"1") ,count++: fprintf(stdout,"0");
	(val & 000000020000) ? fprintf(stdout,"1") ,count++: fprintf(stdout,"0");
	(val & 000000010000) ? fprintf(stdout,"1") ,count++: fprintf(stdout,"0");
	(val & 000000004000) ? fprintf(stdout,"1") ,count++: fprintf(stdout,"0");
	(val & 000000002000) ? fprintf(stdout,"1") ,count++: fprintf(stdout,"0");
	(val & 000000001000) ? fprintf(stdout,"1") ,count++: fprintf(stdout,"0");
	(val & 000000000400) ? fprintf(stdout,"1") ,count++: fprintf(stdout,"0");
	(val & 000000000200) ? fprintf(stdout,"1") ,count++: fprintf(stdout,"0");
	(val & 000000000100) ? fprintf(stdout,"1") ,count++: fprintf(stdout,"0");
	(val & 000000000040) ? fprintf(stdout,"1") ,count++: fprintf(stdout,"0");
	(val & 000000000020) ? fprintf(stdout,"1") ,count++: fprintf(stdout,"0");
	(val & 000000000010) ? fprintf(stdout,"1") ,count++: fprintf(stdout,"0");
	(val & 000000000004) ? fprintf(stdout,"1") ,count++: fprintf(stdout,"0");
	(val & 000000000002) ? fprintf(stdout,"1") ,count++: fprintf(stdout,"0");
	(val & 000000000001) ? fprintf(stdout,"1") ,count++: fprintf(stdout,"0");

	fprintf(stdout,"\n");
	return(count);
}

#endif

/* 
 * lo1, lo2 in millidegrees; di output in millidegrees 
 */
void GetThinnedLonParams
#if NhlNeedProto
(unsigned char *gds,
 int nlat,
 int lo1,
 int lo2,
 int idir,
 int *nlon,
 double *di
)
#else
(gds,nlat,lo1,lo2,idir,nlon,di)
 unsigned char *gds;
 int nlat;
 int lo1;
 int lo2;
 int idir;
 int *nlon;
 double *di;
#endif
{
	int pl_ix;
	int nmax = 0;
	int max_ix = 0;
	int i,n;
	int diff;

	*nlon = 0;
	pl_ix = (gds[4] == 255) ? -1 : (int) gds[3] * 4 + (int) gds[4] - 1;

	if (pl_ix == -1) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  "NclGRIB: Invalid thinned longitude grid");
		return;
	}

	for (i = 0; i < nlat; i++) {
		n = CnvtToDecimal(2,&(gds[pl_ix + i * 2]));
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
			lo2 += 360000;
		}
		diff = lo2 - lo1;
	}
	else {
		while (lo1 < lo2) {
			lo1 += 360000;
		}
		diff = lo1 - lo2;
	}
	*di =  diff / (double) (*nlon - 1);
	return;
}

/* 
 * la1, la2 in millidegrees; dj output in millidegrees 
 */

void GetThinnedLatParams
#if NhlNeedProto
(unsigned char *gds,
 int nlon,
 int la1,
 int la2,
 int jdir,
 int *nlat,
 double *dj
)
#else
(gds,nlon,la1,la2,jdir,nlat,dj)
 unsigned char *gds;
 int nlon;
 int la1;
 int la2;
 int jdir;
 int *nlat;
 double *dj;
#endif
{
	
	int pl_ix;
	int nmax = 0;
	int max_ix = 0;
	int i,n;
	int diff;

	*nlat = 0;
	pl_ix = (gds[4] == 255) ? -1 : (int) gds[3] * 4 + (int) gds[4] - 1;

	if (pl_ix == -1) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  "NclGRIB: Invalid thinned latitude grid");
		return;
	}

	for (i = 0; i < nlon; i++) {
		n = CnvtToDecimal(2,&(gds[pl_ix + i * 2]));
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
	diff = MAX(la2,la1) - MIN(la1,la2);
	*dj =  diff / (double)(*nlat - 1);
	return;
}

void GribPushAtt
#if NhlNeedProto
(GribAttInqRecList **att_list_ptr,char* name,void *val,ng_size_t dimsize,NclObjClass type) 
#else
(att_list_ptr,name,val,dimsize,type) 
GribAttInqRecList **att_list_ptr;
char* name;
void *val;
ng_size_t dimsize;
NclObjClass type;
#endif
{
	GribAttInqRecList* tmp_att_list_ptr;

	tmp_att_list_ptr = (*att_list_ptr);
	(*att_list_ptr) = (GribAttInqRecList*)NclMalloc((unsigned)sizeof(GribAttInqRecList));
	(*att_list_ptr)->next = tmp_att_list_ptr;
	(*att_list_ptr)->att_inq = (GribAttInqRec*)NclMalloc((unsigned)sizeof(GribAttInqRec));
	(*att_list_ptr)->att_inq->name = NrmStringToQuark(name);
	(*att_list_ptr)->att_inq->thevalue = (NclMultiDValData)_NclCreateVal( NULL, NULL, Ncl_MultiDValData, 0, (void*) val, NULL, 1 , &dimsize, PERMANENT, NULL, type);
}

void GetNCEPGrid
#if NhlNeedProto
(int *kgds, float** lat, int* n_dims_lat, ng_size_t** dimsizes_lat, float** lon, int* n_dims_lon, ng_size_t** dimsizes_lon, float **rot)
#else
(kgds, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot)
int *kgds;
float** lat;
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float **rot;
#endif
{
	int nx, ny;
	int iopt;
	ng_size_t npts;
	float fillval;
	int lrot;
	int nret;
	float *srot = NULL;
	int i;
	int do_rot;

	nx = kgds[1];
	ny = kgds[2];

        *lat = (float*)NclMalloc(sizeof(float) * nx * ny);
        *lon= (float*)NclMalloc(sizeof(float) * nx * ny);
        *dimsizes_lat = (ng_size_t*)NclMalloc(sizeof(ng_size_t) * 2);
        *dimsizes_lon = (ng_size_t*)NclMalloc(sizeof(ng_size_t) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ny;
        (*dimsizes_lat)[1] = nx;
        (*dimsizes_lon)[0] = ny;
        (*dimsizes_lon)[1] = nx;

	iopt = 0;
	lrot = 0;
	npts = nx * ny;
	do_rot = (010 & kgds[5])?1:0;
	if (do_rot) {
		srot = NhlMalloc(npts * sizeof(float));
		*rot = NhlMalloc(npts * sizeof(float));
		lrot = 1;
	}
	
        if(npts <= INT_MAX)
        {
          int inpts = (int) npts;
	  NGCALLF(gdswiz,GDSWIZ)(kgds,&iopt,&inpts,&fillval,*lon,*lat,*lon,*lat,&nret,&lrot,*rot,srot);
    }
        else
        {
          NhlPError(NhlFATAL,NhlEUNKNOWN,"gdswiz: npts = %ld is greater than INT_MAX", npts);
          return;
        }

	if (do_rot) {
		for (i = 0; i < npts; i++) {
			(*rot)[i] = asin(srot[i]);
			(*lon)[i] = (*lon)[i] > 180.0 ? (*lon)[i] - 360.0 : (*lon)[i];
		}
		NhlFree(srot);
	}
	else {
		for (i = 0; i < npts; i++) {
			(*lon)[i] = (*lon)[i] > 180.0 ? (*lon)[i] - 360.0 : (*lon)[i];
		}
	}
}

int ConsistentWithGDS
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	int *kgds
)
#else
(thevarrec, kgds)
GribParamList* thevarrec; 
int kgds;
#endif
{
	unsigned char *gds = thevarrec->ref_rec->gds;
	int nx,ny;

	nx = UnsignedCnvtToDecimal(2,&(gds[6]));
	ny = UnsignedCnvtToDecimal(2,&(gds[8]));

	if (! ((int) gds[5] == kgds[0] && nx == kgds[1] && ny == kgds[2])) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "GribOpenFile: Grid Description Section not consistent with NCEP documention of grid %d; using GDS values for variables with this grid",
			  thevarrec->grid_number);
		return 0;
	}

	return 1;
}

/*
* START Mercator
*/
void GenMercator
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, ng_size_t** dimsizes_lat, float** lon, int* n_dims_lon, ng_size_t** dimsizes_lon,float lat0, float lon0, float lat1, float lon1, float dx, float dy, float latin, int nx,int ny)
#else  
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, lat0, lon0, lat1, lon1, dx, dy, latin, nx, ny)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float lat0;
float lon0;
float lat1;
float lon1;
float dx;
float dy;
float latin;
int nx;
int ny;
#endif
{
	double dumx,dumy;
	double nx0,nx1,ny0,ny1;
	double tmplon,tmplat;
	double dlon1,dlon0,dlat1,dlat0;
	double udx,udy;
	int i,j;

	dlon0 = lon0;
	dlon1 = lon1;
	dlat0 = lat0;
	dlat1 = lat1;

	_NclInitMapTrans("ME",0.0,(dlon1 - dlon0)/2.0,0.0);
	
	*lat = (float*)NclMalloc(sizeof(float)*ny);
	*lon = (float*)NclMalloc(sizeof(float)*nx);

        *dimsizes_lat = (ng_size_t*)NclMalloc(sizeof(ng_size_t));
        *dimsizes_lon = (ng_size_t*)NclMalloc(sizeof(ng_size_t));
        *n_dims_lat = 1;
        *n_dims_lon = 1;
        (*dimsizes_lat)[0] = ny;
        (*dimsizes_lon)[0] = nx;

	tmplon = (dlon1 - dlon0) / 2.0;
	tmplat = (dlat1 - dlat0) / 2.0;
	NGCALLF(mdptrn,MDPTRN)(&tmplat,&tmplon,&dumx,&dumy);
	NGCALLF(mdptrn,MDPTRN)(&dlat0,&dlon0,&nx0,&ny0);
	NGCALLF(mdptrn,MDPTRN)(&dlat1,&dlon1,&nx1,&ny1);
        udx = fabs(nx1 - nx0) / (nx -1);
	udy = fabs(ny1 - ny0) / (ny-1);

	for(i = 0; i < ny; i++) {
		double uy = ny0 + i * udy;
		NGCALLF(mdptri,MDPTRI)(&dumx,&uy,&tmplat,&tmplon);
		(*lat)[i] = (float) tmplat;
	}
	for(j = 0; j < nx; j++) {
		double ux = nx0 + j * udx;
		NGCALLF(mdptri,MDPTRI)(&ux,&dumy,&tmplat,&tmplon);
		(*lon)[j] = (float) tmplon;
	}
	for(j = 0; j < nx; j++) {
		(*lon)[j] = ((*lon)[j] < 0)? ((*lon)[j] + 360) : (*lon)[j];
	}
}

void GetGrid_210
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[3];

	kgds[0] = 1;
	kgds[1] = 25;
	kgds[2] = 25;
	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

	GenMercator(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 9.0/*lat0*/, -77.0 /*lon0*/, 26.422 /*lat1*/, -58.625/* lon1*/, 80.0 /*dx*/, 80.0 /*dy*/, 20.0 /*latin*/, 25/*nx*/, 25 /*ny*/);
}
void GetGrid_208
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[3];

	kgds[0] = 1;
	kgds[1] = 29;
	kgds[2] = 27;
	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

	GenMercator(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 9.343/*lat0*/, -167.315 /*lon0*/, 28.092 /*lat1*/, -145.878/* lon1*/, 80.0 /*dx*/, 80. /*dy*/, 20.0 /*latin*/, 29/*nx*/, 27/*ny*/);
}
void GetGrid_204
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[3];

	kgds[0] = 1;
	kgds[1] = 93;
	kgds[2] = 68;
	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

	GenMercator(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, -25.0/*lat0*/, 110.0 /*lon0*/, 60.644 /*lat1*/, 250.871/* lon1*/, 160.0 /*dx*/, 160.0 /*dy*/, 20.0 /*latin*/, 93/*nx*/, 68/*ny*/);
}

void GetAtts_1
#if NhlNeedProto
(GribParamList* thevarrec, GribAttInqRecList **lat_att_list_ptr, int * nlatatts, GribAttInqRecList **lon_att_list_ptr, int *nlonatts, 
 int do_rot, int grid_oriented, GribAttInqRecList **rot_att_list_ptr, int *nrotatts)
#else
(thevarrec,lat_att_list_ptr, nlatatts, lon_att_list_ptr, nlonatts, do_rot,rot_att_list_ptr, nrotatts)
GribParamList* thevarrec;
GribAttInqRecList **lat_att_list_ptr;
int * nlatatts; 
GribAttInqRecList **lon_att_list_ptr;
int *nlonatts;
int do_rot;
int grid_oriented;
GribAttInqRecList **rot_att_list_ptr;
int *nrotatts;
#endif
{
	NclQuark *tmp_string = NULL;
	float *tmp_float = NULL;


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

	GenAtts(thevarrec,lat_att_list_ptr, nlatatts, lon_att_list_ptr, nlonatts, do_rot, grid_oriented,rot_att_list_ptr, nrotatts);
}
void GetGrid_1
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[3];

	kgds[0] = 1;
	kgds[1] = 73;
	kgds[2] = 23;
	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

	GenMercator(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, -48.09 /*lat0*/, 0.0/*lon0*/, 48.09 /*lat1*/, 360.0/* lon1*/, 513.669 /*dx*/, 513.669 /*dy*/, 22.5 /*latin*/, 73/*nx*/, 23/*ny*/);
}
/*
* END MERCATOR
*/
/*
* START Lambert Conformal Grids
*/
void GenLambert
#if	NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon,
	float **rot,
	double lat0, 
	double lat1, 
	double lon0, 
	double dx, 
	double dy,
	double start_lat, 
	double start_lon,
	int nx,
	int ny,
	int idir,
	int jdir)
#else  
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot,
 lat0, lat1, lon0, dx, dy, start_lat, start_lon, nx, ny)
	GribParamList* thevarrec;
	float** lat;
	int* n_dims_lat;
	ng_size_t** dimsizes_lat;
	float** lon;
	int* n_dims_lon;
	ng_size_t** dimsizes_lon;
	float ** rot;
	double lat0;
	double lat1;
	double lon0;
	double dx;
	double dy;
	double start_lat;
	double start_lon;
	int nx;
	int ny;
#endif
{
	double tlon;
	double nx0,nx1,ny0,ny1;
	double C,d_per_km,dlon;
	double udx,udy;
	int i,j;
	double an;

	_NclInitMapTrans("LC",lat0,lon0,lat1);

	/* this has already been done now */
	*lat = (float*)NclMalloc(sizeof(float)*nx*ny);
	*lon = (float*)NclMalloc(sizeof(float)*nx*ny);
	*rot = (float*)NclMalloc(sizeof(float)*nx*ny);
        *dimsizes_lat = (ng_size_t*)NclMalloc(sizeof(ng_size_t)*2);
        *dimsizes_lon = (ng_size_t*)NclMalloc(sizeof(ng_size_t)*2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ny;
        (*dimsizes_lat)[1] = nx;
        (*dimsizes_lon)[0] = ny;
        (*dimsizes_lon)[1] = nx;

/*
* Southern case
*/
	if((lat0 < 0)&&(lat1 < 0)) {
		
		if (lat0 == lat1) {
			an = sin(-1 * lat0 * dtor);
		}
		else {
			an = log(cos(lat0 * dtor)/cos(lat1 * dtor)) /
				log(tan(dtor * (-90 - lat0) / 2) / tan(dtor * (-90 - lat1) / 2));
		}
		C = 2 * pi * EAR * cos(dtor * lat0);
		d_per_km = 360.0/C;
		dlon = dx * d_per_km;
/*
* lat0 is always closest to pole
*/
		tlon = lon0 + dlon;
		NGCALLF(mdptrn,MDPTRN)(&lat0,&lon0,&nx0,&ny0);
		NGCALLF(mdptrn,MDPTRN)(&lat0,&tlon,&nx1,&ny1);
		udx = fabs(nx0 - nx1);
		udy = dy/dx * udx;
		NGCALLF(mdptrn,MDPTRN)(&start_lat,&start_lon,&nx0,&ny0);
		for(i = 0; i < ny; i++) {
			for(j = 0; j < nx; j++) {
				double tmpx =  nx0 + j * udx * idir;
				double tmpy =  ny0 + i * udy * jdir;
				double tmplat,tmplon;
				NGCALLF(mdptri,MDPTRI)
					(&tmpx,&tmpy,&tmplat,&tmplon);
				(*lat)[i * nx + j] = (float)tmplat;
				(*lon)[i * nx + j] = (float)tmplon;
				tlon = fmod(tmplon - lon0 + 180 + 3600, 360) - 180.0;
				(*rot)[i * nx + j] = (float) (an * tlon * dtor);
			}
		}
	} else {
/*
* Northern case
*/
		if (lat0 == lat1) {
			an = sin(lat0 * dtor);
		}
		else {
			an = log(cos(lat0 * dtor)/cos(lat1 * dtor)) /
				log(tan(dtor * (90 - lat0) / 2) / tan(dtor * (90 - lat1) / 2));
		}
		C = 2 * pi * EAR * cos(dtor * lat0);
		d_per_km = 360.0/C;
		dlon = dx * d_per_km;
/*
* lat0 is always closest to pole
*/
		tlon = lon0 + dlon;
		NGCALLF(mdptrn,MDPTRN)(&lat0,&lon0,&nx0,&ny0);
		NGCALLF(mdptrn,MDPTRN)(&lat0,&tlon,&nx1,&ny1);
		udx = fabs(nx0 - nx1);
		udy = dy/dx * udx;
		NGCALLF(mdptrn,MDPTRN)(&start_lat,&start_lon,&nx0,&ny0);
		for(i = 0; i < ny; i++) {
			for(j = 0; j < nx; j++) {
				double tmpx =  nx0 + j * udx * idir;
				double tmpy =  ny0 + i * udy * jdir;
				double tmplat,tmplon;
				NGCALLF(mdptri,MDPTRI)
					(&tmpx,&tmpy,&tmplat,&tmplon);
				(*lat)[i * nx + j] = (float)tmplat;
				(*lon)[i * nx + j] = (float)tmplon;
				tlon = fmod(tmplon - lon0 + 180 + 3600, 360) - 180.0;
				(*rot)[i * nx + j] = (float) (an * tlon * dtor);
			}
		}
	}

}


void GenLambertAtts
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	GribAttInqRecList **lat_att_list_ptr, 
	int * nlatatts, 
	GribAttInqRecList **lon_att_list_ptr, 
	int *nlonatts, 
	int do_rot, 
	GribAttInqRecList **rot_att_list_ptr, 
	int *nrotatts,
	int *kgds,
	double dx,
	double dy
	)
#else
(thevarrec,lat_att_list_ptr, nlatatts, lon_att_list_ptr, nlonatts, do_rot, rot_att_list_ptr, nrotatts,kgds,dx,dy)
GribParamList* thevarrec;
GribAttInqRecList **lat_att_list_ptr;
int * nlatatts; 
GribAttInqRecList **lon_att_list_ptr;
int *nlonatts;
int do_rot;
GribAttInqRecList **rot_att_list_ptr;
int *nrotatts;
int *kgds;
double dx;
double dy;
#endif
{
	NclQuark *tmp_string = NULL;
	float *tmp_float = NULL;
	int grid_oriented = 1;

	if (thevarrec->has_gds) {
		grid_oriented = (thevarrec->ref_rec->gds[16] & 010) ? 1 : 0;
	}

/* lat atts */
	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = kgds[3] / 1000.0;
	GribPushAtt(lat_att_list_ptr,"La1",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = kgds[4] / 1000.0;
	GribPushAtt(lat_att_list_ptr,"Lo1",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = kgds[6] / 1000.0;
	GribPushAtt(lat_att_list_ptr,"Lov",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = dx * 1000.0;
	GribPushAtt(lat_att_list_ptr,"Dx",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = dy * 1000.0;
	GribPushAtt(lat_att_list_ptr,"Dy",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = kgds[11] / 1000.0;
	GribPushAtt(lat_att_list_ptr,"Latin1",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = kgds[12] / 1000.0;
	GribPushAtt(lat_att_list_ptr,"Latin2",tmp_float,1,nclTypefloatClass); (*nlatatts)++;

	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark("LAMBERTCONFORMAL");
	GribPushAtt(lat_att_list_ptr,"mpProjection",tmp_string,1,nclTypestringClass); (*nlatatts)++;


	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float =  kgds[11] / 1000.0;
	GribPushAtt(lat_att_list_ptr,"mpLambertParallel1F",tmp_float,1,nclTypefloatClass); (*nlatatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = kgds[12] / 1000.0;
	GribPushAtt(lat_att_list_ptr,"mpLambertParallel2F",tmp_float,1,nclTypefloatClass); (*nlatatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float =  kgds[6] / 1000.0;
	GribPushAtt(lat_att_list_ptr,"mpLambertMeridianF",tmp_float,1,nclTypefloatClass); (*nlatatts)++;

/* lon atts */

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float =  kgds[3] / 1000.0;
	GribPushAtt(lon_att_list_ptr,"La1",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float =  kgds[4] / 1000.0;
	GribPushAtt(lon_att_list_ptr,"Lo1",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float =  kgds[6] / 1000.0;
	GribPushAtt(lon_att_list_ptr,"Lov",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = dx * 1000.0;
	GribPushAtt(lon_att_list_ptr,"Dx",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = dy * 1000.0;
	GribPushAtt(lon_att_list_ptr,"Dy",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float =  kgds[11] / 1000.0;
	GribPushAtt(lon_att_list_ptr,"Latin1",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float =  kgds[12] / 1000.0;
	GribPushAtt(lon_att_list_ptr,"Latin2",tmp_float,1,nclTypefloatClass); (*nlonatts)++;

	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark("LAMBERTCONFORMAL");
	GribPushAtt(lon_att_list_ptr,"mpProjection",tmp_string,1,nclTypestringClass); (*nlonatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float =  kgds[11] / 1000.0;
	GribPushAtt(lon_att_list_ptr,"mpLambertParallel1F",tmp_float,1,nclTypefloatClass); (*nlonatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = kgds[12] / 1000.0;
	GribPushAtt(lon_att_list_ptr,"mpLambertParallel2F",tmp_float,1,nclTypefloatClass); (*nlonatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float =  kgds[6] / 1000.0;
	GribPushAtt(lon_att_list_ptr,"mpLambertMeridianF",tmp_float,1,nclTypefloatClass); (*nlonatts)++;

	GenAtts(thevarrec,lat_att_list_ptr, nlatatts, lon_att_list_ptr, nlonatts,do_rot,grid_oriented,rot_att_list_ptr, nrotatts);
}

/* --- we're not calling these specific att-getting routines any more */
#if 0 
void GetAtts_206
#if NhlNeedProto
(GribParamList* thevarrec, GribAttInqRecList **lat_att_list_ptr, int * nlatatts, GribAttInqRecList **lon_att_list_ptr, int *nlonatts,
 int do_rot, GribAttInqRecList **rot_att_list_ptr, int *nrotatts)
#else
(thevarrec,lat_att_list_ptr, nlatatts, lon_att_list_ptr, nlonatts, do_rot, rot_att_list_ptr, nrotatts)
GribParamList* thevarrec;
GribAttInqRecList **lat_att_list_ptr;
int * nlatatts; 
GribAttInqRecList **lon_att_list_ptr;
int *nlonatts;
int do_rot;
GribAttInqRecList **rot_att_list_ptr;
int *nrotatts;
#endif
{
	GribAttInqRecList* tmp_att_list_ptr;
	NclQuark *tmp_string = NULL;
	float *tmp_float = NULL;
	ng_size_t tmp_dimsizes = 1;


	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark("LAMBERTCONFORMAL");
	GribPushAtt(lat_att_list_ptr,"mpProjection",tmp_string,1,nclTypestringClass); (*nlatatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 25.0;
	GribPushAtt(lat_att_list_ptr,"mpLambertParallel1F",tmp_float,1,nclTypefloatClass); (*nlatatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 25.0;
	GribPushAtt(lat_att_list_ptr,"mpLambertParallel2F",tmp_float,1,nclTypefloatClass); (*nlatatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = -95.0;
	GribPushAtt(lat_att_list_ptr,"mpLambertMeridianF",tmp_float,1,nclTypefloatClass); (*nlatatts)++;

	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark("LAMBERTCONFORMAL");
	GribPushAtt(lon_att_list_ptr,"mpProjection",tmp_string,1,nclTypestringClass); (*nlonatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 25.0;
	GribPushAtt(lon_att_list_ptr,"mpLambertParallel1F",tmp_float,1,nclTypefloatClass); (*nlonatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 25.0;
	GribPushAtt(lon_att_list_ptr,"mpLambertParallel2F",tmp_float,1,nclTypefloatClass); (*nlonatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = -95.0;
	GribPushAtt(lon_att_list_ptr,"mpLambertMeridianF",tmp_float,1,nclTypefloatClass); (*nlonatts)++;

	GenAtts(thevarrec,lat_att_list_ptr, nlatatts, lon_att_list_ptr, nlonatts, do_rot, rot_att_list_ptr, nrotatts);
}

void GetAtts_209
#if NhlNeedProto
(GribParamList* thevarrec, GribAttInqRecList **lat_att_list_ptr, int * nlatatts, GribAttInqRecList **lon_att_list_ptr, int *nlonatts,
 int do_rot, GribAttInqRecList **rot_att_list_ptr, int *nrotatts)
#else
(thevarrec,lat_att_list_ptr, nlatatts, lon_att_list_ptr, nlonatts, int do_rot, rot_att_list_ptr, nrotatts)
GribParamList* thevarrec;
GribAttInqRecList **lat_att_list_ptr;
int * nlatatts; 
GribAttInqRecList **lon_att_list_ptr;
int *nlonatts;
int do_rot;
GribAttInqRecList **rot_att_list_ptr;
int *nrotatts;
#endif
{
	GribAttInqRecList* tmp_att_list_ptr;
	NclQuark *tmp_string = NULL;
	float *tmp_float = NULL;
	ng_size_t tmp_dimsizes = 1;


	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark("LAMBERTCONFORMAL");
	GribPushAtt(lat_att_list_ptr,"mpProjection",tmp_string,1,nclTypestringClass); (*nlatatts)++;


	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 25.0;
	GribPushAtt(lat_att_list_ptr,"mpLambertParallel1F",tmp_float,1,nclTypefloatClass); (*nlatatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 25.0;
	GribPushAtt(lat_att_list_ptr,"mpLambertParallel2F",tmp_float,1,nclTypefloatClass); (*nlatatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = -95.0;
	GribPushAtt(lat_att_list_ptr,"mpLambertMeridianF",tmp_float,1,nclTypefloatClass); (*nlatatts)++;

	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark("LAMBERTCONFORMAL");
	GribPushAtt(lon_att_list_ptr,"mpProjection",tmp_string,1,nclTypestringClass); (*nlonatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 25.0;
	GribPushAtt(lon_att_list_ptr,"mpLambertParallel1F",tmp_float,1,nclTypefloatClass); (*nlonatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 25.0;
	GribPushAtt(lon_att_list_ptr,"mpLambertParallel2F",tmp_float,1,nclTypefloatClass); (*nlonatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = -95.0;
	GribPushAtt(lon_att_list_ptr,"mpLambertMeridianF",tmp_float,1,nclTypefloatClass); (*nlonatts)++;

	GenAtts(thevarrec,lat_att_list_ptr, nlatatts, lon_att_list_ptr, nlonatts, do_rot, rot_att_list_ptr, nrotatts);
}

void GetAtts_211
#if NhlNeedProto
(GribParamList* thevarrec, GribAttInqRecList **lat_att_list_ptr, int * nlatatts, GribAttInqRecList **lon_att_list_ptr, int *nlonatts, 
 int do_rot, GribAttInqRecList **rot_att_list_ptr, int *nrotatts)
#else
(thevarrec,lat_att_list_ptr, nlatatts, lon_att_list_ptr, nlonatts, do_rot, rot_att_list_ptr, nrotatts)
GribParamList* thevarrec;
GribAttInqRecList **lat_att_list_ptr;
int * nlatatts; 
GribAttInqRecList **lon_att_list_ptr;
int *nlonatts;
int do_rot;
GribAttInqRecList **rot_att_list_ptr;
int *nrotatts;
#endif
{
	GribAttInqRecList* tmp_att_list_ptr;
	NclQuark *tmp_string = NULL;
	float *tmp_float = NULL;
	ng_size_t tmp_dimsizes = 1;


	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark("LAMBERTCONFORMAL");
	GribPushAtt(lat_att_list_ptr,"mpProjection",tmp_string,1,nclTypestringClass); (*nlatatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 25.0;
	GribPushAtt(lat_att_list_ptr,"mpLambertParallel1F",tmp_float,1,nclTypefloatClass); (*nlatatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 25.0;
	GribPushAtt(lat_att_list_ptr,"mpLambertParallel2F",tmp_float,1,nclTypefloatClass); (*nlatatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = -95.0;
	GribPushAtt(lat_att_list_ptr,"mpLambertMeridianF",tmp_float,1,nclTypefloatClass); (*nlatatts)++;

	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark("LAMBERTCONFORMAL");
	GribPushAtt(lon_att_list_ptr,"mpProjection",tmp_string,1,nclTypestringClass); (*nlonatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 25.0;
	GribPushAtt(lon_att_list_ptr,"mpLambertParallel1F",tmp_float,1,nclTypefloatClass); (*nlonatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 25.0;
	GribPushAtt(lon_att_list_ptr,"mpLambertParallel2F",tmp_float,1,nclTypefloatClass); (*nlonatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = -95.0;
	GribPushAtt(lon_att_list_ptr,"mpLambertMeridianF",tmp_float,1,nclTypefloatClass); (*nlonatts)++;

	GenAtts(thevarrec,lat_att_list_ptr, nlatatts, lon_att_list_ptr, nlonatts, do_rot, rot_att_list_ptr, nrotatts);
}

void GetAtts_212
#if NhlNeedProto
(GribParamList* thevarrec, GribAttInqRecList **lat_att_list_ptr, int * nlatatts, GribAttInqRecList **lon_att_list_ptr, int *nlonatts, 
 int do_rot, GribAttInqRecList **rot_att_list_ptr, int *nrotatts)
#else
(thevarrec,lat_att_list_ptr, nlatatts, lon_att_list_ptr, nlonatts, do_rot, rot_att_list_ptr, nrotatts)
GribParamList* thevarrec;
GribAttInqRecList **lat_att_list_ptr;
int * nlatatts; 
GribAttInqRecList **lon_att_list_ptr;
int *nlonatts;
int do_rot;
GribAttInqRecList **rot_att_list_ptr;
int *nrotatts;
#endif
{
	GribAttInqRecList* tmp_att_list_ptr;
	NclQuark *tmp_string = NULL;
	float *tmp_float = NULL;
	ng_size_t tmp_dimsizes = 1;


	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark("LAMBERTCONFORMAL");
	GribPushAtt(lat_att_list_ptr,"mpProjection",tmp_string,1,nclTypestringClass); (*nlatatts)++;


	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 25.0;
	GribPushAtt(lat_att_list_ptr,"mpLambertParallel1F",tmp_float,1,nclTypefloatClass); (*nlatatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 25.0;
	GribPushAtt(lat_att_list_ptr,"mpLambertParallel2F",tmp_float,1,nclTypefloatClass); (*nlatatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = -95.0;
	GribPushAtt(lat_att_list_ptr,"mpLambertMeridianF",tmp_float,1,nclTypefloatClass); (*nlatatts)++;

	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark("LAMBERTCONFORMAL");
	GribPushAtt(lon_att_list_ptr,"mpProjection",tmp_string,1,nclTypestringClass); (*nlonatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 25.0;
	GribPushAtt(lon_att_list_ptr,"mpLambertParallel1F",tmp_float,1,nclTypefloatClass); (*nlonatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 25.0;
	GribPushAtt(lon_att_list_ptr,"mpLambertParallel2F",tmp_float,1,nclTypefloatClass); (*nlonatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = -95.0;
	GribPushAtt(lon_att_list_ptr,"mpLambertMeridianF",tmp_float,1,nclTypefloatClass); (*nlonatts)++;

	GenAtts(thevarrec,lat_att_list_ptr, nlatatts, lon_att_list_ptr, nlonatts, do_rot,rot_att_list_ptr, nrotatts);
}

#endif
void GetGrid_130
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[32];
	int nx;
	int ny;
	double latin1;
	double latin2;
	double lov;
	double dx;
	double dy;
	double la1;
	double lo1;
		
	kgds[0] = 3;
	kgds[1] = 451;
	kgds[2] = 337;
	kgds[3] = 16281;
	kgds[4] = 233862;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 265000;
	kgds[7] = 13545; /* 13.545087 */
	kgds[8] = 13545; /* 13.545087 */
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */
	kgds[11] = 25000;
	kgds[12] = 25000;

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

	nx = kgds[1];
	ny = kgds[2];
	latin1 = kgds[11] / 1000.0;
	latin2 = kgds[12] / 1000.0;
	lov = kgds[6] / 1000.0;
	dx = 13.545087;
	dy = 13.545087;
	la1 = kgds[3] / 1000.0;
	lo1 = kgds[4] / 1000.0;

	GenLambert(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot,
		   latin1, latin2, lov, dx,dy,la1,lo1,nx,ny,1,1);

	GenLambertAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 1, rot_att_list, nrotatts,kgds,dx,dy);
}

void GetGrid_163
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[32];
	double dx,dy;
		
	kgds[0] = 3;
	kgds[1] = 1008;
	kgds[2] = 722;
	kgds[3] = 20600;
	kgds[4] = 241700;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 265000;
	kgds[7] = 5000; 
	kgds[8] = 5000; 
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */
	kgds[11] = 38000;
	kgds[12] = 38000;

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}
	GetNCEPGrid(kgds,lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = kgds[7] / 1000.0;
	dy = kgds[8] / 1000.0;

	GenLambertAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 1, rot_att_list, nrotatts,kgds,dx,dy);
}

void GetGrid_185
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[32];
	double dx,dy;
		
	kgds[0] = 3;
	kgds[1] = 491;
	kgds[2] = 303;
	kgds[3] = 19943;
	kgds[4] = 234907;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 262000;
	kgds[7] = 12000; 
	kgds[8] = 12000; 
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */
	kgds[11] = 40000;
	kgds[12] = 40000;

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}
	GetNCEPGrid(kgds,lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = kgds[7] / 1000.0;
	dy = kgds[8] / 1000.0;

	GenLambertAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 1, rot_att_list, nrotatts,kgds,dx,dy);
}

void GetGrid_206
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[32];
	int nx;
	int ny;
	double latin1;
	double latin2;
	double lov;
	double dx;
	double dy;
	double la1;
	double lo1;
		
	kgds[0] = 3;
	kgds[1] = 51;
	kgds[2] = 41;
	kgds[3] = 22289;
	kgds[4] = 242009;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 265000;
	kgds[7] = 81271; /* 81.2705 */
	kgds[8] = 81271; /* 81.2705 */
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */
	kgds[11] = 25000;
	kgds[12] = 25000;

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

	nx = kgds[1];
	ny = kgds[2];
	latin1 = kgds[11] / 1000.0;
	latin2 = kgds[12] / 1000.0;
	lov = kgds[6] / 1000.0;
	dx = 81.2705;
	dy = 81.2705;
	la1 = kgds[3] / 1000.0;
	lo1 = kgds[4] / 1000.0;

	GenLambert(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot,
		   latin1, latin2, lov, dx,dy,la1,lo1,nx,ny,1,1);

	GenLambertAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 1, rot_att_list, nrotatts,kgds,dx,dy);

}


void GetGrid_209
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[32];
	double dx,dy;
		
	kgds[0] = 3;
	kgds[1] = 275;
	kgds[2] = 223;
	kgds[3] = -4850;
	kgds[4] = 208900;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 249000;
	kgds[7] = 44000; 
	kgds[8] = 44000; 
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */
	kgds[11] = 45000;
	kgds[12] = 45000;

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

	GetNCEPGrid(kgds,lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = kgds[7] / 1000.0;
	dy = kgds[8] / 1000.0;

	GenLambertAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 1, rot_att_list, nrotatts,kgds,dx,dy);
}


void GetGrid_211
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[32];
	int nx;
	int ny;
	double latin1;
	double latin2;
	double lov;
	double dx;
	double dy;
	double la1;
	double lo1;
		
	kgds[0] = 3;
	kgds[1] = 93;
	kgds[2] = 65;
	kgds[3] = 12190;
	kgds[4] = 226541;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 265000;
	kgds[7] = 81271; /* 81.2705 */
	kgds[8] = 81271; /* 81.2705 */
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */
	kgds[11] = 25000;
	kgds[12] = 25000;

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}


	nx = kgds[1];
	ny = kgds[2];
	latin1 = kgds[11] / 1000.0;
	latin2 = kgds[12] / 1000.0;
	lov = kgds[6] / 1000.0;
	dx = 81.2705;
	dy = 81.2705;
	la1 = kgds[3] / 1000.0;
	lo1 = kgds[4] / 1000.0;

	GenLambert(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot,
		   latin1, latin2, lov, dx,dy,la1,lo1,nx,ny,1,1);

	GenLambertAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 1, rot_att_list, nrotatts,kgds,dx,dy);
}

void GetGrid_212
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[32];
	int nx;
	int ny;
	double latin1;
	double latin2;
	double lov;
	double dx;
	double dy;
	double la1;
	double lo1;
		
	kgds[0] = 3;
	kgds[1] = 185;
	kgds[2] = 129;
	kgds[3] = 12190;
	kgds[4] = 226541;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 265000;
	kgds[7] = 40635; /* 40.63525 */
	kgds[8] = 40635; /* 40.63525 */
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */
	kgds[11] = 25000;
	kgds[12] = 25000;

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

	nx = kgds[1];
	ny = kgds[2];
	latin1 = kgds[11] / 1000.0;
	latin2 = kgds[12] / 1000.0;
	lov = kgds[6] / 1000.0;
	dx = 40.63525;
	dy = 40.63525;
	la1 = kgds[3] / 1000.0;
	lo1 = kgds[4] / 1000.0;

	GenLambert(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot,
		   latin1, latin2, lov, dx,dy,la1,lo1,nx,ny,1,1);

	GenLambertAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 1, rot_att_list, nrotatts,kgds,dx,dy);
}

void GetGrid_215
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[32];
	int nx;
	int ny;
	double latin1;
	double latin2;
	double lov;
	double dx;
	double dy;
	double la1;
	double lo1;
		
	kgds[0] = 3;
	kgds[1] = 369;
	kgds[2] = 257;
	kgds[3] = 12190;
	kgds[4] = 226514;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 265000;
	kgds[7] = 20318; /* 20.317625 */
	kgds[8] = 20318; /* 20.317625 */
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */
	kgds[11] = 25000;
	kgds[12] = 25000;

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

	nx = kgds[1];
	ny = kgds[2];
	latin1 = kgds[11] / 1000.0;
	latin2 = kgds[12] / 1000.0;
	lov = kgds[6] / 1000.0;
	dx = 20.317625;
	dy = 20.317625;
	la1 = kgds[3] / 1000.0;
	lo1 = kgds[4] / 1000.0;

	GenLambert(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot,
		   latin1, latin2, lov, dx,dy,la1,lo1,nx,ny,1,1);

	GenLambertAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 1, rot_att_list, nrotatts,kgds,dx,dy);
}

void GetGrid_218
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[32];
	int nx;
	int ny;
	double latin1;
	double latin2;
	double lov;
	double dx;
	double dy;
	double la1;
	double lo1;
		
	kgds[0] = 3;
	kgds[1] = 614;
	kgds[2] = 428;
	kgds[3] = 12190;
	kgds[4] = 226514;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 265000;
	kgds[7] = 12191; /* 12.19058 */
	kgds[8] = 12191; /* 12.19058 */
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */
	kgds[11] = 25000;
	kgds[12] = 25000;

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}
	nx = kgds[1];
	ny = kgds[2];
	latin1 = kgds[11] / 1000.0;
	latin2 = kgds[12] / 1000.0;
	lov = kgds[6] / 1000.0;
	dx = 12.19058;
	dy = 12.19058;
	la1 = kgds[3] / 1000.0;
	lo1 = kgds[4] / 1000.0;

	GenLambert(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot,
		   latin1, latin2, lov, dx,dy,la1,lo1,nx,ny,1,1);


	GenLambertAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 1, rot_att_list, nrotatts,kgds,dx,dy);
}

void GetGrid_221
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[32];
	int nx;
	int ny;
	double latin1;
	double latin2;
	double lov;
	double dx;
	double dy;
	double la1;
	double lo1;
		
	kgds[0] = 3;
	kgds[1] = 349;
	kgds[2] = 277;
	kgds[3] = 1000;
	kgds[4] = 214500;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 253000;
	kgds[7] = 32463; /* 32.46341 */
	kgds[8] = 32463; /* 32.46341 */
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */
	kgds[11] = 50000;
	kgds[12] = 50000;

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

	nx = kgds[1];
	ny = kgds[2];
	latin1 = kgds[11] / 1000.0;
	latin2 = kgds[12] / 1000.0;
	lov = kgds[6] / 1000.0;
	dx = 32.46341;
	dy = 32.46341;
	la1 = kgds[3] / 1000.0;
	lo1 = kgds[4] / 1000.0;

	GenLambert(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot,
		   latin1, latin2, lov, dx,dy,la1,lo1,nx,ny,1,1);

	GenLambertAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 1, rot_att_list, nrotatts,kgds,dx,dy);

}

void GetGrid_222
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[32];
	double dx,dy;
		
	kgds[0] = 3;
	kgds[1] = 138;
	kgds[2] = 112;
	kgds[3] = -4850;
	kgds[4] = 208900;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 249000;
	kgds[7] = 88000;
	kgds[8] = 88000;
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */
	kgds[11] = 45000;
	kgds[12] = 45000;

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

	GetNCEPGrid(kgds,lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = kgds[7] / 1000.0;
	dy = kgds[8] / 1000.0;

	GenLambertAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 1, rot_att_list, nrotatts,kgds,dx,dy);
}


void GetGrid_226
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[32];
	int nx;
	int ny;
	double latin1;
	double latin2;
	double lov;
	double dx;
	double dy;
	double la1;
	double lo1;
		
	kgds[0] = 3;
	kgds[1] = 737;
	kgds[2] = 517;
	kgds[3] = 12190;
	kgds[4] = 226514;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 265000;
	kgds[7] = 10159; /* 10.1588125 */
	kgds[8] = 10159; /* 10.1588125 */
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */
	kgds[11] = 25000;
	kgds[12] = 25000;

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

	nx = kgds[1];
	ny = kgds[2];
	latin1 = kgds[11] / 1000.0;
	latin2 = kgds[12] / 1000.0;
	lov = kgds[6] / 1000.0;
	dx = 10.1588125;
	dy = 10.1588125;
	la1 = kgds[3] / 1000.0;
	lo1 = kgds[4] / 1000.0;

	GenLambert(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot,
		   latin1, latin2, lov, dx,dy,la1,lo1,nx,ny,1,1);

	GenLambertAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 1, rot_att_list, nrotatts,kgds,dx,dy);
}

void GetGrid_227
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[32];
	int nx;
	int ny;
	double latin1;
	double latin2;
	double lov;
	double dx;
	double dy;
	double la1;
	double lo1;
		
	kgds[0] = 3;
	kgds[1] = 1473;
	kgds[2] = 1025;
	kgds[3] = 12190;
	kgds[4] = 226514;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 265000;
	kgds[7] = 5079; /* 5.07940625 */
	kgds[8] = 5079; /* 5.07940625 */
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */
	kgds[11] = 25000;
	kgds[12] = 25000;

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

	nx = kgds[1];
	ny = kgds[2];
	latin1 = kgds[11] / 1000.0;
	latin2 = kgds[12] / 1000.0;
	lov = kgds[6] / 1000.0;
	dx = 5.07940625;
	dy = 5.07940625;
	la1 = kgds[3] / 1000.0;
	lo1 = kgds[4] / 1000.0;

	GenLambert(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot,
		   latin1, latin2, lov, dx,dy,la1,lo1,nx,ny,1,1);

	GenLambertAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 1, rot_att_list, nrotatts,kgds,dx,dy);
}


void GetGrid_236
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[32];
	int nx;
	int ny;
	double latin1;
	double latin2;
	double lov;
	double dx;
	double dy;
	double la1;
	double lo1;
		
	kgds[0] = 3;
	kgds[1] = 151;
	kgds[2] = 113;
	kgds[3] = 16281;
	kgds[4] = 233862;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 265000;
	kgds[7] = 40635; /* 40.63525 */
	kgds[8] = 40635; /* 40.63525 */
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */
	kgds[11] = 25000;
	kgds[12] = 25000;

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

	nx = kgds[1];
	ny = kgds[2];
	latin1 = kgds[11] / 1000.0;
	latin2 = kgds[12] / 1000.0;
	lov = kgds[6] / 1000.0;
	dx = 40.63525;
	dy = 40.63525;
	la1 = kgds[3] / 1000.0;
	lo1 = kgds[4] / 1000.0;

	GenLambert(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot,
		   latin1, latin2, lov, dx,dy,la1,lo1,nx,ny,1,1);

	GenLambertAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 1, rot_att_list, nrotatts,kgds,dx,dy);
}

void GetGrid_237
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[32];
	int nx;
	int ny;
	double latin1;
	double latin2;
	double lov;
	double dx;
	double dy;
	double la1;
	double lo1;
		
	kgds[0] = 3;
	kgds[1] = 54;
	kgds[2] = 47;
	kgds[3] = 16201;
	kgds[4] = 285720;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 253000;
	kgds[7] = 32463; /* 32.46341 */
	kgds[8] = 32463; /* 32.46341 */
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */
	kgds[11] = 50000;
	kgds[12] = 50000;

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

	nx = kgds[1];
	ny = kgds[2];
	latin1 = kgds[11] / 1000.0;
	latin2 = kgds[12] / 1000.0;
	lov = kgds[6] / 1000.0;
	dx = 32.46341;
	dy = 32.46341;
	la1 = kgds[3] / 1000.0;
	lo1 = kgds[4] / 1000.0;

	GenLambert(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot,
		   latin1, latin2, lov, dx,dy,la1,lo1,nx,ny,1,1);

	GenLambertAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 1, rot_att_list, nrotatts,kgds,dx,dy);
}

void GetGrid_241
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[32];
	double dx,dy;
		
	kgds[0] = 3;
	kgds[1] = 549;
	kgds[2] = 445;
	kgds[3] = -4850;
	kgds[4] = 208900;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 249000;
	kgds[7] = 22000;
	kgds[8] = 22000;
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */
	kgds[11] = 45000;
	kgds[12] = 45000;

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

	GetNCEPGrid(kgds,lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = kgds[7] / 1000.0;
	dy = kgds[8] / 1000.0;

	GenLambertAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 1, rot_att_list, nrotatts,kgds,dx,dy);
}

void GetGrid_245
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[32];
	double dx,dy;
		
	kgds[0] = 3;
	kgds[1] = 336;
	kgds[2] = 372;
	kgds[3] = 22980;
	kgds[4] = 267160;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 280000;
	kgds[7] = 8000;
	kgds[8] = 8000;
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */
	kgds[11] = 35000;
	kgds[12] = 35000;

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

	GetNCEPGrid(kgds,lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = kgds[7] / 1000.0;
	dy = kgds[8] / 1000.0;

	GenLambertAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 1, rot_att_list, nrotatts,kgds,dx,dy);
}

void GetGrid_246
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[32];
	double dx,dy;
		
	kgds[0] = 3;
	kgds[1] = 336;
	kgds[2] = 371;
	kgds[3] = 25970;
	kgds[4] = 232027;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 245000;
	kgds[7] = 8000;
	kgds[8] = 8000;
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */
	kgds[11] = 40000;
	kgds[12] = 40000;

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

	GetNCEPGrid(kgds,lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = kgds[7] / 1000.0;
	dy = kgds[8] / 1000.0;

	GenLambertAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 1, rot_att_list, nrotatts,kgds,dx,dy);
}


void GetGrid_247
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[32];
	double dx,dy;
		
	kgds[0] = 3;
	kgds[1] = 336;
	kgds[2] = 372;
	kgds[3] = 22980;
	kgds[4] = 249160;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 262000;
	kgds[7] = 8000;
	kgds[8] = 8000;
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */
	kgds[11] = 35000;
	kgds[12] = 35000;

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

	GetNCEPGrid(kgds,lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = kgds[7] / 1000.0;
	dy = kgds[8] / 1000.0;

	GenLambertAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 1, rot_att_list, nrotatts,kgds,dx,dy);
}

void GetGrid_252
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[32];
	int nx;
	int ny;
	double latin1;
	double latin2;
	double lov;
	double dx;
	double dy;
	double la1;
	double lo1;
		
	kgds[0] = 3;
	kgds[1] = 301;
	kgds[2] = 225;
	kgds[3] = 16281;
	kgds[4] = 233862;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 265000;
	kgds[7] = 20.318; /* 20.317625 */
	kgds[8] = 20.318; /* 20.317625 */
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */
	kgds[11] = 25000;
	kgds[12] = 25000;

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

	nx = kgds[1];
	ny = kgds[2];
	latin1 = kgds[11] / 1000.0;
	latin2 = kgds[12] / 1000.0;
	lov = kgds[6] / 1000.0;
	dx = 20.317625;
	dy = 20.317625;
	la1 = kgds[3] / 1000.0;
	lo1 = kgds[4] / 1000.0;

	GenLambert(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot,
		   latin1, latin2, lov,dx,dy,la1,lo1,nx,ny,1,1);

	GenLambertAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 1, rot_att_list, nrotatts,kgds,dx,dy);
}


/*
* END Lambert Conformal Grids
*/
/*
* START lat lon grids
*/
void GenLatLon 
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, ng_size_t** dimsizes_lat, float** lon, int* n_dims_lon, ng_size_t** dimsizes_lon,int xsize,int ysize, float lon_start,float lat_start, float lon_dir, float lat_dir)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon,xsize,ysize, lon_start,lat_start, lon_dir, lat_dir)
)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
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
        *dimsizes_lat = (ng_size_t*)NclMalloc(sizeof(ng_size_t));
        *dimsizes_lon = (ng_size_t*)NclMalloc(sizeof(ng_size_t));
        *n_dims_lat = 1;
        *n_dims_lon = 1;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lon)[0] = xsize;
	for(j = 0; j < ysize; j++) {
		(*lat)[j] = lat_start + lat_dir * j;
	}
	for(j = 0; j < xsize; j++) {
		(*lon)[j] = lon_start + lon_dir * j;
	}
	return;
}
void GetGrid_86
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[3];

	kgds[0] = 0;
	kgds[1] = 360;
	kgds[2] = 90;
	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 360, 90, 0.5, -89.5 , 1.0, 1.0);
        return;
}

void GetGrid_85
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[3];

	kgds[0] = 0;
	kgds[1] = 360;
	kgds[2] = 90;
	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 360, 90, 0.5, 0.5 , 1.0, 1.0);
        return;
}
void GetGrid_64
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 91, 46, -180.0, -90.0, 2.0, 2.0);
        return;
}

void GetGrid_63
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 91, 46, 0.0, -90.0, 2.0, 2.0);
        return;
}

void GetGrid_62
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 91, 46, -180.0, 0.0, 2.0, 2.0);
        return;
}

void GetGrid_61
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 91, 46, 0.0, 0.0, 2.0, 2.0);
        return;
}

void GetGrid_50
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 36, 33, -140.0, 20.0, 2.5, 1.25);
        return;
}

void GetGrid_45
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[3];

	kgds[0] = 0;
	kgds[1] = 288;
	kgds[2] = 145;
	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 288, 145, 0.0, 90.0, 1.25, -1.25);
        return;
}

void GetGrid_34
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[3];

	kgds[0] = 0;
	kgds[1] = 181;
	kgds[2] = 46;
	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 181, 46, 0.0, -90.0, 2, 2);
        return;
}

void GetGrid_33
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[3];

	kgds[0] = 0;
	kgds[1] = 181;
	kgds[2] = 46;
	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 181, 46, 0.0, 0.0, 2, 2);
        return;
}

void GetGrid_30
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[3];

	kgds[0] = 0;
	kgds[1] = 145;
	kgds[2] = 37;
	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 145, 37, 0.0, -90.0, 2.5, 2.5);
        return;
}
void GetGrid_29
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[3];

	kgds[0] = 0;
	kgds[1] = 145;
	kgds[2] = 37;
	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 145, 37, 0.0, 0.0, 2.5, 2.5);
        return;
}
void GetGrid_26
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 72, 19, 0.0, -90.0, 5.0, 5.0);
        return;
}

void GetGrid_25
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 72, 19, 0.0, 0.0, 5.0, 5.0);
        return;
}

void GetGrid_24
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 37, 37, -180.0, -90.0, 5.0, 2.5);
        return;
}

void GetGrid_23
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 37, 37, 0.0, -90.0, 5.0, 2.5);
        return;
}

void GetGrid_22
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 37, 37, -180.0, 0.0, 5.0, 2.5);
        return;
}


void GetGrid_21
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{

        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 37, 37, 0.0, 0.0, 5.0, 2.5);
        return;
}


void GetGrid_4
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[3];

	kgds[0] = 0;
	kgds[1] = 720;
	kgds[2] = 361;
	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 720, 361, 0.0, 90.0, .5, -.5);
        return;
}

void GetGrid_3
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[3];

	kgds[0] = 0;
	kgds[1] = 360;
	kgds[2] = 181;
	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 360, 181, 0.0, 90.0, 1.0, -1.0);
        return;
}

void GetGrid_2
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[3];

	kgds[0] = 0;
	kgds[1] = 144;
	kgds[2] = 73;
	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

	GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 144, 73, 0.0, 90.0, 2.5, -2.5);
	return;
}


/*
* END lat lon grids
*/




/*
* START Polar Stereographic GRIDS
*/

static float er;
static float er2;
static float xax;
static float xpp;
static float ypp;
static int southern;

/* 
 * grdsetup, grdloc and grdrot are used for polar stereo grids that require precision higher
 * than the ncep codes can handle
 */

static void grdsetup(double x,double y,double gsp,double d,double ax) 
{
	if(d< 0) {
		southern = -1;
	} else {
		southern = 1;
	}
	er =  EAR * (1.0 + sin(fabs(d)/rtod)) / gsp;
	er2 = er * er;
	xax = ax;
	xpp = x;
	ypp = y;
	return;
}


static void grdloc(double xp,double yp, float *xlo, float* xla)
{
	double r2,ss;
	double yy,xx,elong;


	yy = yp - ypp;
	xx = xp - xpp;
	*xlo = 0.0;
	if((yy != 0.0)||(xx!=0) ) {
		elong = rtod * atan2(yy,xx);
		*xlo =  (float) elong + xax;
		if(*xlo > 180.0) 
			*xlo = *xlo - 360.0;
		if(*xlo < -180.0)
			*xlo = *xlo + 360;
	} 
	r2 = xx * xx + yy * yy;
	ss = (er2-r2)/(er2+r2);
	*xla = (float) southern * rtod * asin(ss);
} 

void gridrot(float ore,float lon, float *rot)
{
	float trot = lon - ore; /* rotation in degrees */
	if (trot > 180) 
		trot -= 360;
	if (trot < -180)
		trot += 360;
	*rot = trot * dtor; 
	return;
}

void GetHiResPolarStereoGrid
#if NhlNeedProto
(
	int *kgds, 
	double polex,
	double poley,
	double dist,
	double deg,
	double ore,
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot
)
#else
(kgds, polex, poley, dist, deg, ore, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot)
int *kgds;
double polex;
double poley;
double dist;
double deg;
double ore;
float** lat;
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float **rot;
#endif
{
	int nx,ny;
	int x,y;
	
	nx = kgds[1];
	ny = kgds[2];
        *lat = (float*)NclMalloc(sizeof(float) * nx * ny);
        *lon= (float*)NclMalloc(sizeof(float) * nx * ny);
	*rot = (float*) NclMalloc(sizeof(float) * nx * ny);
        *dimsizes_lat = (ng_size_t*)NclMalloc(sizeof(ng_size_t) * 2);
        *dimsizes_lon = (ng_size_t*)NclMalloc(sizeof(ng_size_t) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ny;
        (*dimsizes_lat)[1] = nx;
        (*dimsizes_lon)[0] = ny;
        (*dimsizes_lon)[1] = nx;

	grdsetup(polex,poley,dist,deg, ore + 90.0 );
	for (y = 0; y < ny; y++) {
		for(x = 0; x < nx; x++) {
			grdloc(x+1,y+1,&((*lon)[y * nx + x]),&((*lat)[y * nx + x]));
			gridrot((float) ore,(*lon)[y * nx + x],&((*rot)[y * nx + x]));
		}
	}
	return;
}


void GenPolarStereographicAtts
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	GribAttInqRecList **lat_att_list_ptr, 
	int * nlatatts, 
	GribAttInqRecList **lon_att_list_ptr, 
	int *nlonatts, 
	int do_rot, 
	GribAttInqRecList **rot_att_list_ptr, 
	int *nrotatts,
	int *kgds,
	double dx,
	double dy
	)
#else
(thevarrec,lat_att_list_ptr, nlatatts, lon_att_list_ptr, nlonatts, do_rot, rot_att_list_ptr, nrotatts,kgds,dx,dy)
GribParamList* thevarrec;
GribAttInqRecList **lat_att_list_ptr;
int * nlatatts; 
GribAttInqRecList **lon_att_list_ptr;
int *nlonatts;
int do_rot;
GribAttInqRecList **rot_att_list_ptr;
int *nrotatts;
int *kgds;
double dx;
double dy;
#endif
{
	NclQuark *tmp_string = NULL;
	float *tmp_float = NULL;
	int grid_oriented = 1;

	if (thevarrec->has_gds) {
		grid_oriented = (thevarrec->ref_rec->gds[16] & 010) ? 1 : 0;
	}


/* lat atts */
	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = kgds[3] / 1000.0;
	GribPushAtt(lat_att_list_ptr,"La1",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = kgds[4] / 1000.0;
	GribPushAtt(lat_att_list_ptr,"Lo1",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = kgds[6] / 1000.0;
	GribPushAtt(lat_att_list_ptr,"Lov",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = dx * 1000.0;
	GribPushAtt(lat_att_list_ptr,"Dx",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = dy * 1000;
	GribPushAtt(lat_att_list_ptr,"Dy",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
	tmp_string= (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string= NrmStringToQuark(kgds[9] & 0200 ? "south" : "north");
	GribPushAtt(lat_att_list_ptr,"ProjectionCenter",tmp_string,1,nclTypestringClass); (*nlatatts)++;

	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark("STEREOGRAPHIC");
	GribPushAtt(lat_att_list_ptr,"mpProjection",tmp_string,1,nclTypestringClass); (*nlatatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = kgds[6] / 1000.0;
	GribPushAtt(lat_att_list_ptr,"mpCenterLonF",tmp_float,1,nclTypefloatClass); (*nlatatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = kgds[9] & 0200 ? -90 : 90;
	GribPushAtt(lat_att_list_ptr,"mpCenterLatF",tmp_float,1,nclTypefloatClass); (*nlatatts)++;

/* lon atts */

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float =  kgds[3] / 1000.0;
	GribPushAtt(lon_att_list_ptr,"La1",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float =  kgds[4] / 1000.0;
	GribPushAtt(lon_att_list_ptr,"Lo1",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float =  kgds[6] / 1000.0;
	GribPushAtt(lon_att_list_ptr,"Lov",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = dx * 1000.0;
	GribPushAtt(lon_att_list_ptr,"Dx",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = dy * 1000.0;
	GribPushAtt(lon_att_list_ptr,"Dy",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
	tmp_string= (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string= NrmStringToQuark(kgds[9] & 0200 ? "south" : "north");
	GribPushAtt(lon_att_list_ptr,"ProjectionCenter",tmp_string,1,nclTypestringClass); (*nlonatts)++;

	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark("STEREOGRAPHIC");
	GribPushAtt(lon_att_list_ptr,"mpProjection",tmp_string,1,nclTypestringClass); (*nlonatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = kgds[6] / 1000.0;
	GribPushAtt(lon_att_list_ptr,"mpCenterLonF",tmp_float,1,nclTypefloatClass); (*nlonatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = kgds[9] & 0200 ? -90 : 90;
	GribPushAtt(lon_att_list_ptr,"mpCenterLatF",tmp_float,1,nclTypefloatClass); (*nlonatts)++;

	GenAtts(thevarrec,lat_att_list_ptr, nlatatts, lon_att_list_ptr, nlonatts, do_rot,grid_oriented,rot_att_list_ptr, nrotatts);
}


void GetGrid_249
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[32];
	double polex = 182.889; /* from GRIB docs */
	double poley = 460.425;
	double dist = 9.86789;
	double deg = 60.0;
	double ore = -150.0;
	double dx,dy;
		

	kgds[0] = 5;
	kgds[1] = 367;
	kgds[2] = 343;
	kgds[3] = 45400;
	kgds[4] = 188400;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 210000;
	kgds[7] = 9868;
	kgds[8] = 9868;
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

	
	GetHiResPolarStereoGrid(kgds,polex,poley,dist,deg,ore,
				lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);

	dx = dy = dist;


	GenPolarStereographicAtts(thevarrec,lat_att_list,nlatatts,lon_att_list,nlonatts,1, 
				  rot_att_list,nrotatts,kgds,dx,dy);

	return; 
}

void GetGrid_242
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[32];
	double polex = 375.636;
	double poley = 481.792;
	double dist = 11.25;
	double deg = 60.0;
	double ore = -135;
	double dx,dy;

	kgds[0] = 5;
	kgds[1] = 553;
	kgds[2] = 425;
	kgds[3] = 30000;
	kgds[4] = 187000;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 225000;
	kgds[7] = 11250;
	kgds[8] = 11250;
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}
/*
	GetNCEPGrid(kgds,lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = kgds[7] / 1000.0;
	dy = kgds[8] / 1000.0;
*/
	GetHiResPolarStereoGrid(kgds,polex,poley,dist,deg,ore,
				lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);

	dx = dy = dist;

	GenPolarStereographicAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 
				  1, rot_att_list, nrotatts,kgds,dx,dy);

	return; 
}



void GetGrid_240
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[32];
	double polex = 401.; /* from GRIB docs */
	double poley = 1601.;
	double dist = 4.7625;
	double deg = 60.0;
	double ore = -105.0;
	double dx,dy;
		
	kgds[0] = 5;
	kgds[1] = 1121;
	kgds[2] = 881;
	kgds[3] = 23098;
	kgds[4] = 240964;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 255000;
	kgds[7] = 4763;
	kgds[8] = 4763;
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

	GetHiResPolarStereoGrid(kgds,polex,poley,dist,deg,ore,
				lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);

	dx = dy = dist;

	GenPolarStereographicAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 
				  1, rot_att_list, nrotatts,kgds,dx,dy);

	return; 
}

void GetGrid_224
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[32];
	double polex = 33.0;
	double poley = 33.0;
	double dist = 381.0;
	double deg = -60.0;
	double ore = -105.0;
	double dx,dy;

		
	kgds[0] = 5;
	kgds[1] = 65;
	kgds[2] = 65;
	kgds[3] = 20826;
	kgds[4] = 120000;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 75000;  /* 225 - 180 -- the ncep code requires this */
	kgds[7] = 381000;
	kgds[8] = 381000;
	kgds[9] = 0200;
	kgds[10] = 0100; /* 0100 000 */

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}
#if 0
	GetNCEPGrid(kgds,lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = kgds[7] / 1000.0;
	dy = kgds[8] / 1000.0;
#endif
	GetHiResPolarStereoGrid(kgds,polex,poley,dist,deg,ore,
				lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);

	dx = dy = dist;

	GenPolarStereographicAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 1, rot_att_list, nrotatts,kgds,dx,dy);

	return; 
}

void GetGrid_223
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[32];
	double polex = 65.0;
	double poley = 65.0;
	double dist = 190.5;
	double deg = 60.0;
	double ore = -105.0;
	double dx,dy;

		
	kgds[0] = 5;
	kgds[1] = 129;
	kgds[2] = 129;
	kgds[3] = -20826;
	kgds[4] = 210000;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 255000;
	kgds[7] = 190500;
	kgds[8] = 190500;
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}
/*
	GetNCEPGrid(kgds,lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = kgds[7] / 1000.0;
	dy = kgds[8] / 1000.0;
*/
	GetHiResPolarStereoGrid(kgds,polex,poley,dist,deg,ore,
				lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);

	dx = dy = dist;

	GenPolarStereographicAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 1, rot_att_list, nrotatts,kgds,dx,dy);

	return; 
}

void GetGrid_220
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[32];
	double polex = 151.0;
	double poley = 181.0;
	double dist = 25.4;
	double deg = -60.0;
	double ore = 100.0;
	double dx,dy;
		
	kgds[0] = 5;
	kgds[1] = 345;
	kgds[2] = 355;
	kgds[3] = -36889;
	kgds[4] = 139806;
	kgds[5] = 000;  /* 0000 0000 */
	kgds[6] = 100000; /* 280 -180 -- required by the NCEP code */
	kgds[7] = 25400;
	kgds[8] = 25400;
	kgds[9] = 0200;
	kgds[10] = 0100; /* 0100 000 */

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}
/*
	GetNCEPGrid(kgds,lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = kgds[7] / 1000.0;
	dy = kgds[8] / 1000.0;
*/
	GetHiResPolarStereoGrid(kgds,polex,poley,dist,deg,ore,
				lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);

	dx = dy = dist;

	GenPolarStereographicAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 1, rot_att_list, nrotatts,kgds,dx,dy);

	return; 
	

}

void GetGrid_219
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[32];
	double polex = 191.00;
	double poley = 231.0;
	double dist = 25.4;
	double deg = 60.0;
	double ore = -80.0;
	double dx,dy;
		
	kgds[0] = 5;
	kgds[1] = 385;
	kgds[2] = 465;
	kgds[3] = 25008;
	kgds[4] = 240441;
	kgds[5] = 000;  /* 0000 0000 */
	kgds[6] = 280000;
	kgds[7] = 25400;
	kgds[8] = 25400;
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

/*
	GetNCEPGrid(kgds,lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = kgds[7] / 1000.0;
	dy = kgds[8] / 1000.0;
*/
	GetHiResPolarStereoGrid(kgds,polex,poley,dist,deg,ore,
				lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = dy = dist;

	GenPolarStereographicAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 1, rot_att_list, nrotatts,kgds,dx,dy);

	return; 
	

}

void GetGrid_217
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[32];
	double polex = 188.818;
	double poley = 241.397;
	double dist = 22.5;
	double deg = 60.0;
	double ore = -135.0;
	double dx,dy;
		
	kgds[0] = 5;
	kgds[1] = 277;
	kgds[2] = 213;
	kgds[3] = 30000;
	kgds[4] = 187000;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 225000;
	kgds[7] = 22500;
	kgds[8] = 22500;
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}
/*
	GetNCEPGrid(kgds,lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = kgds[7] / 1000.0;
	dy = kgds[8] / 1000.0;
*/
	GetHiResPolarStereoGrid(kgds,polex,poley,dist,deg,ore,
				lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = dy = dist;

	GenPolarStereographicAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 1, rot_att_list, nrotatts,kgds,dx,dy);

	return; 
	

}

void GetGrid_216
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[32];
	double polex = 94.909;
	double poley = 121.198;
	double dist = 45.0;
	double deg = 60.0;
	double ore = -135.0;
	double dx,dy;
		
	kgds[0] = 5;
	kgds[1] = 139;
	kgds[2] = 107;
	kgds[3] = 30000;
	kgds[4] = 187000;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 225000;
	kgds[7] = 45000;
	kgds[8] = 45000;
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}
/*
	GetNCEPGrid(kgds,lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = kgds[7] / 1000.0;
	dy = kgds[8] / 1000.0;
*/
	GetHiResPolarStereoGrid(kgds,polex,poley,dist,deg,ore,
				lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = dy = dist;

	GenPolarStereographicAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 1, rot_att_list, nrotatts,kgds,dx,dy);

	return; 
	

}

void GetGrid_214
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{

        float polex = 49;
        float poley = 101;
        float dist = 47.625;
        float deg = 60.0;
        float ore = -150.0;
	int kgds[32];
	double dx,dy;
		
	kgds[0] = 5;
	kgds[1] = 97;
	kgds[2] = 67;
	kgds[3] = 42085;
	kgds[4] = 184359;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 210000;
	kgds[7] = 47625;
	kgds[8] = 47625;
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}
/*
	GetNCEPGrid(kgds,lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = kgds[7] / 1000.0;
	dy = kgds[8] / 1000.0;
*/
	GetHiResPolarStereoGrid(kgds,polex,poley,dist,deg,ore,
				lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = dy = dist;

	GenPolarStereographicAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 1, rot_att_list, nrotatts,kgds,dx,dy);


	return; 
	

}

void GetGrid_213
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[32];
	double polex = 65.0;
	double poley = 89.0;
	double dist = 95.250;
	double deg = 60.0;
	double ore = -105.0;
	double dx,dy;
		
	kgds[0] = 5;
	kgds[1] = 129;
	kgds[2] = 85;
	kgds[3] = 7838;
	kgds[4] = 218972;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 255000;
	kgds[7] = 95250;
	kgds[8] = 95250;
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}
/*
	GetNCEPGrid(kgds,lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = kgds[7] / 1000.0;
	dy = kgds[8] / 1000.0;
*/
	GetHiResPolarStereoGrid(kgds,polex,poley,dist,deg,ore,
				lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = dy = dist;

	GenPolarStereographicAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 1, rot_att_list, nrotatts,kgds,dx,dy);

	return; 


}

void GetGrid_207
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[32];
        double polex = 25;
        double poley = 51;
        double dist = 95.250;
        double deg = 60.0;
        double ore = -150.0;
	double dx,dy;
		
	kgds[0] = 5;
	kgds[1] = 49;
	kgds[2] = 35;
	kgds[3] = 42085;
	kgds[4] = 184359;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 210000;
	kgds[7] = 95250;
	kgds[8] = 95250;
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}
/*
	GetNCEPGrid(kgds,lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = kgds[7] / 1000.0;
	dy = kgds[8] / 1000.0;
*/
	GetHiResPolarStereoGrid(kgds,polex,poley,dist,deg,ore,
				lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = dy = dist;
	
	GenPolarStereographicAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 1, rot_att_list, nrotatts,kgds,dx,dy);

	return; 

}

void GetGrid_205
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{

	int kgds[32];
        double polex = 27;
        double poley = 57;
        double dist = 190.5;
        double deg = 60.0;
        double ore = -60.0;
	double dx,dy;
		
	kgds[0] = 5;
	kgds[1] = 45;
	kgds[2] = 39;
	kgds[3] = 0616;
	kgds[4] = 275096;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 300000;
	kgds[7] = 190500;
	kgds[8] = 190500;
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}
/*
	GetNCEPGrid(kgds,lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = kgds[7] / 1000.0;
	dy = kgds[8] / 1000.0;
*/
	GetHiResPolarStereoGrid(kgds,polex,poley,dist,deg,ore,
				lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = dy = dist;

	GenPolarStereographicAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 1, rot_att_list, nrotatts,kgds,dx,dy);

	return; 

}

void GetGrid_203
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[32];
        double polex = 27.0;
        double poley = 37.0;
        double dist = 190.5;
        double deg = 60.0;
        double ore = -150.0;
	double dx,dy;
		
	kgds[0] = 5;
	kgds[1] = 45;
	kgds[2] = 39;
	kgds[3] = 19132;
	kgds[4] = 174163;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 210000;
	kgds[7] = 190500;
	kgds[8] = 190500;
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}
/*
	GetNCEPGrid(kgds,lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = kgds[7] / 1000.0;
	dy = kgds[8] / 1000.0;
*/

	GetHiResPolarStereoGrid(kgds,polex,poley,dist,deg,ore,
				lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = dy = dist;

	GenPolarStereographicAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 1, rot_att_list, nrotatts,kgds,dx,dy);

	return; 

}

void GetGrid_202
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{

	int kgds[32];
        double polex = 33;
        double poley = 45;
        double dist = 190.5;
        double deg = 60.0;
        double ore = -105.0;
	double dx,dy;
		
	kgds[0] = 5;
	kgds[1] = 65;
	kgds[2] = 43;
	kgds[3] = 7838;
	kgds[4] = 218972;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 255000;
	kgds[7] = 190500;
	kgds[8] = 190500;
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}
/*
	GetNCEPGrid(kgds,lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = kgds[7] / 1000.0;
	dy = kgds[8] / 1000.0;
*/
	GetHiResPolarStereoGrid(kgds,polex,poley,dist,deg,ore,
				lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = dy = dist;

	GenPolarStereographicAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 1, rot_att_list, nrotatts,kgds,dx,dy);

	return; 

}

void GetGrid_201
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{

	int kgds[32];
        double polex = 33;
        double poley = 33;
        double dist = 381.0;
        double deg = 60.0;
        double ore = -105.0;
	double dx,dy;
		
	kgds[0] = 5;
	kgds[1] = 65;
	kgds[2] = 65;
	kgds[3] = -20826;
	kgds[4] = 210000;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 255000;
	kgds[7] = 381000;
	kgds[8] = 381000;
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}
/*
	GetNCEPGrid(kgds,lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = kgds[7] / 1000.0;
	dy = kgds[8] / 1000.0;
*/
	GetHiResPolarStereoGrid(kgds,polex,poley,dist,deg,ore,
				lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = dy = dist;

	GenPolarStereographicAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 1, rot_att_list, nrotatts,kgds,dx,dy);

	return; 

}

void GetGrid_186
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{

	int kgds[32];
	double dx,dy;
		
	kgds[0] = 5;
	kgds[1] = 377;
	kgds[2] = 237;
	kgds[3] = 44196; 
	kgds[4] = 174759; 
	kgds[5] = 010;  /* 0100 1000 */
	kgds[6] = 20300;
	kgds[7] = 12000;
	kgds[8] = 12000;
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

	GetNCEPGrid(kgds,lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = kgds[7] / 1000.0;
	dy = kgds[8] / 1000.0;

	GenPolarStereographicAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 
				  1, rot_att_list, nrotatts,kgds,dx,dy);

	return; 
}

void GetGrid_172
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{

	int kgds[32];
	double dx,dy;
	double polex = 302.0;
	double poley = 362.0;
	double dist = 12.7;
	double deg = -60.0;
	double ore = -80.0;
		
	kgds[0] = 5;
	kgds[1] = 690;
	kgds[2] = 710;
	kgds[3] = -36899; /* 36.899599 how do you do more resolution? */
	kgds[4] = 139805; /* 139.805573 */
	kgds[5] = 0110;  /* 0100 1000 */
	kgds[6] = 100000; /* 280 - 180 -- the NCEP code requires this */
	kgds[7] = 12700;
	kgds[8] = 12700;
	kgds[9] = 0200; /* 1000 0000 */
	kgds[10] = 0100; /* 0100 0000 */

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}
/*
	GetNCEPGrid(kgds,lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = kgds[7] / 1000.0;
	dy = kgds[8] / 1000.0;
*/

	GetHiResPolarStereoGrid(kgds,polex,poley,dist,deg,ore,
				lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = dy = dist;

	GenPolarStereographicAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 
				  1, rot_att_list, nrotatts,kgds,dx,dy);

	return; 
}

void GetGrid_171
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{

	int kgds[32];
	double dx,dy;
	double polex = 384.0;
	double poley = 462.0;
	double dist = 12.7;
	double deg = 60.0;
	double ore = -80.0;
		
	kgds[0] = 5;
	kgds[1] = 770;
	kgds[2] = 930;
	kgds[3] = 25009; /* 25.008621 how do you do more resolution? */
	kgds[4] = 240440; /* 240.440331 */
	kgds[5] = 0110;  /* 0100 1000 */
	kgds[6] = 280000;
	kgds[7] = 12700;
	kgds[8] = 12700;
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}
/*
	GetNCEPGrid(kgds,lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = kgds[7] / 1000.0;
	dy = kgds[8] / 1000.0;
*/
	GetHiResPolarStereoGrid(kgds,polex,poley,dist,deg,ore,
				lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = dy = dist;

	GenPolarStereographicAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 
				  1, rot_att_list, nrotatts,kgds,dx,dy);

	return; 
}


void GetGrid_160
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{

	int kgds[32];
	double dx,dy;
		
	kgds[0] = 5;
	kgds[1] = 180;
	kgds[2] = 156;
	kgds[3] = 19132;
	kgds[4] = 174163;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 210000;
	kgds[7] = 47500;
	kgds[8] = 47500;
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

	GetNCEPGrid(kgds,lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = kgds[7] / 1000.0;
	dy = kgds[8] / 1000.0;

	GenPolarStereographicAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 
				  1, rot_att_list, nrotatts,kgds,dx,dy);

	return; 
}

void GetGrid_107
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{

	int kgds[32];
	double polex = 46.; /* from GRIB docs */
	double poley = 167.;
	double dist = 45.37732;
	double deg = 60.0;
	double ore = -105.0;
	double dx,dy;
		
	kgds[0] = 5;
	kgds[1] = 120;
	kgds[2] = 92;
	kgds[3] = 23434;
	kgds[4] = 239833;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 255000;
	kgds[7] = 45377;
	kgds[8] = 45377;
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

	GetHiResPolarStereoGrid(kgds,polex,poley,dist,deg,ore,
				lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);

	dx = dy = dist;
	GenPolarStereographicAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 
				  1, rot_att_list, nrotatts,kgds,dx,dy);

	return; 

}

void GetGrid_106
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{

	int kgds[32];
	double polex = 80.; /* from GRIB docs */
	double poley = 176.;
	double dist = 45.37732;
	double deg = 60.0;
	double ore = -105.0;
	double dx,dy;
		
	kgds[0] = 5;
	kgds[1] = 165;
	kgds[2] = 117;
	kgds[3] = 17529;
	kgds[4] = 230704;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 255000;
	kgds[7] = 45377;
	kgds[8] = 45377;
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

	GetHiResPolarStereoGrid(kgds,polex,poley,dist,deg,ore,
				lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);

	dx = dy = dist;
	GenPolarStereographicAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 
				  1, rot_att_list, nrotatts,kgds,dx,dy);

	return; 

}

void GetGrid_105
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{

	int kgds[32];
	double polex = 40.5; /* from GRIB docs */
	double poley = 88.5;
	double dist = 90.75464;
	double deg = 60.0;
	double ore = -105.0;
	double dx,dy;
		
	kgds[0] = 5;
	kgds[1] = 83;
	kgds[2] = 83;
	kgds[3] = 17529;
	kgds[4] = 230704;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 255000;
	kgds[7] = 90755;
	kgds[8] = 90755;
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */


	GetHiResPolarStereoGrid(kgds,polex,poley,dist,deg,ore,
				lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);

	dx = dy = dist;


	GenPolarStereographicAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 
				  1, rot_att_list, nrotatts,kgds,dx,dy);

	return; 
}

void GetGrid_104
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[32];
	double polex = 75.5; /* from GRIB docs */
	double poley = 109.5;
	double dist = 90.75464;
	double deg = 60.0;
	double ore = -105.0;
	double dx,dy;
		
	kgds[0] = 5;
	kgds[1] = 147;
	kgds[2] = 110;
	kgds[3] = -706;
	kgds[4] = 220525;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 255000;
	kgds[7] = 90755;
	kgds[8] = 90755;
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

	GetHiResPolarStereoGrid(kgds,polex,poley,dist,deg,ore,
				lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);

	dx = dy = dist;
	GenPolarStereographicAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 
				  1, rot_att_list, nrotatts,kgds,dx,dy);

	return; 
}

void GetGrid_103
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[32];
	double dx,dy;
		
	kgds[0] = 5;
	kgds[1] = 65;
	kgds[2] = 56;
	kgds[3] = 22405;
	kgds[4] = 238648;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 255000;
	kgds[7] = 91452;
	kgds[8] = 91452;
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

	GetNCEPGrid(kgds,lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = kgds[7] / 1000.0;
	dy = kgds[8] / 1000.0;

	GenPolarStereographicAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 
				  1, rot_att_list, nrotatts,kgds,dx,dy);

	return; 

#if 0
        float polex = 25.5;
        float poley = 84.5;
        float dist = 91.452;
        float deg = 60.0;
        float ore = -105.0;
#endif
}

void GetGrid_101
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[32];
	double dx,dy;
		
	kgds[0] = 5;
	kgds[1] = 113;
	kgds[2] = 91;
	kgds[3] = 10528;
	kgds[4] = 222854;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 255000;
	kgds[7] = 91452;
	kgds[8] = 91452;
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

	GetNCEPGrid(kgds,lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = kgds[7] / 1000.0;
	dy = kgds[8] / 1000.0;

	GenPolarStereographicAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 1, rot_att_list, nrotatts,kgds,dx,dy);

	return; 
#if 0
        float polex = 58.5;
        float poley = 92.5;
        float dist = 91.452;
        float deg = 60.0;
        float ore = -105.0;
#endif
}

void GetGrid_100
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{

	int kgds[32];
	double dx,dy;
		
	kgds[0] = 5;
	kgds[1] = 83;
	kgds[2] = 83;
	kgds[3] = 17110;
	kgds[4] = 230704;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 255000;
	kgds[7] = 91452;
	kgds[8] = 91452;
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

	GetNCEPGrid(kgds,lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = kgds[7] / 1000.0;
	dy = kgds[8] / 1000.0;

	GenPolarStereographicAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 
				  1, rot_att_list, nrotatts,kgds,dx,dy);

	return; 
#if 0
        float polex = 40.5;
        float poley = 88.5;
        float dist = 91.452;
        float deg = 60.0;
        float ore = -105.0;
#endif
}


void GetGrid_88
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{

	int kgds[32];
	double dx,dy;
		
	kgds[0] = 5;
	kgds[1] = 540;
	kgds[2] = 548;
	kgds[3] = 10000;
	kgds[4] = 232000;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 255000;
	kgds[7] = 15000;
	kgds[8] = 15000;
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

	GetNCEPGrid(kgds,lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = kgds[7] / 1000.0;
	dy = kgds[8] / 1000.0;

	GenPolarStereographicAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 
				  1, rot_att_list, nrotatts,kgds,dx,dy);

	return; 
}

void GetGrid_87
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{


	int kgds[32];
	double dx,dy;
		
	kgds[0] = 5;
	kgds[1] = 81;
	kgds[2] = 62;
	kgds[3] = 22876;
	kgds[4] = 239509;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 255000;
	kgds[7] = 68153;
	kgds[8] = 68153;
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

	GetNCEPGrid(kgds,lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = kgds[7] / 1000.0;
	dy = kgds[8] / 1000.0;

	GenPolarStereographicAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 
				  1, rot_att_list, nrotatts,kgds,dx,dy);

	return; 
#if 0
        float polex = 31.91;
        float poley = 112.53;
        float dist =  68.153;
        float deg = 60.0;
        float ore = -105.0;
#endif
}

void GetGrid_56
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{

	int kgds[32];
	double dx,dy;
		
	kgds[0] = 5;
	kgds[1] = 87;
	kgds[2] = 71;
	kgds[3] = 7647;
	kgds[4] = 226557;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 255000;
	kgds[7] = 127000;
	kgds[8] = 127000;
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

	GetNCEPGrid(kgds,lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = kgds[7] / 1000.0;
	dy = kgds[8] / 1000.0;

	GenPolarStereographicAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 
				  1, rot_att_list, nrotatts,kgds,dx,dy);

	return; 

#if 0
        float polex = 40;
        float poley = 73;
        float dist =  127;
        float deg = 60.0;
        float ore = -105.0;
#endif
}

void GetGrid_55
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{

	int kgds[32];
	double dx,dy;
        double polex = 44;
        double poley = 38;
        double dist = 254;
        double deg = 60.0;
        double ore = -105.0;
		
	kgds[0] = 5;
	kgds[1] = 87;
	kgds[2] = 71;
	kgds[3] = -10947;
	kgds[4] = 205711;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 255000;
	kgds[7] = 254000;
	kgds[8] = 254000;
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}
/*
	GetNCEPGrid(kgds,lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = kgds[7] / 1000.0;
	dy = kgds[8] / 1000.0;
*/
	GetHiResPolarStereoGrid(kgds,polex,poley,dist,deg,ore,
				lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = dy = dist;

	GenPolarStereographicAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 
				  1, rot_att_list, nrotatts,kgds,dx,dy);

	return; 

}

void GetGrid_28
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{

	int kgds[32];
	double dx,dy;
        double polex = 33;
        double poley = 33;
        double dist = 381; 
        double deg = -60.0;
        double ore = -80.0;

		
	kgds[0] = 5;
	kgds[1] = 65;
	kgds[2] = 65;
	kgds[3] = 20825;
	kgds[4] = 145000;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 100000;  /* 280 - 180  The NCEP code needs this for southern hemisphere */
	kgds[7] = 381000;
	kgds[8] = 381000;
	kgds[9] = 0200;
	kgds[10] = 0100; /* 0100 000 */

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}
/*
	GetNCEPGrid(kgds,lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = kgds[7] / 1000.0;
	dy = kgds[8] / 1000.0;
*/
	GetHiResPolarStereoGrid(kgds,polex,poley,dist,deg,ore,
				lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = dy = dist;

	GenPolarStereographicAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 
				  1, rot_att_list, nrotatts,kgds,dx,dy);

	return; 

}

void GetGrid_27
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[32];
	double dx,dy;
        double polex = 33;
        double poley = 33;
        double dist = 381; 
        double deg = 60.0;
        double ore = -80.0;
		
	kgds[0] = 5;
	kgds[1] = 65;
	kgds[2] = 65;
	kgds[3] = -20825;
	kgds[4] = 235000;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 280000;
	kgds[7] = 381000;
	kgds[8] = 381000;
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}
/*
	GetNCEPGrid(kgds,lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = kgds[7] / 1000.0;
	dy = kgds[8] / 1000.0;
*/
	GetHiResPolarStereoGrid(kgds,polex,poley,dist,deg,ore,
				lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = dy = dist;

	GenPolarStereographicAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 
				  1, rot_att_list, nrotatts,kgds,dx,dy);

	return; 
}

void GetGrid_6
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[32];
	double dx,dy;
		
	kgds[0] = 5;
	kgds[1] = 53;
	kgds[2] = 45;
	kgds[3] = 7647;
	kgds[4] = 226557;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 255000;
	kgds[7] = 190500;
	kgds[8] = 190500;
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}

	GetNCEPGrid(kgds,lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = kgds[7] / 1000.0;
	dy = kgds[8] / 1000.0;

	GenPolarStereographicAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 
				  1, rot_att_list, nrotatts,kgds,dx,dy);

	return; 
#if 0
        float polex = 27;
        float poley = 49;
        float dist = 190.5 ;
        float deg = 60.0;
        float ore = -105.0;
#endif
}

void GetGrid_5
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat, 
	ng_size_t** dimsizes_lat, 
	float** lon, 
	int* n_dims_lon, 
	ng_size_t** dimsizes_lon, 
	float **rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int kgds[32];
	double dx,dy;
	double polex = 27.00;
	double poley = 49.0;
	double dist = 190.5;
	double deg = 60.0;
	double ore = -105.0;
		
	kgds[0] = 5;
	kgds[1] = 53;
	kgds[2] = 57;
	kgds[3] = 7647;
	kgds[4] = 226556;
	kgds[5] = 010;  /* 0000 1000 */
	kgds[6] = 255000;
	kgds[7] = 190500;
	kgds[8] = 190500;
	kgds[9] = 0;
	kgds[10] = 0100; /* 0100 000 */

	if (thevarrec->has_gds && ! ConsistentWithGDS(thevarrec,kgds)) {
		return;
	}
/*
	GetNCEPGrid(kgds,lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = kgds[7] / 1000.0;
	dy = kgds[8] / 1000.0;
*/
	GetHiResPolarStereoGrid(kgds,polex,poley,dist,deg,ore,
				lat,n_dims_lat,dimsizes_lat,lon,n_dims_lon,dimsizes_lon,rot);
	dx = dy = dist;

	GenPolarStereographicAtts(thevarrec,lat_att_list,nlatatts,lon_att_list, nlonatts, 
				  1, rot_att_list, nrotatts,kgds,dx,dy);

	return;      
	
}




/*
* END Polar Stereographic GRIDS
*/
/*
* Grid dimensions must be set in var_info field of 
*/


static int GenericUnPack
#if NhlNeedProto
(int fd, void** outdat, void** missing_value, GribRecordInqRec *therec, GribParamList* thevarrec)
#else
(fd, outdat, missing_value, therec, thevarrec)
int fd;
void** outdat;
void** missing_value;
GribRecordInqRec *therec;
GribParamList* thevarrec;
#endif
{
	int index =0,i=0;
	int integer = 0;
	int spherical_harm = 0;
	int second_order = 0;
	int complex_packing = 0;
	int additional_flags = 0;
	int sign;
	unsigned char tmp[4];
	int number_of_bits;
	int binary_scale_factor;
	int decimal_scale_factor;
	int unused_bits;
	float reference_value;
	float tmpb,tmpa;
	unsigned char *bds;
	int total = 0;
	void *data = NULL;
	int isize = sizeof(int)*8;
	unsigned int X;
	int tbits;
	int ijswap;
	int bboff;
	int dnum = 0;
	ng_size_t grid_size = 0;
	unsigned char *bms = NULL;
	int numeric = 0;
	int offset_1o;
	float pmsval = DEFAULT_MISSING_FLOAT;
	int kret =1;
	int kcode = 3;
	int is_thinned_lon = 0;
	int is_thinned_lat = 0;
	int is_staggered_grid = 0;
	int is_uv = 0;
	int nlon = 0,nlat = 0;
	Index_Func index_func = compute_index;
	int lat_size, lon_size;
	int j;
	int has_missing, operio, oveggy;
	double bin_scale, dec_scale;
	
	if (therec->has_gds) {
		nlon = CnvtToDecimal(2,&(therec->gds[6]));
		nlat = CnvtToDecimal(2,&(therec->gds[8]));
		is_thinned_lon = (nlon == 65535);
		is_thinned_lat = (nlat == 65535);
		is_staggered_grid = (therec->gds_type == 203);
		is_staggered_grid = False; /* we're not going to expand the staggered grids for now at least */
		is_uv = is_staggered_grid && Is_UV(therec->param_number);
		if (is_staggered_grid) {
			index_func = compute_index_staggered;
		}
		if (is_thinned_lon && is_thinned_lat) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"GenericUnPack: invalid thinning parameters in GDS");
			*outdat = NULL;
			*missing_value = NULL;
			return 0;
		}
	}

	bds = (unsigned char*)NclMalloc((unsigned)therec->bds_size + 4); /* 4 added so that array bounds will never be ovewitten*/
	lseek(fd,therec->offset + therec->bds_off,SEEK_SET);
	read(fd,(void*)bds,therec->bds_size);
	bds[therec->bds_size] = (char)0;
	bds[therec->bds_size +1] = (char)0;
	bds[therec->bds_size + 2] = (char)0;
	bds[therec->bds_size + 3] = (char)0;

        if(therec->has_bms) {
                bms = (unsigned char*)NclMalloc((unsigned)therec->bms_size);
                lseek(fd,therec->offset + therec->bms_off,SEEK_SET);
                read(fd,(void*)bms,therec->bms_size);
                numeric = CnvtToDecimal(2,&(bms[4]));
                if(numeric != 0) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"GribUnPack: Record uses predefined bit map. Predefined bit maps are not supported yet");
                        NclFree(bms);
                        bms = NULL;
                }
        }


	spherical_harm = (int)(bds[3] & (char)0200) ? 1 : 0;
	if (therec->gds_type < 50 || therec->gds_type > 80) {
		spherical_harm = False;
	}
	second_order = (int)(bds[3] & (char)0100) ? 1 : 0;
	integer = (int)(bds[3] & (char)0040) ? 1 : 0;
	additional_flags = (bds[3] & (char)0020) ? 1 : 0;

	if((spherical_harm)&&(therec->has_gds)) {
        	complex_packing = CnvtToDecimal(1,&(therec->gds[13])) == 2 ? 1:0;
	} else {
		complex_packing = 0;
	}
	if(therec->has_gds) {
/*
		fprintf(stdout,"%d)--> %d\n",count++,(int)therec->gds[27]);
*/
		if(((int)therec->pds[4] == 7)&&((int)therec->pds[25]==1)) {
			ijswap = 0;
		} else {
			ijswap = (int)(therec->gds[27]&(unsigned char)0040);
		}
	} else {
		ijswap = 0;
	}

	if(therec->version != 0) {
		tmp[0] = (therec->pds[26] & (char)0177);
		tmp[1] = therec->pds[27];
		decimal_scale_factor  = CnvtToDecimal(2,tmp);
		if(therec->pds[26] & (char)0200) 
			decimal_scale_factor = -decimal_scale_factor;
	} else {
		decimal_scale_factor = 0;
	}

	number_of_bits = (int)bds[10];
	tmp[0] = (unsigned char)(bds[3] & (char)0017);
	unused_bits = CnvtToDecimal(1,tmp);
	tmp[0] = (unsigned char)(bds[4] & (char)0177);
	tmp[1] = bds[5];
	binary_scale_factor = CnvtToDecimal(2,tmp);
	if(bds[4] & (char)0200) {
		binary_scale_factor = -binary_scale_factor;
	}
	bin_scale = pow(2.0,(double)binary_scale_factor);
	dec_scale = pow(10.0,(double)decimal_scale_factor);
	sign  = (bds[6] & (char) 0200)? 1 : 0;
/*
* Compute exponent GRIB docs specify 7 bit exponent
*/
	tmpa = (float)(bds[6] & (char)0177);
/*
* Compute mantisa GRIB docs specify 24 bit mantisa no hidden bit
*/
	tmpb = (float)CnvtToDecimal(3,&(bds[7]));

	reference_value = tmpb;
	reference_value *= (float)pow(2.0,-24.0);
	reference_value *= (float)pow(16.0,(double)(tmpa - 64));
	if(sign) {
		reference_value = -reference_value;
	}
	lon_size = thevarrec->var_info.dim_sizes[thevarrec->var_info.num_dimensions-1];
	lat_size =  thevarrec->var_info.dim_sizes[thevarrec->var_info.num_dimensions-2];
	grid_size = is_staggered_grid ? lon_size * lat_size / 2 : lon_size * lat_size;

	if((!spherical_harm)&&(!second_order) && (!is_thinned_lat)) {
		if(integer) {
			*missing_value= (void*)NclMalloc((unsigned)sizeof(int));
			*(int*)(*missing_value) = DEFAULT_MISSING_INT;
		} else {
			*missing_value= (void*)NclMalloc((unsigned)sizeof(float));
			*(float*)(*missing_value) = DEFAULT_MISSING_FLOAT;
		}
		if(number_of_bits != 0) {
			i = 11;
			bboff = 0;
			index = 0;
			tbits = 0;
			total = (int)(((therec->bds_size - 11) * 8 - unused_bits)/ number_of_bits);
			if(integer) {
				if(*outdat == NULL) {
					data = (void*)NclMalloc((unsigned)sizeof(ng_size_t)*grid_size);
				} else {
					data = *outdat;
				}
			} else {
				if(*outdat == NULL) {
					data = (void*)NclMalloc((unsigned)sizeof(float)*grid_size);
				} else {
					data = *outdat;
				}
			}
			has_missing = 0;
			while((index < grid_size)&&(dnum < total)) {
				if(is_gpoint(bms,index)) {
					X = UnsignedCnvtToDecimal(4,&(bds[i]));
					X = X << bboff;
					X = X >> (isize - number_of_bits);
					if(integer) {
						((int*)data)[(*index_func)(index,lon_size,lat_size,ijswap,is_uv)] = 
							(int)(reference_value + (X * bin_scale))/dec_scale;
						index++;
						dnum++;

					} else {
						((float*)data)[(*index_func)(index,lon_size,lat_size,ijswap,is_uv)] = 
							(float)(reference_value + (X * bin_scale)) / dec_scale;
						index++;
						dnum++;
					}
					tbits += number_of_bits;
					i = (int)(tbits/8.0) + 11;
					bboff = tbits % 8;
				} else {
					has_missing = 1;
					 if(integer) {
                                                ((int*)data)[index] = DEFAULT_MISSING_INT;
                                                index++;

                                        } else {
                                                ((float*)data)[index] = DEFAULT_MISSING_FLOAT;
                                                index++;
                                        }
				}
			}
			if(index < grid_size && ! (is_thinned_lon || is_thinned_lat)) {
				if(integer) {
					for(;index<grid_size;index++) 
                                              ((int*)data)[index] = DEFAULT_MISSING_INT;
                                } else {
					for(;index<grid_size;index++) 
                                	        ((float*)data)[index] = DEFAULT_MISSING_FLOAT;
                                }
			}
			*outdat = data;
		} else {
			total = lon_size * lat_size;;
			if(integer) {
				if(*outdat == NULL) {
					data = (void*)NclMalloc((unsigned)sizeof(int)*total);
				} else {
					data = *outdat;
				}
				for(i = 0; i < total; i++) {
					if(is_gpoint(bms,i)) {
						((int*)data)[i]= (int)reference_value;
					}
					else {
                                                ((int*)data)[i] = DEFAULT_MISSING_INT;
					}
				}
			} else {
				float val;
				if(*outdat == NULL) {
					data = (void*)NclMalloc((unsigned)sizeof(float)*total);
				} else {
					data = *outdat;
				}
				val = (float) reference_value/pow(10.0,(double)(decimal_scale_factor));
				for(i = 0; i < total; i++) {
					if(is_gpoint(bms,i)) {
						((float*)data)[i]= val;
					}
					else {
                                                ((float*)data)[i] = DEFAULT_MISSING_FLOAT;
					}
				}
			}
			*outdat = data;
		}
		if(is_thinned_lon || is_thinned_lat){
			unsigned char *gds = therec->gds;
			int pl_ix;
			int *rc_count;
			int i,n;
			int jpmax;
			float *ztemp, *zline, *zwork;

			pl_ix = (gds[4] == 255) ? -1 : (int) gds[3] * 4 + (int) gds[4] - 1;

			if (pl_ix == -1) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  "GenericUnpack: Invalid thinned longitude grid");
				return integer;
			}
			if (is_thinned_lon) {
				n = nlat;
				kcode = (therec->interp_method == 0 || therec->has_bms) ? 1 : 3;
			}
			else {
				n = nlon;
				kcode = (therec->interp_method == 0 || therec->has_bms) ? 11 : 13;
			}
			rc_count = (int*)NclMalloc(sizeof(int)*n);
			jpmax = MAX(lat_size,lon_size / 2 + 1);
			ztemp = NclMalloc(jpmax * jpmax * 2 * sizeof(float));
			zline = NclMalloc(jpmax * 2 * sizeof(float));
			zwork = NclMalloc((2 * jpmax + 3) * 3 * sizeof(float));
			if (! (rc_count && ztemp && zline && zwork)) {
				NhlPError(NhlFATAL,ENOMEM,NULL);
				return integer;
			}
			for (i = 0; i < n; i++) {
				rc_count[i] = CnvtToDecimal(2,&(gds[pl_ix + i * 2]));
			}
			operio = 0;
			oveggy = 0;
			NGCALLF(qu2reg3,QU2REG3)(*outdat,rc_count,&lat_size,&lon_size,&kcode,&pmsval,&kret,
				                 &has_missing,&operio,&oveggy,&jpmax,ztemp,zline,zwork);
			NclFree(rc_count);
			NclFree(ztemp);
			NclFree(zline);
			NclFree(zwork);
		}
		else if(is_staggered_grid){
#if 0
			if (integer) {
				int *idata = (int*)data;
				for (j = 0; j < lat_size; j++) {
					if ((j % 2 + is_uv) % 2 == 0) {
						for (i = 1; i < lon_size - 1; i += 2) {
							idata[j * lon_size + i] = (idata[j * lon_size + i - 1] + idata[j * lon_size + i + 1]) / 2;

						}
						idata[j * lon_size + lon_size - 1] = 2 * (idata[j * lon_size + lon_size - 2]) - idata[j * lon_size + lon_size - 4]; 
					}
					else {
						idata[j * lon_size] = 2 * (idata[j * lon_size + 1]) - idata[j * lon_size + 3]; 
						for (i = 2; i < lon_size - 1; i += 2) {
							idata[j * lon_size + i] = (idata[j * lon_size + i - 1] + idata[j * lon_size + i + 1]) / 2;
						}
					}
				}
			}
			else {
				float *fdata = (float*)data;
				for (j = 0; j < lat_size; j++) {
					if ((j % 2 + is_uv) % 2 == 0) {
						for (i = 1; i < lon_size - 1; i += 2) {
							fdata[j * lon_size + i] = (fdata[j * lon_size + i - 1] + fdata[j * lon_size + i + 1]) / 2;
						}
						fdata[j * lon_size + lon_size - 1] = 2 * (fdata[j * lon_size + lon_size - 2]) - fdata[j * lon_size + lon_size - 4]; 
					}
					else {
						fdata[j * lon_size] = 2 * (fdata[j * lon_size + 1]) - fdata[j * lon_size + 3]; 
						for (i = 2; i < lon_size - 1; i += 2) {
							fdata[j * lon_size + i] = (fdata[j * lon_size + i - 1] + fdata[j * lon_size + i + 1]) / 2;
						}
					}
				}
			}
#endif
			if (integer) {
				int *idata = (int*)data;
				for (j = 0; j < lat_size; j++) {
					if ((j % 2 + is_uv) % 2 == 0) {
						for (i = 1; i < lon_size - 1; i += 2) {
							idata[j * lon_size + i] = idata[j * lon_size + i - 1];

						}
						idata[j * lon_size + lon_size - 1] = DEFAULT_MISSING_INT;
					}
					else {
						idata[j * lon_size] = DEFAULT_MISSING_INT;
						for (i = 2; i < lon_size - 1; i += 2) {
							idata[j * lon_size + i] = idata[j * lon_size + i - 1];
						}
					}
				}
			}
			else {
				float *fdata = (float*)data;
				for (j = 0; j < lat_size; j++) {
					if ((j % 2 + is_uv) % 2 == 0) {
						for (i = 1; i < lon_size - 1; i += 2) {
							fdata[j * lon_size + i] = fdata[j * lon_size + i - 1];
						}
						fdata[j * lon_size + lon_size - 1] = DEFAULT_MISSING_FLOAT;
					}
					else {
						fdata[j * lon_size] = DEFAULT_MISSING_FLOAT;
						for (i = 2; i < lon_size - 1; i += 2) {
							fdata[j * lon_size + i] = fdata[j * lon_size + i - 1];
						}
					}
				}
			}
			*outdat = data;

		}

	} else if(spherical_harm) {
		if(complex_packing) {
			int nvals;
			int ip;
			int j,k,m,sindex,packed_start;
			int M,N;
			int jmain,kmain,mmain;
			int counter;
			int mcounter;
			int diff;
			float *vals;
			float *imvals;
			float *factor;


			if(integer) {
				*missing_value= (void*)NclMalloc((unsigned)sizeof(int));
				*(int*)(*missing_value) = DEFAULT_MISSING_INT;
			} else {
				*missing_value= (void*)NclMalloc((unsigned)sizeof(float));
				*(float*)(*missing_value) = DEFAULT_MISSING_FLOAT;
			}
			offset_1o = UnsignedCnvtToDecimal(2,&bds[11]) - therec->bds_off;
			ip = UnsignedCnvtToDecimal(2,&bds[13]);
			j = (int)bds[15];
			k = (int)bds[16];
			m = (int)bds[17];
			nvals = (offset_1o - 18)/4;
			jmain = UnsignedCnvtToDecimal(2,&(therec->gds[6]));
			kmain = UnsignedCnvtToDecimal(2,&(therec->gds[8]));
			mmain = UnsignedCnvtToDecimal(2,&(therec->gds[10]));
			if((j==k)&&(k==m)/*&&(jmain==kmain)&&(kmain==mmain)*/) {
				factor = NclMalloc(sizeof(float)*(jmain+1));
/*
* compute number of values  in the unpcacked portion
*/
				counter =  (m+1)*(m+2)/2;
				mcounter = (mmain +1)*(mmain+2)/2;
				factor[0] = 1;
				for(i = 1; i < mmain+1; i++) {
					factor[i] = 1.0/pow((double)(i * (i+1)),(double)ip/1000.0);
				}
				if(*outdat == NULL) {
					*outdat = vals = (float*)calloc(2*(jmain+1)*(jmain+1),sizeof(float));
					imvals = &(vals[(jmain+1)*(jmain+1)]);
                                } else {
                                        vals = *outdat;
					memset(vals,0,2*(jmain+1)*(jmain+1)*sizeof(float));
					imvals = &(vals[(jmain+1)*(jmain+1)]);
                                }
	
				
				
			} else {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"GenericUnPack: NCL has encountered a truncation scheme it can't handle, please report this");
				return(integer);
			}

			sindex = 18;
			diff = (jmain+1) - (j+1);
			N = 0;
			M = 0;
			for( i = counter;i>0; i--) {
				sign  = (bds[sindex] & (char) 0200)? 1 : 0;
/*
* Compute exponent GRIB docs specify 7 bit exponent
*/
				tmpa = (float)(bds[sindex++] & (char)0177);
/*
* Compute mantisa GRIB docs specify 24 bit mantisa no hidden bit
*/
				tmpb = (float)CnvtToDecimal(3,&(bds[sindex]));
				sindex+=3;

				vals[N*(jmain+1)+M] = tmpb;
				vals[N*(jmain+1)+M] *= (float)pow(2.0,-24.0);
				vals[N*(jmain+1)+M] *= (float)sqrt(2.0)*2.0*(float)pow(16.0,(double)(tmpa - 64));
				if(sign) {
					vals[N*(jmain+1)+M] = -vals[N*(jmain+1)+M];
				}
			
				sign  = (bds[sindex] & (char) 0200)? 1 : 0;
/*
* Compute exponent GRIB docs specify 7 bit exponent
*/
				tmpa = (float)(bds[sindex++] & (char)0177);
/*
* Compute mantisa GRIB docs specify 24 bit mantisa no hidden bit
*/
				tmpb = (float)CnvtToDecimal(3,&(bds[sindex]));
				sindex+=3;

				imvals[N*(jmain+1)+M] = tmpb;
				imvals[N*(jmain+1)+M] *= (float)pow(2.0,-24.0);
				imvals[N*(jmain+1)+M] *=  (float)sqrt(2.0)*2.0*(float)pow(16.0,(double)(tmpa - 64));
				if(sign) {
					imvals[N*(jmain+1)+M] = -imvals[N*(jmain+1)+M];
				}
				if(N==(j)) {
					vals[N*(jmain+1)+M] *= factor[N];
					imvals[N*(jmain+1)+M] *= factor[N];
					M++;
					N = M;
				} else {
					N++;
				}
			}
/*
* sindex should be pointing to begining of packed data
*/ 
			packed_start = sindex;
			M = 0;
			N = m+1;
			bboff = 0;	
			index = 0;
			tbits = 0;
			total = (int)(((therec->bds_size - packed_start)*8 -unused_bits)/number_of_bits);
			for(i=0;i<mcounter-counter;i++)  {
				X = UnsignedCnvtToDecimal(4,&(bds[sindex]));
				X = X << bboff;
				X = X >> (isize - number_of_bits);
				vals[N*(jmain+1)+M] = (float)(reference_value + (X * pow(2.0,(double)binary_scale_factor)))/pow(10.0,(double)(decimal_scale_factor));
				vals[N*(jmain+1)+M] *=  (float)sqrt(2.0)*2.0*factor[N];
				tbits += number_of_bits;
				sindex = (int)(tbits/8.0) + packed_start;
				bboff = tbits % 8;

				X = UnsignedCnvtToDecimal(4,&(bds[sindex]));
				X = X << bboff;
				X = X >> (isize - number_of_bits);
				imvals[N*(jmain+1)+M] = (float)(reference_value + (X * pow(2.0,(double)binary_scale_factor)))/pow(10.0,(double)(decimal_scale_factor));
				imvals[N*(jmain+1)+M] *=  (float)sqrt(2.0)*2.0*factor[N];
				tbits += number_of_bits;
				sindex = (int)(tbits/8.0) + packed_start;
				bboff = tbits % 8;
				
				if(N==(jmain)) {
					M++;
					if(M>m) {
						  N = M;
					} else {
						N = m+1;
					}
				} else {
					N++;
				}

			}
/*
* sindex now points to begining of packed data
*/
			NclFree(factor);
		} else{
			*outdat = NULL;
		}
	} 
	else if (second_order) {
		int n1_first_order_start;
		int n2_second_order_start;
		int secondary_bit_maps;
		int second_order_variable_width;  /* variable or constant width for the second order values */
		int second_order_width = 0;
		int *second_order_widths = NULL;
		int extended_second_order_packing;
		int boustrophedonic_ordering;
		int spatial_differencing; /* 0 -none, 1 - 1st order, 2 - 2nd order, 3 - 3rd order */
		int p1_num_sub_sections;
		int p2_num_second_order;
		int i;
		unsigned char *sec_bm = NULL;
		int sec_bm_size = 0;
		int *first_order_vals = NULL;
		

		n1_first_order_start = UnsignedCnvtToDecimal(2, &(bds[11]));
		n2_second_order_start = UnsignedCnvtToDecimal(2, &(bds[14]));
		p1_num_sub_sections = UnsignedCnvtToDecimal(2, &(bds[16]));
		p2_num_second_order = UnsignedCnvtToDecimal(2, &(bds[18]));
		secondary_bit_maps = (bds[13] & (char)0040) ? 1 : 0;
		second_order_variable_width = (bds[13] & (char)0020) ? 1 : 0;
		extended_second_order_packing = (bds[13] & (char)0010) ? 1 : 0;
		boustrophedonic_ordering = (bds[13] & (char)04) ? 1 : 0;
		spatial_differencing = (int) (bds[13] & (char)03);
		

		if(integer) {
			*missing_value= (void*)NclMalloc((unsigned)sizeof(int));
			*(int*)(*missing_value) = DEFAULT_MISSING_INT;
		} else {
			*missing_value= (void*)NclMalloc((unsigned)sizeof(float));
			*(float*)(*missing_value) = DEFAULT_MISSING_FLOAT;
		}
		if (! second_order_variable_width) {
			second_order_width = UnsignedCnvtToDecimal(1, &(bds[21]));
		}
		else {
			second_order_widths = NclMalloc(p1_num_sub_sections * sizeof(int));
			for (i = 0; i < p1_num_sub_sections; i++) {
				second_order_widths[i] = (int) UnsignedCnvtToDecimal(1, &(bds[21 + i]));
			}
			
		}
		if (secondary_bit_maps) {
			sec_bm_size = p2_num_second_order / 8 +  (p2_num_second_order % 8 == 0 ? 0 : 1);
			sec_bm_size += sec_bm_size % 2;
			sec_bm = bds + 21 + p1_num_sub_sections;
			/*
			if (bds + n1_first_order_start != sec_bm + sec_bm_size) {
				printf("not correct yet\n");
			}
			*/
		}
		first_order_vals = NclMalloc(p1_num_sub_sections * sizeof(int));
		
		if (first_order_vals) {
			if (number_of_bits == 0) {
				for (i = 0; i < p1_num_sub_sections; i++) {
					first_order_vals[i] = 0;
				}
			}
			else {
				int bit_offset = 0;
				int tbits = 0;
				int cix = 0;
				unsigned char *start = bds + n1_first_order_start - 1;
				for (i = 0; i < p1_num_sub_sections; i++) {
					X = UnsignedCnvtToDecimal(4,start + cix);
					X = X << bit_offset;
					X = X >> (isize - number_of_bits);
					first_order_vals[i] = X;
					tbits += number_of_bits;
					cix = tbits / 8;
					bit_offset = tbits % 8;
				}
			}
		}
		if(integer) {
			if(*outdat == NULL) {
				data = (void*)NclMalloc((unsigned)sizeof(int)*grid_size);
			} else {
				data = *outdat;
			}
		} else {
			if(*outdat == NULL) {
				data = (void*)NclMalloc((unsigned)sizeof(float)*grid_size);
			} else {
				data = *outdat;
			}
		}
		has_missing = 0;
		index = 0;  /* this indexes the output data array */
		if (! secondary_bit_maps && first_order_vals) {  /* packing is by row or column depending on ijswap  */
			unsigned char *dstart = bds + n2_second_order_start -1;
			int num_per_subsection = ijswap ? lat_size : lon_size;
			int bit_offset = 0;
			int tbits = 0;
			int cix = 0;
			for (i = 0; i < p1_num_sub_sections; i++) {
				int bit_width = second_order_widths ? second_order_widths[i] : second_order_width;
				int first_order_val = first_order_vals[i];
				if (bit_width == 0) {
					for (j = 0; j < num_per_subsection; j++) {
						if (! is_gpoint(bms,index)) {
                                                        has_missing = 1;                                                        
							if(integer) {
                                                                ((int*)data)[index] = DEFAULT_MISSING_INT;
                                                        } else {
                                                                ((float*)data)[index] = DEFAULT_MISSING_FLOAT;                                                        
							}
							index++;
                                                        continue;
                                                }
						if(integer) {
							((int*)data)[index] = (int) (reference_value + first_order_val * bin_scale) / dec_scale;
						} else {
							((float*)data)[index] = (float) (reference_value + first_order_val * bin_scale) / dec_scale;
						}
						index++;
					}
					if (index == grid_size) break;
				}
				else {
					for (j = 0; j < num_per_subsection; j++) {
						if (! is_gpoint(bms,index)) {
							has_missing = 1;
							if(integer) {
								((int*)data)[index] = DEFAULT_MISSING_INT;
							} else {
								((float*)data)[index] = DEFAULT_MISSING_FLOAT;
							}
							index++;
							continue;
						}
						X = UnsignedCnvtToDecimal(4,dstart + cix);
						X = X << bit_offset;
						X = X >> (isize - bit_width);
						if(integer) {
							((int*)data)[index] = (int) (reference_value + (first_order_val + X) * bin_scale) / dec_scale;
						} else {
							((float*)data)[index] = (float) (reference_value + (first_order_val + X) * bin_scale) / dec_scale;
						}
						tbits += bit_width;
						cix = tbits / 8;
						bit_offset = tbits % 8;
						index++;
						if (index == grid_size) break;
					}
				}
			}

		}
		else { /* don't know how to do secondary bit maps yet */
			*outdat = NULL;
			NhlPError(NhlWARNING,NhlEUNKNOWN,"GenericUnPack: NCL does not yet handle gridded data using secondary bitmaps with complex packing : no valid values returned");
		}
		if (first_order_vals)
			NhlFree(first_order_vals);
	}
	else {
			*outdat = NULL;
	}
	NclFree(bds);
	NclFree(bms);
	return(integer);


}

static int IFOS50UnPack
#if NhlNeedProto
(int fd, void** outdat, void** missing_value, GribRecordInqRec *therec, GribParamList* thevarrec)
#else
(fd, outdat, missing_value, therec, thevarrec)
int fd;
void** outdat;
void** missing_value;
GribRecordInqRec *therec;
GribParamList* thevarrec;
#endif
{
	int count =0,i=0,j = 0;
	int rindex =0;
	int integer = 0;
	int lat= 0;
	int spherical_harm = 0;
	int second_order = 0;
	int additional_flags = 0;
	int sign;
	unsigned char tmp[4];
	int number_of_bits;
	int binary_scale_factor;
	int decimal_scale_factor;
	int unused_bits;
	float reference_value;
	float tmpb,tmpa;
	unsigned char *bds;
	int total = 0;
	void *data = NULL;
	int isize = sizeof(int)*8;
	unsigned int X;
	int tbits;
	int bboff;
	int fill = 0;
	


	bds = (unsigned char*)NclMalloc((unsigned)therec->bds_size + 4);
	lseek(fd,therec->offset + therec->bds_off,SEEK_SET);
	read(fd,(void*)bds,therec->bds_size);
	bds[therec->bds_size] = (char)0;
	bds[therec->bds_size +1] = (char)0;
	bds[therec->bds_size + 2] = (char)0;
	bds[therec->bds_size + 3] = (char)0;

	spherical_harm = (int)(bds[3] & (char)0200) ? 1 : 0;
	second_order = (int)(bds[3] & (char)0100) ? 1 : 0;
	integer = (int)(bds[3] & (char)0040) ? 1 : 0;
	additional_flags = (bds[3] & (char)0020) ? 1 : 0;


	if(therec->version != 0 ) {
		tmp[0] = (therec->pds[26] & (char)0177);
		tmp[1] = therec->pds[27];
		decimal_scale_factor  = CnvtToDecimal(2,tmp);
		if(therec->pds[26] & (char)0200) 
			decimal_scale_factor = -decimal_scale_factor;
	} else {
		decimal_scale_factor = 0;
	}

	number_of_bits = (int)bds[10];
	tmp[0] = (unsigned char)(bds[3] & (char)0017);
	unused_bits = CnvtToDecimal(1,tmp);
	tmp[0] = (unsigned char)(bds[4] & (char)0177);
	tmp[1] = bds[5];
	binary_scale_factor = CnvtToDecimal(2,tmp);
	if(bds[4] & (char)0200) {
		binary_scale_factor = -binary_scale_factor;
	}
	sign  = (bds[6] & (char) 0200)? 1 : 0;
	tmpa = (float)(bds[6] & (char)0177);
	tmpb = (float)CnvtToDecimal(3,&(bds[7]));

	reference_value = tmpb;
	reference_value *= (float)pow(2.0,-24.0);
	reference_value *= (float)pow(16.0,(double)(tmpa - 64));
	if(sign) {
		reference_value = -reference_value;
	}

	if((!spherical_harm)&&(!second_order)&&(!additional_flags)) {
		if(number_of_bits != 0) {
			if(integer) {
				*missing_value= (void*)NclMalloc((unsigned)sizeof(int));
				*(int*)(*missing_value) = DEFAULT_MISSING_INT;
			} else {
				*missing_value= (void*)NclMalloc((unsigned)sizeof(float));
				*(float*)(*missing_value) = DEFAULT_MISSING_FLOAT;
			}
			i = 11;
			bboff = 0;
			count = 0;
			rindex = 0;
			tbits = 0;
			total = (int)(((therec->bds_size - 11) * 8 - unused_bits)/ number_of_bits);
			if(integer) {
				if(*outdat == NULL) {
					data = (void*)NclMalloc((unsigned)sizeof(int)*36*33);
				} else {
					data = *outdat;
				}
			} else {
				if(*outdat == NULL) {
					data = (void*)NclMalloc((unsigned)sizeof(float)*36*33);
				} else {
					data = *outdat;
				}
			}
			for(lat = 0; lat < 33; lat++) {
				switch(lat) {
				case 0:
				case 1:
				case 2:
				case 3:
					fill = 7;
					break;
				case 4:
				case 5:
				case 6:
				case 7:
					fill = 6;
					break;
				case 8:
				case 9:
				case 10:
				case 11:
					fill = 5;
					break;
				case 12:
				case 13:
				case 14:
				case 15:
					fill = 4;
					break;
				case 16:
				case 17:
				case 18:
				case 19:
					fill = 3;
					break;
				case 20:
				case 21:
				case 22:
				case 23:
					fill = 2;
					break;
				case 24:
				case 25:	
				case 26:
				case 27:
					fill = 1;
					break;
				default:
					fill = 0;
					break;
				}
				for( j = 0; j< fill; j++) {
					if(integer) {
                                        	((int*)data)[rindex] = DEFAULT_MISSING_INT;

	                                } else {
                                        	((float*)data)[rindex] = DEFAULT_MISSING_FLOAT;
					}
					rindex++;
				}
				for(j = 0; j < 36 - fill * 2;	j++) {
					X = UnsignedCnvtToDecimal(4,&(bds[i]));
					X = X << bboff;
					X = X >> (isize - number_of_bits);
					if(integer) {
						((int*)data)[rindex] = (int)(reference_value + (X * pow(2.0,(double)binary_scale_factor)))/pow(10.0,(double)(decimal_scale_factor));
	
					} else {
						((float*)data)[rindex] = (float)(reference_value + (X * pow(2.0,(double)binary_scale_factor)))/pow(10.0,(double)(decimal_scale_factor));
					}
					tbits += number_of_bits;
					i = (int)(tbits/8.0) + 11;
					bboff = tbits % 8;
					count++;
					rindex++;
				}
				for( j = 0; j< fill; j++) {
					if(integer) {
                                        	((int*)data)[rindex] = DEFAULT_MISSING_INT;

	                                } else {
                                        	((float*)data)[rindex] = DEFAULT_MISSING_FLOAT;
					}
					rindex++;
				}
			}
			*outdat = data;
		} else {
			total = thevarrec->var_info.dim_sizes[thevarrec->var_info.num_dimensions-1] * thevarrec->var_info.dim_sizes[thevarrec->var_info.num_dimensions-2];
			if(integer) {
				if(*outdat == NULL) {
					data = (void*)NclMalloc((unsigned)sizeof(int)*total);
				} else {
					data = *outdat;
				}
				for(i = 0; i < total; i++) {
					((int*)data)[i]= (int)reference_value;
				}
			} else {
				if(*outdat == NULL) {
					data = (void*)NclMalloc((unsigned)sizeof(float)*total);
				} else {
					data = *outdat;
				}
				for(i = 0; i < total; i++) {
					((float*)data)[i]= reference_value;
				}
			}
			*outdat = data;
		}
	} else if((second_order)&&(spherical_harm)) {

		if(spherical_harm)
			NhlPError(NhlWARNING,NhlEUNKNOWN,"GribUnPack : Spherical Harmonics Detected can't un pack\n");
		if(second_order)
			NhlPError(NhlWARNING,NhlEUNKNOWN,"GribUnPack : Second Order Detected can't un pack\n");
		*outdat = NULL;
		*missing_value = NULL;
	}
	NclFree(bds);
	return(integer);
}

static int IFOSUnPack
#if NhlNeedProto
(int fd, void** outdat, void** missing_value, GribRecordInqRec *therec, GribParamList* thevarrec)
#else
(fd, outdat, missing_value, therec, thevarrec)
int fd;
void** outdat;
void** missing_value;
GribRecordInqRec *therec;
GribParamList* thevarrec;
#endif
{
	int index =0,i=0;
	int integer = 0;
	int spherical_harm = 0;
	int second_order = 0;
	int additional_flags = 0;
	int sign;
	unsigned char tmp[4];
	int number_of_bits;
	int binary_scale_factor;
	int decimal_scale_factor;
	int unused_bits;
	float reference_value;
	float tmpb,tmpa;
	unsigned char *bds;
	int total = 0;
	ng_size_t grid_size = 0;
	void *data = NULL;
	int isize = sizeof(int)*8;
	unsigned int X;
	int tbits;
	int bboff;
	int npole =0;
	int polefirst = 0;
	unsigned char *bms = NULL;
	int numeric;
	int gpoint = 0;
	int dnum= 0;
	ng_size_t total_gpoints=0;


	bds = (unsigned char*)NclMalloc((unsigned)therec->bds_size + 4);
	lseek(fd,therec->offset + therec->bds_off,SEEK_SET);
	read(fd,(void*)bds,therec->bds_size);
	bds[therec->bds_size] = (char)0;
	bds[therec->bds_size +1] = (char)0;
	bds[therec->bds_size + 2] = (char)0;
	bds[therec->bds_size + 3] = (char)0;

	if(therec->has_bms) {
		bms = (unsigned char*)NclMalloc((unsigned)therec->bms_size);
		lseek(fd,therec->offset + therec->bms_off,SEEK_SET);
		read(fd,(void*)bms,therec->bms_size);
		numeric = CnvtToDecimal(2,&(bms[4]));
		if(numeric != 0) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"GribUnPack: Record uses predefined bit map. Predefined bit maps are not supported yet");
			NclFree(bms);
			bms = NULL;
		}
	}

	spherical_harm = (int)(bds[3] & (char)0200) ? 1 : 0;
	second_order = (int)(bds[3] & (char)0100) ? 1 : 0;
	integer = (int)(bds[3] & (char)0040) ? 1 : 0;
	additional_flags = (bds[3] & (char)0020) ? 1 : 0;


	if(therec->version != 0) {
		tmp[0] = (therec->pds[26] & (char)0177);
		tmp[1] = therec->pds[27];
		decimal_scale_factor  = CnvtToDecimal(2,tmp);
		if(therec->pds[26] & (char)0200) 
			decimal_scale_factor = -decimal_scale_factor;
	} else {
		decimal_scale_factor = 0;
	}

	number_of_bits = (int)bds[10];
	tmp[0] = (char)(bds[3] & (char)0017);
	unused_bits = CnvtToDecimal(1,tmp);
	tmp[0] = (char)(bds[4] & (char)0177);
	tmp[1] = bds[5];
	binary_scale_factor = CnvtToDecimal(2,tmp);
	if(bds[4] & (char)0200) {
		binary_scale_factor = -binary_scale_factor;
	}
	sign  = (bds[6] & (char) 0200)? 1 : 0;
	tmpa = (float)(bds[6] & (char)0177);
	tmpb = (float)CnvtToDecimal(3,&(bds[7]));

	reference_value = tmpb;
	reference_value *= (float)pow(2.0,-24.0);
	reference_value *= (float)pow(16.0,(double)(tmpa - 64));
	if(sign) {
		reference_value = -reference_value;
	}
	grid_size = thevarrec->var_info.dim_sizes[thevarrec->var_info.num_dimensions-1] * thevarrec->var_info.dim_sizes[thevarrec->var_info.num_dimensions-2];

	if((!spherical_harm)&&(!second_order)&&(!additional_flags)) {
		if(integer) {
			*missing_value= (void*)NclMalloc((unsigned)sizeof(int));
			*(int*)(*missing_value) = DEFAULT_MISSING_INT;
		} else {
			*missing_value= (void*)NclMalloc((unsigned)sizeof(float));
			*(float*)(*missing_value) = DEFAULT_MISSING_FLOAT;
		}
		if(number_of_bits != 0) {
			i = 11;
			bboff = 0;
			index = 0;
			tbits = 0;
			total = (int)(((therec->bds_size - 11) * 8 - unused_bits)/ number_of_bits);
			switch(therec->grid_number) {
			case 23:
			case 24:
				polefirst = 1;
				npole = 37;
				break;
			case 26:
				polefirst = 1;
				npole = 72;
				break;
			case 63:
			case 64:
				polefirst = 1;
				npole = 91;
				break;
			case 21:
			case 22:
				polefirst = 0;
				npole = 37;
				break;	
			case 25:
				polefirst = 0;	
				npole = 72;
				break;
			case 61:
			case 62:
				polefirst = 0;
				npole = 91;
				break;
			case 50:
				polefirst = 0;
				npole = 0;
				break;
				
			}
			total_gpoints = grid_size - (npole - 1);
			if(integer) {
				if(*outdat == NULL) {
					data = (void*)NclMalloc((unsigned)sizeof(int)*grid_size );
					*outdat = data;
				} else {
					data = *outdat;
				}
			} else {
				if(*outdat == NULL) {
					data = (void*)NclMalloc((unsigned)sizeof(float)*grid_size );
					*outdat = data;
				} else {
					data = *outdat;
				}
			}
			gpoint= 0;
			if((polefirst)&&(npole >0)) {
				X = UnsignedCnvtToDecimal(4,&(bds[i]));
				X = X << bboff;
				X = X >> (isize - number_of_bits);
				if(is_gpoint(bms,gpoint)) {
					if(integer) {
						((int*)data)[0] = (int)(reference_value + (X * pow(2.0,(double)binary_scale_factor)))/pow(10.0,(double)(decimal_scale_factor));
						gpoint++;
						dnum++;
					} else {
						((float*)data)[0] = (float)(reference_value + (X * pow(2.0,(double)binary_scale_factor)))/pow(10.0,(double)(decimal_scale_factor));
						gpoint++;
						dnum++;
					}
					tbits += number_of_bits;
					i = (int)(tbits/8.0) + 11;
					bboff = tbits % 8;
				} else {
					if(integer) {
						((int*)data)[index] = DEFAULT_MISSING_INT;
						gpoint++;

					} else {
						((float*)data)[index] = DEFAULT_MISSING_FLOAT;
						gpoint++;
					}
				}
				for(index = 1; index < npole; index++) {
					if(integer) {
						((int*)data)[index] = ((int*)data)[0];
					} else {
						((float*)data)[index] = ((float*)data)[0];
					}
				}
			}
			while((index < grid_size)&&(gpoint < total_gpoints)&&(dnum < total)) {
				if(is_gpoint(bms,gpoint)) {
					X = UnsignedCnvtToDecimal(4,&(bds[i]));
					X = X << bboff;
					X = X >> (isize - number_of_bits);
	
					if(integer) {
						((int*)data)[index] = (int)(reference_value + (X * pow(2.0,(double)binary_scale_factor)))/pow(10.0,(double)(decimal_scale_factor));
						gpoint++;
						dnum++;
						index++;
					} else {
						((float*)data)[index] = (float)(reference_value + (X * pow(2.0,(double)binary_scale_factor)))/pow(10.0,(double)(decimal_scale_factor));
						gpoint++;
						dnum++;
						index++;
					}
					tbits += number_of_bits;
					i = (int)(tbits/8.0) + 11;
					bboff = tbits % 8;
				} else {
					if(integer) {
						((int*)data)[index] = DEFAULT_MISSING_INT;
						index++;
						gpoint++;

					} else {
						((float*)data)[index] = DEFAULT_MISSING_FLOAT;
						index++;
						gpoint++;
					}
				}
			}
			if(!(polefirst)&&(npole > 0)) {
				for( ; index < grid_size;index++) {
					if(integer) {
                                                ((int*)data)[index] = ((int*)data)[index -1];
                                        } else {
                                                ((float*)data)[index] = ((float*)data)[index -1];
                                        }
				}
			}
			else if(index < grid_size) {
				if(integer) {
					for(;index<grid_size;index++) 
                                              ((int*)data)[index] = DEFAULT_MISSING_INT;
                                } else {
					for(;index<grid_size;index++) 
                                	        ((float*)data)[index] = DEFAULT_MISSING_FLOAT;
                                }
			}
		} else {
			total = thevarrec->var_info.dim_sizes[thevarrec->var_info.num_dimensions-1] * thevarrec->var_info.dim_sizes[thevarrec->var_info.num_dimensions-2];
			if(integer) {
				if(*outdat == NULL) {
					data = (void*)NclMalloc((unsigned)sizeof(int)*total);
				} else {
					data = *outdat;
				}
				for(i = 0; i < total; i++) {
					((int*)data)[i]= (int)reference_value;
				}
			} else {
				if(*outdat == NULL) {
					data = (void*)NclMalloc((unsigned)sizeof(float)*total);
				} else {
					data = *outdat;
				}
				for(i = 0; i < total; i++) {
					((float*)data)[i]= reference_value;
				}
			}
			*outdat = data;
		}
	} else {
		if(spherical_harm)
			NhlPError(NhlWARNING,NhlEUNKNOWN,"GribUnPack : Spherical Harmonics Detected can't un pack\n");
		if(second_order)
			NhlPError(NhlWARNING,NhlEUNKNOWN,"GribUnPack : Second Order Detected can't un pack\n");
		*outdat = NULL;
		*missing_value = NULL;
	}
	NclFree(bms);
	NclFree(bds);
	return(integer);
}

void Do_Rotation_Atts
(
	NrmQuark grid_name,
	GribAttInqRecList** rot_att_list,
	int* nrotatts,
	NhlBoolean grid_oriented
)
{
	NclQuark* tmp_string;
	
	char *note1[2] = {"u and v components of vector quantities are resolved relative to grid",
			  "u and v components of vector quantities are resolved relative to earth"};
	char *note2[2] = {"apply formulas to derive u and v components relative to earth",
			  "apply formulas to derive u and v components relative to grid"};
	char *formula_u[2] = {"Uearth = sin(rot)*Vgrid + cos(rot)*Ugrid",
			      "Ugrid = cos(rot)*Uearth - sin(rot)*Vearth"};
	char *formula_v[2] = {"Vearth = cos(rot)*Vgrid - sin(rot)*Ugrid",
			      "Vgrid = sin(rot)*Uearth + cos(rot)*Vearth"};
			  

	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark(grid_oriented ? note2[0] : note2[1]);
	GribPushAtt(rot_att_list,"note2",tmp_string,1,nclTypestringClass); (*nrotatts)++;

	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark(grid_oriented ? note1[0] : note1[1]);
	GribPushAtt(rot_att_list,"note1",tmp_string,1,nclTypestringClass); (*nrotatts)++;

	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark(grid_oriented ? formula_v[0] : formula_v[1]);
	GribPushAtt(rot_att_list,"formula_v",tmp_string,1,nclTypestringClass); (*nrotatts)++;

	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark(grid_oriented ? formula_u[0] : formula_u[1]);
	GribPushAtt(rot_att_list,"formula_u",tmp_string,1,nclTypestringClass); (*nrotatts)++;

	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark("radians");
	GribPushAtt(rot_att_list,"units",tmp_string,1,nclTypestringClass); (*nrotatts)++;

	if (grid_name > NrmNULLQUARK) {
		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string = grid_name;
		GribPushAtt(rot_att_list,"GridType",tmp_string,1,nclTypestringClass); (*nrotatts)++;
	}

	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark("vector rotation angle");
	GribPushAtt(rot_att_list,"long_name",tmp_string,1,nclTypestringClass); (*nrotatts)++;
}

void GdsMEGrid
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat,
	ng_size_t** dimsizes_lat,
	float** lon,
	int* n_dims_lon,
	ng_size_t** dimsizes_lon,
	float** rot,
	int* n_dims_rot,
	ng_size_t **dimsizes_rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon,
 rot, n_dims_rot, dimsizes_rot,
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
int* n_dims_rot;
ng_size_t **dimsizes_rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	unsigned char *gds;
	int la1;
	int lo1;
	int la2;
	int lo2;
	int latin;
	float di,dj;
	int idir, jdir;
	unsigned char tmp[4];
	int sign;
	float *tmp_float;
	NclQuark* tmp_string;
	int ni,nj;
	double udx,udy;
	double dumx,dumy;
	int i,j;
	double lon1,lon0,lat1,lat0;
	double nx0,nx1,ny0,ny1;
	double tmplon,tmplat;
	double latin1;
	double starty;
	
	*lat = NULL;
	*n_dims_lat = 0;
	*dimsizes_lat = NULL;
	*lon = NULL;
	*n_dims_lon= 0;
	*dimsizes_lon= NULL;
	*rot = NULL;
	*n_dims_rot= 0;
	*dimsizes_rot= NULL;

	if((thevarrec->thelist == NULL)||(thevarrec->ref_rec == NULL)) 
		return;

	gds = thevarrec->ref_rec->gds;
	if(gds == NULL) {
		return;
	}

	ni = CnvtToDecimal(2,&(gds[6]));
	nj = CnvtToDecimal(2,&(gds[8]));

	sign = ((char)0200 & gds[10] )? -1 : 1;
	tmp[0] = (char)0177 & gds[10];
	tmp[1] = gds[11];
	tmp[2] = gds[12];
	la1 = sign * CnvtToDecimal(3,tmp);

	sign = ((char)0200 & gds[13] )? -1 : 1;
	tmp[0] = (char)0177 & gds[13];
	tmp[1] = gds[14];
	tmp[2] = gds[15];
	lo1 = sign * CnvtToDecimal(3,tmp);

	sign = ((char)0200 & gds[17] )? -1 : 1;
	tmp[0] = (char)0177 & gds[17];
	tmp[1] = gds[18];
	tmp[2] = gds[19];
	la2 = sign * CnvtToDecimal(3,tmp);

	sign = ((char)0200 & gds[20] )? -1 : 1;
	tmp[0] = (char)0177 & gds[20];
	tmp[1] = gds[21];
	tmp[2] = gds[22];
	lo2 = sign * CnvtToDecimal(3,tmp);

	sign = ((char)0200 & gds[23] )? -1 : 1;
	tmp[0] = (char)0177 & gds[23];
	tmp[1] = gds[24];
	tmp[2] = gds[25];
	latin = sign * CnvtToDecimal(3,tmp);

	tmp[0] = gds[28];
	tmp[1] = gds[29];
	tmp[2] = gds[30];
	di = sign * CnvtToDecimal(3,tmp);

	tmp[0] = gds[31];
	tmp[1] = gds[32];
	tmp[2] = gds[33];
	dj = sign * CnvtToDecimal(3,tmp);
	idir = ((unsigned char)0200 & (unsigned char)gds[27])?-1:1;
	jdir = ((unsigned char)0100 & (unsigned char)gds[27])?1:-1;

#if 0

	kgds[0] = 1;
	kgds[1] = ni;
	kgds[2] = nj;
	kgds[3] = la1;
	kgds[4] = lo1;
	kgds[5] = UnsignedCnvtToDecimal(1,&(gds[16]));
	kgds[6] = la2;
	kgds[7] = lo2;
	kgds[8] = latin;
	kgds[10] = UnsignedCnvtToDecimal(1,&(gds[27]));
	kgds[11] = di;
	kgds[12] = dj;

	iopt = 1;
	lrot = 0;

	npts = ni;
	tlon = (float*)NclMalloc((unsigned)sizeof(float)* ni);
	tlat = (float*)NclMalloc((unsigned)sizeof(float)* ni);
	for (i = 0; i < ni; i++) {
		tlon[i] = i + 1;
		tlat[i] = nj / 2;
	}
	
	NGCALLF(gdswiz,GDSWIZ)(kgds,&iopt,&npts,&fillval,tlon,tlat,tlon,tlat,&nret,&lrot,*rot,*rot);
	NhlFree(tlat);
	if (nret != ni) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  "GdsMEGrid: Error computing Mercator longitudes");
		NhlFree(tlon);
		return;
	}
	for (i = 0; i < ni; i++) {
		tlon[i] = (tlon[i] > 180) ? tlon[i] - 360.0 : tlon[i];
	}
	*lon = tlon;

	npts = nj;
	tlon = (float*)NclMalloc((unsigned)sizeof(float)* nj);
	tlat = (float*)NclMalloc((unsigned)sizeof(float)* nj);
	for (i = 0; i < nj; i++) {
		tlat[i] = i + 1;
		tlon[i] = ni / 2;
	}
	NGCALLF(gdswiz,GDSWIZ)(kgds,&iopt,&npts,&fillval,tlon,tlat,tlon,tlat,&nret,&lrot,*rot,*rot);
	NhlFree(tlon);
	if (nret != nj) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  "GdsMEGrid: Error computing Mercator longitudes");
		NhlFree(tlat);
		NhlFree(*lon);
		*lon = NULL;
		return;
	}

/*
 * this is the code that was used in the GRIB2 version of this routine. It is basically a translation of the NCEP code
 * and seems to have the same error

	{
		double lon1,lon0,lat1,lat0;
		double tlo1,tlo2;
		double earth_radius;
		double latd;
		double dx,dy;
		double dlon, dlat;
		double ye;
		double RadPerDeg = dtor;
		double DegPerRad = rtod;

		lon0 = lo1 / 1000.0;
		lon1 = lo2 / 1000.0;
		lat0 = la1 / 1000.0;
		lat1 = la2 / 1000.0;
		dy = dj / 1000.0;
		dx = di / 1000.0;
		latd =  latin/1000.0;

		earth_radius = EAR;

		tlo1 = lon0;
		tlo2 = lon1;
		if (idir == 1) {
			if (tlo2 < tlo1) {
				tlo1 -= 360.0;
			}
		}
		else {
			if (tlo2 > tlo1) {
				tlo2 -= 360.0;
			}
		}
		dlon = (tlo2 - tlo1) / (double) (ni - 1);
		dlat = jdir * dy / (earth_radius * cos(latd * RadPerDeg));
		ye = 1 - log(tan(((lat0 + 90.0)/ 2.0) * RadPerDeg)) / dlat;
    			
		for (i = 0; i < *(*dimsizes_lon) ; i++) {
			double tlon = (float)(lon0 + idir * i * dlon);
			(*lon)[i] = tlon;
		}

		for (i = 0; i < *(*dimsizes_lat) ; i++) {
			double tlat = 2 * atan(exp(dlat * (i + 1 - ye))) * DegPerRad - 90.0;
			(*lat)[i] = tlat;
		}

	}	
*/
#endif

	*n_dims_lon = 1;
	*dimsizes_lon = (ng_size_t*)NclMalloc(sizeof(ng_size_t));
	*(*dimsizes_lon) = ni;
	*dimsizes_lat = (ng_size_t*)NclMalloc(sizeof(ng_size_t));
	*n_dims_lat = 1;
	*(*dimsizes_lat) = nj;
        *lon = (float*)NclMalloc((unsigned)sizeof(float)* ni);
        *lat = (float*)NclMalloc((unsigned)sizeof(float)* nj);


	lon0 = lo1 / 1000.0;
	lon1 = lo2 / 1000.0;
	lat0 = la1 / 1000.0;
        lat1 = la2 / 1000.0;
	latin1 =  latin/1000.0;

	_NclInitMapTrans("ME",0,idir * (lon1 - lon0)/2.0,0.0);

	tmplon = (lon1-lon0) / 2.0;
	tmplat = (lat1 - lat0) / 2.0;
	NGCALLF(mdptrn,MDPTRN)(&tmplat,&tmplon,&dumx,&dumy);
	NGCALLF(mdptrn,MDPTRN)(&lat0,&lon0,&nx0,&ny0);
	NGCALLF(mdptrn,MDPTRN)(&lat1,&lon1,&nx1,&ny1);
        udx = fabs(nx1 - nx0) / (ni -1);
	udy = fabs(ny1 - ny0) / (nj-1);
	starty = jdir == 1 ? MIN(ny0,ny1) : MAX(ny0,ny1);

	for(i = 0; i < nj; i++) {
		double uy = starty + i * udy * jdir;
		NGCALLF(mdptri,MDPTRI)(&dumx,&uy,&tmplat,&tmplon);
		(*lat)[i] = (float) tmplat;
	}
	for(j = 0; j < ni; j++) {
		double ux = nx0 + j * udx * idir;
		NGCALLF(mdptri,MDPTRI)(&ux,&dumy,&tmplat,&tmplon);
		(*lon)[j] = (float) tmplon;
	}
	for(j = 0; j < ni; j++) {
		(*lon)[j] = ((*lon)[j] < 0)? ((*lon)[j] + 360) : (*lon)[j];
	}

	if(lon_att_list != NULL) {
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = la1/1000.0;
		GribPushAtt(lon_att_list,"La1",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = lo1/1000.0;
		GribPushAtt(lon_att_list,"Lo1",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = la2/1000.0;
		GribPushAtt(lon_att_list,"La2",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = lo2/1000.0;
		GribPushAtt(lon_att_list,"Lo2",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = latin/1000.0;
		GribPushAtt(lon_att_list,"Latin",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = di/1000.0;
		GribPushAtt(lon_att_list,"Di",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = dj/1000.0;
		GribPushAtt(lon_att_list,"Dj",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string = NrmStringToQuark("degrees_east");
		GribPushAtt(lon_att_list,"units",tmp_string,1,nclTypestringClass); (*nlonatts)++;
		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string = NrmStringToQuark("Mercator Projection Grid");
		GribPushAtt(lon_att_list,"GridType",tmp_string,1,nclTypestringClass); (*nlonatts)++;
		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string = NrmStringToQuark("longitude");
		GribPushAtt(lon_att_list,"long_name",tmp_string,1,nclTypestringClass); (*nlonatts)++;
	}
	if(lat_att_list != NULL) {
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = la1/1000.0;
		GribPushAtt(lat_att_list,"La1",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = lo1/1000.0;
		GribPushAtt(lat_att_list,"Lo1",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = la2/1000.0;
		GribPushAtt(lat_att_list,"La2",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = lo2/1000.0;
		GribPushAtt(lat_att_list,"Lo2",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = latin/1000.0;
		GribPushAtt(lat_att_list,"Latin",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = di/1000.0;
		GribPushAtt(lat_att_list,"Di",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = dj/1000.0;
		GribPushAtt(lat_att_list,"Dj",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string = NrmStringToQuark("degrees_north");
		GribPushAtt(lat_att_list,"units",tmp_string,1,nclTypestringClass); (*nlatatts)++;
		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string = NrmStringToQuark("Mercator Projection Grid");
		GribPushAtt(lat_att_list,"GridType",tmp_string,1,nclTypestringClass); (*nlatatts)++;
		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string = NrmStringToQuark("latitude");
		GribPushAtt(lat_att_list,"long_name",tmp_string,1,nclTypestringClass); (*nlatatts)++;
	}
	return;

}
void GdsGNGrid
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat,
	ng_size_t** dimsizes_lat,
	float** lon,
	int* n_dims_lon,
	ng_size_t** dimsizes_lon,
	float** rot,
	int* n_dims_rot,
	ng_size_t **dimsizes_rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon,
 rot, n_dims_rot, dimsizes_rot,
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
int* n_dims_rot;
ng_size_t **dimsizes_rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
			*lat = NULL;
			*n_dims_lat = 0;
			*dimsizes_lat = NULL;
			*lon = NULL;
			*n_dims_lon= 0;
			*dimsizes_lon= NULL;
}
void GdsLEGrid
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat,
	ng_size_t** dimsizes_lat,
	float** lon,
	int* n_dims_lon,
	ng_size_t** dimsizes_lon,
	float** rot,
	int* n_dims_rot,
	ng_size_t **dimsizes_rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon,
 rot, n_dims_rot, dimsizes_rot,
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
int* n_dims_rot;
ng_size_t **dimsizes_rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int nx;
	int ny;
	double la1;
	double lo1;
	double lov;
	double dx;
	double dy;
	double latin1;
	double latin2;
	int idir,jdir;
	unsigned char tmpc[4];
	unsigned char *gds;
	float *tmp_float;
	NclQuark *tmp_string;
	int do_rot = 1;
	NhlBoolean grid_oriented = False;
	NrmQuark grid_name = NrmNULLQUARK;

	*lat = NULL;
	*n_dims_lat = 0;
	*dimsizes_lat = NULL;
	*lon = NULL;
	*n_dims_lon= 0;
	*dimsizes_lon= NULL;
	*rot = NULL;
	*dimsizes_rot = NULL;
	*n_dims_rot = 0;
	if((thevarrec->thelist == NULL)||(thevarrec->ref_rec == NULL)) 
		return;

	gds = (unsigned char*)thevarrec->ref_rec->gds;

	nx = UnsignedCnvtToDecimal(2,&(gds[6]));
	ny = UnsignedCnvtToDecimal(2,&(gds[8]));
	tmpc[0] = gds[10] & (unsigned char) 0177;
	tmpc[1] = gds[11];
	tmpc[2] = gds[12];
	la1 = (double)(UnsignedCnvtToDecimal(3,tmpc))/1000.0;
	la1 = ((gds[10] & (unsigned char) 0200)? -la1:la1);

	tmpc[0] = gds[13] & (unsigned char) 0177;
	tmpc[1] = gds[14];
	tmpc[2] = gds[15];
	lo1 = (double)(UnsignedCnvtToDecimal(3,tmpc))/1000.0;
	lo1 = ((gds[13] & (unsigned char) 0200)? -lo1:lo1);

	tmpc[0] = gds[17] & (unsigned char) 0177;
	tmpc[1] = gds[18];
	tmpc[2] = gds[19];
	lov = (double)(UnsignedCnvtToDecimal(3,tmpc))/1000.0;
	lov = ((gds[17] & (unsigned char) 0200)? -lov:lov);

	dx = (double)UnsignedCnvtToDecimal(3,&(gds[20])) / 1000;
	dy = (double)UnsignedCnvtToDecimal(3,&(gds[23])) / 1000;
	tmpc[0] = gds[28] & (unsigned char) 0177;
	tmpc[1] = gds[29];
	tmpc[2] = gds[30];
	latin1 = (double)UnsignedCnvtToDecimal(3,tmpc)/1000.0;
	latin1 = ((gds[28] & (unsigned char) 0200)? -latin1:latin1);

	tmpc[0] = gds[31] & (unsigned char) 0177;
	tmpc[1] = gds[32];
	tmpc[2] = gds[33];
	latin2 = (double)UnsignedCnvtToDecimal(3,tmpc)/1000.0;
	latin2 = ((gds[28] & (unsigned char) 0200)? -latin2:latin2);
	idir = ((unsigned char)0200 & (unsigned char)gds[27])?-1:1;
	jdir = ((unsigned char)0100 & (unsigned char)gds[27])?1:-1;

	GenLambert(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, rot,
		   latin1, latin2, lov, dx,dy,la1,lo1,nx,ny,idir,jdir);

	if(lon_att_list != NULL) {
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = la1;
		GribPushAtt(lon_att_list,"La1",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = lo1;
		GribPushAtt(lon_att_list,"Lo1",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = lov;
		GribPushAtt(lon_att_list,"Lov",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = dx * 1000.0;
		GribPushAtt(lon_att_list,"Dx",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = dy * 1000.0;
		GribPushAtt(lon_att_list,"Dy",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = latin1;
		GribPushAtt(lon_att_list,"Latin1",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = latin2;
		GribPushAtt(lon_att_list,"Latin2",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string = NrmStringToQuark("degrees_east");
		GribPushAtt(lon_att_list,"units",tmp_string,1,nclTypestringClass); (*nlonatts)++;
		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string = NrmStringToQuark("Lambert Conformal Secant or Tangent, Conical or bipolar");
		GribPushAtt(lon_att_list,"GridType",tmp_string,1,nclTypestringClass); (*nlonatts)++;
		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string = NrmStringToQuark("longitude");
		GribPushAtt(lon_att_list,"long_name",tmp_string,1,nclTypestringClass); (*nlonatts)++;
	}
	if(lat_att_list != NULL) {
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = la1;
		GribPushAtt(lat_att_list,"La1",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = lo1;
		GribPushAtt(lat_att_list,"Lo1",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = lov;
		GribPushAtt(lat_att_list,"Lov",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = dx * 1000.0;
		GribPushAtt(lat_att_list,"Dx",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = dy * 1000.0;
		GribPushAtt(lat_att_list,"Dy",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = latin1;
		GribPushAtt(lat_att_list,"Latin1",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = latin2;
		GribPushAtt(lat_att_list,"Latin2",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string = NrmStringToQuark("degrees_north");
		GribPushAtt(lat_att_list,"units",tmp_string,1,nclTypestringClass); (*nlatatts)++;
		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string = NrmStringToQuark("Lambert Conformal Secant or Tangent, Conical or bipolar");
		grid_name = *tmp_string;
		GribPushAtt(lat_att_list,"GridType",tmp_string,1,nclTypestringClass); (*nlatatts)++;
		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string = NrmStringToQuark("latitude");
		GribPushAtt(lat_att_list,"long_name",tmp_string,1,nclTypestringClass); (*nlatatts)++;
	}
	if(do_rot && rot_att_list != NULL) {
		Do_Rotation_Atts(grid_name,rot_att_list,nrotatts,grid_oriented);
	}

}

void GdsGAGrid
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat,
	ng_size_t** dimsizes_lat,
	float** lon,
	int* n_dims_lon,
	ng_size_t** dimsizes_lon,
	float** rot,
	int* n_dims_rot,
	ng_size_t **dimsizes_rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon,
 rot, n_dims_rot, dimsizes_rot,
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
int* n_dims_rot;
ng_size_t **dimsizes_rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int nlat,nlon;
	unsigned char tmpc[4];
	double *theta;
	double *wts;
	int lwork= 0;
	double *work = NULL;
	int i,ierror,k;
	int ila1;
	int ila2;
	int ilo1;
	int ilo2;
        int start, end;
	double loinc;
	float *tmp_float;
	NclQuark *tmp_string;
	int is_thinned_lon = 0;
	int idir;

	if((thevarrec->thelist != NULL)&&(thevarrec->ref_rec != NULL)) {

		/* we may need the number of latitudes if the grid is thinned in longitude */
		*n_dims_lat = 1;
		*dimsizes_lat = malloc(sizeof(ng_size_t));
		(*dimsizes_lat)[0] = (int)UnsignedCnvtToDecimal(2,&(thevarrec->ref_rec->gds[8]));

		/* do longitude first because the information may be needed for calculating latitude correctly */

		tmpc[0] = thevarrec->ref_rec->gds[13] & (char)0177;
		tmpc[1] = thevarrec->ref_rec->gds[14];
		tmpc[2] = thevarrec->ref_rec->gds[15];
		ilo1 = ((thevarrec->ref_rec->gds[13] & (char)0200) ? -1:1)*(int)UnsignedCnvtToDecimal(3,tmpc);
		tmpc[0] = thevarrec->ref_rec->gds[20] & (char)0177;
		tmpc[1] = thevarrec->ref_rec->gds[21];
		tmpc[2] = thevarrec->ref_rec->gds[22];
		ilo2 = ((thevarrec->ref_rec->gds[20] & (char)0200) ? -1:1)*(int)UnsignedCnvtToDecimal(3,tmpc);
		
		*n_dims_lon = 1;
		*dimsizes_lon = malloc(sizeof(ng_size_t));
		nlon = CnvtToDecimal(2,&thevarrec->ref_rec->gds[6]);
		idir = ((char)0200 & thevarrec->ref_rec->gds[27]) ? -1 : 1;

		if (nlon == 0xffff) {
			is_thinned_lon = 1;
			GetThinnedLonParams(thevarrec->ref_rec->gds,
					    (*dimsizes_lat)[0],ilo1,ilo2,idir,&nlon,&loinc);
		} else if (nlon == 1) {
			loinc = ((double)CnvtToDecimal(2,&((thevarrec->ref_rec->gds)[23])));
		}
		else {
			if (idir == 1) {
				int ti = ilo2;
				while (ti < ilo1) {
					ti += 360000;
				}
				loinc = (ti - ilo1) / (double) (nlon - 1);
				ilo2 = ti;
			}
			else {
				int ti = ilo1;
				while (ti < ilo2) {
					ti += 360000;
				}
				loinc = (ti - ilo2) / (double) (nlon - 1);
				ilo1 = ti;
			}

		}
		(*dimsizes_lon)[0] = nlon;
		*lon = malloc(sizeof(float)*nlon);
		for(i = 0; i < (*dimsizes_lon)[0]; i++) {
			(*lon)[i] = (ilo1 + i*idir*loinc)/1000.0;
		}


		
		nlat = 2 * UnsignedCnvtToDecimal(2,&(thevarrec->ref_rec->gds[25]));
		
		/* 
		 * this is a hack for certain IPCC data that does not have the correct info in gds[25+]. 
		 * nlat can legitimately be larger than the lat dimension size if a non-global grid is being constructed, but it would not make sense for it to be orders of magnitude larger
		 */

		if (nlat > 5 * (*dimsizes_lat)[0]) {
			nlat = (*dimsizes_lat)[0];
 			NhlPError(NhlWARNING,NhlEUNKNOWN,
			"GdsGAGrid: Invalid value for Gaussian LatLon grid in GDS octets 26-27; inferring N from octets 9-10 (See GRIB Section 2 documentation)");
		}
/*
 * These come out south to north
 */

		tmpc[0] = thevarrec->ref_rec->gds[10] & (char)0177;
		tmpc[1] = thevarrec->ref_rec->gds[11];
		tmpc[2] = thevarrec->ref_rec->gds[12];
		ila1 = ((thevarrec->ref_rec->gds[10] & (char)0200) ? -1:1)*(int)UnsignedCnvtToDecimal(3,tmpc);
		tmpc[0] = thevarrec->ref_rec->gds[17] & (char)0177;
		tmpc[1] = thevarrec->ref_rec->gds[18];
		tmpc[2] = thevarrec->ref_rec->gds[19];
		ila2 = ((thevarrec->ref_rec->gds[17] & (char)0200) ? -1:1)*(int)UnsignedCnvtToDecimal(3,tmpc);

		if (nlat == (*dimsizes_lat)[0] && (fabs (ila1 / 1000.0) < 85.0 || fabs (ila1 / 1000.0) < 85.0)) {
			/* not a global grid but the value of N must have been set as if it were ;
			   therefore try to infer N from the longitude increment (this should work even when the longitude is not global)
			*/
			int global_nlon = (int) (360000 / loinc + 0.5);
			nlat = global_nlon / 2;
		}
			
		theta = (double*)NclMalloc(sizeof(double)*nlat);
		wts = (double*)NclMalloc(sizeof(double)*nlat);
		lwork = 4 * nlat*(nlat+1)+2;
		work = (double*)NclMalloc(sizeof(double)*lwork);
		*lat = (float*)NclMalloc(sizeof(float)*nlat);

		NGCALLF(gaqdnio,GAQDNIO)(&nlat,theta,wts,work,&lwork,&ierror);

		if(!(thevarrec->ref_rec->gds[27] & (char)0100)) {
                        /* -j direction implies north to south*/
			int done = 0;
			int redo_nlat = 0;
			i = nlat - 1;
			/* 
			 * ila1 should be the start and ila2 should be the end but allow for the possibility that they
			 * are reversed
			 */
			if (ila1 < ila2) {
				start = ila2;
				end = ila1;
			} else {
				start = ila1;
				end = ila2;
			}
			while (! done) {
				if (start > (int)(rtod*theta[i] * 1000.0 + .5) - 90000) {
					if (nlat < (*dimsizes_lat)[0]) {
						redo_nlat = 1;
					}
					else {
						NhlPError(NhlWARNING,NhlEUNKNOWN,
							  "GdsGAGrid: GRIB attributes La1 and/or La2 are incorrectly out of range of the gaussian latitude array (See GRIB Section 2 documentation)");
					}
				}
				else {
					/* 
					 * nlat can legitimately be larger than the size of the lat array if the grid is not global 
					 */
					while(i >= 0) {
						if((start == (int)(rtod*theta[i] * 1000.0) - 90000)||(start == (int)(rtod*theta[i] * 1000.0 + .5) - 90000)) {
							break;
						} else {
							i--;
						}
					}
					if (i < (*dimsizes_lat)[0] - 1) { /* this is the only thing that is clearly going to generate some undefined values */
						if (nlat != *(dimsizes_lat)[0]) {
							redo_nlat = 1;
						}
						else {
							NhlPError(NhlWARNING,NhlEUNKNOWN,
								  "GdsGAGrid: Possible error generating Gaussian latitude coordinates: continuing anyway");
						}
					}
				}
				if (! redo_nlat) {
					done = 1;
				}
				else {
					NhlPError(NhlWARNING,NhlEUNKNOWN,
						  "GdsGAGrid: Invalid value for Gaussian LatLon grid in GDS octets 26-27; inferring N from octets 9-10 (See GRIB Section 2 documentation)");
					nlat = (*dimsizes_lat)[0];
					theta = (double*)NclRealloc(theta,sizeof(double)*nlat);
					wts = (double*)NclRealloc(wts,sizeof(double)*nlat);
					lwork = 4 * nlat*(nlat+1)+2;
					work = (double*)NclRealloc(work,sizeof(double)*lwork);
					*lat = (float*)NclRealloc(*lat,sizeof(float)*nlat);
					NGCALLF(gaqdnio,GAQDNIO)(&nlat,theta,wts,work,&lwork,&ierror);
					redo_nlat = 0;
					i = nlat - 1;
				}
			}
			k = 0;
			while((k<(*dimsizes_lat)[0])&&(i>=0)) {
				if((end == (int)(rtod*theta[i] * 1000.0) - 90000)||(end == (int)(rtod*theta[i] * 1000.0+.5) - 90000)) {
					break;
				} else {
					(*lat)[k++] = rtod*theta[i] - 90.0;
					i--;	
				}
			}
			if((i >=0)&&(k<(*dimsizes_lat)[0])) {
				(*lat)[k] = rtod*theta[i] - 90.0;
			}
	
		} else {
                        /* +j direction implies south to north*/
			int done = 0;
			int redo_nlat = 0;
			i = 0;
			/* 
			 * ila1 should be the start and ila2 should be the end but allow for the possibility that they
			 * are reversed
			 */
			if (ila2 < ila1) {
				start = ila2;
				end = ila1;
			} else {
				start = ila1;
				end = ila2;
			}
			while (! done) {
				if (start <  (int)(rtod*theta[i] * 1000.0) - 90000) {
					if (nlat < (*dimsizes_lat)[0]) {
						redo_nlat = 1;
					}
					else {
						NhlPError(NhlWARNING,NhlEUNKNOWN,
							  "GdsGAGrid: GRIB attributes La1 and/or La2 are incorrectly out of range of the gaussian latitude array (See GRIB Section 2 documentation)");
					}
				}
				else {					/* 
					 * nlat can legitimately be larger than the size of the lat array if the grid is not global 
					 */
					while(i < nlat) {
						if((start == (int)(rtod*theta[i] * 1000.0) - 90000)||(start == (int)(rtod*theta[i] * 1000.0 + .5) - 90000)) {
							break;
						} else {
							i++;
						}
					}
					if (nlat - i < (*dimsizes_lat)[0]) {
						if (nlat != *(dimsizes_lat)[0]) {
							redo_nlat = 1;
						}
						else {
							NhlPError(NhlWARNING,NhlEUNKNOWN,
								  "GdsGAGrid: Possible error generating Gaussian latitude coordinates: continuing anyway");
						}
					}
				}
				if (! redo_nlat) {
					done = 1;
				}
				else {
					NhlPError(NhlWARNING,NhlEUNKNOWN,
						  "GdsGAGrid: Invalid value for Gaussian LatLon grid in GDS octets 26-27; inferring N from octets 9-10 (See GRIB Section 2 documentation)");
					nlat = (*dimsizes_lat)[0];
					theta = (double*)NclRealloc(theta,sizeof(double)*nlat);
					wts = (double*)NclRealloc(wts,sizeof(double)*nlat);
					lwork = 4 * nlat*(nlat+1)+2;
					work = (double*)NclRealloc(work,sizeof(double)*lwork);
					*lat = (float*)NclRealloc(*lat,sizeof(float)*nlat);
					NGCALLF(gaqdnio,GAQDNIO)(&nlat,theta,wts,work,&lwork,&ierror);
					redo_nlat = 0;
					i = 0;
				}
			}
			k = 0;
			while((i<nlat)&&(k<(*dimsizes_lat)[0])) {
				if((end == (int)(rtod*theta[i] * 1000.0 + .5) - 90000)||(end == (int)(rtod*theta[i] * 1000.0) - 90000)) {
					break;
				} else {
					(*lat)[k++] = rtod*theta[i] - 90.0;
					i++;	
				}
			}
			if((i < nlat)&&(k<(*dimsizes_lat)[0])) {
				(*lat)[i] = rtod*theta[i] - 90.0;
			}

		}
		if (k < (*dimsizes_lat)[0] - 1) {
 			NhlPError(NhlWARNING,NhlEUNKNOWN,
			 "GdsGAGrid: Gaussian latitude coordinate array is partially or wholly undefined");
		}

		NclFree(work);
		NclFree(wts);
		NclFree(theta);

	} else {
		*lat = NULL;
		*n_dims_lat = 0;
		*dimsizes_lat = NULL;
		*lon = NULL;
		*n_dims_lon= 0;
		*dimsizes_lon= NULL;
		ila1 = ila2 = ilo1 = ilo2 = 0;
	}
	if(lon_att_list != NULL) {
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = ila1/1000.0;
		GribPushAtt(lon_att_list,"La1",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = ilo1/1000.0;
		GribPushAtt(lon_att_list,"Lo1",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = ila2/1000.0;
		GribPushAtt(lon_att_list,"La2",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = ilo2/1000.0;
		GribPushAtt(lon_att_list,"Lo2",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = loinc/1000.0;
		GribPushAtt(lon_att_list,"Di",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = nlat/2.0;
		GribPushAtt(lon_att_list,"N",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string = NrmStringToQuark("degrees_east");
		GribPushAtt(lon_att_list,"units",tmp_string,1,nclTypestringClass); (*nlonatts)++;
		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		if (is_thinned_lon) {
			*tmp_string = NrmStringToQuark("Gaussian Latitude/Longitude Grid (Quasi-Regular)");
		} else {
			*tmp_string = NrmStringToQuark("Gaussian Latitude/Longitude Grid");
		}
		GribPushAtt(lon_att_list,"GridType",tmp_string,1,nclTypestringClass); (*nlonatts)++;
		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string = NrmStringToQuark("longitude");
		GribPushAtt(lon_att_list,"long_name",tmp_string,1,nclTypestringClass); (*nlonatts)++;
	}
	if(lat_att_list != NULL) {
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = ila1/1000.0;
		GribPushAtt(lat_att_list,"La1",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = ilo1/1000.0;
		GribPushAtt(lat_att_list,"Lo1",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = ila2/1000.0;
		GribPushAtt(lat_att_list,"La2",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = ilo2/1000.0;
		GribPushAtt(lat_att_list,"Lo2",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = loinc/1000.0;
		GribPushAtt(lat_att_list,"Di",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = nlat/2.0;
		GribPushAtt(lat_att_list,"N",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string = NrmStringToQuark("degrees_north");
		GribPushAtt(lat_att_list,"units",tmp_string,1,nclTypestringClass); (*nlatatts)++;
		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		if (is_thinned_lon) {
			*tmp_string = NrmStringToQuark("Gaussian Latitude/Longitude Grid (Quasi-Regular)");
		} else {
			*tmp_string = NrmStringToQuark("Gaussian Latitude/Longitude Grid");
		}
		GribPushAtt(lat_att_list,"GridType",tmp_string,1,nclTypestringClass); (*nlatatts)++;
		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string = NrmStringToQuark("latitude");
		GribPushAtt(lat_att_list,"long_name",tmp_string,1,nclTypestringClass); (*nlatatts)++;
	}
}
void GdsSTGrid
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat,
	ng_size_t** dimsizes_lat,
	float** lon,
	int* n_dims_lon,
	ng_size_t** dimsizes_lon,
	float** rot,
	int* n_dims_rot,
	ng_size_t **dimsizes_rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon,
 rot, n_dims_rot, dimsizes_rot,
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
int* n_dims_rot;
ng_size_t **dimsizes_rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int nx;
	int ny;
	float la1;
	float lo1;
	float lov,tlon;
	float dx;
	float dy;
	float deltax;
	float deltay;
	float latin0;
	int north;
	unsigned char tmpc[4];
	int idir,jdir,i,j;
	unsigned char *gds;
	float nx0,nx1,ny0,ny1;
	float C,d_per_km,dlon;
	float *tmp_float;
	NclQuark* tmp_string;
	NhlBoolean do_rot;
	NhlBoolean grid_oriented;
	NrmQuark grid_name = NrmNULLQUARK;


	*lat = NULL;
	*n_dims_lat = 0;
	*dimsizes_lat = NULL;
	*lon = NULL;
	*n_dims_lon= 0;
	*dimsizes_lon= NULL;
	*rot = NULL;
	*dimsizes_rot = NULL;
	*n_dims_rot = 0;
	if((thevarrec->thelist == NULL)||(thevarrec->ref_rec == NULL)) 
		return;

	gds = (unsigned char*)thevarrec->ref_rec->gds;

	nx = UnsignedCnvtToDecimal(2,&(gds[6]));
	ny = UnsignedCnvtToDecimal(2,&(gds[8]));
	tmpc[0] = gds[10] & (unsigned char) 0177;
	tmpc[1] = gds[11];
	tmpc[2] = gds[12];
	la1 = (UnsignedCnvtToDecimal(3,tmpc))/1000.0;
	la1 = ((gds[10] & (unsigned char) 0200)? -la1:la1);

	tmpc[0] = gds[13] & (unsigned char) 0177;
	tmpc[1] = gds[14];
	tmpc[2] = gds[15];
	lo1 = (UnsignedCnvtToDecimal(3,tmpc))/1000.0;
	lo1 = ((gds[13] & (unsigned char) 0200)? -lo1:lo1);

	tmpc[0] = gds[17] & (unsigned char) 0177;
	tmpc[1] = gds[18];
	tmpc[2] = gds[19];
	lov = (UnsignedCnvtToDecimal(3,tmpc))/1000.0;
	lov = ((gds[17] & (unsigned char) 0200)? -lov:lov);

	dx = (float)UnsignedCnvtToDecimal(3,&(gds[20]));
	dy = (float)UnsignedCnvtToDecimal(3,&(gds[23]));
	tmpc[0] = gds[28] & (unsigned char) 0177;
	tmpc[1] = gds[29];
	tmpc[2] = gds[30];

        *dimsizes_lat = (ng_size_t*)NclMalloc(sizeof(ng_size_t) * 2);
        *dimsizes_lon = (ng_size_t*)NclMalloc(sizeof(ng_size_t) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ny;
        (*dimsizes_lat)[1] = nx;
        (*dimsizes_lon)[0] = ny;
        (*dimsizes_lon)[1] = nx;
	*lat = (float*)NclMalloc(sizeof(float)*nx*ny);
	*lon = (float*)NclMalloc(sizeof(float)*nx*ny);
	north= ((unsigned char)0200 & (unsigned char)gds[26])?0:1;
	idir = ((unsigned char)0200 & (unsigned char)gds[27])?-1:1;
	jdir = ((unsigned char)0100 & (unsigned char)gds[27])?1:-1;
	do_rot = 1;
	grid_oriented = ((unsigned char)010 & (unsigned char)gds[16])?1:0;

#if 1
	if (do_rot) {
		*rot = NhlMalloc(nx * ny * sizeof(float));
	}
	
	if(north) {
		_NclInitMapTrans("ST",90.0,lov,0.0);
/*
* Northern case
*/
		latin0 = 60.0;
		C = 2 * pi * EAR * cos(degtorad * 60)*1000.0;

		d_per_km = 360.0/C;
		dlon = dx * d_per_km;
/*
* latin1 is always closest to pole
*/
		tlon = lov + dlon;
		NGCALLF(maptrn,MAPTRN)(&latin0,&lov,&nx0,&ny0);
		NGCALLF(maptrn,MAPTRN)(&latin0,&tlon,&nx1,&ny1);
		deltax = fabs(nx0 - nx1);
		deltay = dy/dx * deltax;
		NGCALLF(maptrn,MAPTRN)(&la1,&lo1,&nx0,&ny0);
		for(j = 0; j < ny; j++) {
			for(i = 0; i < nx; i++) {
				float tmpx = nx0 + idir * i * deltax;
				float tmpy = ny0 + jdir * j * deltay;
				NGCALLF(maptri,MAPTRI)
				(&tmpx,&tmpy,&((*lat)[j * nx + i]),&((*lon)[j * nx + i]));
				if (do_rot) {
					gridrot(lov,(*lon)[j * nx + i],&((*rot)[j * nx + i]));
				}

			}
		}
	} else {
		_NclInitMapTrans("ST",-90.0,lov,0.0);
/*
* Southern case
*/
		latin0 = -60.0;
		C = 2 * pi * EAR * cos(degtorad * -60)*1000.0;
		d_per_km = 360.0/C;
		dlon = dx * d_per_km;

/*
* latin1 is always closest to pole
*/
		tlon = dlon + lov;
		NGCALLF(maptrn,MAPTRN)(&latin0,&tlon,&nx0,&ny0);
		NGCALLF(maptrn,MAPTRN)(&latin0,&lov,&nx1,&ny1);
		deltax = fabs(nx0 - nx1);
		deltay = dy/dx * deltax;
		NGCALLF(maptrn,MAPTRN)(&la1,&lo1,&nx0,&ny0);
		for(j = 0; j < ny; j++) {
			for(i = 0; i < nx; i++) {
				float tmpx = nx0 + idir * i * deltax;
				float tmpy = ny0 + jdir * j * deltay;
				NGCALLF(maptri,MAPTRI)
				(&tmpx,&tmpy,&((*lat)[j * nx + i]),&((*lon)[j * nx + i]));
				if (do_rot) {
					gridrot(lov,(*lon)[j * nx + i],&((*rot)[j * nx + i]));
				}
			}
		}
	}
#endif

#if 0
	{
		int kgds[32];
		int iopt;
		int npts;
		float fillval;
		int lrot;
		int nret;
		float *srot = NULL;
		
		kgds[0] = 5;
		kgds[1] = nx;
		kgds[2] = ny;
		kgds[3] = la1 * 1000;
		kgds[4] = lo1 * 1000;
		if (do_rot) {
			/* force gdswiz to think uv components are grid oriented so rotation angles are produced */
			unsigned char res_comp_flag = gds[16] | 010;
			kgds[5] = UnsignedCnvtToDecimal(1,&res_comp_flag);
		}
		else {
			kgds[5] = UnsignedCnvtToDecimal(1,&gds[16]);
		}
		kgds[6] = lov * 1000;
		kgds[7] = dx;
		kgds[8] = dy;
		kgds[9] = UnsignedCnvtToDecimal(1,&(gds[26]));;
		kgds[10] = UnsignedCnvtToDecimal(1,&(gds[27]));

		iopt = 0;
		lrot = do_rot;
		npts = nx * ny;
		if (do_rot) {
			srot = NhlMalloc(npts * sizeof(float));
			*rot = NhlMalloc(npts * sizeof(float));
		}
	
		NGCALLF(gdswiz,GDSWIZ)(kgds,&iopt,&npts,&fillval,*lon,*lat,*lon,*lat,&nret,&lrot,*rot,srot);

		if (do_rot) {
			for (i = 0; i < npts; i++) {
				(*rot)[i] = asin(srot[i]);
				(*lon)[i] = (*lon)[i] > 180.0 ? (*lon)[i] - 360.0 : (*lon)[i];
			}
			NhlFree(srot);
		}
		else {
			for (i = 0; i < npts; i++) {
				(*lon)[i] = (*lon)[i] > 180.0 ? (*lon)[i] - 360.0 : (*lon)[i];
			}
		}
	}
#endif

	if(lon_att_list != NULL) {
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = la1;
		GribPushAtt(lon_att_list,"La1",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = lo1;
		GribPushAtt(lon_att_list,"Lo1",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = lov;
		GribPushAtt(lon_att_list,"Lov",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = dx;
		GribPushAtt(lon_att_list,"Dx",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = dy;
		GribPushAtt(lon_att_list,"Dy",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
		tmp_string= (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string= NrmStringToQuark((north?"north":"south"));
		GribPushAtt(lon_att_list,"ProjectionCenter",tmp_string,1,nclTypestringClass); (*nlonatts)++;
		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string = NrmStringToQuark("degrees_east");
		GribPushAtt(lon_att_list,"units",tmp_string,1,nclTypestringClass); (*nlonatts)++;
		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string = NrmStringToQuark("Polar Stereographic Projection Grid");
		GribPushAtt(lon_att_list,"GridType",tmp_string,1,nclTypestringClass); (*nlonatts)++;
		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string = NrmStringToQuark("longitude");
		GribPushAtt(lon_att_list,"long_name",tmp_string,1,nclTypestringClass); (*nlonatts)++;
	}
	if(lat_att_list != NULL) {
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = la1;
		GribPushAtt(lat_att_list,"La1",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = lo1;
		GribPushAtt(lat_att_list,"Lo1",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = lov;
		GribPushAtt(lat_att_list,"Lov",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = dx;
		GribPushAtt(lat_att_list,"Dx",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = dy;
		GribPushAtt(lat_att_list,"Dy",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
		tmp_string= (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string= NrmStringToQuark((north?"north":"south"));
		GribPushAtt(lat_att_list,"ProjectionCenter",tmp_string,1,nclTypestringClass); (*nlatatts)++;
		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string = NrmStringToQuark("degrees_north");
		GribPushAtt(lat_att_list,"units",tmp_string,1,nclTypestringClass); (*nlatatts)++;
		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string = NrmStringToQuark("Polar Stereographic Projection Grid");
		grid_name = *tmp_string;
		GribPushAtt(lat_att_list,"GridType",tmp_string,1,nclTypestringClass); (*nlatatts)++;
		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string = NrmStringToQuark("latitude");
		GribPushAtt(lat_att_list,"long_name",tmp_string,1,nclTypestringClass); (*nlatatts)++;
	}

	if(do_rot && rot_att_list != NULL) {
		Do_Rotation_Atts(grid_name,rot_att_list,nrotatts,grid_oriented);
	}
	
}
void GdsOLGrid
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat,
	ng_size_t** dimsizes_lat,
	float** lon,
	int* n_dims_lon,
	ng_size_t** dimsizes_lon,
	float** rot,
	int* n_dims_rot,
	ng_size_t **dimsizes_rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon,
 rot, n_dims_rot, dimsizes_rot,
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
int* n_dims_rot;
ng_size_t **dimsizes_rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
			*lat = NULL;
			*n_dims_lat = 0;
			*dimsizes_lat = NULL;
			*lon = NULL;
			*n_dims_lon= 0;
			*dimsizes_lon= NULL;
}

void GdsSHGrid
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat,
	ng_size_t** dimsizes_lat,
	float** lon,
	int* n_dims_lon,
	ng_size_t** dimsizes_lon,
	float** rot,
	int* n_dims_rot,
	ng_size_t **dimsizes_rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon,
 rot, n_dims_rot, dimsizes_rot,
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
int* n_dims_rot;
ng_size_t **dimsizes_rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int j,k,m,type,mode;
	unsigned char *gds;


	*lat = NULL;
	*n_dims_lat = 0;
	*dimsizes_lat = NULL;
	*lon = NULL;
	*n_dims_lon= 0;
	*dimsizes_lon= NULL;
	*rot = NULL;
	*dimsizes_rot = NULL;
	*n_dims_rot = 0;
	if((thevarrec->thelist == NULL)||(thevarrec->ref_rec == NULL)) 
		return;

	gds = (unsigned char*)thevarrec->ref_rec->gds;


	j = UnsignedCnvtToDecimal(2,&(gds[6]));
	k = UnsignedCnvtToDecimal(2,&(gds[8]));
	m = UnsignedCnvtToDecimal(2,&(gds[10]));
	type = (int)gds[12]; 
	mode = (int)gds[13];

	*lat = NULL;
	*n_dims_lat =  1;
	*dimsizes_lat = NclMalloc(sizeof(ng_size_t));
	*(*dimsizes_lat) = j + 1;
	*lon = NULL;
	*n_dims_lon= 1;
	*dimsizes_lon= NclMalloc(sizeof(ng_size_t));
	*(*dimsizes_lon) = j + 1;
}


void GdsCEGrid
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat,
	ng_size_t** dimsizes_lat,
	float** lon,
	int* n_dims_lon,
	ng_size_t** dimsizes_lon,
	float** rot,
	int* n_dims_rot,
	ng_size_t **dimsizes_rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon,
 rot, n_dims_rot, dimsizes_rot,
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
int* n_dims_rot;
ng_size_t **dimsizes_rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	unsigned char *gds;
	int la1;
	int lo1;
	int la2;
	int lo2;
	double di;
	double dj;
	int latXlon;
	int idir;
	int jdir;
	int has_dir_inc;
	int is_thinned_lat;
	int is_thinned_lon;
	unsigned char tmp[4];
	int sign;
	int i;
	float *tmp_float;
	NclQuark* tmp_string;
	int nlon, nlat;
        int start_lat;
	
	
	*lat = NULL;
	*n_dims_lat = 0;
	*dimsizes_lat = NULL;
	*lon = NULL;
	*n_dims_lon= 0;
	*dimsizes_lon= NULL;
	*rot = NULL;
	*dimsizes_rot = NULL;
	*n_dims_rot = 0;
	if((thevarrec->thelist == NULL)||(thevarrec->ref_rec == NULL)) 
		return;

	gds = thevarrec->ref_rec->gds;
	if(gds == NULL) {
		return;
	}

	nlon = CnvtToDecimal(2,&(gds[6]));
	nlat = CnvtToDecimal(2,&(gds[8]));
	is_thinned_lon = (nlon == 65535); /* all bits set indicates missing: missing means thinned */
	is_thinned_lat = (nlat == 65535);
	if (nlon < 1 || nlat < 1 || (is_thinned_lon && is_thinned_lat)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  "GdsCEGrid: Invalid grid detected");
		*lat = NULL;
		*n_dims_lat = 0;
		*dimsizes_lat = NULL;
		*lon = NULL;
		*n_dims_lon= 0;
		*dimsizes_lon= NULL;
		return;
	}

	idir = ((char)0200 & gds[27]) ? -1 : 1;
	jdir = ((char)0100 & gds[27]) ? 1 : -1;
	latXlon = ((char)0040 & gds[27])? 0 : 1; 
	sign = ((char)0200 & gds[10] )? -1 : 1;
	tmp[0] = (char)0177 & gds[10];
	tmp[1] = gds[11];
	tmp[2] = gds[12];
	la1 = sign * CnvtToDecimal(3,tmp);
	sign = ((char)0200 & gds[13] )? -1 : 1;
	tmp[0] = (char)0177 & gds[13];
	tmp[1] = gds[14];
	tmp[2] = gds[15];
	lo1 = sign * CnvtToDecimal(3,tmp);
	sign = ((char)0200 & gds[17] )? -1 : 1;
	tmp[0] = (char)0177 & gds[17];
	tmp[1] = gds[18];
	tmp[2] = gds[19];
	la2 = sign * CnvtToDecimal(3,tmp);
	sign = ((char)0200 & gds[20] )? -1 : 1;
	tmp[0] = (char)0177 & gds[20];
	tmp[1] = gds[21];
	tmp[2] = gds[22];
	lo2 = sign * CnvtToDecimal(3,tmp);
	has_dir_inc = ((char)0200 & gds[16]) ? 1 : 0;
	if (is_thinned_lon) {
		GetThinnedLonParams(gds,nlat,lo1,lo2,idir,&nlon,&di);
	}
	else {
		/* this is adapted from the NCEP code: it should account for all cases of modular longitude values 
		   --- but actually doesn't work in all cases - like when the longitude range is < one degree

		di = 1000 * ((fmod((lo2 - lo1) * 1e-3 - 1.0 + 3600.0,360.0)+1.0) / (double) (nlon - 1));
		if (di < 0) di = -di;
		*/

		if (lo1 == lo2) { /* assume this is a badly specified global grid */
			if (idir == 1) {
				if (lo1 > 0) {
					lo1 -= 360000;
				}
				else {
					lo2 += 360000;
				}
			}
			else {
				if (lo1 > 0) {
					lo2 -= 360000;
				}
				else {
					lo1 += 360000;
				}
			}
		}
		/* this seems to work */
		if (nlon == 1) {
			di = ((double)CnvtToDecimal(2,&gds[23]));
		}
		else if (idir == 1) {
			int ti = lo2;
			while (ti < lo1) {
				ti += 360000;
			}
			di = (ti - lo1) / (double) (nlon - 1);
		}
		else {
			int ti = lo1;
			while (ti < lo2) {
				ti += 360000;
			}
			di = (ti - lo2) / (double) (nlon - 1);
		}
	}

	if (is_thinned_lat) {
		GetThinnedLatParams(gds,nlon,la1,la2,jdir,&nlat,&dj);
	}
	else {
		/* this is more accurate: do it this way */
		/* not specified: must be calculated from the endpoints and number of steps */
		if (nlat == 1) {
			dj  = ((double) CnvtToDecimal(2,&gds[25]));
		}
		else {
			dj = (la2 - la1) / (double) (nlat - 1);
			if (dj < 0) dj = -dj;
		}
	}
			
	*dimsizes_lat = (ng_size_t*)NclMalloc(sizeof(ng_size_t));
	*dimsizes_lon = (ng_size_t*)NclMalloc(sizeof(ng_size_t));
	*(*dimsizes_lon) = nlon;
	*(*dimsizes_lat) = nlat;
	*n_dims_lat = 1;
	*n_dims_lon = 1;
	*lat = (float*)NclMalloc((unsigned)sizeof(float)* nlat);
	*lon = (float*)NclMalloc((unsigned)sizeof(float)* nlon);
        start_lat = jdir == 1 ? MIN(la1,la2) : MAX(la1,la2);
	for(i = 0;i < *(*dimsizes_lat) ; i++) {
		(*lat)[i] = (float)((double)(start_lat + jdir * i * dj)) / 1000.0;
	}
	for(i = 0;i < *(*dimsizes_lon) ; i++) {
		(*lon)[i] = (float)((double)(lo1 + idir * i * di)) / 1000.0;
	}
	
	if(lon_att_list != NULL) {
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = la1/1000.0;
		GribPushAtt(lon_att_list,"La1",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = lo1/1000.0;
		GribPushAtt(lon_att_list,"Lo1",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = la2/1000.0;
		GribPushAtt(lon_att_list,"La2",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = lo2/1000.0;
		GribPushAtt(lon_att_list,"Lo2",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = di/1000.0;
		GribPushAtt(lon_att_list,"Di",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = dj/1000.0;
		GribPushAtt(lon_att_list,"Dj",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string = NrmStringToQuark("degrees_east");
		GribPushAtt(lon_att_list,"units",tmp_string,1,nclTypestringClass); (*nlonatts)++;
		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		if (is_thinned_lat || is_thinned_lon)
			*tmp_string = NrmStringToQuark("Cylindrical Equidistant Projection Grid (Quasi-Regular)");
		else 
			*tmp_string = NrmStringToQuark("Cylindrical Equidistant Projection Grid");
		GribPushAtt(lon_att_list,"GridType",tmp_string,1,nclTypestringClass); (*nlonatts)++;
		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string = NrmStringToQuark("longitude");
		GribPushAtt(lon_att_list,"long_name",tmp_string,1,nclTypestringClass); (*nlonatts)++;
	}
	if(lat_att_list != NULL) {
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = la1/1000.0;
		GribPushAtt(lat_att_list,"La1",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = lo1/1000.0;
		GribPushAtt(lat_att_list,"Lo1",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = la2/1000.0;
		GribPushAtt(lat_att_list,"La2",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = lo2/1000.0;
		GribPushAtt(lat_att_list,"Lo2",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = di/1000.0;
		GribPushAtt(lat_att_list,"Di",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = dj/1000.0;
		GribPushAtt(lat_att_list,"Dj",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string = NrmStringToQuark("degrees_north");
		GribPushAtt(lat_att_list,"units",tmp_string,1,nclTypestringClass); (*nlatatts)++;
		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		if (is_thinned_lon || is_thinned_lat)
			*tmp_string = NrmStringToQuark("Cylindrical Equidistant Projection Grid (Quasi-Regular)");
		else 
			*tmp_string = NrmStringToQuark("Cylindrical Equidistant Projection Grid");
		GribPushAtt(lat_att_list,"GridType",tmp_string,1,nclTypestringClass); (*nlatatts)++;
		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string = NrmStringToQuark("latitude");
		GribPushAtt(lat_att_list,"long_name",tmp_string,1,nclTypestringClass); (*nlatatts)++;
	}
	return;
}

void GdsUnknownGrid
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat,
	ng_size_t** dimsizes_lat,
	float** lon,
	int* n_dims_lon,
	ng_size_t** dimsizes_lon,
	float** rot,
	int* n_dims_rot,
	ng_size_t **dimsizes_rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon,
 rot, n_dims_rot, dimsizes_rot,
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
int* n_dims_rot;
ng_size_t **dimsizes_rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	unsigned char *gds;
	int nlon,nlat;
	int is_thinned_lat;
	int is_thinned_lon;
	int gds_type;
	
	*lat = NULL;
	*n_dims_lat = 0;
	*dimsizes_lat = NULL;
	*lon = NULL;
	*n_dims_lon= 0;
	*dimsizes_lon = NULL;
	*rot = NULL;
	*n_dims_rot = 0;
	*dimsizes_rot = NULL;
	if((thevarrec->thelist == NULL)||(thevarrec->ref_rec == NULL)) 
		return;

	gds = thevarrec->ref_rec->gds;
	if(gds == NULL) {
		return;
	}

	gds_type = (int) gds[5];
	switch (gds_type) {
	default:
		NhlPError(NhlWARNING,NhlEUNKNOWN,
	  "GdsUnknownGrid: GDS grid %d is unknown and may not be decoded correctly; no coordinate variables will be supplied",
			gds_type);
		/* we will try to at least get the dimension sizes, but no guarantee */
		break;
	case 10:
	case 14:
	case 20:
	case 24:
	case 30:
	case 34:
	case 90:
		NhlPError(NhlWARNING,NhlEUNKNOWN,
		  "NCL does not yet fully support GDS grid type %d, no coordinate variables will be supplied for this grid",
			  gds_type);

		/* we can at least get the x and y dimensions for these grids */
		break;
	}
		
	nlon = CnvtToDecimal(2,&(gds[6]));
	nlat = CnvtToDecimal(2,&(gds[8]));
	is_thinned_lon = (nlon == 65535); /* all bits set indicates missing: missing means thinned */
	is_thinned_lat = (nlat == 65535);
	if (is_thinned_lon || is_thinned_lat) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  "GdsUnknownGrid: Cannot decode unsupported grid containing thinned lats or lons");
	}
	/* this is a purely heuristic test: passing it does not definitively mean the dimensions have been decoded correctly. */
	else if (nlat <= 1 || nlon <= 1 || nlat > 15000 || nlon > 15000) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  "GdsUnknownGrid: Not enough known about grid %d to determine grid size or shape",
			gds_type);
		return;
	}
	*dimsizes_lat = (ng_size_t*)NclMalloc(sizeof(ng_size_t));
	*dimsizes_lon = (ng_size_t*)NclMalloc(sizeof(ng_size_t));
	*(*dimsizes_lon) = nlon;
	*(*dimsizes_lat) = nlat;
	*n_dims_lat = 1;
	*n_dims_lon = 1;

	return;
}


static void rot2ll
(
	double latpole,
	double lonpole,
	double latin,
	double lonin,
	double *latout,
	double *lonout
)
{
	double dtr = atan(1) / 45.0;
	double x,y,z;
	double rotang,sinrot,cosrot;
	double rx,ry,rz;
	double tlat,tlon;
	    
	/* convert to xyz coordinates */

	x = cos(latin * dtr) * cos(lonin * dtr);
	y = cos(latin * dtr) * sin(lonin * dtr);
	z = sin(latin * dtr);

	/*  rotate around y axis */
	rotang = - (latpole + 90) * dtr;
	sinrot = sin(rotang);
	cosrot = cos(rotang);
	ry = y;
	rx = x * cosrot + z * sinrot;
	rz = -x * sinrot + z * cosrot;
    
	/* convert back to lat/lon */

	tlat = asin(rz) / dtr;
	if (fabs(rx) > 0.0001) {
		tlon = atan2(ry,rx) / dtr;
	}
	else if (ry > 0) {
		tlon = 90.0;
	}
        else {
		tlon = -90.0;
	}
	/* remove the longitude rotation */

	tlon = tlon + lonpole;
	if (tlon < -180) {
		tlon = tlon + 360.0;
	}
	if (tlon >= 180) {
		tlon = tlon - 360.0;
	}

	*latout = tlat;
	*lonout = tlon;

	return;
}

static void ll2rot
(
	double latpole,
	double lonpole,
	double latin,
	double lonin,
	double *latout,
	double *lonout
)
{
	double dtr = atan(1) / 45.0;
	double x,y,z;
	double rotang,sinrot,cosrot;
	double rx,ry,rz;
	double tlat,tlon;
	    
	tlon = lonin - lonpole;

	/* convert to xyz coordinates */

	x = cos(latin * dtr) * cos(tlon * dtr);
	y = cos(latin * dtr) * sin(tlon * dtr);
	z = sin(latin * dtr);

	/*  rotate around y axis */
	rotang = (latpole + 90) * dtr;
	sinrot = sin(rotang);
	cosrot = cos(rotang);
	ry = y;
	rx = x * cosrot + z * sinrot;
	rz = -x * sinrot + z * cosrot;
    
	/* convert back to lat/lon */

	tlat = asin(rz) / dtr;
	if (fabs(rx) > 0.0001) {
		tlon = atan2(ry,rx) / dtr;
	}
	else if (ry > 0) {
		tlon = 90.0;
	}
        else {
		tlon = -90.0;
	}

	if (tlon < -180) {
		tlon = tlon + 360.0;
	}
	if (tlon >= 180) {
		tlon = tlon - 360.0;
	}

	*latout = tlat;
	*lonout = tlon;

	return;
}


/*
 * Rotated Lat/Lon grids GDS grid type 10  (also GDS type 205 -- I hope)
 */
void GdsRLLGrid
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat,
	ng_size_t** dimsizes_lat,
	float** lon,
	int* n_dims_lon,
	ng_size_t** dimsizes_lon,
	float** rot,
	int* n_dims_rot,
	ng_size_t **dimsizes_rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon,
 rot, n_dims_rot, dimsizes_rot,
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
int* n_dims_rot;
ng_size_t **dimsizes_rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	unsigned char *gds;
	double la1,lo1,la2,lo2,laC,loC;
	double di;
	double dj;
	int latXlon;
	int idir;
	int jdir;
	int has_dir_inc;
	int is_thinned_lat;
	int is_thinned_lon;
	unsigned char tmp[4];
	int sign;
	int i,j;
	float *tmp_float;
	NclQuark* tmp_string;
	int ni, nj;
	double lasp;
	double losp;
	double rotang;
	double clat,llat,llon,rlat,rlon;
	int gds_type;
	NhlBoolean do_rot = True;
	NhlBoolean grid_oriented;
	NrmQuark grid_name;
	int do_180 = 0;
		
	gds_type = thevarrec->gds_type;
	*lat = NULL;
	*n_dims_lat = 0;
	*dimsizes_lat = NULL;
	*lon = NULL;
	*n_dims_lon= 0;
	*dimsizes_lon= NULL;
	*rot = NULL;
	*n_dims_rot = 0;
	*dimsizes_rot = NULL; 
		
	if((thevarrec->thelist == NULL)||(thevarrec->ref_rec == NULL)) 
		return;

	gds = thevarrec->ref_rec->gds;
	if(gds == NULL) {
		return;
	}

	ni = CnvtToDecimal(2,&(gds[6]));
	nj = CnvtToDecimal(2,&(gds[8]));
	is_thinned_lon = (ni == 65535); /* all bits set indicates missing: missing means thinned */
	is_thinned_lat = (nj == 65535);
	if (ni < 1 || nj < 1 || (is_thinned_lon && is_thinned_lat)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  "GdsRLLGrid: Invalid grid detected");
		return;
	}

	idir = ((char)0200 & gds[27]) ? -1 : 1;
	jdir = ((char)0100 & gds[27]) ? 1 : -1;
	latXlon = ((char)0040 & gds[27])? 0 : 1; 
	sign = ((char)0200 & gds[10] )? -1 : 1;
	tmp[0] = (char)0177 & gds[10];
	tmp[1] = gds[11];
	tmp[2] = gds[12];
	la1 = sign * ((double)CnvtToDecimal(3,tmp)) / 1000.0;
	sign = ((char)0200 & gds[13] )? -1 : 1;
	tmp[0] = (char)0177 & gds[13];
	tmp[1] = gds[14];
	tmp[2] = gds[15];
	lo1 = sign * ((double)CnvtToDecimal(3,tmp)) / 1000.0;
	sign = ((char)0200 & gds[17] )? -1 : 1;
	tmp[0] = (char)0177 & gds[17];
	tmp[1] = gds[18];
	tmp[2] = gds[19];
	la2 = sign * ((double)CnvtToDecimal(3,tmp)) / 1000.0;
	sign = ((char)0200 & gds[20] )? -1 : 1;
	tmp[0] = (char)0177 & gds[20];
	tmp[1] = gds[21];
	tmp[2] = gds[22];
	lo2 = sign * ((double)CnvtToDecimal(3,tmp)) / 1000.0;

	if (gds_type == 10) {
		sign = ((char)0200 & gds[32] )? -1 : 1;
		tmp[0] = (char)0177 & gds[32];
		tmp[1] = gds[33];
		tmp[2] = gds[34];
		lasp = ((double) sign * CnvtToDecimal(3,tmp)) / 1000.0;

		sign = ((char)0200 & gds[35] )? -1 : 1;
		tmp[0] = (char)0177 & gds[35];
		tmp[1] = gds[36];
		tmp[2] = gds[37];
		losp = ((double) sign * CnvtToDecimal(3,tmp)) / 1000.0;

		sign = ((char)0200 & gds[38] )? -1 : 1;
		tmp[0] = (char)0177 & gds[38];
		tmp[1] = gds[39];
		tmp[2] = gds[40];
		tmp[3] = gds[41];
		rotang = (double)sign * ((double)CnvtToDecimal(3,&(tmp[1])) * pow(2.0,-24.0) * pow(16.0,tmp[0] - 64));
	}
	else if (gds_type == 205) {
		loC = lo2;
		laC = la2;

		sign = ((char)0200 & gds[28] )? -1 : 1;
		tmp[0] = (char)0177 & gds[28];
		tmp[1] = gds[29];
		tmp[2] = gds[30];
		la2 = ((double) sign * CnvtToDecimal(3,tmp)) / 1000.0;

		sign = ((char)0200 & gds[31] )? -1 : 1;
		tmp[0] = (char)0177 & gds[31];
		tmp[1] = gds[32];
		tmp[2] = gds[33];
		lo2 = ((double) sign * CnvtToDecimal(3,tmp)) / 1000.0;
		lasp = laC - 90;
		losp = loC;
		rotang = 0;
	}
	has_dir_inc = ((char)0200 & gds[16]) ? 1 : 0;

	ll2rot(lasp,losp,la1,lo1,&llat,&llon);
	ll2rot(lasp,losp,la2,lo2,&rlat,&rlon);
	if (is_thinned_lon) {
		if (gds_type == 10)
			GetThinnedLonParams(gds,nj,(int)(lo1*1000.0),(int)(lo2*1000.0),idir,&ni,&di);
		else
			GetThinnedLonParams(gds,nj,(int)(llon*1000.0),(int)(rlon*1000.0),idir,&ni,&di);
	}
	if (gds_type == 10) {
		/* this is adapted from the NCEP code: it should account for all cases of modular longitude values 
		--- but actually doesn't work in all cases - like when the longitude range is < one degree
		di = (idir * (fmod(idir * (lo2 - lo1) - 1.0 + 3600.0,360.0)+1.0) / (double) (ni - 1));
		*/
		/* so now we do it this way */
		if (ni == 1) {
			di = ((double)CnvtToDecimal(2,&gds[23])) / 1000.0;
		}
		else if (idir == 1) {
			double ti = lo2;
			while (ti < lo1) {
				ti += 360;
			}
			di = (ti - lo1) / (double) (ni - 1);
		}
		else {
			double ti = lo1;
			while (ti < lo2) {
				ti += 360;
			}
			di = (ti - lo2) / (double) (ni - 1);
		}

	}
	else {
		if (ni == 1) {
			di = ((double)CnvtToDecimal(2,&gds[23])) / 1000.0;
		}
		else if (idir == 1) {
			double ti = rlon;
			while (ti < llon) {
				ti += 360;
			}
			di = (ti - llon) / (double) (ni - 1);
		}
		else {
			double ti = llon;
			while (ti < rlon) {
				ti += 360;
			}
			di = (ti - rlon) / (double) (ni - 1);
		}

	}

	if (is_thinned_lat) {
		if (gds_type == 10)
			GetThinnedLatParams(gds,ni,(int)(la1*1000.0),(int)(la2*1000.0),jdir,&nj,&dj);
		else
			GetThinnedLatParams(gds,ni,(int)(llat*1000.0),(int)(rlat*1000.0),jdir,&nj,&dj);
	}
	else if (gds_type == 10) {
		if (nj == 1) {
			dj  = ((double) CnvtToDecimal(2,&gds[25])) / 1000.0;
		}
		else {
			if (jdir == 1) 
				dj = (la2 - la1) / (double) (nj - 1);
			else
				dj = (la1 - la2) / (double) (nj - 1);
		}
	}
	else {
		if (nj == 1) {
			dj  = ((double) CnvtToDecimal(2,&gds[25])) / 1000.0;
		}
		else {
			if (jdir == 1) 
				dj = (rlat - llat) / (double) (nj - 1);
			else
				dj = (llat - rlat) / (double) (nj - 1);
		}
	}

	grid_oriented  = ((unsigned char)010 & (unsigned char)gds[16])?1:0;
	do_rot = 1;
			
	if (rotang != 0.0) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "GdsRLLGrid: Nonzero rotation angle not yet supported for GDS grid type %d: cannot handle grid", 
			  gds_type);
		return;
	}
	else {
		*dimsizes_lat = (ng_size_t*)NclMalloc(2 * sizeof(ng_size_t));
		*dimsizes_lon = (ng_size_t*)NclMalloc(2 * sizeof(ng_size_t));
		(*dimsizes_lon)[0] = nj;
		(*dimsizes_lon)[1] = ni;
		(*dimsizes_lat)[0] = nj;
		(*dimsizes_lat)[1] = ni;
		*n_dims_lat = 2;
		*n_dims_lon = 2;
		*lat = (float*)NclMalloc((unsigned)sizeof(float)* nj * ni);
		*lon = (float*)NclMalloc((unsigned)sizeof(float)* nj * ni);

		clat = lasp + 90.0;
		if (gds_type == 10) {
			rot2ll(lasp,losp,la1,lo1,&llat,&llon);
			rot2ll(lasp,losp,la2,lo2,&rlat,&rlon);
		}
		else if (gds_type == 205) {
			llat = la1;
			llon = lo1;
			rlat = la2;
			rlon = lo2;
		}

		if (idir == 1) {
			if (llon > rlon) {
				llon -= 360;
			}
			if (llon < 0 && rlon > 0) {
				do_180 = 1;
			}
		}
		else {
			if (rlon > llon) {
				rlon -= 360;
			}
			if (rlon < 0 && llon > 0) {
				do_180 = 1;
			}
		}
			

		if (do_rot) {
			double dtr = atan(1) / 45.0;
			if (gds_type == 10) {
				rlat = la1; 
				rlon = lo1;
			}
			else if (gds_type == 205) {
				ll2rot(lasp,losp,la1,lo1,&rlat,&rlon);
			}
			*rot = (float*)NclMalloc((unsigned)sizeof(float)* nj * ni);

			for(j = 0;j < nj; j++) {
				for (i = 0; i < ni; i++) {
					double tlon,tlat;
					double cgridlat, slon,srot,crot;
					/*double crot1, eps;*/
					rot2ll(lasp,losp,rlat + j * jdir * dj,rlon + i * idir * di,&tlat,&tlon);
					if (do_180) {
						tlon = tlon > 180 ? tlon - 360 : tlon;
					}
					(*lat)[j * ni + i] = (float)tlat;
					(*lon)[j * ni + i] = (float)tlon;
					slon = sin((tlon - losp)*dtr);
					cgridlat = cos((rlat + j * jdir * dj) * dtr);
					if (cgridlat <= 0.0)
						(*rot)[j * ni + i] = 0.0;
					else {
						crot = (cos(clat * dtr) * cos(tlat * dtr) + 
							 sin(clat * dtr) * sin(tlat * dtr) * cos(tlon * dtr)) / cgridlat;
						srot =  -sin(clat * dtr) * slon / cgridlat;
						/*
						(*rot)[j * ni + i] = (float) asin(srot);
						(*rot)[j * ni + i] = (float) acos(crot);
						*/
						(*rot)[j * ni + i] = (float) atan2(srot,crot);

#if 0					
						/* diagnostics */
						crot1 = sqrt(1 - srot * srot);
						eps = fabs(crot) - fabs(crot1);

					}
					if ((i%10 == 0 && j%10 == 0) ) {
						printf("j/i %d %d lat/lon %f %f rot %f slon cgridlat srot crot %f %f %f %f crot1 eps %f %f\n",
						       j,i,tlat,tlon,(*rot)[j * ni + i],
						       slon,cgridlat,srot,crot,crot1,eps);
#endif
					}
				}
			}
		}
		else {
			double rlat = la1; 
			double rlon = lo1;
			if (gds_type == 205) {
				ll2rot(lasp,losp,la1,lo1,&rlat,&rlon);
			}
			for(j = 0;j < nj; j++) {
				for (i = 0; i < ni; i++) {
					double tlon,tlat;
					rot2ll(lasp,losp,rlat + j * jdir * dj,rlon + i * idir * di,&tlat,&tlon);
					if (do_180) {
						tlon = tlon > 180 ? tlon - 360 : tlon;
					}
					(*lat)[j * ni + i] = (float)tlat;
					(*lon)[j * ni + i] = (float)tlon;
				}
			}
		}
	}	
	if (gds_type == 10) {
		grid_name = (is_thinned_lat || is_thinned_lon) ?
			NrmStringToQuark("Rotated Latitude/Longitude Grid (Quasi-Regular)") :
			NrmStringToQuark("Rotated Latitude/Longitude Grid");
		if(lon_att_list != NULL) {
			tmp_float= (float*)NclMalloc(sizeof(float));
			*tmp_float = lasp;
			GribPushAtt(lon_att_list,"Latitude_of_southern_pole",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
			tmp_float= (float*)NclMalloc(sizeof(float));
			*tmp_float = losp;
			GribPushAtt(lon_att_list,"Longitude_of_southern_pole",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
			tmp_float= (float*)NclMalloc(sizeof(float));
			*tmp_float = rotang;
			GribPushAtt(lon_att_list,"Angle_of_rotation",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
			tmp_float= (float*)NclMalloc(sizeof(float));
			*tmp_float = la1;
			GribPushAtt(lon_att_list,"La1",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
			tmp_float= (float*)NclMalloc(sizeof(float));
			*tmp_float = lo1;
			GribPushAtt(lon_att_list,"Lo1",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
			tmp_float= (float*)NclMalloc(sizeof(float));
			*tmp_float = la2;
			GribPushAtt(lon_att_list,"La2",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
			tmp_float= (float*)NclMalloc(sizeof(float));
			*tmp_float = lo2;
			GribPushAtt(lon_att_list,"Lo2",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
			tmp_float= (float*)NclMalloc(sizeof(float));
			*tmp_float = di;
			GribPushAtt(lon_att_list,"Di",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
			tmp_float= (float*)NclMalloc(sizeof(float));
			*tmp_float = dj;
			GribPushAtt(lon_att_list,"Dj",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
			tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
			*tmp_string = NrmStringToQuark("degrees_east");
			GribPushAtt(lon_att_list,"units",tmp_string,1,nclTypestringClass); (*nlonatts)++;
			tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
			*tmp_string = grid_name;
			GribPushAtt(lon_att_list,"GridType",tmp_string,1,nclTypestringClass); (*nlonatts)++;
			tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
			*tmp_string = NrmStringToQuark("longitude");
			GribPushAtt(lon_att_list,"long_name",tmp_string,1,nclTypestringClass); (*nlonatts)++;
		}
		if(lat_att_list != NULL) {
			tmp_float= (float*)NclMalloc(sizeof(float));
			*tmp_float = lasp;
			GribPushAtt(lat_att_list,"Latitude_of_southern_pole",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
			tmp_float= (float*)NclMalloc(sizeof(float));
			*tmp_float = losp;
			GribPushAtt(lat_att_list,"Longitude_of_southern_pole",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
			tmp_float= (float*)NclMalloc(sizeof(float));
			*tmp_float = rotang;
			GribPushAtt(lat_att_list,"Angle_of_rotation",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
			tmp_float= (float*)NclMalloc(sizeof(float));
			*tmp_float = la1;
			GribPushAtt(lat_att_list,"La1",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
			tmp_float= (float*)NclMalloc(sizeof(float));
			*tmp_float = lo1;
			GribPushAtt(lat_att_list,"Lo1",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
			tmp_float= (float*)NclMalloc(sizeof(float));
			*tmp_float = la2;
			GribPushAtt(lat_att_list,"La2",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
			tmp_float= (float*)NclMalloc(sizeof(float));
			*tmp_float = lo2;
			GribPushAtt(lat_att_list,"Lo2",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
			tmp_float= (float*)NclMalloc(sizeof(float));
			*tmp_float = di;
			GribPushAtt(lat_att_list,"Di",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
			tmp_float= (float*)NclMalloc(sizeof(float));
			*tmp_float = dj;
			GribPushAtt(lat_att_list,"Dj",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
			tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
			*tmp_string = NrmStringToQuark("degrees_north");
			GribPushAtt(lat_att_list,"units",tmp_string,1,nclTypestringClass); (*nlatatts)++;
			tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
			*tmp_string = grid_name;
			GribPushAtt(lat_att_list,"GridType",tmp_string,1,nclTypestringClass); (*nlatatts)++;
			tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
			*tmp_string = NrmStringToQuark("latitude");
			GribPushAtt(lat_att_list,"long_name",tmp_string,1,nclTypestringClass); (*nlatatts)++;
		}
	}
	else {
		grid_name = (is_thinned_lat || is_thinned_lon) ?
			NrmStringToQuark("Arakawa Non-E Staggered Rotated Latitude/Longitude Grid (Quasi-Regular)") :
			NrmStringToQuark("Arakawa Non-E Staggered Rotated Latitude/Longitude Grid");

		if(lon_att_list != NULL) {
			tmp_float= (float*)NclMalloc(sizeof(float));
			*tmp_float = la1;
			GribPushAtt(lon_att_list,"La1",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
			tmp_float= (float*)NclMalloc(sizeof(float));
			*tmp_float = lo1;
			GribPushAtt(lon_att_list,"Lo1",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
			tmp_float= (float*)NclMalloc(sizeof(float));
			*tmp_float = laC;
			GribPushAtt(lon_att_list,"CenterLat",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
			tmp_float= (float*)NclMalloc(sizeof(float));
			*tmp_float = loC;
			GribPushAtt(lon_att_list,"CenterLon",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
			tmp_float= (float*)NclMalloc(sizeof(float));
			*tmp_float = la2;
			GribPushAtt(lon_att_list,"La2",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
			tmp_float= (float*)NclMalloc(sizeof(float));
			*tmp_float = lo2;
			GribPushAtt(lon_att_list,"Lo2",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
			tmp_float= (float*)NclMalloc(sizeof(float));
			*tmp_float = di;
			GribPushAtt(lon_att_list,"Di",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
			tmp_float= (float*)NclMalloc(sizeof(float));
			*tmp_float = dj;
			GribPushAtt(lon_att_list,"Dj",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
			tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
			*tmp_string = NrmStringToQuark("degrees_east");
			GribPushAtt(lon_att_list,"units",tmp_string,1,nclTypestringClass); (*nlonatts)++;
			tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
			*tmp_string = grid_name;
			GribPushAtt(lon_att_list,"GridType",tmp_string,1,nclTypestringClass); (*nlonatts)++;
			tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
			*tmp_string = NrmStringToQuark("longitude");
			GribPushAtt(lon_att_list,"long_name",tmp_string,1,nclTypestringClass); (*nlonatts)++;
		}
		if(lat_att_list != NULL) {
			tmp_float= (float*)NclMalloc(sizeof(float));
			*tmp_float = la1;
			GribPushAtt(lat_att_list,"La1",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
			tmp_float= (float*)NclMalloc(sizeof(float));
			*tmp_float = lo1;
			GribPushAtt(lat_att_list,"Lo1",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
			tmp_float= (float*)NclMalloc(sizeof(float));
			*tmp_float = laC;
			GribPushAtt(lat_att_list,"CenterLat",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
			tmp_float= (float*)NclMalloc(sizeof(float));
			*tmp_float = loC;
			GribPushAtt(lat_att_list,"CenterLon",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
			tmp_float= (float*)NclMalloc(sizeof(float));
			*tmp_float = la2;
			GribPushAtt(lat_att_list,"La2",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
			tmp_float= (float*)NclMalloc(sizeof(float));
			*tmp_float = lo2;
			GribPushAtt(lat_att_list,"Lo2",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
			tmp_float= (float*)NclMalloc(sizeof(float));
			*tmp_float = di;
			GribPushAtt(lat_att_list,"Di",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
			tmp_float= (float*)NclMalloc(sizeof(float));
			*tmp_float = dj;
			GribPushAtt(lat_att_list,"Dj",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
			tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
			*tmp_string = NrmStringToQuark("degrees_north");
			GribPushAtt(lat_att_list,"units",tmp_string,1,nclTypestringClass); (*nlatatts)++;
			tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
			*tmp_string = grid_name;
			GribPushAtt(lat_att_list,"GridType",tmp_string,1,nclTypestringClass); (*nlatatts)++;
			tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
			*tmp_string = NrmStringToQuark("latitude");
			GribPushAtt(lat_att_list,"long_name",tmp_string,1,nclTypestringClass); (*nlatatts)++;
		}
	}
	if (do_rot && rot_att_list != NULL) {
		Do_Rotation_Atts(grid_name,rot_att_list,nrotatts,grid_oriented);
	}
	return;
}


void GdsArakawaRLLGrid 
#if NhlNeedProto
(
	GribParamList* thevarrec, 
	float** lat, 
	int* n_dims_lat,
	ng_size_t** dimsizes_lat,
	float** lon,
	int* n_dims_lon,
	ng_size_t** dimsizes_lon,
	float** rot,
	int* n_dims_rot,
	ng_size_t **dimsizes_rot,
	GribAttInqRecList** lat_att_list, 
	int* nlatatts, 
	GribAttInqRecList** lon_att_list, 
	int* nlonatts,
	GribAttInqRecList** rot_att_list,
	int* nrotatts
)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon,
 rot, n_dims_rot, dimsizes_rot,
 lat_att_list,nlatatts,lon_att_list, nlonatts, rot_att_list, nrotatts)
GribParamList* thevarrec; 
float** lat; 
int* n_dims_lat;
ng_size_t** dimsizes_lat;
float** lon;
int* n_dims_lon;
ng_size_t** dimsizes_lon;
float** rot;
int* n_dims_rot;
ng_size_t **dimsizes_rot;
GribAttInqRecList** lat_att_list; 
int* nlatatts; 
GribAttInqRecList** lon_att_list; 
int* nlonatts;
GribAttInqRecList** rot_att_list;
int* nrotatts;
#endif
{
	int nx;
	int ny;
	int la1,lo1;
	int loc,lac;
	int di;
	int dj;
	unsigned char tmpc[4];
	int i;
	unsigned char *gds;
	float *tmp_float;
	NclQuark* tmp_string;
	int kgds[200];
	int iopt = 0;
	int npts,nret,lrot;
	float fillval = -9999;
	float *srot = NULL;
	int is_uv = Is_UV(thevarrec->param_number);
	NhlBoolean do_rot = True;
	NhlBoolean grid_oriented = False;
	NrmQuark grid_name = NrmNULLQUARK;
	int gtype;

	/* arakawa RLL: Rotated LatLon grids - 201,202 and 203 */
	
	*lat = NULL;
	*n_dims_lat = 0;
	*dimsizes_lat = NULL;
	*lon = NULL;
	*n_dims_lon= 0;
	*dimsizes_lon= NULL;
	*rot = NULL;
	*dimsizes_rot = NULL;
	*n_dims_rot = 0;
	if((thevarrec->thelist == NULL)||(thevarrec->ref_rec == NULL)) 
		return;

	gds = (unsigned char*)thevarrec->ref_rec->gds;

	gtype = UnsignedCnvtToDecimal(1,&(gds[5]));

	nx = UnsignedCnvtToDecimal(2,&(gds[6]));
	ny = UnsignedCnvtToDecimal(2,&(gds[8]));
	tmpc[0] = gds[10] & (unsigned char) 0177;
	tmpc[1] = gds[11];
	tmpc[2] = gds[12];
	la1 = (UnsignedCnvtToDecimal(3,tmpc));
	la1 = ((gds[10] & (unsigned char) 0200)? -la1:la1);

	tmpc[0] = gds[13] & (unsigned char) 0177;
	tmpc[1] = gds[14];
	tmpc[2] = gds[15];
	lo1 = (UnsignedCnvtToDecimal(3,tmpc));
	lo1 = ((gds[13] & (unsigned char) 0200)? -lo1:lo1);

	tmpc[0] = gds[17] & (unsigned char) 0177;
	tmpc[1] = gds[18];
	tmpc[2] = gds[19];
	lac = (UnsignedCnvtToDecimal(3,tmpc));
	lac = ((gds[17] & (unsigned char) 0200)? -lac:lac);

	tmpc[0] = gds[20] & (unsigned char) 0177;
	tmpc[1] = gds[21];
	tmpc[2] = gds[22];
	loc = (UnsignedCnvtToDecimal(3,tmpc));
	loc = ((gds[20] & (unsigned char) 0200)? -loc:loc);

	di = CnvtToDecimal(2,&gds[23]);
	dj = CnvtToDecimal(2,&gds[25]);

	switch (gtype) {
	case 201:
		grid_oriented = ((unsigned char)010 & (unsigned char)gds[16])?1:0;
		do_rot = True;
		grid_name = NrmStringToQuark("Arakawa semi-staggered E-grid on rotated latitude/longitude grid-point array");
		break;
	case 202:
		grid_oriented =  ((unsigned char)010 & (unsigned char)gds[16])?1:0;
		do_rot = True;
		grid_name = NrmStringToQuark("Arakawa filled E-grid on rotated latitude/longitude grid-point array");
		break;

	case 203:
		do_rot = is_uv;
		grid_oriented = ((unsigned char)010 & (unsigned char)gds[16])?1:0;
		if (is_uv) {
			grid_name = NrmStringToQuark("Arakawa staggered E-grid on rotated latitude/longitude grid-point array (velocity points)");
		}
		else {
			grid_name = NrmStringToQuark("Arakawa staggered E-grid on rotated latitude/longitude grid-point array (mass points)");
		}
		break;
	}

	kgds[0] = gtype;
	kgds[1] = nx;
	kgds[2] = ny;
	kgds[3] = la1;
	kgds[4] = lo1;
	if (do_rot) {
		/* force gdswiz to think uv components are grid oriented so rotation angles are produced */
		unsigned char res_comp_flag = gds[16] | 010;
		kgds[5] = UnsignedCnvtToDecimal(1,&res_comp_flag);
	}
	else {
		kgds[5] = UnsignedCnvtToDecimal(1,&gds[16]);
	}
	kgds[6] = lac;
	kgds[7] = loc;
	kgds[8] = di;
	kgds[9] = dj;
	kgds[10] = UnsignedCnvtToDecimal(1,&(gds[27]));

        *dimsizes_lat = (ng_size_t*)NclMalloc(sizeof(ng_size_t) * 2);
        *dimsizes_lon = (ng_size_t*)NclMalloc(sizeof(ng_size_t) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
	/* for now at least return the native coordinates, not attempting to shift to a regular grid */

        (*dimsizes_lat)[0] = ny;
        (*dimsizes_lat)[1] = nx;
        (*dimsizes_lon)[0] = ny;
        (*dimsizes_lon)[1] = nx;
	(*lon) = (float*)NclMalloc(sizeof(float)*nx*ny);
	(*lat) = (float*)NclMalloc(sizeof(float)*nx*ny);


	npts = nx * ny;
	lrot = 0;
	if (do_rot) {
		srot = NhlMalloc(npts * sizeof(float));
		*rot = NhlMalloc(npts * sizeof(float));
		lrot = 1;
	}

	if (is_uv) {
		kgds[10] |= 256;
	}

	NGCALLF(gdswiz,GDSWIZ)(kgds,&iopt,&npts,&fillval,*lon,*lat,*lon,*lat,&nret,&lrot,*rot,srot);
	if (do_rot) {
		for (i = 0; i < npts; i++) {
			(*rot)[i] = asin(srot[i]);
			(*lon)[i] = (*lon)[i] > 180.0 ? (*lon)[i] - 360.0 : (*lon)[i];
		}
		NhlFree(srot);
	}
	else {
		for (i = 0; i < npts; i++) {
			(*lon)[i] = (*lon)[i] > 180.0 ? (*lon)[i] - 360.0 : (*lon)[i];
		}
	}

	
	if(lon_att_list != NULL) {
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float =  la1 * 1e-3;
		GribPushAtt(lon_att_list,"La1",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = lo1 * 1e-3;
		GribPushAtt(lon_att_list,"Lo1",tmp_float,1,nclTypefloatClass); (*nlonatts)++;

		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float =  lac * 1e-3;
		GribPushAtt(lon_att_list,"LaC",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = loc * 1e-3;
		GribPushAtt(lon_att_list,"LoC",tmp_float,1,nclTypefloatClass); (*nlonatts)++;

		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = di/1000.0;
		GribPushAtt(lon_att_list,"Di",tmp_float,1,nclTypefloatClass); (*nlonatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = dj/1000.0;
		GribPushAtt(lon_att_list,"Dj",tmp_float,1,nclTypefloatClass); (*nlonatts)++;

		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string = NrmStringToQuark("degrees_east");
		GribPushAtt(lon_att_list,"units",tmp_string,1,nclTypestringClass); (*nlonatts)++;
		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string = grid_name;
		GribPushAtt(lon_att_list,"GridType",tmp_string,1,nclTypestringClass); (*nlonatts)++;
		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string = NrmStringToQuark("longitude");
		GribPushAtt(lon_att_list,"long_name",tmp_string,1,nclTypestringClass); (*nlonatts)++;
	}
	if(lat_att_list != NULL) {
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float =  la1 * 1e-3;
		GribPushAtt(lat_att_list,"La1",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = lo1 * 1e-3;
		GribPushAtt(lat_att_list,"Lo1",tmp_float,1,nclTypefloatClass); (*nlatatts)++;

		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float =  lac * 1e-3;
		GribPushAtt(lat_att_list,"LaC",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = loc * 1e-3;
		GribPushAtt(lat_att_list,"LoC",tmp_float,1,nclTypefloatClass); (*nlatatts)++;

		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = di/1000.0;
		GribPushAtt(lat_att_list,"Di",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
		tmp_float= (float*)NclMalloc(sizeof(float));
		*tmp_float = dj/1000.0;
		GribPushAtt(lat_att_list,"Dj",tmp_float,1,nclTypefloatClass); (*nlatatts)++;
		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string = NrmStringToQuark("degrees_north");
		GribPushAtt(lat_att_list,"units",tmp_string,1,nclTypestringClass); (*nlatatts)++;
		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string = grid_name;
		grid_name = *tmp_string;
		GribPushAtt(lat_att_list,"GridType",tmp_string,1,nclTypestringClass); (*nlatatts)++;
		tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*tmp_string = NrmStringToQuark("latitude");
		GribPushAtt(lat_att_list,"long_name",tmp_string,1,nclTypestringClass); (*nlatatts)++;
	}
	if (do_rot && rot_att_list != NULL) {
		Do_Rotation_Atts(grid_name,rot_att_list,nrotatts,grid_oriented);
	}
	
}



GridGDSInfoRecord grid_gds[] = {
	{GenericUnPack,GdsUnknownGrid,"Unsupported Gds Grid"}, /*-1*/
	{GenericUnPack,GdsCEGrid,"Cylindrical Equidistant Projection Grid"}, /*0*/
	{GenericUnPack,(void *)GdsMEGrid,"Mercator Projection Grid"}, /*1*/
#if 0
	/**/		{GenericUnPack,GdsGNGrid,"Gnomonic Projection Grid"}, /*2*/
#endif
	{GenericUnPack,(void *)GdsLEGrid,"Lambert Conformal Secant or Tangent, Conical or bipolar"}, /*3*/
	{GenericUnPack,(void *)GdsGAGrid,"Gaussian Latitude/Longitude Grid"}, /*4*/
	{GenericUnPack,(void *)GdsSTGrid,"Polar Stereographic Projection Grid"}, /*5*/
#if 0
	{NULL,NULL,"Universal Transverse Mercator (UTM) Projection Grid"}, /*6*/
	{NULL,NULL,"Simple Polyconic Projection Grid"}, /*7*/
	{NULL,NULL,"Albers equal-area, secant or tangent, conic or bi-polar, Projection Grid"}, /*8*/
	{NULL,NULL,"Miller's cylindrical projection Grid"}, /*9*/
#endif
	{GenericUnPack,GdsRLLGrid,"Rotated Latitude/Longitude Grid"}, /*10*/
#if 0
	{GenericUnPack,GdsOLGrid,"Oblique Lambert conformal, secant or tangent, conical or bipolar, projection"}, /*13*/
	{NULL,NULL,"Rotated Gaussian Latitude/Longitude Grid"}, /* 14 */
	{NULL,NULL,"Stretched Latitude/Longitude Grid"}, /*20*/
	{NULL,NULL,"Stretched Gaussian Latitude/Longitude Grid"}, /*24*/
	{NULL,NULL,"Stretched and Rotated Latitude/Longitude Grid"}, /*30*/
	{NULL,NULL,"Stretched and Rotated Gaussian Latitude/Longitude Grid"}, /*34*/
#endif
	{GenericUnPack,GdsSHGrid,"Spherical Harmonic Coefficients"}, /*50*/
#if 0
	{NULL,NULL,"Rotated Spherical Harmonic Coefficients"}, /*60*/
	{NULL,NULL,"Stretched Spherical Harmonic Coefficients"}, /*70*/
	{NULL,NULL,"Stretched and Rotated Spherical Harmonic Coefficients"}, /*80*/
	{NULL,NULL,"Space View perspective or orthographic grid"}, /*90*/
#endif
	{GenericUnPack,GdsArakawaRLLGrid,"Arakawa semi-staggered E-grid on rotated latitude/longitude grid-point array"}, /*201*/
	{GenericUnPack,GdsArakawaRLLGrid,"Arakawa filled E-grid on rotated latitude/longitude grid-point array"}, /*202*/
	{GenericUnPack,GdsArakawaRLLGrid,"Arakawa staggered E-grid on rotated latitude/longitude grid-point array"}, /*203*/
	{GenericUnPack,GdsRLLGrid,"Arakawa Non-E Staggered rotated Latitude/Longitude Grid"} /*205*/
		
};

GridInfoRecord grid[] = {
	{GenericUnPack,GetGrid_1,GetAtts_1,
	 "1679-point (23x73) Mercator grid with (0,0) at (0W,48.09S), (73,23) at (0W,48.09N); I increasing eastward, Equator at J=12. Grid increment of 5 degs of longitude"}, /*01*/
	{GenericUnPack,GetGrid_2,GenAtts,
	 "10512-point (73x144) global longitude-latitude grid.  (0,0) at 0E, 90N, latitude grid.  (0,0) at 0E, 90N, matrix layout.  N.B.: prime meridian not duplicated."}, /*2*/
	{GenericUnPack,GetGrid_3,GenAtts,
	 "65160-point (181x360) global longitude-latitude grid.  (0,0) at 0E, 90N, matrix layout.  N.B.: prime meridian not duplicated."}, /*3*/
	{GenericUnPack,GetGrid_4,GenAtts,
	 "259920-point (361x720) global lon/lat grid. (0,0) at 0E, 90N; matrix layout; prime meridian not duplicated"}, /*4*/
	{GenericUnPack,GetGrid_5,GenAtts,
	 "3021-point (57x53) N. Hemisphere stereographic grid oriented 105W; Pole at (27,49). (LFM analysis)"},/*5*/
	{GenericUnPack,GetGrid_6,GenAtts,
	 "2385-point (45x53) N. Hemisphere polar stereographic grid oriented 105W; Pole at (27,49). (LFM Forecast)"}, /*6*/
	{IFOSUnPack,GetGrid_21,GenAtts,
	 "1369-point (37x37) longitude-latitude grid. 0-180E, 0-90N"}, /*21*/
	{IFOSUnPack,GetGrid_22,GenAtts,
	 "1369-point (37x37) longitude-latitude grid. 180W-0, 0-90N"}, /*22*/
	{IFOSUnPack,GetGrid_23,GenAtts,
	 "1369-point (37x37) longitude-latitude grid. 0-180E, 90S-0"}, /*23*/
	{IFOSUnPack,GetGrid_24,GenAtts,
	 "1369-point (37x37) longitude-latitude grid. 180W-0, 90S-0"}, /*24*/
	{IFOSUnPack,GetGrid_25,GenAtts,
	 "1368-point (19x72) longitude-latitude grid. 0-355E, 0-90N"}, /*25*/
	{IFOSUnPack,GetGrid_26,GenAtts,
	 "1368-point (19x72) longitude-latitude grid. 0-355E, 90S-0"}, /*26*/
	{GenericUnPack,GetGrid_27,GenAtts,
	 "4225-point (65x65) N. Hemisphere polar stereographic grid oriented 80W; Pole at (33,33)."}, /*27*/
	{GenericUnPack,GetGrid_28,GenAtts,
	 "4225-point (65x65) S. Hemisphere polar stereographic grid oriented 100E; Pole at (33,33)."}, /*28*/
	{GenericUnPack,GetGrid_29,GenAtts,
	 "5365-point (37x145) N. Hemisphere longitude/latitude grid for latitudes 0N to 90N; (0,0) at (0E,0N)."}, /*29*/
	{GenericUnPack,GetGrid_30,GenAtts,
	 "5365-point (37x145) S. Hemisphere longitude/latitude grid for latitudes 90S to 0S; (0,0) at (0E,90S)."}, /*30*/
	{GenericUnPack,GetGrid_33,GenAtts,
	 "8326-point (46x181) N. Hemisphere longitude/latitude grid for latitudes 0N to 90N; (0,0) at (0E,0N)."}, /*33*/
	{GenericUnPack,GetGrid_34,GenAtts,
	 "8326-point (46x181) S. Hemisphere longitude/latitude grid for latitudes 90S to 0S; (0,0) at (0E,90S)."}, /*34*/
	{NULL,NULL,GenAtts,
	 "3447-point (73x73) \"Thinned\" longitude-latitude grid. 330E-60E, 0-90N"}, /*37*/
	{NULL,NULL,GenAtts,
	 "3447-point (73x73) \"Thinned\" longitude-latitude grid. 60E-150E, 0-90N"}, /*38*/
	{NULL,NULL,GenAtts,
	 "3447-point (73x73) \"Thinned\" longitude-latitude grid. 150E-240E, 0-90N"}, /*39*/
	{NULL,NULL,GenAtts,
	 "3447-point (73x73) \"Thinned\" longitude-latitude grid. 240E-330E, 0-90N"}, /*40*/
	{NULL,NULL,GenAtts,
	 "3447-point (73x73) \"Thinned\" longitude-latitude grid. 330E-60E, 90S-0"}, /*41*/
	{NULL,NULL,GenAtts,
	 "3447-point (73x73) \"Thinned\" longitude-latitude grid. 60E-150E, 90S-0"}, /*42*/
	{NULL,NULL,GenAtts,
	 "3447-point (73x73) \"Thinned\" longitude-latitude grid. 150E-240E,90S-0"}, /*43*/
	{NULL,NULL,GenAtts,
	 "3447-point (73x73) \"Thinned\" longitude-latitude grid. 240E-330E, 90S-0"}, /*44*/
	{GenericUnPack,GetGrid_45,GenAtts,
	 "41760-point (145x288) Global Latitude/Longitude 1.25 deg Resoulution. 0E-358.75E, 90N-90S"},/*45*/
	{IFOS50UnPack,GetGrid_50,GenAtts,
	 "1188-point (33x36) longitude-latitude grid. 140.0W-52.5W, 20N-60N"}, /*50*/
	{GenericUnPack,GetGrid_55,GenAtts,
	 "6177-point (71x87) N. Hemisphere polar stereographic grid oriented 105W; Pole at (44,38). (2/3 bedient NH sfc anl)"}, /*55*/
	{GenericUnPack,GetGrid_56,GenAtts,
	 "6177-point (71x87) N. Hemisphere polar stereographic grid oriented 105W; Pole at (40,73). (1/3 bedient NA sfc anl)"}, /*56*/
	{IFOSUnPack,GetGrid_61,GenAtts,
	 "4186-point (46x91) longitude-latitude grid. 0-180E, 0-90N"}, /*61*/
	{IFOSUnPack,GetGrid_62,GenAtts,
	 "4186-point (46x91) longitude-latitude grid. 180W-0, 0-90N"}, /*62*/
	{IFOSUnPack,GetGrid_63,GenAtts,
	 "4186-point (46x91) longitude-latitude grid. 0-180E, 90S-0"}, /*63*/
	{IFOSUnPack,GetGrid_64,GenAtts,
	 "4186-point (46x91) longitude-latitude grid. 180W-0, 90S-0"}, /*64*/
	{NULL,NULL,GenAtts,
	 "12321-point (111x111) N. Hemisphere Lambert Conformal grid.  No fixed location; used by QLM Hurricane model."}, /*75*/
	{NULL,NULL,GenAtts,
	 "12321-point (111x111) S. Hemisphere Lambert Conformal grid.  No fixed location; used by QLM Hurricane model."}, /*76*/
	{NULL,NULL,GenAtts,
	 "12321-point (111x111) N. Hemisphere Mercator grid.  No fixed location; used by QLM Hurricane model."}, /*77*/
	{GenericUnPack,GetGrid_85,GenAtts,
	 "32400-point (90x360) N. Hemisphere longitude/latitude grid; longitudes: 0.5E to 359.5E (0.5W); latitudes: 0.5N to 89.5N; origin (0,0) at (0.5E,0.5N)"}, /*85*/
	{GenericUnPack,GetGrid_86,GenAtts,
	 "32400-point (90x360) S. Hemisphere longitude/latitude grid; longitudes: 0.5E to 359.5E (0.5W); latitudes: 89.5S to 0.5S; origin (0,0) at (0.5E,89.5S)"}, /*86*/
	{GenericUnPack,GetGrid_87,GenAtts,
	 "5022-point (62x81) N. Hemisphere  polar stereographic grid oriented at 105W. Pole at (31.91, 112.53) Used for RUC."}, /*87*/
	{GenericUnPack,GetGrid_88,GenAtts,
	 "317840 point (580x548) N. American polar stereographic grid oriented at 105W. Pole at (260.853, 613.176) Used for RSAS. (15 km at 60N)"}, /*88*/
	{NULL,NULL,GenAtts,
	 "12902-point (141x92 semi-staggered) lat. long., rotated such that center located at 52.0N, 111.0W; LL at 37.5W, 35S Unfilled E grid for 80 km ETA model"}, /*90*/
	{NULL,NULL,GenAtts,
	 "25803-point (141x183) lat. long., rotated such that center located at 52.0N, 111.0W; LL at 37.5W,35S Filled E grid for 80 km ETA model"}, /*91*/
	{NULL,NULL,GenAtts,
	 "24162-point (191x127 semi-staggered) lat. long., rotated such that center located at 41.0N, 97.0W; LL at 35W,25S Unfilled E grid for 40 km ETA model"}, /*92*/
	{NULL,NULL,GenAtts,
	 "48323-point (191x253)lat. long., rotated such that center located at 41.0N, 97.0W; LL at 35W ,25S Filled E grid for 40 km ETA model"}, /*93*/
	{NULL,NULL,GenAtts,
	 "18048-point (94x192) Global Gaussian T62 Latitude/Longitude Resolution."}, /*98*/
	{GenericUnPack,GetGrid_100,GenAtts,
	 "6889-point (83x83) N. Hemisphere polar stereographic grid oriented 105W; Pole at (40.5,88.5). (NGM Original C-Grid)"},  /*100*/
	{GenericUnPack,GetGrid_101,GenAtts,
	 "10283-point (91x113) N. Hemisphere polar stereographic grid oriented 105W; Pole at (58.5,92.5). (NGM \"Big C-Grid\")"}, /*101*/
	{GenericUnPack,GetGrid_103,GenAtts,
	 "3640-point (56x65) N. Hemisphere polar stereographic grid oriented 105W; Pole at (25.5,84.5) (used by ARL)"}, /*103*/
	{GenericUnPack,GetGrid_104,GenAtts,
	 "16170-point (110x147) N. Hemisphere polar stereographic grid oriented 105W; pole at (75.5,109.5). (NGM Super C grid)"}, /*104*/
	{GenericUnPack,GetGrid_105,GenAtts,
	 "6889-point (83x83) N. Hemisphere polar stereographic grid oriented 105W; pole at  (40.5,88.5).  (U.S. area subset of NGM Super C grid, used by ETA model)"}, /*105*/
	{GenericUnPack,GetGrid_106,GenAtts,
	 "19305-point (117x165) N. Hemisphere stereographic grid oriented 105W; pole at (80,176) Hi res. ETA (2 x resolution of Super C)"}, /*106*/
	{GenericUnPack,GetGrid_107,GenAtts,
	 "11040 point (92x120) N. Hemisphere stereographic grid oriented 105W; pole at (46,167) subset of Hi res. ETA; for ETA & MAPS/RUC"}, /*107*/
	{NULL,NULL,GenAtts,
	 "72960-point (190x384) Global Gaussian Latitude/Longitude T126 Resolution"}, /*126*/
	{GenericUnPack,GetGrid_130,GenAtts,
	 "Regional (CONUS) Lambert Conformal grid for AWIPS"}, /*130*/

	{GenericUnPack,GetGrid_160,GenAtts,
	 "AWIPS North Polar Stereographic grid for Alaska (Quadruple grid 203)"}, /*160*/
	{GenericUnPack,GetGrid_163,GenAtts,
	 "Regional (CONUS) Lambert Conformal grid"}, /*163*/
	{GenericUnPack,GetGrid_171,GenAtts,
	 "AWIPS Northern Hemisphere High Resolution Sea Ice grid (polar stereographic) "}, /*171*/
	{GenericUnPack,GetGrid_172,GenAtts,
	 "AWIPS Southern Hemisphere High Resolution Sea Ice grid (polar stereographic) "}, /*172*/
	{GenericUnPack,GetGrid_185,GenAtts,
	 "Limited domain CONUS Lambert Conformal (used by the DGEX)"}, /*185*/
#if 0
	{GenericUnPack,GetGrid_186,GenAtts,
	 "Limited domain Alaska Polar Stereographic (used by the DGEX)"}, /*186*/ 
#endif
	{GenericUnPack,GetGrid_201,GenAtts,
	 "4225-point (65x65) Hemispheric polar stereographic grid oriented 105W; pole at (33,33)"}, /*201*/
	{GenericUnPack,GetGrid_202,GenAtts,
	 "2795-point (43x65) National - CONUS polar stereographic oriented 105W; pole at (33,45)"}, /*202*/
	{GenericUnPack,GetGrid_203,GenAtts,
	 "1755-point (39x45) National - Alaska polar stereographic oriented 150W; pole at (27,37)"}, /*203*/
	{GenericUnPack,GetGrid_204,GenAtts,
	 "6324-point (68x93) National - Hawaii Mercator (0,0) is 25S,110E, (93,68) is 60.644S,109.129W"}, /*204*/
	{GenericUnPack,GetGrid_205,GenAtts,
	 "1755-point (39x45) National - Puerto Rico stereographic oriented 60W; pole at (27,57)"}, /*205*/
	{GenericUnPack,GetGrid_206,GenAtts,
	 "2091-point (41x51) Regional - Central MARD Lambert Conformal oriented 95W; pole at (30.00,169.745)"}, /*206*/
	{GenericUnPack,GetGrid_205,GenAtts,
	 "1715-point (35x49) Regional - Alaska polar stereographic oriented 150W; pole at 25,51"}, /*207*/
	{GenericUnPack,GetGrid_208,GenAtts,
	 "783-point (27x29) Regional - Hawaii mercator (0,0) is 9.343N,167.315W, (29,27) is 28.092N,145.878W"}, /*208*/
	{GenericUnPack,GetGrid_209,GenAtts,
	 "8181-point (81x101) Regional - Centeral US MARD - Double Res. Lambert Conformal oriented 95W; pole at (59.000,338.490)"}, /* 209*/
	{GenericUnPack,GetGrid_210,GenAtts,
	 "625-point (25x25) Regional - Puerto Rico mercator (0,0) is 9.000N,77.00W (25,25) is 26.422,58.625"}, /*210*/
	{GenericUnPack,GetGrid_211,GenAtts,
	 "6045-point (65x93) Regional - CONUS lambert conformal oriented 95W; pole at (53.000,178.745)"}, /*211*/
	{GenericUnPack,GetGrid_212,GenAtts,
	 "23865-point (129x185) Regional - CONUS - double resolution lambert conformal oriented 95W; pole at (105.000,256.490)"}, /* 212 */
	{GenericUnPack,GetGrid_213,GenAtts,
	 "10965-point (85x129) National - CONUS - Double Resolution polar stereographic oriented 105W; pole at (65,89)"}, /*213*/
	{GenericUnPack,GetGrid_214,GenAtts,
	 "6693-point (69x97) Regional - Alaska - Double Resolution polar stereographic oriented 150W; pole at (49,101)"}, /*214*/
	{GenericUnPack,GetGrid_215,GenAtts,
	 "AWIPS grid over the contiguous United States - Quadruple Resolution (used by the 29-km ETA Model) (Lambert Conformal"}, /*215*/
	{GenericUnPack,GetGrid_216,GenAtts,
	 "AWIPS - Grid over Alaska (polar stereographic)"}, /*216*/
	{GenericUnPack,GetGrid_217,GenAtts,
	 "AWIPS - Grid over Alaska - Double Resolution (polar stereographic)"}, /*217*/
	{GenericUnPack,GetGrid_218,GenAtts,
	 "AWIPS grid over the Contiguous United States (used by the 12-km ETA Model) (Lambert Conformal)"}, /*218*/
	{GenericUnPack,GetGrid_219,GenAtts,
	 "AWIPS grid over the Northern Hemisphere to depict SSMI-derived ice concentrations (polar stereographic)"}, /*219*/
	{GenericUnPack,GetGrid_220,GenAtts,
	 "AWIPS grid over the Southern Hemisphere to depict SSMI-derived ice concentrations (polar stereographic)"}, /*220*/
	{GenericUnPack,GetGrid_221,GenAtts,
	 "AWIPS - Regional - NOAMHI - High Resolution North American Master Grid (Lambert Conformal)"}, /*221*/
	{GenericUnPack,GetGrid_222,GenAtts,
	 "AWIPS - Regional - NOAMLO - Low Resolution North American Master Grid (Lambert Conformal)"}, /*222*/
	{GenericUnPack,GetGrid_223,GenAtts,
	 "AWIPS - Hemispheric - Double Resolution (polar stereographic)"}, /*223*/
	{GenericUnPack,GetGrid_224,GenAtts,
	 "AWIPS - Southern Hemispheric (polar stereographic)"}, /*224*/
	{GenericUnPack,GetGrid_226,GenAtts,
	 "AWIPS grid over the contiguous United States - 8X Resolution (10 km) (Used by the Radar mosaics) (Lambert Conformal)"}, /*226*/
	{GenericUnPack,GetGrid_227,GenAtts,
	 "AWIPS grid over the contiguous United States - 16X Resolution(5 km) (Used by the Radar Stage IV precipitation analyses and Satellite-derived Precipitation Estimates) (Lambert Conformal"}, /*227*/
	{GenericUnPack,GetGrid_236,GenAtts,
	 "AWIPS - Regional - CONUS (Lambert Conformal)"}, /*236*/
	{GenericUnPack,GetGrid_237,GenAtts,
	 "AWIPS - Puerto Rico FAA Regional Grid (Lambert Conformal)"}, /*237*/
	{GenericUnPack,GetGrid_240,GenAtts,
	 "AWIPS - HRAP Grid over the Contiguous United States and Puerto Rico (polar stereographic)"}, /*240*/
	{GenericUnPack,GetGrid_241,GenAtts,
	 "AWIPS - Regional - NOAMHI - High Resolution North American Grid (Lambert Conformal)"}, /*241*/
	{GenericUnPack,GetGrid_242,GenAtts,
	 "AWIPS - Grid over Alaska - Quadruple Resolution Grid (polar stereographic)"}, /*242*/
	{GenericUnPack,GetGrid_245,GenAtts,
	 "AWIPS - Regional - NOAMHI - High Resolution over Eastern US (Lambert Conformal for 8 km NMM)"}, /*245*/
	{GenericUnPack,GetGrid_246,GenAtts,
	 "AWIPS - Regional - NOAMHI - High Resolution over Western US (Lambert Conformal for 8 km NMM)"}, /*246*/
	{GenericUnPack,GetGrid_247,GenAtts,
	 "AWIPS - Regional - NOAMHI - High Resolution over Central US (Lambert Conformal for 8 km NMM)"}, /*247*/
	{GenericUnPack,GetGrid_249,GenAtts,
	 "AWIPS - Grid over Alaska for 10-km Alaska nest (Polar Stereographic)"}, /*249*/
	{GenericUnPack,GetGrid_252,GenAtts,
	 "AWIPS - Regional - CONUS (Lambert Conformal)"}, /*252*/

};

void GenAtts
#if NhlNeedProto
(GribParamList* thevarrec, GribAttInqRecList **lat_att_list_ptr, int * nlatatts, GribAttInqRecList **lon_att_list_ptr, int *nlonatts, 
 int do_rot, int grid_oriented, GribAttInqRecList **rot_att_list_ptr, int *nrotatts)
#else
(thevarrec,lat_att_list_ptr, nlatatts, lon_att_list_ptr, nlonatts, do_rot,grid_oriented,rot_att_list_ptr, nrotatts)
GribParamList* thevarrec;
GribAttInqRecList **lat_att_list_ptr;
int * nlatatts; 
GribAttInqRecList **lon_att_list_ptr;
int *nlonatts;
int do_rot;
int grid_oriented;
GribAttInqRecList **rot_att_list_ptr;
int *nrotatts;
#endif
{
	GribAttInqRecList* tmp_att_list_ptr;
	NclQuark *tmp_string = NULL;
	ng_size_t tmp_dimsizes = 1;

	tmp_att_list_ptr = (*lat_att_list_ptr);
	(*lat_att_list_ptr) = (GribAttInqRecList*)NclMalloc((unsigned)sizeof(GribAttInqRecList));
	(*lat_att_list_ptr)->next = tmp_att_list_ptr;
	(*lat_att_list_ptr)->att_inq = (GribAttInqRec*)NclMalloc((unsigned)sizeof(GribAttInqRec));
	(*lat_att_list_ptr)->att_inq->name = NrmStringToQuark("units");
	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark("degrees_north");
	(*lat_att_list_ptr)->att_inq->thevalue = (NclMultiDValData)_NclCreateVal( NULL, NULL, Ncl_MultiDValData, 0, (void*) tmp_string, NULL, 1 , &tmp_dimsizes, PERMANENT, NULL, nclTypestringClass);
	(*nlatatts)++;

	tmp_att_list_ptr = (*lat_att_list_ptr);
	(*lat_att_list_ptr) = (GribAttInqRecList*)NclMalloc((unsigned)sizeof(GribAttInqRecList));
	(*lat_att_list_ptr)->next = tmp_att_list_ptr;
	(*lat_att_list_ptr)->att_inq = (GribAttInqRec*)NclMalloc((unsigned)sizeof(GribAttInqRec));
	(*lat_att_list_ptr)->att_inq->name = NrmStringToQuark("grid_description");
	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark(grid[thevarrec->grid_tbl_index].grid_name);
	(*lat_att_list_ptr)->att_inq->thevalue = (NclMultiDValData)_NclCreateVal( NULL, NULL, Ncl_MultiDValData, 0, (void*) tmp_string, NULL, 1 , &tmp_dimsizes, PERMANENT, NULL, nclTypestringClass);
	(*nlatatts)++;

	tmp_att_list_ptr = (*lat_att_list_ptr);
	(*lat_att_list_ptr) = (GribAttInqRecList*)NclMalloc((unsigned)sizeof(GribAttInqRecList));
	(*lat_att_list_ptr)->next = tmp_att_list_ptr;
	(*lat_att_list_ptr)->att_inq = (GribAttInqRec*)NclMalloc((unsigned)sizeof(GribAttInqRec));
	(*lat_att_list_ptr)->att_inq->name = NrmStringToQuark("long_name");
	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark("latitude");
	(*lat_att_list_ptr)->att_inq->thevalue = (NclMultiDValData)_NclCreateVal( NULL, NULL, Ncl_MultiDValData, 0, (void*) tmp_string, NULL, 1 , &tmp_dimsizes, PERMANENT, NULL, nclTypestringClass);
	(*nlatatts)++;

	tmp_att_list_ptr = (*lon_att_list_ptr);
	(*lon_att_list_ptr) = (GribAttInqRecList*)NclMalloc((unsigned)sizeof(GribAttInqRecList));
	(*lon_att_list_ptr)->next = tmp_att_list_ptr;
	(*lon_att_list_ptr)->att_inq = (GribAttInqRec*)NclMalloc((unsigned)sizeof(GribAttInqRec));
	(*lon_att_list_ptr)->att_inq->name = NrmStringToQuark("units");
	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark("degrees_east");
	(*lon_att_list_ptr)->att_inq->thevalue = (NclMultiDValData)_NclCreateVal( NULL, NULL, Ncl_MultiDValData, 0, (void*) tmp_string, NULL, 1 , &tmp_dimsizes, PERMANENT, NULL, nclTypestringClass);
	(*nlonatts)++;

	tmp_att_list_ptr = (*lon_att_list_ptr);
	(*lon_att_list_ptr) = (GribAttInqRecList*)NclMalloc((unsigned)sizeof(GribAttInqRecList));
	(*lon_att_list_ptr)->next = tmp_att_list_ptr;
	(*lon_att_list_ptr)->att_inq = (GribAttInqRec*)NclMalloc((unsigned)sizeof(GribAttInqRec));
	(*lon_att_list_ptr)->att_inq->name = NrmStringToQuark("grid_description");
	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark(grid[thevarrec->grid_tbl_index].grid_name);
(*lon_att_list_ptr)->att_inq->thevalue = (NclMultiDValData)_NclCreateVal( NULL, NULL, Ncl_MultiDValData, 0, (void*) tmp_string, NULL, 1 , &tmp_dimsizes, PERMANENT, NULL, nclTypestringClass);
	(*nlonatts)++;

	tmp_att_list_ptr = (*lon_att_list_ptr);
	(*lon_att_list_ptr) = (GribAttInqRecList*)NclMalloc((unsigned)sizeof(GribAttInqRecList));
	(*lon_att_list_ptr)->next = tmp_att_list_ptr;
	(*lon_att_list_ptr)->att_inq = (GribAttInqRec*)NclMalloc((unsigned)sizeof(GribAttInqRec));
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

}
