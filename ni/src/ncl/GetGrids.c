#include <stdlib.h>
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
#include <math.h>

#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/BaseP.h>
#include <ncarg/hlu/Workstation.h>
#include <ncarg/hlu/WorkstationP.h>
#include <ncarg/hlu/ConvertersP.h>
#include <ncarg/hlu/FortranP.h>
#include <ncarg/hlu/hluutil.h>
#include <ncarg/hlu/ErrorI.h>
#include <ncarg/hlu/TransformI.h>
#include <ncarg/hlu/MapPlot.h>

typedef struct _NhlDummyWorkstationLayerPart{
	char *foo;
} NhlDummyWorkstationLayerPart;

typedef struct _NhlDummyWorkstationLayerRec{
	NhlBaseLayerPart	base;
	NhlWorkstationLayerPart	work;
	NhlDummyWorkstationLayerPart	dwork;
} NhlDummyWorkstationLayerRec;

typedef struct _NhlDummyWorkstationClassPart{
	char *foo;
} NhlDummyWorkstationClassPart;

typedef struct _NhlDummyWorkstationClassRec{
	NhlBaseClassPart		base_class;
	NhlWorkstationClassPart		work_class;
	NhlDummyWorkstationClassPart		dwork_class;
} NhlDummyWorkstationClassRec;
	
static void GenAtts(
#if	NhlNeedProto
GribParamList* thevarrec, GribAttInqRecList **lat_att_list, int * nlatatts, GribAttInqRecList **lon_att_list, int *lonatts
#endif
);


static NhlErrorTypes DummyWorkstationInitialize(
#if	NhlNeedProto
        NhlClass,	/* class */
        NhlLayer,	/* req */
        NhlLayer,	/* new */
        _NhlArgList,	/* args */
        int		/* num_args */
#endif
);


static NhlErrorTypes DummyWorkstationDestroy(
#if	NhlNeedProto
        NhlLayer           /* inst */
#endif
);



static NhlErrorTypes DummyWorkstationOpen(
#if	NhlNeedProto
	NhlLayer	/* instance */
#endif
);

static NhlErrorTypes DummyWorkstationClose(
#if	NhlNeedProto
	NhlLayer	/* instance */
#endif
);

static NhlErrorTypes DummyWorkstationActivate(
#if	NhlNeedProto
	NhlLayer	/* instance */
#endif
);

static NhlErrorTypes DummyWorkstationDeactivate(
#if	NhlNeedProto
	NhlLayer	/* instance */
#endif
);

static int	DummyWksCount = 0;

NhlDummyWorkstationClassRec NhldummyWorkstationClassRec = {
        {
/* class_name			*/	"dummyWorkstationClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlDummyWorkstationLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)&NhlworkstationClassRec,
/* cvt_table			*/	NULL,

/* resources			*/	NULL,
/* num_resources		*/	0,
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	DummyWorkstationInitialize,
/* layer_set_values		*/	NULL,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	DummyWorkstationDestroy,

/* child_resources		*/	NULL,

/* layer_draw			*/	NULL,

/* layer_pre_draw		*/	NULL,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/	NULL,
/* layer_clear			*/	NULL
        },
	{
/* current_wks_count	*/	&DummyWksCount,                
/* def_background	*/	{0.0,0.0,0.0},
/* pal			*/	NhlInheritPalette,
/* open_work		*/	DummyWorkstationOpen,
/* close_work		*/	DummyWorkstationClose,
/* activate_work	*/	DummyWorkstationActivate,
/* deactivate_work	*/	DummyWorkstationDeactivate,
/* alloc_colors		*/	DummyWorkstationOpen,
/* update_work		*/	DummyWorkstationOpen,
/* clear_work		*/	DummyWorkstationOpen,
/* lineto_work 		*/	(NhlWorkstationLineTo)DummyWorkstationOpen,
/* fill_work		*/	(NhlWorkstationFill)DummyWorkstationOpen,
/* marker_work		*/	(NhlWorkstationMarker)DummyWorkstationOpen
	},
	{
				NULL
	}
};

NhlClass NhldummyWorkstationClass = (NhlClass)&NhldummyWorkstationClassRec;

static NhlErrorTypes DummyWorkstationInitialize
#if  NhlNeedProto
(
	NhlClass	lc,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		num_args
)
#else
(lc,req,new,args,num_args)
	NhlClass 	lc;
	NhlLayer	req;
	NhlLayer	new;
	_NhlArgList	args;
	int		num_args;
#endif
{

	return(NhlNOERROR);
}



static NhlErrorTypes DummyWorkstationDestroy
#if	NhlNeedProto
(
	NhlLayer	inst
)
#else
(inst)
	NhlLayer	inst;
#endif
{
	NhlErrorTypes	retcode = NhlNOERROR;

	return(retcode);
}


static NhlErrorTypes
DummyWorkstationOpen
#if NhlNeedProto
(
	NhlLayer	l
)
#else
(l)
	NhlLayer	l;
#endif
{	
	return(NhlNOERROR);
}

static NhlErrorTypes
DummyWorkstationClose
#if NhlNeedProto
(
	NhlLayer	l
)
#else
(l)
	NhlLayer	l;
#endif
{
	return	NhlNOERROR;;
}

static NhlErrorTypes
DummyWorkstationActivate
#if NhlNeedProto
(
	NhlLayer	l
)
#else
(l)
	NhlLayer	l;
#endif
{

	return NhlNOERROR;
}

static NhlErrorTypes
DummyWorkstationDeactivate
#if NhlNeedProto
(
	NhlLayer	l
)
#else
(l)
	NhlLayer	l;
#endif
{
	NhlErrorTypes		retcode = NhlNOERROR;

	return retcode;
}









int grid_index[] = { 1, 2, 3, 4, 5, 6, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 33, 34, 37, 38, 39, 40, 41, 42, 43, 44, 45, 50, 55, 56, 61, 62, 63, 64, 75, 76, 77, 85, 86, 87, 90, 91, 92, 93, 98, 100, 101, 103, 104, 105, 106, 107, 126, 201, 202, 203, 204, 205, 206,207, 208, 209, 210, 211, 212, 213, 214 };

int grid_tbl_len = sizeof(grid_index)/sizeof(int);

int grid_gds_index[] = { 0, 1, 2, 3, 4, 5, 13, 50, 90, 201, 202 };

int grid_gds_tbl_len = sizeof(grid_gds_index)/sizeof(int);
#define EAR 6371.2213
#define ear 6367.47
#define PI 3.14159265
#define PI4  (PI/4)
#define RADDEG (180./PI)
#define EAST 1
#define WEST -1

static float er;
static float er2;
static float xax;
static float xpp;
static float ypp;
static int southern;

float pi = 3.14159265358979323846;
float pi2 = 1.57079632679489661923;
float pi4 = 0.78539816339744830962;
float degtorad = 1.745329e-02;
float radtodeg = 5.729578e+01;
double dpi = (double)3.14159265358979323846;
double rtod = (double)180.0/(double)3.14159265358979323846;
double dtor = (double)3.14159265358979323846/(double)180.0;


static int is_gpoint
#if NhlNeedProto
( unsigned char *bms, int index)
#else
(unsigned char *bms, int index)
#endif
{
	int i = 0;
	int off = 0;
	unsigned char test;
	unsigned char test1;

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
}
static void grdsetup(float x,float y,float gsp,float d,float ax) 
{
	if(d< 0) {
		southern = -1;
	} else {
		southern = 1;
	}
	er =  EAR * (1.0 + sin(fabs(d)/RADDEG)) / gsp;
	er2 = er * er;
	xax = ax;
	xpp = x;
	ypp = y;
	return;
}


static void grdloc(float xp,float yp, float *xlo, float* xla)
{
	float r2,ss;
	float yy,xx,elong;


	yy = yp - ypp;
	xx = xp - xpp;
	*xlo = 0.0;
	if((yy != 0.0)||(xx!=0) ) {
		elong = RADDEG * atan2(yy,xx);
		*xlo =  elong + xax;
		if(*xlo > 180.0) 
			*xlo = *xlo - 360.0;
		if(*xlo < -180.0)
			*xlo = *xlo + 360;
	} 
	r2 = xx * xx + yy * yy;
	ss = (er2-r2)/(er2+r2);
	*xla = southern * RADDEG * asin(ss);
} 

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

void GribPushAtt
#if NhlNeedProto
(GribAttInqRecList **att_list_ptr,char* name,void *val,int dimsize,NclObjClass type) 
#else
(att_list_ptr,name,val,dimsize,type) 
GribAttInqRecList **att_list_ptr;
char* name;
void *val;
int dimsize;
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

/*
* START Mercator
*/
void GenMercator
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon,float lat0, float lon0, float lat1, float lon1, float dx, float dy, float latin, int nx,int ny)
#else  
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, lat0, lon0, lat1, lon1, dx, dy, latin, nx, ny)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
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
	static int mapid = -1;
	static int vpid = -1;
	static int rlist = -1;
	float tlat;
	float tlon;
	float nx0,nx1,ny0,ny1;
	float C,d_per_km,dlon,dlat;
	float ndcdx,ndcdy,start_ndcx,start_ndcy,start_lon = 0.0;
	float dumx,dumy;
	int status;
	float orv;
	int i,j;
	float *dummy = NULL;

	if(mapid == -1) {
		rlist = NhlRLCreate(NhlSETRL);
		NhlRLClear(rlist);
		NhlCreate(&vpid,"Map0",NhldummyWorkstationClass,NULL,rlist);
		NhlRLClear(rlist);
		NhlRLSetFloat(rlist,NhlNvpXF,0.01);
		NhlRLSetFloat(rlist,NhlNvpYF,0.99);
		NhlRLSetFloat(rlist,NhlNvpWidthF,0.98);
		NhlRLSetFloat(rlist,NhlNvpHeightF,0.98);
		NhlRLSetString(rlist,NhlNmpProjection,"MERCATOR");
		NhlRLSetFloat(rlist,NhlNmpCenterLonF,(lon1-lon0)/2.0);
		NhlRLSetFloat(rlist,NhlNmpCenterLatF,0.0);
		NhlCreate(&mapid,"Map0",NhlmapPlotClass,vpid,rlist);
	} else {
		NhlRLClear(rlist);
		NhlRLSetString(rlist,NhlNmpProjection,"MERCATOR");
		NhlRLSetFloat(rlist,NhlNmpCenterLonF,(lon1 - lon0)/2.0);
		NhlRLSetFloat(rlist,NhlNmpCenterLatF,0.0);
		NhlSetValues(mapid,rlist);
	}
	*lat = (float*)NclMalloc(sizeof(float)*ny);
	*lon = (float*)NclMalloc(sizeof(float)*nx);
	dummy = (float*)NclMalloc(sizeof(float)* ( nx > ny ? nx : ny));
        *dimsizes_lat = (int*)NclMalloc(sizeof(int));
        *dimsizes_lon = (int*)NclMalloc(sizeof(int));
        *n_dims_lat = 1;
        *n_dims_lon = 1;
        (*dimsizes_lat)[0] = ny;
        (*dimsizes_lon)[0] = nx;

	C = 2 * pi * EAR * cos(degtorad * latin);
	d_per_km = 360.0/C;
	dlon = dx * d_per_km;
/*
* lat0 is always closest to pole
*/
/*
	tlon = (lon1-lon0) / 2.0;
	tlat = (lat1-lat0) / 2.0;
	NhlDataToNDC(mapid,&tlon,&tlat,1,&dumx,&dumy,NULL,NULL,&status,&orv);
	tlon = lon0 + dlon;
	NhlDataToNDC(mapid,&lo1,&lat0,1,&nx0,&ny0,NULL,NULL,&status,&orv);
	NhlDataToNDC(mapid,&tlon,&lat0,1,&nx1,&ny1,NULL,NULL,&status,&orv);
	ndcdx = fabs(nx0 - nx1);
	ndcdy = dy/dx * ndcdx;
	NhlDataToNDC(mapid,&lon0,&lat0,1,&nx0,&ny0,NULL,NULL,&status,&orv);
	for(i = 0; i < ny; i++) {
		(*lat)[i] = ny0 + i * ndcdy;
		dummy[i] = dumx;
	}
	NhlNDCToData(mapid,dummy,*lat,ny,dummy,*lat,NULL,NULL,&status,&orv);
	for(j = 0; j < nx; j++) {
		dummy[j] = dumy;
		(*lon)[j] = nx0 + j * ndcdx;
	}
	NhlNDCToData(mapid,*lon,dummy,nx,*lon,dummy,NULL,NULL,&status,&orv);
	for(j = 0; j < nx; j++) {
		(*lon)[j] = ((*lon)[j] < 0)? ((*lon)[j] + 360) : (*lon)[j];
	}
	NclFree(dummy);
*/
}

void GetGrid_210
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
	GenMercator(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, -25.0/*lat0*/, 110.0 /*lon0*/, 60.644 /*lat1*/, -109.129/* lon1*/, 160.0 /*dx*/, 160.0 /*dy*/, 20.0 /*latin*/, 93/*nx*/, 68/*ny*/);
}
void GetGrid_208
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
	GenMercator(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, -25.0/*lat0*/, 110.0 /*lon0*/, 60.644 /*lat1*/, -109.129/* lon1*/, 160.0 /*dx*/, 160.0 /*dy*/, 20.0 /*latin*/, 93/*nx*/, 68/*ny*/);
}
void GetGrid_204
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
	GenMercator(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, -25.0/*lat0*/, 110.0 /*lon0*/, 60.644 /*lat1*/, -109.129/* lon1*/, 160.0 /*dx*/, 160.0 /*dy*/, 20.0 /*latin*/, 93/*nx*/, 68/*ny*/);
}

void GetAtts_1
#if NhlNeedProto
(GribParamList* thevarrec, GribAttInqRecList **lat_att_list_ptr, int * nlatatts, GribAttInqRecList **lon_att_list_ptr, int *nlonatts)
#else
(thevarrec,lat_att_list_ptr, nlatatts, lon_att_list_ptr, nlonatts)
GribParamList* thevarrec;
GribAttInqRecList **lat_att_list_ptr;
int * nlatatts; 
GribAttInqRecList **lon_att_list_ptr;
int *nlonatts;
#endif
{
	GribAttInqRecList* tmp_att_list_ptr;
	NclQuark *tmp_string = NULL;
	float *tmp_float = NULL;
	int tmp_dimsizes = 1;


	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark("MERCATOR");
	GribPushAtt(lat_att_list_ptr,NhlNmpProjection,tmp_string,1,nclTypestringClass); (*nlatatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 0.0;
	GribPushAtt(lat_att_list_ptr,NhlNmpCenterLatF,tmp_float,1,nclTypefloatClass); (*nlatatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 180.0;
	GribPushAtt(lat_att_list_ptr,NhlNmpCenterLonF,tmp_float,1,nclTypefloatClass); (*nlatatts)++;

	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark("MERCATOR");
	GribPushAtt(lon_att_list_ptr,NhlNmpProjection,tmp_string,1,nclTypestringClass); (*nlonatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 0.0;
	GribPushAtt(lon_att_list_ptr,NhlNmpCenterLatF,tmp_float,1,nclTypefloatClass); (*nlonatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 180.0;
	GribPushAtt(lon_att_list_ptr,NhlNmpCenterLonF,tmp_float,1,nclTypefloatClass); (*nlonatts)++;

	GenAtts(thevarrec,lat_att_list_ptr, nlatatts, lon_att_list_ptr, nlonatts);
}
void GetGrid_1
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
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
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon,float lat0, float lat1, float lon0, float dx, float dy,float start_lat, float start_lon,int nx,int ny)
#else  
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, lat0, lat1, lon0, dx, dy, start_lat, start_lon, nx, ny)
	GribParamList* thevarrec;
	float** lat;
	int* n_dims_lat;
	int** dimsizes_lat;
	float** lon;
	int* n_dims_lon;
	int** dimsizes_lon;
	loat lat0;
	float lat1;
	float lon0;
	float dx;
	float dy;
	float start_lat;
	float start_lon;
	int nx;
	int ny;
#endif
{
	static int mapid = -1;
	static int vpid = -1;
	static int rlist = -1;
	float tlat;
	float tlon;
	float nx0,nx1,ny0,ny1;
	float C,d_per_km,dlon,dlat;
	float ndcdx,ndcdy,start_ndcx,start_ndcy;
	int status;
	float orv;
	int i,j;

	if(mapid == -1) {
		rlist = NhlRLCreate(NhlSETRL);
		NhlRLClear(rlist);
		NhlCreate(&vpid,"Map0",NhldummyWorkstationClass,NULL,rlist);
		NhlRLClear(rlist);
		NhlRLSetFloat(rlist,NhlNvpXF,0.01);
		NhlRLSetFloat(rlist,NhlNvpYF,0.99);
		NhlRLSetFloat(rlist,NhlNvpWidthF,0.98);
		NhlRLSetFloat(rlist,NhlNvpHeightF,0.98);
		NhlRLSetString(rlist,NhlNmpProjection,"LAMBERTCONFORMAL");
		NhlRLSetFloat(rlist,NhlNmpLambertParallel1F,lat0);
		NhlRLSetFloat(rlist,NhlNmpLambertParallel2F,lat1);
		NhlRLSetFloat(rlist,NhlNmpLambertMeridianF,lon0);
		NhlCreate(&mapid,"Map0",NhlmapPlotClass,vpid,rlist);
	} else {
		NhlRLClear(rlist);
		NhlRLSetFloat(rlist,NhlNmpLambertParallel1F,lat0);
		NhlRLSetFloat(rlist,NhlNmpLambertParallel2F,lat1);
		NhlRLSetFloat(rlist,NhlNmpLambertMeridianF,lon0);
		NhlSetValues(mapid,rlist);
	}
	*lat = (float*)NclMalloc(sizeof(float)*nx*ny);
	*lon = (float*)NclMalloc(sizeof(float)*nx*ny);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int)*2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int)*2);
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
		
		C = 2 * pi * EAR * cos(degtorad * lat0);
		d_per_km = 360.0/C;
		dlon = dx * d_per_km;
/*
* lat0 is always closest to pole
*/
		tlon = lon0 + dlon;
		NhlDataToNDC(mapid,&lon0,&lat0,1,&nx0,&ny0,NULL,NULL,&status,&orv);
		NhlDataToNDC(mapid,&tlon,&lat0,1,&nx1,&ny1,NULL,NULL,&status,&orv);
		ndcdx = fabs(nx0 - nx1);
		ndcdy = dy/dx * ndcdx;
		NhlDataToNDC(mapid,&start_lon,&start_lat,1,&nx0,&ny0,NULL,NULL,&status,&orv);
		for(i = 0; i < ny; i++) {
			for(j = 0; j < nx; j++) {
				(*lon)[i * nx + j] = nx0 + j * ndcdx;
				(*lat)[i * nx + j] = ny0 + i * ndcdy;
			}
		}
		NhlNDCToData(mapid,*lon,*lat,nx*ny,*lon,*lat,NULL,NULL,&status,&orv);

	} else {
/*
* Northern case
*/
		C = 2 * pi * EAR * cos(degtorad * lat0);
		d_per_km = 360.0/C;
		dlon = dx * d_per_km;
/*
* lat0 is always closest to pole
*/
		tlon = lon0 + dlon;
		NhlDataToNDC(mapid,&lon0,&lat0,1,&nx0,&ny0,NULL,NULL,&status,&orv);
		NhlDataToNDC(mapid,&tlon,&lat0,1,&nx1,&ny1,NULL,NULL,&status,&orv);
		ndcdx = fabs(nx0 - nx1);
		ndcdy = dy/dx * ndcdx;
		NhlDataToNDC(mapid,&start_lon,&start_lat,1,&nx0,&ny0,NULL,NULL,&status,&orv);
		for(i = 0; i < ny; i++) {
			for(j = 0; j < nx; j++) {
				(*lon)[i * nx + j] = nx0 + j * ndcdx;
				(*lat)[i * nx + j] = ny0 + i * ndcdy;
			}
		}
		NhlNDCToData(mapid,*lon,*lat,nx*ny,*lon,*lat,NULL,NULL,&status,&orv);
	}

}


void GetAtts_212
#if NhlNeedProto
(GribParamList* thevarrec, GribAttInqRecList **lat_att_list_ptr, int * nlatatts, GribAttInqRecList **lon_att_list_ptr, int *nlonatts)
#else
(thevarrec,lat_att_list_ptr, nlatatts, lon_att_list_ptr, nlonatts)
GribParamList* thevarrec;
GribAttInqRecList **lat_att_list_ptr;
int * nlatatts; 
GribAttInqRecList **lon_att_list_ptr;
int *nlonatts;
#endif
{
	GribAttInqRecList* tmp_att_list_ptr;
	NclQuark *tmp_string = NULL;
	float *tmp_float = NULL;
	int tmp_dimsizes = 1;


	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark("LAMBERTCONFORMAL");
	GribPushAtt(lat_att_list_ptr,NhlNmpProjection,tmp_string,1,nclTypestringClass); (*nlatatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 25.0;
	GribPushAtt(lat_att_list_ptr,NhlNmpLambertParallel1F,tmp_float,1,nclTypefloatClass); (*nlatatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 25.0;
	GribPushAtt(lat_att_list_ptr,NhlNmpLambertParallel2F,tmp_float,1,nclTypefloatClass); (*nlatatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = -95.0;
	GribPushAtt(lat_att_list_ptr,NhlNmpLambertMeridianF,tmp_float,1,nclTypefloatClass); (*nlatatts)++;

	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark("LAMBERTCONFORMAL");
	GribPushAtt(lon_att_list_ptr,NhlNmpProjection,tmp_string,1,nclTypestringClass); (*nlonatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 25.0;
	GribPushAtt(lon_att_list_ptr,NhlNmpLambertParallel1F,tmp_float,1,nclTypefloatClass); (*nlonatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 25.0;
	GribPushAtt(lon_att_list_ptr,NhlNmpLambertParallel2F,tmp_float,1,nclTypefloatClass); (*nlonatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = -95.0;
	GribPushAtt(lon_att_list_ptr,NhlNmpLambertMeridianF,tmp_float,1,nclTypefloatClass); (*nlonatts)++;

	GenAtts(thevarrec,lat_att_list_ptr, nlatatts, lon_att_list_ptr, nlonatts);
}
void GetGrid_212
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
	GenLambert(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 25.0 /*lat0*/, 25.0 /*lat1*/, -95.0 /*lon0*/, 40.63525 /*dx*/, 40.63525 /*dy*/, 12.190 /*start_lat*/,  -133.459  /*start_lon*/, 185 /*nx*/, 129 /*ny*/);
}


void GetAtts_209
#if NhlNeedProto
(GribParamList* thevarrec, GribAttInqRecList **lat_att_list_ptr, int * nlatatts, GribAttInqRecList **lon_att_list_ptr, int *nlonatts)
#else
(thevarrec,lat_att_list_ptr, nlatatts, lon_att_list_ptr, nlonatts)
GribParamList* thevarrec;
GribAttInqRecList **lat_att_list_ptr;
int * nlatatts; 
GribAttInqRecList **lon_att_list_ptr;
int *nlonatts;
#endif
{
	GribAttInqRecList* tmp_att_list_ptr;
	NclQuark *tmp_string = NULL;
	float *tmp_float = NULL;
	int tmp_dimsizes = 1;


	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark("LAMBERTCONFORMAL");
	GribPushAtt(lat_att_list_ptr,NhlNmpProjection,tmp_string,1,nclTypestringClass); (*nlatatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 25.0;
	GribPushAtt(lat_att_list_ptr,NhlNmpLambertParallel1F,tmp_float,1,nclTypefloatClass); (*nlatatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 25.0;
	GribPushAtt(lat_att_list_ptr,NhlNmpLambertParallel2F,tmp_float,1,nclTypefloatClass); (*nlatatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = -95.0;
	GribPushAtt(lat_att_list_ptr,NhlNmpLambertMeridianF,tmp_float,1,nclTypefloatClass); (*nlatatts)++;

	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark("LAMBERTCONFORMAL");
	GribPushAtt(lon_att_list_ptr,NhlNmpProjection,tmp_string,1,nclTypestringClass); (*nlonatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 25.0;
	GribPushAtt(lon_att_list_ptr,NhlNmpLambertParallel1F,tmp_float,1,nclTypefloatClass); (*nlonatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 25.0;
	GribPushAtt(lon_att_list_ptr,NhlNmpLambertParallel2F,tmp_float,1,nclTypefloatClass); (*nlonatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = -95.0;
	GribPushAtt(lon_att_list_ptr,NhlNmpLambertMeridianF,tmp_float,1,nclTypefloatClass); (*nlonatts)++;

	GenAtts(thevarrec,lat_att_list_ptr, nlatatts, lon_att_list_ptr, nlonatts);
}
void GetGrid_209
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
	GenLambert(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 25.0 /*lat0*/, 25.0 /*lat1*/, -95.0 /*lon0*/, 40.63525 /*dx*/, 40.63525 /*dy*/, 22.289 /*start_lat*/,  -117.991 /*start_lon*/, 101 /*nx*/, 81 /*ny*/);
}

void GetAtts_206
#if NhlNeedProto
(GribParamList* thevarrec, GribAttInqRecList **lat_att_list_ptr, int * nlatatts, GribAttInqRecList **lon_att_list_ptr, int *nlonatts)
#else
(thevarrec,lat_att_list_ptr, nlatatts, lon_att_list_ptr, nlonatts)
GribParamList* thevarrec;
GribAttInqRecList **lat_att_list_ptr;
int * nlatatts; 
GribAttInqRecList **lon_att_list_ptr;
int *nlonatts;
#endif
{
	GribAttInqRecList* tmp_att_list_ptr;
	NclQuark *tmp_string = NULL;
	float *tmp_float = NULL;
	int tmp_dimsizes = 1;


	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark("LAMBERTCONFORMAL");
	GribPushAtt(lat_att_list_ptr,NhlNmpProjection,tmp_string,1,nclTypestringClass); (*nlatatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 25.0;
	GribPushAtt(lat_att_list_ptr,NhlNmpLambertParallel1F,tmp_float,1,nclTypefloatClass); (*nlatatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 25.0;
	GribPushAtt(lat_att_list_ptr,NhlNmpLambertParallel2F,tmp_float,1,nclTypefloatClass); (*nlatatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = -95.0;
	GribPushAtt(lat_att_list_ptr,NhlNmpLambertMeridianF,tmp_float,1,nclTypefloatClass); (*nlatatts)++;

	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark("LAMBERTCONFORMAL");
	GribPushAtt(lon_att_list_ptr,NhlNmpProjection,tmp_string,1,nclTypestringClass); (*nlonatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 25.0;
	GribPushAtt(lon_att_list_ptr,NhlNmpLambertParallel1F,tmp_float,1,nclTypefloatClass); (*nlonatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 25.0;
	GribPushAtt(lon_att_list_ptr,NhlNmpLambertParallel2F,tmp_float,1,nclTypefloatClass); (*nlonatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = -95.0;
	GribPushAtt(lon_att_list_ptr,NhlNmpLambertMeridianF,tmp_float,1,nclTypefloatClass); (*nlonatts)++;

	GenAtts(thevarrec,lat_att_list_ptr, nlatatts, lon_att_list_ptr, nlonatts);
}
void GetGrid_206
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
	GenLambert(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 25.0 /*lat0*/, 25.0 /*lat1*/, -95.0 /*lon0*/, 81.2705 /*dx*/, 81.2705 /*dy*/, 22.289 /*start_lat*/,  -117.991 /*start_lon*/, 51 /*nx*/, 41 /*ny*/);
}


void GetAtts_211
#if NhlNeedProto
(GribParamList* thevarrec, GribAttInqRecList **lat_att_list_ptr, int * nlatatts, GribAttInqRecList **lon_att_list_ptr, int *nlonatts)
#else
(thevarrec,lat_att_list_ptr, nlatatts, lon_att_list_ptr, nlonatts)
GribParamList* thevarrec;
GribAttInqRecList **lat_att_list_ptr;
int * nlatatts; 
GribAttInqRecList **lon_att_list_ptr;
int *nlonatts;
#endif
{
	GribAttInqRecList* tmp_att_list_ptr;
	NclQuark *tmp_string = NULL;
	float *tmp_float = NULL;
	int tmp_dimsizes = 1;


	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark("LAMBERTCONFORMAL");
	GribPushAtt(lat_att_list_ptr,NhlNmpProjection,tmp_string,1,nclTypestringClass); (*nlatatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 25.0;
	GribPushAtt(lat_att_list_ptr,NhlNmpLambertParallel1F,tmp_float,1,nclTypefloatClass); (*nlatatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 25.0;
	GribPushAtt(lat_att_list_ptr,NhlNmpLambertParallel2F,tmp_float,1,nclTypefloatClass); (*nlatatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = -95.0;
	GribPushAtt(lat_att_list_ptr,NhlNmpLambertMeridianF,tmp_float,1,nclTypefloatClass); (*nlatatts)++;

	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark("LAMBERTCONFORMAL");
	GribPushAtt(lon_att_list_ptr,NhlNmpProjection,tmp_string,1,nclTypestringClass); (*nlonatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 25.0;
	GribPushAtt(lon_att_list_ptr,NhlNmpLambertParallel1F,tmp_float,1,nclTypefloatClass); (*nlonatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = 25.0;
	GribPushAtt(lon_att_list_ptr,NhlNmpLambertParallel2F,tmp_float,1,nclTypefloatClass); (*nlonatts)++;

	tmp_float= (float*)NclMalloc(sizeof(float));
	*tmp_float = -95.0;
	GribPushAtt(lon_att_list_ptr,NhlNmpLambertMeridianF,tmp_float,1,nclTypefloatClass); (*nlonatts)++;

	GenAtts(thevarrec,lat_att_list_ptr, nlatatts, lon_att_list_ptr, nlonatts);
}
void GetGrid_211
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
	GenLambert(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 25.0 /*lat0*/, 25.0 /*lat1*/, -95.0 /*lon0*/, 81.2705 /*dx*/, 81.2705 /*dy*/, 12.190 /*start_lat*/,  -133.459 /*start_lon*/, 93 /*nx*/, 65 /*ny*/);
}
/*
* END Lambert Conformal Grids
*/
/*
* START lat lon grids
*/
void GenLatLon 
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon,int xsize,int ysize, float lon_start,float lat_start, float lon_dir, float lat_dir)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon,xsize,ysize, lon_start,lat_start, lon_dir, lat_dir)
)
GribParamList* thevarrec;
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
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 360, 90, 0.5, -89.5 , 1.0, 1.0);
        return;
}

void GetGrid_85
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 360, 90, 0.5, 0.5 , 1.0, 1.0);
        return;
}
void GetGrid_64
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 91, 46, -180.0, -90.0, 2.0, 2.0);
        return;
}

void GetGrid_63
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 91, 46, 0.0, -90.0, 2.0, 2.0);
        return;
}

void GetGrid_62
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 91, 46, -180.0, 0.0, 2.0, 2.0);
        return;
}

void GetGrid_61
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 91, 46, 0.0, 0.0, 2.0, 2.0);
        return;
}

void GetGrid_50
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 36, 33, -140.0, 20.0, 2.5, 1.25);
        return;
}

void GetGrid_45
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 288, 145, 0.0, 90.0, 1.25, -1.25);
        return;
}

void GetGrid_34
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 181, 46, 0.0, -90.0, 2, 2);
        return;
}

void GetGrid_33
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 181, 46, 0.0, 0.0, 2, 2);
        return;
}

void GetGrid_30
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 145, 37, 0.0, -90.0, 2.5, 2.5);
        return;
}
void GetGrid_29
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 145, 37, 0.0, 0.0, 2.5, 2.5);
        return;
}
void GetGrid_26
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 72, 19, 0.0, -90.0, 5.0, 5.0);
        return;
}

void GetGrid_25
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 72, 19, 0.0, 0.0, 5.0, 5.0);
        return;
}

void GetGrid_24
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 37, 37, -180.0, -90.0, 5.0, 2.5);
        return;
}

void GetGrid_23
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 37, 37, 0.0, -90.0, 5.0, 2.5);
        return;
}

void GetGrid_22
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 37, 37, -180.0, 0.0, 5.0, 2.5);
        return;
}


void GetGrid_21
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 37, 37, 0.0, 0.0, 5.0, 2.5);
        return;
}


void GetGrid_4
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 720, 361, 0.0, 90.0, .5, -.5);
        return;
}

void GetGrid_3
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
        GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 360, 181, 0.0, 90.0, 1.0, -1.0);
        return;
}

void GetGrid_2
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
	GenLatLon(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon, 144, 73, 0.0, 90.0, 2.5, -2.5);
	return;
}


/*
* END lat lon grids
*/
/*
* START Polar Stereographic GRIDS
*/
void GetGrid_214
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
        int xsize = 97;
        int ysize = 69;
        float polex = 49;
        float poley = 101;
        float dist = 47.625;
        float deg = 60.0;
        float ore = -150.0;
        int x,y;


        *lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

        grdsetup(polex,poley,dist,deg, ore + 90.0 );
        for (y = 0; y < ysize; y++) {
                for(x = 0; x < xsize; x++) {
                        grdloc(x+1,y+1,&((*lon)[y * xsize + x]),&((*lat)[y * xsize + x]));
                }
        }

}

void GetGrid_213
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
        int xsize = 129;
        int ysize = 85;
        float polex = 65;
        float poley = 89;
        float dist = 95.250;
        float deg = 60.0;
        float ore = -105.0;
        int x,y;


        *lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

        grdsetup(polex,poley,dist,deg, ore + 90.0 );
        for (y = 0; y < ysize; y++) {
                for(x = 0; x < xsize; x++) {
                        grdloc(x+1,y+1,&((*lon)[y * xsize + x]),&((*lat)[y * xsize + x]));
                }
        }

}

void GetGrid_207
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
        int xsize = 49;
        int ysize = 35;
        float polex = 25;
        float poley = 51;
        float dist = 95.250;
        float deg = 60.0;
        float ore = -150.0;
        int x,y;


        *lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

        grdsetup(polex,poley,dist,deg, ore + 90.0 );
        for (y = 0; y < ysize; y++) {
                for(x = 0; x < xsize; x++) {
                        grdloc(x+1,y+1,&((*lon)[y * xsize + x]),&((*lat)[y * xsize + x]));
                }
        }

}

void GetGrid_205
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
        int xsize = 45;
        int ysize = 39;
        float polex = 27;
        float poley = 167;
        float dist = 57;
        float deg = 60.0;
        float ore = -60.0;
        int x,y;


        *lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

        grdsetup(polex,poley,dist,deg, ore + 90.0 );
        for (y = 0; y < ysize; y++) {
                for(x = 0; x < xsize; x++) {
                        grdloc(x+1,y+1,&((*lon)[y * xsize + x]),&((*lat)[y * xsize + x]));
                }
        }

}

void GetGrid_203
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
        int xsize = 45;
        int ysize = 39;
        float polex = 27;
        float poley = 37;
        float dist = 190.5;
        float deg = 60.0;
        float ore = -150.0;
        int x,y;


        *lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

        grdsetup(polex,poley,dist,deg, ore + 90.0 );
        for (y = 0; y < ysize; y++) {
                for(x = 0; x < xsize; x++) {
                        grdloc(x+1,y+1,&((*lon)[y * xsize + x]),&((*lat)[y * xsize + x]));
                }
        }

}

void GetGrid_202
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
        int xsize = 65;
        int ysize = 43;
        float polex = 33;
        float poley = 45;
        float dist = 190.5;
        float deg = 60.0;
        float ore = -105.0;
        int x,y;


        *lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

        grdsetup(polex,poley,dist,deg, ore + 90.0 );
        for (y = 0; y < ysize; y++) {
                for(x = 0; x < xsize; x++) {
                        grdloc(x+1,y+1,&((*lon)[y * xsize + x]),&((*lat)[y * xsize + x]));
                }
        }

}

void GetGrid_201
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
        int xsize = 65;
        int ysize = 65;
        float polex = 33;
        float poley = 33;
        float dist =381.0;
        float deg = 60.0;
        float ore = -105.0;
        int x,y;


        *lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

        grdsetup(polex,poley,dist,deg, ore + 90.0 );
        for (y = 0; y < ysize; y++) {
                for(x = 0; x < xsize; x++) {
                        grdloc(x+1,y+1,&((*lon)[y * xsize + x]),&((*lat)[y * xsize + x]));
                }
        }

}

void GetGrid_107
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
        int xsize = 120;
        int ysize = 92;
        float polex = 46;
        float poley = 167;
        float dist = 91.452;
        float deg = 60.0;
        float ore = -105.0;
        int x,y;


        *lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

        grdsetup(polex,poley,dist,deg, ore + 90.0 );
        for (y = 0; y < ysize; y++) {
                for(x = 0; x < xsize; x++) {
                        grdloc(x+1,y+1,&((*lon)[y * xsize + x]),&((*lat)[y * xsize + x]));
                }
        }

}

void GetGrid_106
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
        int xsize = 165;
        int ysize = 117;
        float polex = 80;
        float poley = 176;
        float dist = 91.452;
        float deg = 60.0;
        float ore = -105.0;
        int x,y;


        *lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

        grdsetup(polex,poley,dist,deg, ore + 90.0 );
        for (y = 0; y < ysize; y++) {
                for(x = 0; x < xsize; x++) {
                        grdloc(x+1,y+1,&((*lon)[y * xsize + x]),&((*lat)[y * xsize + x]));
                }
        }

}

void GetGrid_104
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
        int xsize = 147;
        int ysize = 110;
        float polex = 75.5;
        float poley = 109.5;
        float dist = 91.452;
        float deg = 60.0;
        float ore = -105.0;
        int x,y;


        *lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

        grdsetup(polex,poley,dist,deg, ore + 90.0 );
        for (y = 0; y < ysize; y++) {
                for(x = 0; x < xsize; x++) {
                        grdloc(x+1,y+1,&((*lon)[y * xsize + x]),&((*lat)[y * xsize + x]));
                }
        }

}

void GetGrid_103
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
        int xsize = 65;
        int ysize = 56;
        float polex = 25.5;
        float poley = 84.5;
        float dist = 91.452;
        float deg = 60.0;
        float ore = -105.0;
        int x,y;


        *lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

        grdsetup(polex,poley,dist,deg, ore + 90.0 );
        for (y = 0; y < ysize; y++) {
                for(x = 0; x < xsize; x++) {
                        grdloc(x+1,y+1,&((*lon)[y * xsize + x]),&((*lat)[y * xsize + x]));
                }
        }

}

void GetGrid_101
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
        int xsize = 113;
        int ysize = 91;
        float polex = 58.5;
        float poley = 92.5;
        float dist = 91.452;
        float deg = 60.0;
        float ore = -105.0;
        int x,y;


        *lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

        grdsetup(polex,poley,dist,deg, ore + 90.0 );
        for (y = 0; y < ysize; y++) {
                for(x = 0; x < xsize; x++) {
                        grdloc(x+1,y+1,&((*lon)[y * xsize + x]),&((*lat)[y * xsize + x]));
                }
        }

}

void GetGrid_100
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
        int xsize = 83;
        int ysize = 83;
        float polex = 40.5;
        float poley = 88.5;
        float dist = 91.452;
        float deg = 60.0;
        float ore = -105.0;
        int x,y;


        *lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

        grdsetup(polex,poley,dist,deg, ore + 90.0 );
        for (y = 0; y < ysize; y++) {
                for(x = 0; x < xsize; x++) {
                        grdloc(x+1,y+1,&((*lon)[y * xsize + x]),&((*lat)[y * xsize + x]));
                }
        }

}

void GetGrid_87
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
        int xsize = 81;
        int ysize = 62;
        float polex = 31.91;
        float poley = 112.53;
        float dist =  68.153;
        float deg = 60.0;
        float ore = -105.0;
        int x,y;


        *lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

        grdsetup(polex,poley,dist,deg, ore + 90.0 );
        for (y = 0; y < ysize; y++) {
                for(x = 0; x < xsize; x++) {
                        grdloc(x+1,y+1,&((*lon)[y * xsize + x]),&((*lat)[y * xsize + x]));
                }
        }

}

void GetGrid_56
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
        int xsize = 87;
        int ysize = 71;
        float polex = 40;
        float poley = 73;
        float dist =  127;
        float deg = 60.0;
        float ore = -105.0;
        int x,y;


        *lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

        grdsetup(polex,poley,dist,deg, ore + 90.0 );
        for (y = 0; y < ysize; y++) {
                for(x = 0; x < xsize; x++) {
                        grdloc(x+1,y+1,&((*lon)[y * xsize + x]),&((*lat)[y * xsize + x]));
                }
        }

}
void GetGrid_55
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
        int xsize = 87;
        int ysize = 71;
        float polex = 44;
        float poley = 38;
        float dist = 254;
        float deg = 60.0;
        float ore = -105.0;
        int x,y;


        *lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

        grdsetup(polex,poley,dist,deg, ore + 90.0 );
        for (y = 0; y < ysize; y++) {
                for(x = 0; x < xsize; x++) {
                        grdloc(x+1,y+1,&((*lon)[y * xsize + x]),&((*lat)[y * xsize + x]));
                }
        }

}

void GetGrid_28
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
        int xsize = 65;
        int ysize = 65;
        float polex = 33;
        float poley = 33;
        float dist = 381; 
        float deg = -60.0;
        float ore = 100.0;
        int x,y;


        *lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

        grdsetup(polex,poley,dist,deg, ore + 90.0 );
        for (y = 0; y < ysize; y++) {
                for(x = 0; x < xsize; x++) {
                        grdloc(x+1,y+1,&((*lon)[y * xsize + x]),&((*lat)[y * xsize + x]));
                }
        }

}

void GetGrid_27
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
        int xsize = 65;
        int ysize = 65;
        float polex = 33;
        float poley = 33;
        float dist = 381; 
        float deg = 60.0;
        float ore = 80.0;
        int x,y;


        *lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

        grdsetup(polex,poley,dist,deg, ore + 90.0 );
        for (y = 0; y < ysize; y++) {
                for(x = 0; x < xsize; x++) {
                        grdloc(x+1,y+1,&((*lon)[y * xsize + x]),&((*lat)[y * xsize + x]));
                }
        }

}

void GetGrid_6
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
        int xsize = 53;
        int ysize = 45;
        float polex = 27;
        float poley = 49;
        float dist = 190.5 ;
        float deg = 60.0;
        float ore = -105.0;
        int x,y;


        *lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

        grdsetup(polex,poley,dist,deg, ore + 90.0 );
        for (y = 0; y < ysize; y++) {
                for(x = 0; x < xsize; x++) {
                        grdloc(x+1,y+1,&((*lon)[y * xsize + x]),&((*lat)[y * xsize + x]));
                }
        }

}

void GetGrid_5
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
	int xsize = 53;
	int ysize = 57;
	float polex = 27;
	float poley = 49;
	float dist = 190.5 ;
	float deg = 60.0;
	float ore = -105.0;
	int x,y;
	
	
	*lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
	*lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
	*dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
	*dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
	*n_dims_lat = 2;
	*n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

	grdsetup(polex,poley,dist,deg, ore + 90.0 );
	for (y = 0; y < ysize; y++) {
		for(x = 0; x < xsize; x++) {
			grdloc(x+1,y+1,&((*lon)[y * xsize + x]),&((*lat)[y * xsize + x]));
		}
	}
	
}

void GetGrid_105
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
/*
* Very tricky need to figure out how to exchange dimensions correctly
*/
	int xsize = 83;
	int ysize = 83;
	float polex = 40.5;
	float poley = 88.5;
	float dist = 90.75464;
	float deg = 60.0;
	float ore = -105.0;
	int x,y;
	
	
	*lat = (float*)NclMalloc(sizeof(float) * xsize * ysize);
	*lon= (float*)NclMalloc(sizeof(float) * xsize * ysize);
	*dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
	*dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
	*n_dims_lat = 2;
	*n_dims_lon = 2;
        (*dimsizes_lat)[0] = ysize;
        (*dimsizes_lat)[1] = xsize;
        (*dimsizes_lon)[0] = ysize;
        (*dimsizes_lon)[1] = xsize;

	grdsetup(polex,poley,dist,deg, ore + 90.0 );
	for (y = 0; y < ysize; y++) {
		for(x = 0; x < xsize; x++) {
			grdloc(x+1,y+1,&((*lon)[y * xsize + x]),&((*lat)[y * xsize + x]));
		}
	}
	
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
	int dnum = 0;
	int total_gpoints = 0;
	int grid_size = 0;
	unsigned char *bms = NULL;
	int numeric = 0;
	


	bds = (unsigned char*)NclMalloc((unsigned)therec->bds_size + 4); /* 4 added so that array bounds will never be ovewitten*/
	lseek(fd,therec->start + therec->bds_off,SEEK_SET);
	read(fd,(void*)bds,therec->bds_size);
	bds[therec->bds_size] = (char)0;
	bds[therec->bds_size +1] = (char)0;
	bds[therec->bds_size + 2] = (char)0;
	bds[therec->bds_size + 3] = (char)0;

        if(therec->has_bms) {
                bms = (unsigned char*)NclMalloc((unsigned)therec->bms_size);
                lseek(fd,therec->start + therec->bms_off,SEEK_SET);
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
			if(integer) {
				data = (void*)NclMalloc((unsigned)sizeof(int)*grid_size);
			} else {
				data = (void*)NclMalloc((unsigned)sizeof(float)*grid_size);
			}
			while((index < grid_size)&&(dnum < total)) {
				if(is_gpoint(bms,index)) {
					X = UnsignedCnvtToDecimal(4,&(bds[i]));
					X = X << bboff;
					X = X >> (isize - number_of_bits);
					if(integer) {
						((int*)data)[index] = (int)(reference_value + (X * pow(2.0,(double)binary_scale_factor)))/pow(10.0,(double)(decimal_scale_factor));
						index++;
						dnum++;

					} else {
						((float*)data)[index] = (float)(reference_value + (X * pow(2.0,(double)binary_scale_factor)))/pow(10.0,(double)(decimal_scale_factor));
						index++;
						dnum++;
					}
/*
					if((index > 1)&&(((float*)data)[index] > ((float*)data)[index-1] + 500)) {
						fprintf(stdout,"index:%d\n",index);
					} else if((index > 1)&&(((float*)data)[index] + 500 < ((float*)data)[index-1] )) {
						fprintf(stdout,"index:%d\n",index);
					}
*/

					tbits += number_of_bits;
					i = (int)(tbits/8.0) + 11;
					bboff = tbits % 8;
				} else {
					 if(integer) {
                                                ((int*)data)[index] = DEFAULT_MISSING_INT;
                                                index++;

                                        } else {
                                                ((float*)data)[index] = DEFAULT_MISSING_FLOAT;
                                                index++;
                                        }
				}
			}
			*outdat = data;
		} else {
			total = thevarrec->var_info.dim_sizes[thevarrec->var_info.num_dimensions-1] * thevarrec->var_info.dim_sizes[thevarrec->var_info.num_dimensions-2];
			if(integer) {
				data = (void*)NclMalloc((unsigned)sizeof(int)*total);
				for(i = 0; i < total; i++) {
					((int*)data)[i]= (int)reference_value;
				}
			} else {
				data = (void*)NclMalloc((unsigned)sizeof(float)*total);
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
	int lon= 0;
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
	lseek(fd,therec->start + therec->bds_off,SEEK_SET);
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
				data = (void*)NclMalloc((unsigned)sizeof(int)*36*33);
			} else {
				data = (void*)NclMalloc((unsigned)sizeof(float)*36*33);
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
				data = (void*)NclMalloc((unsigned)sizeof(int)*total);
				for(i = 0; i < total; i++) {
					((int*)data)[i]= (int)reference_value;
				}
			} else {
				data = (void*)NclMalloc((unsigned)sizeof(float)*total);
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
	int grid_size = 0;
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
	int total_gpoints=0;
	int ttt = 0;
	


	bds = (unsigned char*)NclMalloc((unsigned)therec->bds_size + 4);
	lseek(fd,therec->start + therec->bds_off,SEEK_SET);
	read(fd,(void*)bds,therec->bds_size);
	bds[therec->bds_size] = (char)0;
	bds[therec->bds_size +1] = (char)0;
	bds[therec->bds_size + 2] = (char)0;
	bds[therec->bds_size + 3] = (char)0;

	if(therec->has_bms) {
		bms = (unsigned char*)NclMalloc((unsigned)therec->bms_size);
		lseek(fd,therec->start + therec->bms_off,SEEK_SET);
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
				data = (void*)NclMalloc((unsigned)sizeof(int)*grid_size );
			} else {
				data = (void*)NclMalloc((unsigned)sizeof(float)*grid_size );
			}
			*outdat = data;
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
		} else {
			total = thevarrec->var_info.dim_sizes[thevarrec->var_info.num_dimensions-1] * thevarrec->var_info.dim_sizes[thevarrec->var_info.num_dimensions-2];
			if(integer) {
				data = (void*)NclMalloc((unsigned)sizeof(int)*total);
				for(i = 0; i < total; i++) {
					((int*)data)[i]= (int)reference_value;
				}
			} else {
				data = (void*)NclMalloc((unsigned)sizeof(float)*total);
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

void GdsMEGrid
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
			*lat = NULL;
			*n_dims_lat = 0;
			*dimsizes_lat = NULL;
			*lon = NULL;
			*n_dims_lon= 0;
			*dimsizes_lon= NULL;
}
void GdsGNGrid
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
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
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
	static int mapid = -1;
	static int vpid = -1;
	static int rlist = -1;
	int nx;
	int ny;
	float la1;
	float lo1;
	float lov,tlon;
	float dx;
	float dy;
	float deltax;
	float deltay;
	float latin1;
	float latin2;
	int north;
	unsigned char tmpc[4];
	int status,idir,jdir,i,j;
	unsigned char *gds = (unsigned char*)thevarrec->thelist->rec_inq->gds;
	float orv;
	float nx0,nx1,ny0,ny1;
	float C,d_per_km,dlon,dlat;
	float start_ndcx,start_ndcy;

	


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
	latin1 = UnsignedCnvtToDecimal(3,tmpc)/1000.0;
	latin1 = ((gds[28] & (unsigned char) 0200)? -latin1:latin1);

	tmpc[0] = gds[31] & (unsigned char) 0177;
	tmpc[1] = gds[32];
	tmpc[2] = gds[33];
	latin2 = UnsignedCnvtToDecimal(3,tmpc)/1000.0;
	latin2 = ((gds[28] & (unsigned char) 0200)? -latin2:latin2);

        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
        *n_dims_lat = 2;
        *n_dims_lon = 2;
        (*dimsizes_lat)[0] = ny;
        (*dimsizes_lat)[1] = nx;
        (*dimsizes_lon)[0] = ny;
        (*dimsizes_lon)[1] = nx;
	*lat = (float*)NclMalloc(sizeof(float)*nx*ny);
	*lon = (float*)NclMalloc(sizeof(float)*nx*ny);
	north= ((unsigned char)0200 & (unsigned char)gds[26])?1:0;
	idir = ((unsigned char)0200 & (unsigned char)gds[27])?-1:1;
	jdir = ((unsigned char)0100 & (unsigned char)gds[27])?1:-1;
	if((latin1 < 0)&&(latin2 < 0)) {
		
		if(mapid == -1) {
			rlist = NhlRLCreate(NhlSETRL);
			NhlRLClear(rlist);
			NhlCreate(&vpid,"Map0",NhldummyWorkstationClass,NULL,rlist);
			NhlRLClear(rlist);
			NhlRLSetFloat(rlist,NhlNvpXF,0.01);
			NhlRLSetFloat(rlist,NhlNvpYF,0.99);
			NhlRLSetFloat(rlist,NhlNvpWidthF,0.98);
			NhlRLSetFloat(rlist,NhlNvpHeightF,0.98);
			NhlRLSetString(rlist,NhlNmpProjection,"LAMBERTCONFORMAL");
			NhlRLSetFloat(rlist,NhlNmpLambertParallel1F,(latin1<latin2)?latin1:latin2);
			NhlRLSetFloat(rlist,NhlNmpLambertParallel2F,(latin1<latin2)?latin2:latin1);
			NhlRLSetFloat(rlist,NhlNmpLambertMeridianF,lov);
			NhlCreate(&mapid,"Map0",NhlmapPlotClass,vpid,rlist);
		} else {
			NhlRLClear(rlist);
			NhlRLSetFloat(rlist,NhlNmpLambertParallel1F,(latin1<latin2)?latin1:latin2);
			NhlRLSetFloat(rlist,NhlNmpLambertParallel2F,(latin1<latin2)?latin2:latin1);
			NhlRLSetFloat(rlist,NhlNmpLambertMeridianF,lov);
			NhlSetValues(mapid,rlist);
		}
		C = 2 * pi * EAR * cos(degtorad * latin1)*1000.0;
		d_per_km = 360.0/C;
		dlon = dx * d_per_km;
/*
* latin1 is always closest to pole
*/
		tlon = lov + dlon;
		NhlDataToNDC(mapid,&lov,((latin1<latin2)?&latin1:&latin2),1,&nx0,&ny0,NULL,NULL,&status,&orv);
		NhlDataToNDC(mapid,&lov,((latin1<latin2)?&latin1:&latin2),1,&nx1,&ny1,NULL,NULL,&status,&orv);
		deltax = fabs(nx0 - nx1);
		deltay = dy/dx * deltax;
		NhlDataToNDC(mapid,&lo1,&la1,1,&nx0,&ny0,NULL,NULL,&status,&orv);
		for(j = 0; j < ny; j++) {
			for(i = 0; i < nx; i++) {
				(*lon)[j * nx + i] = nx0 + idir * i * deltax;
				(*lat)[j * nx + i] = ny0 + jdir * j * deltay;
			}
		}
		NhlNDCToData(mapid,*lon,*lat,nx*ny,*lon,*lat,NULL,NULL,&status,&orv);
	} else {
		if(mapid == -1) {
			rlist = NhlRLCreate(NhlSETRL);
			NhlRLClear(rlist);
			NhlCreate(&vpid,"Map0",NhldummyWorkstationClass,NULL,rlist);
			NhlRLClear(rlist);
			NhlRLSetFloat(rlist,NhlNvpXF,0.01);
			NhlRLSetFloat(rlist,NhlNvpYF,0.99);
			NhlRLSetFloat(rlist,NhlNvpWidthF,0.98);
			NhlRLSetFloat(rlist,NhlNvpHeightF,0.98);
			NhlRLSetString(rlist,NhlNmpProjection,"LAMBERTCONFORMAL");
			NhlRLSetFloat(rlist,NhlNmpLambertParallel1F,(latin1>latin2)?latin1:latin2);
			NhlRLSetFloat(rlist,NhlNmpLambertParallel2F,(latin1>latin2)?latin2:latin1);
			NhlRLSetFloat(rlist,NhlNmpLambertMeridianF,lov);
			NhlCreate(&mapid,"Map0",NhlmapPlotClass,vpid,rlist);
		} else {
			NhlRLClear(rlist);
			NhlRLSetFloat(rlist,NhlNmpLambertParallel1F,(latin1>latin2)?latin1:latin2);
			NhlRLSetFloat(rlist,NhlNmpLambertParallel2F,(latin1>latin2)?latin2:latin1);
			NhlRLSetFloat(rlist,NhlNmpLambertMeridianF,lov);
			NhlSetValues(mapid,rlist);
		}
/*
* Northern case
*/
		C = 2 * pi * EAR * cos(degtorad * latin1)*1000.0;
		d_per_km = 360.0/C;
		dlon = dx * d_per_km;
/*
* latin1 is always closest to pole
*/
		tlon = lov + dlon;
		NhlDataToNDC(mapid,&lov,(latin1>latin2)?&latin1:&latin2,1,&nx0,&ny0,NULL,NULL,&status,&orv);
		NhlDataToNDC(mapid,&tlon,(latin1>latin2)?&latin1:&latin2,1,&nx1,&ny1,NULL,NULL,&status,&orv);
		deltax = fabs(nx0 - nx1);
		deltay = dy/dx * deltax;
		NhlDataToNDC(mapid,&lo1,&la1,1,&nx0,&ny0,NULL,NULL,&status,&orv);
		for(j = 0; j < ny; j++) {
			for(i = 0; i < nx; i++) {
				(*lon)[j * nx + i] = nx0 + idir * i * deltax;
				(*lat)[j * nx + i] = ny0 + jdir * j * deltay;
			}
		}
		NhlNDCToData(mapid,*lon,*lat,nx*ny,*lon,*lat,NULL,NULL,&status,&orv);
	}


/*
	deg_per_gp_y = 360.0 * (dy / (2 * pi * (ear*1000.0)));
	tmp_x[0] = lov;
	tmp_x[1] = lov;
	tmp_x[2] = lo1;
	tmp_y[0] = latin1 - deg_per_gp_y/2.0;
	tmp_y[1] = latin1 + deg_per_gp_y/2.0;
	tmp_y[2] = la1;
	NhlDataToNDC(mapid,tmp_x,tmp_y,3,tmp_x,tmp_y,NULL,NULL,&status,&orv);
	deltay = (float)fabs((double)(tmp_y[0] - tmp_y[1]));
	deltax = (dx/dy) * deltay;
	ndc_x_start = tmp_x[2];
	ndc_y_start = tmp_y[2];
	for(j = 0; j < ny; j++) {
		for(i = 0; i < nx; i++) {
			(*lon)[j * nx + i] = ndc_x_start + idir * i * deltax;
			(*lat)[j * nx + i] = ndc_y_start + jdir * j * deltay;
		}
	}
	NhlNDCToData(mapid,*lon,*lat,nx*ny,*lon,*lat,NULL,NULL,&status,&orv);

*/	
	
	
	



}

void GdsGAGrid
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(GribParamList* thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
		int nlat;
		unsigned char tmpc[4];
		double *theta;
		double *wts;
		int lwork= 0;
		double *work = NULL;
		int i,ierror,tmp,k;
		double la1;
		double la2;
		int ila1;
		int ila2;
		int ilo1;
		int ilo2;
		int loinc;

		if((thevarrec->thelist != NULL)&&(thevarrec->thelist->rec_inq != NULL)) {
			*n_dims_lat = 1;
			*dimsizes_lat = malloc(sizeof(int));
			(*dimsizes_lat)[0] = (int)UnsignedCnvtToDecimal(2,&(thevarrec->thelist->rec_inq->gds[8]));

			nlat = 2 * UnsignedCnvtToDecimal(2,&(thevarrec->thelist->rec_inq->gds[25]));
			theta = (double*)NclMalloc(sizeof(double)*nlat);
			wts = (double*)NclMalloc(sizeof(double)*nlat);
			lwork = 4 * nlat*(nlat+1)+2;
			work = (double*)NclMalloc(sizeof(double)*lwork);
			*lat = (float*)NclMalloc(sizeof(float)*nlat);
/*
* These come out south to north
*/
			NGCALLF(gaqdncl,GAQDNCL)(&nlat,theta,wts,work,&lwork,&ierror);
			NclFree(work);
			NclFree(wts);

			tmpc[0] = thevarrec->thelist->rec_inq->gds[13] & (char)0177;
			tmpc[1] = thevarrec->thelist->rec_inq->gds[14];
			tmpc[2] = thevarrec->thelist->rec_inq->gds[15];
			ilo1 = ((thevarrec->thelist->rec_inq->gds[13] & (char)0200) ? -1:1)*(int)UnsignedCnvtToDecimal(3,tmpc);
			tmpc[0] = thevarrec->thelist->rec_inq->gds[20] & (char)0177;
			tmpc[1] = thevarrec->thelist->rec_inq->gds[21];
			tmpc[2] = thevarrec->thelist->rec_inq->gds[22];
			ilo2 = ((thevarrec->thelist->rec_inq->gds[20] & (char)0200) ? -1:1)*(int)UnsignedCnvtToDecimal(3,tmpc);

			tmpc[0] = thevarrec->thelist->rec_inq->gds[23];
			tmpc[1] = thevarrec->thelist->rec_inq->gds[24];
			loinc = (int)UnsignedCnvtToDecimal(2,tmpc);


			tmpc[0] = thevarrec->thelist->rec_inq->gds[10] & (char)0177;
			tmpc[1] = thevarrec->thelist->rec_inq->gds[11];
			tmpc[2] = thevarrec->thelist->rec_inq->gds[12];
			ila1 = ((thevarrec->thelist->rec_inq->gds[10] & (char)0200) ? -1:1)*(int)UnsignedCnvtToDecimal(3,tmpc);
			tmpc[0] = thevarrec->thelist->rec_inq->gds[17] & (char)0177;
			tmpc[1] = thevarrec->thelist->rec_inq->gds[18];
			tmpc[2] = thevarrec->thelist->rec_inq->gds[19];
			ila2 = ((thevarrec->thelist->rec_inq->gds[17] & (char)0200) ? -1:1)*(int)UnsignedCnvtToDecimal(3,tmpc);

			if(!(thevarrec->thelist->rec_inq->gds[27] & (char)0100)) {
/* -j direction implies north to south*/
				i = nlat -1;
				while(i >= 0) {
					if((ila1 == (int)(rtod*theta[i] * 1000.0) - 90000)||(ila1 == (int)(rtod*theta[i] * 1000.0 + .5) - 90000)) {
						break;
					} else {
						i--;	
					}
				}
				k = 0;
				while((k<(*dimsizes_lat)[0])&&(i>=0)) {
					if((ila2 == (int)(rtod*theta[i] * 1000.0) - 90000)||(ila2 == (int)(rtod*theta[i] * 1000.0+.5) - 90000)) {
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
				i = 0;
				while(i<nlat) {
					if((ila1 == (int)(rtod*theta[i] * 1000.0 + .5) - 90000)||(ila1 == (int)(rtod*theta[i] * 1000.0) - 90000)) {
						break;
					} else {
						i++;		
					}
				}
				k = 0;
				while((i<nlat)&&(k<(*dimsizes_lat)[0])) {
					if((ila2 == (int)(rtod*theta[i] * 1000.0 + .5) - 90000)||(ila2 == (int)(rtod*theta[i] * 1000.0) - 90000)) {
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
			*n_dims_lon = 1;
			*dimsizes_lon = malloc(sizeof(int));
			(*dimsizes_lon)[0] = UnsignedCnvtToDecimal(2,&(thevarrec->thelist->rec_inq->gds[6]));
			*lon = malloc(sizeof(float)*UnsignedCnvtToDecimal(2,&(thevarrec->thelist->rec_inq->gds[6])));
			if(!(thevarrec->thelist->rec_inq->gds[27] & (char)0200)) {
/* +i direction  west to east */
				for(i = 0; i < (*dimsizes_lon)[0]; i++) {
					(*lon)[i] = (ilo1 + i*loinc)/1000.0;
				}
				
			} else {
/* -i direction  east to west*/
				for(i = 0; i < (*dimsizes_lon)[0]; i++) {
					(*lon)[i] = (ilo1 - i*loinc)/1000.0;
				}
				
			}
			NclFree(theta);
		} else {
			*lat = NULL;
			*n_dims_lat = 0;
			*dimsizes_lat = NULL;
			*lon = NULL;
			*n_dims_lon= 0;
			*dimsizes_lon= NULL;
		}
}
void GdsSTGrid
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
	static int mapid = -1;
	static int vpid = -1;
	static int rlist = -1;
	int nx;
	int ny;
	float la1;
	float lo1;
	float lov,tlon;
	float tlat;
	float dx;
	float dy;
	float deltax;
	float deltay;
	float latin0;
	int north;
	unsigned char tmpc[4];
	int status,idir,jdir,i,j;
	unsigned char *gds = (unsigned char*)thevarrec->thelist->rec_inq->gds;
	float orv;
	float nx0,nx1,ny0,ny1;
	float C,d_per_km,dlon,dlat;
	float start_ndcx,start_ndcy;

	


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

        *dimsizes_lat = (int*)NclMalloc(sizeof(int) * 2);
        *dimsizes_lon = (int*)NclMalloc(sizeof(int) * 2);
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

	if(north) {
		if(mapid == -1) {
			rlist = NhlRLCreate(NhlSETRL);
			NhlRLClear(rlist);
			NhlCreate(&vpid,"Map0",NhldummyWorkstationClass,NULL,rlist);
			NhlRLClear(rlist);
			NhlRLSetFloat(rlist,NhlNvpXF,0.01);
			NhlRLSetFloat(rlist,NhlNvpYF,0.99);
			NhlRLSetFloat(rlist,NhlNvpWidthF,0.98);
			NhlRLSetFloat(rlist,NhlNvpHeightF,0.98);
			NhlRLSetString(rlist,NhlNmpProjection,"STEREOGRAPHIC");
			NhlRLSetFloat(rlist,NhlNmpCenterLatF,90.0);
			NhlRLSetFloat(rlist,NhlNmpCenterLonF,lov);
			NhlCreate(&mapid,"Map0",NhlmapPlotClass,vpid,rlist);
		} else {
			NhlRLClear(rlist);
			NhlRLSetFloat(rlist,NhlNmpCenterLatF,90.0);
			NhlRLSetFloat(rlist,NhlNmpCenterLonF,lov);
			NhlSetValues(mapid,rlist);
		}
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

		NhlDataToNDC(mapid,&lov,&latin0,1,&nx0,&ny0,NULL,NULL,&status,&orv);
		NhlDataToNDC(mapid,&tlon,&latin0,1,&nx1,&ny1,NULL,NULL,&status,&orv);
		deltax = fabs(nx0 - nx1);
		deltay = dy/dx * deltax;
		NhlDataToNDC(mapid,&lo1,&la1,1,&nx0,&ny0,NULL,NULL,&status,&orv);
		for(j = 0; j < ny; j++) {
			for(i = 0; i < nx; i++) {
				(*lon)[j * nx + i] = nx0 + idir * i * deltax;
				(*lat)[j * nx + i] = ny0 + jdir * j * deltay;
			}
		}
		NhlNDCToData(mapid,*lon,*lat,nx*ny,*lon,*lat,NULL,NULL,&status,&orv);
	} else {
		if(mapid == -1) {
			rlist = NhlRLCreate(NhlSETRL);
			NhlRLClear(rlist);
			NhlCreate(&vpid,"Map0",NhldummyWorkstationClass,NULL,rlist);
			NhlRLClear(rlist);
			NhlRLSetFloat(rlist,NhlNvpXF,0.01);
			NhlRLSetFloat(rlist,NhlNvpYF,0.99);
			NhlRLSetFloat(rlist,NhlNvpWidthF,0.98);
			NhlRLSetFloat(rlist,NhlNvpHeightF,0.98);
			NhlRLSetString(rlist,NhlNmpProjection,"STEREOGRAPHIC");
			NhlRLSetFloat(rlist,NhlNmpCenterLatF,-90.0);
			NhlRLSetFloat(rlist,NhlNmpCenterLonF,lov);
			NhlCreate(&mapid,"Map0",NhlmapPlotClass,vpid,rlist);
		} else {
			NhlRLClear(rlist);
			NhlRLSetFloat(rlist,NhlNmpCenterLatF,-90.0);
			NhlRLSetFloat(rlist,NhlNmpCenterLonF,lov);
			NhlSetValues(mapid,rlist);
		}
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
		NhlDataToNDC(mapid,&tlon,&latin0,1,&nx0,&ny0,NULL,NULL,&status,&orv);
		NhlDataToNDC(mapid,&lov,&latin0,1,&nx1,&ny1,NULL,NULL,&status,&orv);
		deltax = fabs(nx0 - nx1);
		deltay = dy/dx * deltax;
		NhlDataToNDC(mapid,&lo1,&la1,1,&nx0,&ny0,NULL,NULL,&status,&orv);
		for(j = 0; j < ny; j++) {
			for(i = 0; i < nx; i++) {
				(*lon)[j * nx + i] = nx0 + idir * i * deltax;
				(*lat)[j * nx + i] = ny0 + jdir * j * deltay;
			}
		}
		NhlNDCToData(mapid,*lon,*lat,nx*ny,*lon,*lat,NULL,NULL,&status,&orv);
	}
	
}
void GdsOLGrid
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec, lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
			*lat = NULL;
			*n_dims_lat = 0;
			*dimsizes_lat = NULL;
			*lon = NULL;
			*n_dims_lon= 0;
			*dimsizes_lon= NULL;
}

void GdsCEGrid
#if NhlNeedProto
(GribParamList* thevarrec, float** lat, int* n_dims_lat, int** dimsizes_lat, float** lon, int* n_dims_lon, int** dimsizes_lon)
#else
(thevarrec,lat, n_dims_lat, dimsizes_lat, lon, n_dims_lon, dimsizes_lon)
GribParamList* thevarrec;
float** lat;
int* n_dims_lat;
int** dimsizes_lat;
float** lon;
int* n_dims_lon;
int** dimsizes_lon;
#endif
{
	unsigned char *gds;
	int la1;
	int lo1;
	int la2;
	int lo2;
	int di;
	int dj;
	int latXlon;
	int idir;
	int jdir;
	int has_dir_inc;
	int vectors;
	int is_thinned_lat;
	int is_thinned_lon;
	unsigned char tmp[4];
	int sign;
	int i;
	
	

	if((thevarrec->thelist != NULL)&&(thevarrec->thelist->rec_inq != NULL)) {
		gds = thevarrec->thelist->rec_inq->gds;
		if(gds != NULL) {
			*dimsizes_lat = (int*)NclMalloc(sizeof(int));
			*dimsizes_lon = (int*)NclMalloc(sizeof(int));
/*
* Check for thinned grids
*/
			is_thinned_lat = ((gds[6]==(char)0377)&&(gds[7] ==(char)0377)) ? 1 : 0;
			is_thinned_lon = ((gds[8]==(char)0377)&&(gds[9] ==(char)0377)) ? 1 : 0;
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
			di = CnvtToDecimal(2,&gds[23]);
			dj = CnvtToDecimal(2,&gds[25]);
			has_dir_inc = ((char)0200 & gds[16]) ? 1 : 0;

			if(is_thinned_lat) {
			
				fprintf(stdout,"thinned lat\n");
			*lat = NULL;
			*n_dims_lat = 0;
			*dimsizes_lat = NULL;
			*lon = NULL;
			*n_dims_lon= 0;
			*dimsizes_lon= NULL;
			} else if (is_thinned_lon) {
				fprintf(stdout,"thinned lon\n");
			*lat = NULL;
			*n_dims_lat = 0;
			*dimsizes_lat = NULL;
			*lon = NULL;
			*n_dims_lon= 0;
			*dimsizes_lon= NULL;

			} else {
				*(*dimsizes_lon) = CnvtToDecimal(2,&(gds[6]));
				*(*dimsizes_lat) = CnvtToDecimal(2,&(gds[8]));
				*n_dims_lat = 1;
				*n_dims_lon = 1;
				*lat = (float*)NclMalloc((unsigned)sizeof(float)* (*(*dimsizes_lat)));
				*lon = (float*)NclMalloc((unsigned)sizeof(float)* (*(*dimsizes_lon)));
				for(i = 0;i < *(*dimsizes_lat) ; i++) {
					(*lat)[i] = (float)(la1 + jdir * i * dj) / 1000.0;
				}
				for(i = 0;i < *(*dimsizes_lon) ; i++) {
					(*lon)[i] = (float)(lo1 + idir * i * di) / 1000.0;
				}
			}
		} else {
			*lat = NULL;
			*n_dims_lat = 0;
			*dimsizes_lat = NULL;
			*lon = NULL;
			*n_dims_lon= 0;
			*dimsizes_lon= NULL;
		}
	}

	return;
}




GridInfoRecord grid_gds[] = {
		GenericUnPack,GdsCEGrid,NULL,"Cylindrical Equidistant Projection Grid", /*0*/
/**/		GenericUnPack,GdsMEGrid,NULL,"Mercator Projection Grid", /*1*/
/**/		GenericUnPack,GdsGNGrid,NULL,"Gnomonic Projection Grid", /*2*/
		GenericUnPack,GdsLEGrid,NULL,"Lambert Conformal Secant or Tangent, Conical or bipolar", /*3*/
		GenericUnPack,GdsGAGrid,NULL,"Gaussian Latitude/Longitude Grid", /*4*/
		GenericUnPack,GdsSTGrid,NULL,"Polar Stereographic Projection Grid", /*5*/
/**/		GenericUnPack,GdsOLGrid,NULL,"Oblique Lambert conformal, secant or tangent, conical or bipolar, projection", /*13*/
		NULL,NULL,NULL,"Spherical Harmonic Coefficients", /*50*/
		NULL,NULL,NULL,"Space View perspecitve or orthographic grid", /*90*/
		NULL,NULL,NULL,"Arakawa semi-staggered E-grid on rotated latitude/longitude grid-point array", /*201*/
		NULL,NULL,NULL,"Arakawa filled E-grid on rotated latitude/longitude grid-point array" /*202*/
		
};

GridInfoRecord grid[] = {
		GenericUnPack,GetGrid_1,GetAtts_1,"1679-point (23x73) Mercator grid with (0,0) at (0W,48.09S), (73,23) at (0W,48.09N); I increasing eastward, Equator at J=12. Grid increment of 5 degs of longitude", /*01*/
		GenericUnPack,GetGrid_2,GenAtts,"10512-point (73x144) global longitude-latitude grid.  (0,0) at 0E, 90N, latitude grid.  (0,0) at 0E, 90N, matrix layout.  N.B.: prime meridian not duplicated.", /*2*/
		GenericUnPack,GetGrid_3,GenAtts,"65160-point (181x360) global longitude-latitude grid.  (0,0) at 0E, 90N, matrix layout.  N.B.: prime meridian not duplicated.", /*3*/
		GenericUnPack,GetGrid_4,GenAtts,"259920-point (361x720) global lon/lat grid. (0,0) at 0E, 90N; matrix layout; prime meridian not duplicated", /*4*/
		GenericUnPack,GetGrid_5,GenAtts,"3021-point (57x53) N. Hemisphere stereographic grid oriented 105W; Pole at (27,49). (LFM analysis)",/*5*/
		GenericUnPack,GetGrid_6,GenAtts,"2385-point (45x53) N. Hemisphere polar stereographic grid oriented 105W; Pole at (27,49). (LFM Forecast)", /*6*/
		IFOSUnPack,GetGrid_21,GenAtts,"1369-point (37x37) longitude-latitude grid. 0-180E, 0-90N", /*21*/
		IFOSUnPack,GetGrid_22,GenAtts,"1369-point (37x37) longitude-latitude grid. 180W-0, 0-90N", /*22*/
		IFOSUnPack,GetGrid_23,GenAtts,"1369-point (37x37) longitude-latitude grid. 0-180E, 90S-0", /*23*/
		IFOSUnPack,GetGrid_24,GenAtts,"1369-point (37x37) longitude-latitude grid. 180W-0, 90S-0", /*24*/
		IFOSUnPack,GetGrid_25,GenAtts,"1368-point (19x72) longitude-latitude grid. 0-355E, 0-90N", /*25*/
		IFOSUnPack,GetGrid_26,GenAtts,"1368-point (19x72) longitude-latitude grid. 0-355E, 90S-0", /*26*/
		GenericUnPack,GetGrid_27,GenAtts,"4225-point (65x65) N. Hemisphere polar stereographic grid oriented 80W; Pole at (33,33).", /*27*/
		GenericUnPack,GetGrid_28,GenAtts,"4225-point (65x65) S. Hemisphere polar stereographic grid oriented 100E; Pole at (33,33).", /*28*/
		GenericUnPack,GetGrid_29,GenAtts,"5365-point (37x145) N. Hemisphere longitude/latitude grid for latitudes 0N to 90N; (0,0) at (0E,0N).", /*29*/
		GenericUnPack,GetGrid_30,GenAtts,"5365-point (37x145) S. Hemisphere longitude/latitude grid for latitudes 90S to 0S; (0,0) at (0E,90S).", /*30*/
		GenericUnPack,GetGrid_33,GenAtts,"8326-point (46x181) N. Hemisphere longitude/latitude grid for latitudes 0N to 90N; (0,0) at (0E,0N).", /*33*/
		GenericUnPack,GetGrid_34,GenAtts,"8326-point (46x181) S. Hemisphere longitude/latitude grid for latitudes 90S to 0S; (0,0) at (0E,90S).", /*34*/
		NULL,NULL,GenAtts,"3447-point (73x73) \"Thinned\" longitude-latitude grid. 60E-330E, 0-90N", /*37*/
		NULL,NULL,GenAtts,"3447-point (73x73) \"Thinned\" longitude-latitude grid. 150E-60E, 0-90N", /*38*/
		NULL,NULL,GenAtts,"3447-point (73x73) \"Thinned\" longitude-latitude grid. 240E-150E, 0-90N", /*39*/
		NULL,NULL,GenAtts,"3447-point (73x73) \"Thinned\" longitude-latitude grid. 330E-240E, 0-90N", /*40*/
		NULL,NULL,GenAtts,"3447-point (73x73) \"Thinned\" longitude-latitude grid. 60E-330E, 90S-0", /*41*/
		NULL,NULL,GenAtts,"3447-point (73x73) \"Thinned\" longitude-latitude grid. 150E-60E, 90S-0", /*42*/
		NULL,NULL,GenAtts,"3447-point (73x73) \"Thinned\" longitude-latitude grid. 240E-150E,90S-0", /*43*/
		NULL,NULL,GenAtts,"3447-point (73x73) \"Thinned\" longitude-latitude grid. 330E-240E, 90S-0", /*44*/
		GenericUnPack,GetGrid_45,GenAtts,"41760-point (145x288) Global Latitude/Longitude 1.25 deg Resoulution. 0E-358.75E, 90N-90S",/*45*/
		IFOS50UnPack,GetGrid_50,GenAtts,"1188-point (33x36) longitude-latitude grid. 140.0W-52.5W, 20N-60N", /*50*/
		GenericUnPack,GetGrid_55,GenAtts,"6177-point (71x87) N. Hemisphere polar tereographic grid oriented 105W; Pole at (44,38). (2/3 bedient NH sfc anl)", /*55*/
		GenericUnPack,GetGrid_56,GenAtts,"6177-point (71x87) N. Hemisphere polar stereographic grid oriented 105W; Pole at (40,73). (1/3 bedient NA sfc anl)", /*56*/
		IFOSUnPack,GetGrid_61,GenAtts,"4186-point (46x91) longitude-latitude grid. 0-180E, 0-90N", /*61*/
		IFOSUnPack,GetGrid_62,GenAtts,"4186-point (46x91) longitude-latitude grid. 180W-0, 0-90N", /*62*/
		IFOSUnPack,GetGrid_63,GenAtts,"4186-point (46x91) longitude-latitude grid. 0-180E, 90S-0", /*63*/
		IFOSUnPack,GetGrid_64,GenAtts,"4186-point (46x91) longitude-latitude grid. 180W-0, 90S-0", /*64*/
		NULL,NULL,GenAtts,"12321-point (111x111) N. Hemisphere Lambert Conformal grid.  No fixed location; used by QLM Hurricane model.", /*75*/
		NULL,NULL,GenAtts,"12321-point (111x111) S. Hemisphere Lambert Conformal grid.  No fixed location; used by QLM Hurricane model.", /*76*/
		NULL,NULL,GenAtts,"12321-point (111x111) N. Hemisphere Mercator grid.  No fixed location; used by QLM Hurricane model.", /*77*/
		GenericUnPack,GetGrid_85,GenAtts,"32400-point (90x360) N. Hemisphere longitude/latitude grid; longitudes: 0.5E to 359.5E (0.5W); latitudes: 0.5N to 89.5N; origin (0,0) at (0.5E,0.5N)", /*85*/
		GenericUnPack,GetGrid_86,GenAtts,"32400-point (90x360) S. Hemisphere longitude/latitude grid; longitudes: 0.5E to 359.5E (0.5W); latitudes: 89.5S to 0.5S; origin (0,0) at (0.5E,89.5S)", /*86*/
		GenericUnPack,GetGrid_87,GenAtts,"5022-point (62x81) N. Hemisphere  polar stereographic grid oriented at 105W. Pole at (31.91, 112.53) Used for RUC.", /*87*/
		NULL,NULL,GenAtts,"12902-point (141x92 semi-staggered) lat. long., rotated such that center located at 52.0N, 111.0W; LL at 37.5W, 35S Unfilled E grid for 80 km ETA model", /*90*/
		NULL,NULL,GenAtts,"25803-point (141x183) lat. long., rotated such that center located at 52.0N, 111.0W; LL at 37.5W,35S Filled E grid for 80 km ETA model", /*91*/
		NULL,NULL,GenAtts,"24162-point (191x127 semi-staggered) lat. long., rotated such that center located at 41.0N, 97.0W; LL at 35W,25S Unfilled E grid for 40 km ETA model", /*92*/
		NULL,NULL,GenAtts,"48323-point (191x253)lat. long., rotated such that center located at 41.0N, 97.0W; LL at 35W ,25S Filled E grid for 40 km ETA model", /*93*/
		NULL,NULL,GenAtts,"18048-point (94x192) Global Gaussian T62 Latitude/Longitude Resolution.", /*98*/
		GenericUnPack,GetGrid_100,GenAtts,"6889-point (83x83) N. Hemisphere polar stereographic grid oriented 105W; Pole at (40.5,88.5). (NGM Original C-Grid)",  /*100*/
		GenericUnPack,GetGrid_101,GenAtts,"10283-point (91x113) N. Hemisphere polar stereographic grid oriented 105W; Pole at (58.5,92.5). (NGM \"Big C-Grid\")", /*101*/
		GenericUnPack,GetGrid_103,GenAtts,"3640-point (56x65) N. Hemisphere polar stereographic grid oriented 105W; Pole at (25.5,84.5) (used by ARL)", /*103*/
		GenericUnPack,GetGrid_104,GenAtts,"16170-point (110x147) N. Hemisphere polar stereographic grid oriented 105W; pole at (75.5,109.5). (NGM Super C grid)", /*104*/
		GenericUnPack,GetGrid_105,GenAtts,"6889-point (83x83) N. Hemisphere polar stereographic grid oriented 105W; pole at  (40.5,88.5).  (U.S. area subset of NGM Super C grid, used by ETA model)", /*105*/
		GenericUnPack,GetGrid_106,GenAtts,"19305-point (117x165) N. Hemisphere stereographic grid oriented 105W; pole at (80,176) Hi res. ETA (2 x resolution of Super C)", /*106*/
		GenericUnPack,GetGrid_107,GenAtts,"11040 point (92x120) N. Hemisphere stereographic grid oriented 105W; pole at (46,167) subset of Hi res. ETA; for ETA & MAPS/RUC", /*107*/
		NULL,NULL,GenAtts,"72960-point (190x384) Global Gaussian Latitude/Longitude T126 Resolution", /*126*/
		GenericUnPack,GetGrid_201,GenAtts,"4225-point (65x65) Hemispheric polar stereographic grid oriented 105W; pole at (33,33)", /*201*/
		GenericUnPack,GetGrid_202,GenAtts,"2795-point (43x65) National - CONUS polar stereographic oriented 105W; pole at (33,45)", /*202*/
		GenericUnPack,GetGrid_203,GenAtts,"1755-point (39x45) National - Alaska polar stereographic oriented 150W; pole at (27,37)", /*203*/
		GenericUnPack,GetGrid_204,GenAtts,"6324-point (68x93) National - Hawaii Mercator (0,0) is 25S,110E, (93,68) is 60.644S,109.129W", /*204*/
		GenericUnPack,GetGrid_205,GenAtts,"1755-point (39x45) National - Puerto Rico stereographic oriented 60W; pole at (27,57)", /*205*/
		GenericUnPack,GetGrid_206,GetAtts_206,"2091-point (41x51) Regional - Central MARD Lambert Conformal oriented 95W; pole at (30.00,169.745)", /*206*/
		GenericUnPack,GetGrid_205,GenAtts,"1715-point (35x49) Regional - Alaska polar stereographic oriented 150W; pole at 25,51", /*207*/
		GenericUnPack,GetGrid_208,GenAtts,"783-point (27x29) Regional - Hawaii mercator (0,0) is 9.343N,167.315W, (29,27) is 28.092N,145.878W", /*208*/
		GenericUnPack,GetGrid_209,GetAtts_209,"8181-point (81x101) Regional - Centeral US MARD - Double Res. Lambert Conformal oriented 95W; pole at (59.000,338.490)", /* 209*/
		GenericUnPack,GetGrid_210,GenAtts,"625-point (25x25) Regional - Puerto Rico mercator (0,0) is 9.000N,77.00W (25,25) is 26.422,58.625", /*210*/
		GenericUnPack,GetGrid_211,GetAtts_211,"6045-point (65x93) Regional - CONUS lambert conformal oriented 95W; pole at (53.000,178.745)", /*211*/
		GenericUnPack,GetGrid_212,GetAtts_212,"23865-point (129x185) Regional - CONUS - double resolution lambert conformal oriented 95W; pole at (105.000,256.490)", /* 212 */
		GenericUnPack,GetGrid_213,GenAtts,"10965-point (85x129) National - CONUS - Double Resolution polar stereographic oriented 105W; pole at (65,89)", /*213*/
		GenericUnPack,GetGrid_214,GenAtts,"6693-point (69x97) Regional - Alaska - Double Resolution polar stereographic oriented 150W; pole at (49,101)", /*214*/
};

void GenAtts
#if NhlNeedProto
(GribParamList* thevarrec, GribAttInqRecList **lat_att_list_ptr, int * nlatatts, GribAttInqRecList **lon_att_list_ptr, int *nlonatts)
#else
(thevarrec,lat_att_list_ptr, nlatatts, lon_att_list_ptr, nlonatts)
GribParamList* thevarrec;
GribAttInqRecList **lat_att_list_ptr;
int * nlatatts; 
GribAttInqRecList **lon_att_list_ptr;
int *nlonatts;
#endif
{
	GribAttInqRecList* tmp_att_list_ptr;
	NclQuark *tmp_string = NULL;
	int tmp_dimsizes = 1;
	
	


	tmp_att_list_ptr = (*lat_att_list_ptr);
	(*lat_att_list_ptr) = (GribAttInqRecList*)NclMalloc((unsigned)sizeof(GribAttInqRecList));
	(*lat_att_list_ptr)->next = tmp_att_list_ptr;
	(*lat_att_list_ptr)->att_inq = (GribAttInqRec*)NclMalloc((unsigned)sizeof(GribAttInqRec));
	(*lat_att_list_ptr)->att_inq->name = NrmStringToQuark("long_name");
	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark("latitude");
	(*lat_att_list_ptr)->att_inq->thevalue = (NclMultiDValData)_NclCreateVal( NULL, NULL, Ncl_MultiDValData, 0, (void*) tmp_string, NULL, 1 , &tmp_dimsizes, PERMANENT, NULL, nclTypestringClass);
	(*nlatatts)++;
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

	tmp_att_list_ptr = (*lon_att_list_ptr);
	(*lon_att_list_ptr) = (GribAttInqRecList*)NclMalloc((unsigned)sizeof(GribAttInqRecList));
	(*lon_att_list_ptr)->next = tmp_att_list_ptr;
	(*lon_att_list_ptr)->att_inq = (GribAttInqRec*)NclMalloc((unsigned)sizeof(GribAttInqRec));
	(*lon_att_list_ptr)->att_inq->name = NrmStringToQuark("long_name");
	tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
	*tmp_string = NrmStringToQuark("longitude");
	(*lon_att_list_ptr)->att_inq->thevalue = (NclMultiDValData)_NclCreateVal( NULL, NULL, Ncl_MultiDValData, 0, (void*) tmp_string, NULL, 1 , &tmp_dimsizes, PERMANENT, NULL, nclTypestringClass);
	(*nlonatts)++;
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

}
