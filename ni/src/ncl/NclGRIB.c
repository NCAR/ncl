#include <stdlib.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include "defs.h"
#include <netcdf.h>
#include "NclDataDefs.h"
#include "NclFileInterfaces.h"
#include "tables.h"
#include "date.h"



#include <math.h>
typedef union cvt{
        char c[4];
        unsigned int value;
        int ivalue;
}CVT;
typedef union cvtf {
        char c[4];
        float value;
}CVTF;

#define GRIBEOF  0
#define GRIBERROR -1
#define GRIBOK 1


typedef struct _GribFileRecord GribFileRecord;
typedef struct _GribRecordInqRec GribRecordInqRec;
typedef struct _GribDimInqRec GribDimInqRec;
typedef struct _GribAttInqRec GribAttInqRec;
typedef struct _GribRecordInqRecList GribRecordInqRecList;
typedef struct _GribDimInqRecList GribDimInqRecList;
typedef struct _GribAttInqRecList GribAttInqRecList;
typedef struct _GribParamList GribParamList;

struct _GribParamList {
	int param_number;
	int grid_number;
	int n_entries;
	int time_range_indicator;
	int time_unit_indicator;
	int level_indicator;
	int century;
	int year;
	int month;
	int day;
	int hour;
	int minute;
	GribRecordInqRecList *thelist;
	GribParamList *next;
};
struct _GribRecordInqRecList {
	GribRecordInqRec *rec_inq;
	GribRecordInqRecList *next;
};

struct _GribDimInqRecList {
	GribDimInqRec *dim_inq;
	GribDimInqRecList *next;
};

struct _GribAttInqRecList {
	GribAttInqRec *att_inq;
	GribAttInqRecList *next;
};

struct _GribRecordInqRec {
	NclQuark var_name_q;
	int param_number;
	int param_tbl_index;
	int grid_tbl_index;
	int grid_number;
/*
* This is the time offset from the beginning reference
* time of the parameter set. The units are set in
* the GribParamList structure
*/
	long time_offset;
	int level_indicator;
	int level0;
	int level1;
	char pds[28];
	int pds_size;
	char *var_name;
	NclQuark long_name_q;
	NclQuark units_q;
	unsigned int start;
	unsigned int bds_off;
	unsigned int bds_size;
	int has_gds;
	unsigned int gds_off;
	unsigned int gds_size;
	int has_bms;
	unsigned int bms_off;
	unsigned int bms_size;
};

struct _GribDimInqRec {
	char *foo;
};
	
struct _GribAttInqRec {
	char *foo;
};


struct _GribFileRecord {
NclQuark	file_path_q;
int		wr_status;
int		n_vars;
GribParamList	*var_list;

};


static NclBasicDataTypes GribMapToNcl 
#if	NhlNeedProto
(void* the_type)
#else
(the_type)
	void *the_type;
#endif
{
}

static void *GribMapFromNcl
#if	NhlNeedProto
(NclBasicDataTypes the_type)
#else
(the_type)
	NclBasicDataTypes the_type;
#endif
{
}

static int CnvtToDecimal
#if	NhlNeedProto
(int n_bytes,char *val)
#else
(n_bytes,val)
int n_bytes;
char *val;
#endif
{
	CVT tmp;
	int i = 0;
	
	tmp.c[0] = (char)0;
	tmp.c[1] = (char)0;
	tmp.c[2] = (char)0;
	tmp.c[3] = (char)0;

	if(n_bytes == 4) {
		tmp.c[0] = val[i];
		i++;
	}
	if(n_bytes >= 3) {
		tmp.c[1] = val[i];
		i++;
	}
	if(n_bytes >= 2) {
		tmp.c[2] = val[i];
		i++;
	}
	if(n_bytes >= 1) {
		tmp.c[3] = val[i];
		i++;
	}
	return(tmp.ivalue);
}

int GetNextGribOffset
#if NhlNeedProto
(int gribfile, unsigned int *offset, unsigned int *totalsize, unsigned int startoff, unsigned int *nextoff)
#else
(gribfile, offset, totalsize, startoff, *nextoff)
int gribfile;
unsigned int *offset;
unsigned int *totalsize;
unsigned int startoff;
unsigned int *nextoff;
#endif
{
        int i,ret1,ret2,ret3,ret4;
        char test[5];
        char nd[4];
        char is[4];
        CVT version;
        CVT size;


        ret1 = 0;
        ret2 = 0;
        ret3 = 0;
        ret4 = 0;

        test[4] = '\0';

        i = startoff;
        i = lseek(gribfile,i,SEEK_SET);
        while(1) {
                ret1 = read(gribfile,(void*)is,1);
                i += ret1;
                if(ret1 > 0) {
                        if(is[0] == 'G') {
                                ret2 = read(gribfile,(void*)&(is[1]),7);
                                i += ret2;
                                test[0] = is[0];
                                test[1] = is[1];
                                test[2] = is[2];
                                test[3] = is[3];
                                version.value &= 0;
                                version.c[3] = is[7];
                                if((!strcmp(test,"GRIB"))&&(version.value ==1)){
                                        *offset =  i - (ret1 + ret2);
                                        size.c[0] = (char)0;
                                        size.c[1] = (char)is[4];
                                        size.c[2] = (char)is[5];
                                        size.c[3] = (char)is[6];
                                        ret3 = lseek(gribfile,i+size.value - (ret1 + ret2) - 4,SEEK_SET);
                                        ret4 = read(gribfile,(void*)nd,4);
                                        test[0] = nd[0];
                                        test[1] = nd[1];
                                        test[2] = nd[2];
                                        test[3] = nd[3];
                                        if(!strcmp("7777",test)) {
                                                *nextoff = ret3 + ret4;
                                                *totalsize = size.value;
                                                return(GRIBOK);
                                        } else {
                                                *nextoff = i;
                                                *totalsize = size.value;
                                                return(GRIBERROR);
                                        }
                                }
                        }
                } else {
                        return(GRIBEOF);
                }
        }
}

static int _GetLevels
#if NhlNeedProto
(int *l0,int *l1,int indicator,char* lv)
#else
(l0,l1,indicator,lv)
int *l0;
int *l1;
int indicator;
char *lv;
#endif
{
	CVT tmp;
	tmp.c[0] = (char)0;
	tmp.c[1] = (char)0;
	if(indicator  < 100) {
		*l0 = -1;
		*l1 = -1;
	}
	switch(indicator) {
	case 100:
		tmp.c[2] = lv[0];
		tmp.c[3] = lv[1];
		*l0 = tmp.ivalue;
		*l1 = -1;
		return(1);
	case 101:
		*l0 = (int)lv[0];
		*l1 = (int)lv[1];
		break;
	case 102:
		*l0 = 0;
		*l1 = 0;
		return(1);
	case 103:
		tmp.c[2] = lv[0];
		tmp.c[3] = lv[1];
		*l0 = tmp.ivalue;
		*l1 = -1;
		break;
	case 104:
		*l0 = (int)lv[0];
		*l1 = (int)lv[1];
		break;
	case 105:
		tmp.c[2] = lv[0];
		tmp.c[3] = lv[1];
		*l0 = tmp.ivalue;
		*l1 = -1;
		break;
	case 106:
		*l0 = (int)lv[0];
		*l1 = (int)lv[1];
		break;
	case 107:
		tmp.c[2] = lv[0];
		tmp.c[3] = lv[1];
		*l0 = tmp.ivalue;
		*l1 = -1;
		break;
	case 108:
		*l0 = (int)lv[0];
		*l1 = (int)lv[1];
		break;
	case 109:
		tmp.c[2] = lv[0];
		tmp.c[3] = lv[1];
		*l0 = tmp.ivalue;
		*l1 = -1;
		break;
	case 110:
		*l0 = (int)lv[0];
		*l1 = (int)lv[1];
		break;
	case 111:
		tmp.c[2] = lv[0];
		tmp.c[3] = lv[1];
		*l0 = tmp.ivalue;
		*l1 = -1;
		break;
	case 112:
		*l0 = (int)lv[0];
		*l1 = (int)lv[1];
		break;
	case 113:
		tmp.c[2] = lv[0];
		tmp.c[3] = lv[1];
		*l0 = tmp.ivalue;
		*l1 = -1;
		break;
	case 114:
		*l0 = (int)lv[0];
		*l1 = (int)lv[1];
		break;
	case 115:
		tmp.c[2] = lv[0];
		tmp.c[3] = lv[1];
		*l0 = tmp.ivalue;
		*l1 = -1;
		break;
	case 116:
		*l0 = (int)lv[0];
		*l1 = (int)lv[1];
		break;
	case 121:
		*l0 = (int)lv[0];
		*l1 = (int)lv[1];
		break;
	case 125:
		tmp.c[2] = lv[0];
		tmp.c[3] = lv[1];
		*l0 = tmp.ivalue;
		*l1 = -1;
		break;
	case 128:
		*l0 = (int)lv[0];
		*l1 = (int)lv[1];
		break;
	case 141:
		*l0 = (int)lv[0];
		*l1 = (int)lv[1];
		break;
	case 160:
		tmp.c[2] = lv[0];
		tmp.c[3] = lv[1];
		*l0 = tmp.ivalue;
		*l1 = -1;
		break;
	case 200:
		tmp.c[2] = lv[0];
		tmp.c[3] = lv[1];
		*l0 = tmp.ivalue;
		*l1 = -1;
		break;
	case 201:
		tmp.c[2] = lv[0];
		tmp.c[3] = lv[1];
		*l0 = tmp.ivalue;
		*l1 = -1;
		break;
	case 1:
		*l0 = -1;
		*l1 = -1;
		return(1);
	default: 
		*l0 = -1;
		*l1 = -1;
	}
	return(0);
}

static long _GetTimeDiffOffset
#if	NhlNeedProto
(int c0, int y0, int m0, int d0, int h0, int mn0, int c1, int y1, int m1, int d1, int h1, int mn1, int units)
#else
(c0, y0, m0, d0, h0, mn0, c1, y1, m1, d1, h1, mn1, units)
int y0;
int m0;
int d0;
int h0;
int mn0;
int y1;
int m1;
int d1;
int h1;
int mn1;
int units;
#endif 
{
	long days_diff = 0;
	int year0;
	int year1;
	long outval = 0;;


	year0 = (c0 - 1 )* 100 + y0;
	year1 = (c1 - 1 )* 100 + y1;
	
	days_diff = JulianDayDiff((unsigned short)d0, (unsigned short)m0, (unsigned short)year0,
			(unsigned short)d1, (unsigned short)m1, (unsigned short)year1);

	switch(units) {
	case 0:
		outval = days_diff * 1440 + abs(h1 - h0) * 60 + abs(mn1 - mn0);
		break;
	case 1:
		outval = days_diff * 24 + abs(h1-h0);
		break;
	case 2:
		outval = days_diff;
		break;
	case 3:
		break;
	case 4:
		break;
	case 5:
		break;
	case 6:
		break;
	case 7:
		break;
	case 254:
		break;
	default:
		return(0);
	}
	return(outval);
}


static int _GetTimeOffset 
#if	NhlNeedProto
( int time_indicator, char *offset)
#else
( time_indicator, offset)
int time_indicator;
char *offset;
#endif
{
	CVT tmp;
	switch(time_indicator) {
		case 0: /* reference time + P1 */
		case 1: /* reference time + P1 */
			return((int)offset[0]);
		case 2: /* reference time + P1 < t < reference time + P2 */
		case 3: /* Average from reference time + P1 to reference time + P2 */
		case 4: /* Accumulation from reference time + P1 to reference time + P2 */
		case 5: /* Difference from reference time + P1 to reference time + P2 */
			return((int)offset[1]);
			break;
		case 10:/* P1 occupies both bytes */
			tmp.c[0] = 0;
			tmp.c[1] = 0;
			tmp.c[2] = offset[0];
			tmp.c[3] = offset[1];
			return(tmp.value);
			break;
		case 51:
		case 113:
		case 114:
		case 115:
		case 116:
		case 117:
		case 118:
		case 123:
		case 124:
		default:
			NhlPError(NhlWARNING,NhlEUNKNOWN,"NclGRIB: Unknown or unsupported time range indicator detected, continuing");
			return(-1);
	}
}
static int date_comp
#if 	NhlNeedProto
(Const void *s1, Const void *s2)
#else
(s1, s2)
void *s1;
void *s2;
#endif
{
	GribRecordInqRecList *s_1 = *(GribRecordInqRecList**)s1;
	GribRecordInqRecList *s_2 = *(GribRecordInqRecList**)s2;

	return(s_1->rec_inq->time_offset - s_2->rec_inq->time_offset);
}

static GribParamList *_NewListNode
#if	NhlNeedProto
(GribRecordInqRec *grib_rec)
#else
(grib_rec)
	GribRecordInqRec* grib_rec;
#endif
{
	GribParamList *tmp = NULL;
	GribRecordInqRecList *list = NULL;

	tmp = (GribParamList*)NclMalloc((unsigned)sizeof(GribParamList));
	tmp->next = NULL;
	list = (GribRecordInqRecList*) NclMalloc((unsigned)sizeof(GribRecordInqRecList));
	list->rec_inq = grib_rec;
	list->next = NULL;
	tmp->thelist = list;
	tmp->param_number = grib_rec->param_number;
	tmp->grid_number = grib_rec->grid_number;
	tmp->level_indicator = grib_rec->level_indicator;
	tmp->n_entries = 1;
	tmp->century = (int)grib_rec->pds[24];
	tmp->year = (int)grib_rec->pds[12];
	tmp->month = (int)grib_rec->pds[13];
	tmp->day = (int)grib_rec->pds[14];
	tmp->hour = (int)grib_rec->pds[15];
	tmp->minute = (int)grib_rec->pds[16];
	tmp->time_range_indicator = (int)grib_rec->pds[20];
	tmp->time_unit_indicator = (int)grib_rec->pds[17];
	return(tmp);
}

static void _InsertNodeAfter
#if NhlNeedProto
(GribParamList *node, GribParamList *new_node)
#else
(node, new_node)
GribParamList *node; 
GribParamList *new_node;
#endif
{
	GribParamList * tmp;

	tmp = node->next;
	node->next = new_node;
	new_node->next = tmp;
	return;
}

static void _AddRecordToNode
#if NhlNeedProto
(GribParamList *node, GribRecordInqRec* grib_rec)
#else
(node, grib_rec)
GribParamList *node;
GribRecordInqRec* grib_rec;
#endif
{
	GribRecordInqRecList * grib_rec_list = (GribRecordInqRecList*)NclMalloc((unsigned)sizeof(GribRecordInqRecList));

	
	if(((int)grib_rec->pds[24] < node->century)
		||(((int)grib_rec->pds[24] == node->century)&&((int)grib_rec->pds[12] < node->year))
		||(((int)grib_rec->pds[24] == node->century)&&((int)grib_rec->pds[12] == node->year)&&((int)grib_rec->pds[13] < node->month))
		||(((int)grib_rec->pds[24] == node->century)&&((int)grib_rec->pds[12] == node->year)&&((int)grib_rec->pds[13] == node->month)
			&&((int)grib_rec->pds[14] < node->day))
		||(((int)grib_rec->pds[24] == node->century)&&((int)grib_rec->pds[12] == node->year)&&((int)grib_rec->pds[13] == node->month)
			&&((int)grib_rec->pds[14] == node->day) 	
			&&((int)grib_rec->pds[15] < node->hour))
		||(((int)grib_rec->pds[24] == node->century)&&((int)grib_rec->pds[12] == node->year)&&((int)grib_rec->pds[13] == node->month)
			&&((int)grib_rec->pds[14] == node->day)
			&&((int)grib_rec->pds[15] == node->hour)
			&&((int)grib_rec->pds[16] < node->minute))) {

			node->century = (int)grib_rec->pds[24];
			node->year = (int)grib_rec->pds[12];
			node->month = (int)grib_rec->pds[13];
			node->day = (int)grib_rec->pds[14];
			node->hour = (int)grib_rec->pds[15];
			node->minute = (int)grib_rec->pds[16];
	}
	if(node->time_unit_indicator != (int)grib_rec->pds[17]) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"NclGRIB: Time unit indicator varies for parameter (%s), continuing anyways.",NrmQuarkToString(grib_rec->var_name_q));
	}
	grib_rec_list->rec_inq = grib_rec;
	grib_rec_list->next = node->thelist;
        node->thelist = grib_rec_list;
        node->n_entries++;
}

static int _FirstCheck
#if NhlNeedProto
(GribFileRecord *therec, GribRecordInqRec *grib_rec)
#else
(therec, grib_rec)
GribFileRecord *therec;
GribRecordInqRec *grib_rec;
#endif
{
	GribParamList *step = therec->var_list;

	if((step->param_number > grib_rec->param_number) 
		||((step->param_number == grib_rec->param_number)&&(step->grid_number > grib_rec->grid_number))
		||((step->param_number == grib_rec->param_number)&&(step->grid_number == grib_rec->grid_number)&&(step->time_range_indicator > (int)grib_rec->pds[20]))
		||((step->param_number == grib_rec->param_number)&&(step->grid_number == grib_rec->grid_number)&&(step->time_range_indicator == (int)grib_rec->pds[20])&&(step->level_indicator > grib_rec->level_indicator))) {

		therec->var_list = _NewListNode(grib_rec);
		therec->var_list->next = step;
		therec->n_vars++;
		return(1);
	} else if((step->param_number == grib_rec->param_number)&&(step->grid_number == grib_rec->grid_number)&&(step->time_range_indicator == (int)grib_rec->pds[20])&&(step->level_indicator == grib_rec->level_indicator)){
		_AddRecordToNode(step,grib_rec);
		return(1);
	} else {
		return(0);
	}
}

static void *GribGetFileRec
#if	NhlNeedProto
(NclQuark path,int wr_status)
#else
(path,wr_status)
NclQuark path;
int wr_status;
#endif
{
	int fd;
	int done = 0;
	unsigned int offset = 0;
	unsigned int size = 0;
	unsigned int nextoff = 0;
	GribFileRecord *therec = NULL;
	GribRecordInqRec *grib_rec = NULL;
	GribRecordInqRecList *grib_rec_list = NULL;
	GribParamList *step = NULL,*step2 = NULL ;
	CVT tmp;
	int i;
	int ret;
	char tmpc[4];
	char buffer[80];
	GribRecordInqRecList **sortar;
	TBLE2 *name_rec = NULL;
	int tmp_month;
	int tmp_year;
	int tmp_century;
	int tmp_day;
	int tmp_hour;
	int tmp_minute;

	if(wr_status <= 0) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"NclGRIB: Grib files are read only continueing but opening file as read only");
	}
	fd = open(NrmQuarkToString(path),O_RDONLY);
	if(fd > 0) {
		while(!done) {
			ret = GetNextGribOffset(fd,&offset,&size,offset,&nextoff);
			if(ret == GRIBEOF) {
				done = 1;
			} else if(ret != GRIBERROR) {
				grib_rec = NclMalloc((unsigned)sizeof(GribRecordInqRec));
				lseek(fd,offset+8,SEEK_SET);
				read(fd,(void*)grib_rec->pds,28);
				fprintf(stdout,"Found: %d\n",(int)(int)grib_rec->pds[8]);
				grib_rec->has_gds = (grib_rec->pds[7] & (char)0200) ? 1 : 0;
				grib_rec->has_bms = (grib_rec->pds[7] & (char)0100) ? 1 : 0;
				grib_rec->param_number = (int)grib_rec->pds[8];
				grib_rec->grid_number = (int)grib_rec->pds[6];
				grib_rec->start = offset;
				grib_rec->time_offset = 0;
				grib_rec->pds_size = CnvtToDecimal(3,&(grib_rec->pds[0]));
				if(grib_rec->has_gds) {
					lseek(fd,(unsigned)(grib_rec->start + 8 + grib_rec->pds_size),SEEK_SET);
					read(fd,(void*)tmpc,3);
					grib_rec->gds_size = CnvtToDecimal(3,tmpc);
					grib_rec->gds_off = 8 + grib_rec->pds_size;
				} else {
					grib_rec->gds_off = 0;	
					grib_rec->gds_size = 0;
				}

				if(grib_rec->has_bms) {
					lseek(fd,(unsigned)(grib_rec->start + 8 + grib_rec->pds_size + grib_rec->gds_size),SEEK_SET);
					read(fd,(void*)tmpc,3);
					grib_rec->bms_size = CnvtToDecimal(3,tmpc);
					grib_rec->bms_off = 8 + grib_rec->pds_size + grib_rec->gds_size;
				} else {
					grib_rec->bms_off = 0;
					grib_rec->bms_size = 0;
				}
				grib_rec->bds_off = 8 + grib_rec->pds_size + grib_rec->bms_size + grib_rec->gds_size;
				lseek(fd,(unsigned)(grib_rec->start + grib_rec->bds_off),SEEK_SET);
				read(fd,(void*)tmpc,3);
				grib_rec->bds_size = CnvtToDecimal(3,tmpc);
		
				name_rec = NULL;	
				if(grib_rec->param_number < 128) {	
					for(i = 0; i < sizeof(params_index)/sizeof(int); i++) {
						if(params_index[i] == grib_rec->param_number) {
							name_rec = &(params[i]);
							break;
						}
					}
					if( i ==  sizeof(params_index)/sizeof(int) ) {
						NhlPError(NhlWARNING,NhlEUNKNOWN,"NclGRIB: Unknown grib parameter number detected (%d), skipping",grib_rec->param_number);
						NhlFree(grib_rec);
						grib_rec= NULL;
					} 
				} else {
					switch((int)grib_rec->pds[4]) {
					case 98: 
						for(i = 0; i < sizeof(params_ecmwf_index)/sizeof(int); i++) {
							if(params_ecmwf_index[i] == grib_rec->param_number) {
								name_rec = &(params_ecmwf[i]);
								break;
							}
						}
						if( i ==  sizeof(params_ecmwf_index)/sizeof(int) ) {
							NhlPError(NhlWARNING,NhlEUNKNOWN,"NclGRIB: Unknown grib parameter number detected (%d), skipping",grib_rec->param_number);
							NhlFree(grib_rec);
							grib_rec= NULL;
						} 
						break;
					case 7:
					case 8:
					case 9:
						for(i = 0; i < sizeof(params_nwsnmc_index)/sizeof(int); i++) {
							if(params_nwsnmc_index[i] == grib_rec->param_number) {
								name_rec = &(params_nwsnmc[i]);
								break;
							}
						}
						if( i ==  sizeof(params_nwsnmc_index)/sizeof(int) ) {
							NhlPError(NhlWARNING,NhlEUNKNOWN,"NclGRIB: Unknown grib parameter number detected (%d), skipping",grib_rec->param_number);
							NhlFree(grib_rec);
							grib_rec= NULL;
						} 
						break;
					default:
						NhlPError(NhlWARNING,NhlEUNKNOWN,"NclGRIB: Unlocatable parameter number (%d) from center (%d), extension of NclGRIB required",grib_rec->param_number,(int)grib_rec->pds[4]);
						NhlFree(grib_rec);
						grib_rec= NULL;
					 
					}
				}

				if((name_rec != NULL)&&(grib_rec != NULL)){
					grib_rec->param_tbl_index = i;
					strcpy(buffer,name_rec->abrev);
					sprintf(&(buffer[strlen(buffer)]),"_%d",grib_rec->grid_number);
					switch(grib_rec->pds[20]) {
					case 3:
						sprintf(&(buffer[strlen(buffer)]),"_ave");
						break;
					case 4:
						sprintf(&(buffer[strlen(buffer)]),"_acc");
						break;
					case 5:
						sprintf(&(buffer[strlen(buffer)]),"_dif");
						break;
					default:
						break;
					}
					grib_rec->var_name = (char*)NclMalloc((unsigned)strlen(buffer) + 1);
					strcpy(grib_rec->var_name,buffer);
					grib_rec->var_name_q = NrmStringToQuark(grib_rec->var_name);
					grib_rec->long_name_q = NrmStringToQuark(name_rec->long_name);
					grib_rec->units_q = NrmStringToQuark(name_rec->units);
					grib_rec->level_indicator = (int)grib_rec->pds[9];
					_GetLevels(&grib_rec->level0,&grib_rec->level1,(int)grib_rec->pds[9],&(grib_rec->pds[10]));
				if((grib_rec->param_number == 34)&&(grib_rec->grid_number == 211)&&(grib_rec->level_indicator == 100)) {
					fprintf(stdout,"here_i_am\n");
				}

					if(therec == NULL) {
						therec = (GribFileRecord*)NclMalloc((unsigned)sizeof(GribFileRecord));
						therec->n_vars = 0;
						therec->var_list = NULL;
						therec->wr_status = wr_status;	
						therec->file_path_q = path;
					}
					if(therec->var_list == NULL) {
						therec->var_list = _NewListNode(grib_rec);
						therec->n_vars = 1;
					} else {
						step = therec->var_list;
						if(!_FirstCheck(therec,grib_rec)) {
/*
* Keep in inorder list
*/
							while((step->next != NULL)
								&&(step->next->param_number < grib_rec->param_number)) {

								step = step->next;
							}
				
							if((step->next == NULL) 
								||(step->next->param_number > grib_rec->param_number)) {
/*
* No current instance of grib_rec->param_number insert immediately
*/

								step2 = _NewListNode(grib_rec);
								_InsertNodeAfter(step,step2);
								therec->n_vars++;
							} else if(step->next->grid_number <= grib_rec->grid_number) {
/*
* At this point param_number found and step->next points to first occurance of param_number
*/
								while((step->next != NULL)
									&&(step->next->param_number==grib_rec->param_number)
									&&(step->next->grid_number < grib_rec->grid_number)) {
									step = step->next;
								}
								if((step->next == NULL)
									||(step->next->param_number != grib_rec->param_number)
									||(step->next->grid_number > grib_rec->grid_number)) {

									step2 = _NewListNode(grib_rec);
									_InsertNodeAfter(step,step2);
									therec->n_vars++;
								} else if(step->next->time_range_indicator <= (int)grib_rec->pds[20]) {
/*
* At this point param_number and grib_number found and step->next points to first occurance of
* grid_number
*/
									while((step->next != NULL)
										&&(step->next->param_number == grib_rec->param_number)
										&&(step->next->grid_number == grib_rec->grid_number)
										&&(step->next->time_range_indicator < (int)grib_rec->pds[20])){
										step = step->next;
									}
									if((step->next == NULL)
										||(step->next->param_number != grib_rec->param_number)
                                                                        	||(step->next->grid_number != grib_rec->grid_number)
										||(step->next->time_range_indicator > (int)grib_rec->pds[20])) {
										step2 = _NewListNode(grib_rec);
										_InsertNodeAfter(step,step2);
										therec->n_vars++;
									} else if(step->next->level_indicator <= grib_rec->level_indicator) {
										while((step->next != NULL) 
											&&(step->next->param_number == grib_rec->param_number)
											&&(step->next->grid_number == grib_rec->grid_number)
											&&(step->next->time_range_indicator == (int)grib_rec->pds[20])
											&&(step->next->level_indicator < grib_rec->level_indicator)){
											step = step->next;
										}
										if((step->next == NULL)
											||(step->next->param_number != grib_rec->param_number)
                                                                        		||(step->next->grid_number != grib_rec->grid_number)
											||(step->next->time_range_indicator != (int)grib_rec->pds[20])
											||(step->next->level_indicator > grib_rec->level_indicator)) {
											step2 = _NewListNode(grib_rec);	
											_InsertNodeAfter(step,step2);
											therec->n_vars++;
										} else {
/*
* Att this point it falls through because 
* param_number, grid_number and time_range_indicator level_indicator are equal
* so its time ot add the record
*/
											_AddRecordToNode(step->next,grib_rec);
										}
									} else {
										step2 = _NewListNode(grib_rec);
										_InsertNodeAfter(step,step2);
										therec->n_vars++;
									}
								} else {
									step2 = _NewListNode(grib_rec);
									_InsertNodeAfter(step,step2);
									therec->n_vars++;
								}
							} else {
								step2 = _NewListNode(grib_rec);
								_InsertNodeAfter(step,step2);
								therec->n_vars++;
							}
						}
					}
				}
			} else {
				NhlPError(NhlWARNING, NhlEUNKNOWN, "NclGRIB: Detected incomplete record, skipping record");
			}
			offset = nextoff;
			grib_rec = NULL;
		}
/*
* Next step is to sort by time and then level each of the variables in the list
*/
		step = therec->var_list;
		while(step != NULL) {
			grib_rec_list = step->thelist;


			sortar = (GribRecordInqRecList**)NclMalloc((unsigned)sizeof(GribRecordInqRecList*)*step->n_entries);
			i = 0;	
/*
* Scan through records and compute time offset from top of the grib record. 
* All offsets based time_units_indicator of top of the grib parameter record
* First determine an offset in time units based on time_units_indicator and time_range_indicator
* then determine offset in same units from the top of the parameter list's time reference 
*/		
		
			while(grib_rec_list != NULL) {
				sortar[i] = grib_rec_list;	
				grib_rec_list->rec_inq->time_offset = _GetTimeOffset(
							(int)grib_rec_list->rec_inq->pds[20],
							(char*)&(grib_rec_list->rec_inq->pds[18]));
				if((grib_rec_list->rec_inq->time_offset != -1)&&(
					((int)grib_rec_list->rec_inq->pds[24] != step->century)
					||((int)grib_rec_list->rec_inq->pds[12] != step->year)
					||((int)grib_rec_list->rec_inq->pds[13] != step->month)
					||((int)grib_rec_list->rec_inq->pds[14] != step->day)
					||((int)grib_rec_list->rec_inq->pds[15] != step->hour)
					||((int)grib_rec_list->rec_inq->pds[16] != step->minute))) {
					
					grib_rec_list->rec_inq->time_offset += _GetTimeDiffOffset(step->century,
							step->year,step->month,step->day,step->hour,step->minute,
							(int)grib_rec_list->rec_inq->pds[24],
							(int)grib_rec_list->rec_inq->pds[12],(int)grib_rec_list->rec_inq->pds[13],
							(int)grib_rec_list->rec_inq->pds[14],(int)grib_rec_list->rec_inq->pds[15],
							(int)grib_rec_list->rec_inq->pds[16],(int)grib_rec_list->rec_inq->pds[17]);
			
				}
				grib_rec_list = grib_rec_list->next;
				i++;
			}
			fprintf(stdout,"param# = %d\t%d\t%s\tnrecs=%d\n",step->param_number,step->grid_number,step->thelist->rec_inq->var_name,step->n_entries);
			qsort((void*)sortar,i,sizeof(GribRecordInqRecList*),date_comp);
			step->thelist = sortar[0];
			for(i = 0; i < step->n_entries - 1; i++) {
				fprintf(stdout,"%d/%d/%d\t(%d:%d)-%d,%d\t%d,%d\ttoff=%d\t%d,%d,%d\n",
					(int)sortar[i]->rec_inq->pds[13],
					(int)sortar[i]->rec_inq->pds[14],
					(int)sortar[i]->rec_inq->pds[12],
					(int)sortar[i]->rec_inq->pds[15],
					(int)sortar[i]->rec_inq->pds[16],
					(int)sortar[i]->rec_inq->pds[18],
					(int)sortar[i]->rec_inq->pds[19],
					(int)sortar[i]->rec_inq->pds[17],
					(int)sortar[i]->rec_inq->pds[20],
					sortar[i]->rec_inq->time_offset,
					(int)sortar[i]->rec_inq->pds[9],
					sortar[i]->rec_inq->level0,
					sortar[i]->rec_inq->level1);
				sortar[i]->next = sortar[i+1];
			}
			fprintf(stdout,"%d/%d/%d\t(%d:%d)-%d,%d\t%d,%d\ttoff=%d\t%d,%d,%d\n",
					(int)sortar[i]->rec_inq->pds[13],
					(int)sortar[i]->rec_inq->pds[14],
					(int)sortar[i]->rec_inq->pds[12],
					(int)sortar[i]->rec_inq->pds[15],
					(int)sortar[i]->rec_inq->pds[16],
					(int)sortar[i]->rec_inq->pds[18],
					(int)sortar[i]->rec_inq->pds[19],
					(int)sortar[i]->rec_inq->pds[17],
					(int)sortar[i]->rec_inq->pds[20],
					sortar[i]->rec_inq->time_offset,
					(int)sortar[i]->rec_inq->pds[9],
					sortar[i]->rec_inq->level0,
					sortar[i]->rec_inq->level1);
					
			sortar[step->n_entries - 1]->next = NULL;
			NclFree(sortar);
			sortar = NULL;
			
			step = step->next;

		}
		
		return(therec);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"NclGRIB: Could not open (%s) check permissions",NrmQuarkToString(path));
		return(NULL);
	}
}

static void *GribCreateFileRec
#if	NhlNeedProto
(NclQuark path)
#else
(path)
NclQuark path;
#endif
{
	NhlPError(NhlFATAL,NhlEUNKNOWN,"NclGRIB: Grib files can only be read, Grib file can not be created using NCL");
	return(NULL);
}

static void GribFreeFileRec
#if	NhlNeedProto
(void* therec)
#else
(therec)
void *therec;
#endif
{
	NclFree(therec);
}

static NclQuark* GribGetVarNames
#if	NhlNeedProto
(void* therec, int *num_vars)
#else
(therec, num_vars)
void* therec;
int *num_vars;
#endif
{
	GribFileRecord *thefile = (GribFileRecord*)therec;
	GribParamList *step;
	int i;
	NclQuark *arout;

	*num_vars = thefile->n_vars;
	arout = (NclQuark*)NclMalloc((unsigned)sizeof(NclQuark)* *num_vars);

	step = thefile->var_list;	
	for(i = 0; i < thefile->n_vars; i++) {
		arout[i] = step->thelist->rec_inq->var_name_q;
	}
	return(arout);
}

static NclFVarRec *GribGetVarInfo
#if	NhlNeedProto
(void *therec, NclQuark var_name)
#else
(therec, var_name)
void *therec;
NclQuark var_name;
#endif
{
	return(NULL);
}

static NclQuark *GribGetDimNames
#if	NhlNeedProto
(void* therec, int* num_dims)
#else
(therec,num_dims)
void *therec;
int *num_dims;
#endif
{
	*num_dims = 0;
	return(NULL);
}

static NclFDimRec *GribGetDimInfo
#if	NhlNeedProto
(void* therec, NclQuark dim_name_q)
#else
(therec,dim_name_q)
void* therec;
NclQuark dim_name_q;
#endif
{
	return(NULL);
}

static NclQuark *GribGetAttNames
#if	NhlNeedProto
(void* therec,int *num_atts)
#else
(therec,num_atts)
void* therec;
int *num_atts;
#endif
{	
	*num_atts = 0;
	return(NULL);
}

static NclFAttRec* GribGetAttInfo
#if	NhlNeedProto
(void* therec, NclQuark att_name_q)
#else
(therec, att_name_q)
void* therec;
NclQuark att_name_q;
#endif
{
	return(NULL);
}

static NclQuark *GribGetVarAttNames
#if	NhlNeedProto
(void *therec , NclQuark thevar, int* num_atts)
#else
(therec , thevar, num_atts)
void *therec;
NclQuark thevar;
int* num_atts;
#endif
{
	*num_atts = 0;
	return(NULL);
}

static NclFAttRec *GribGetVarAttInfo
#if	NhlNeedProto
(void *therec, NclQuark thevar, NclQuark theatt)
#else
(therec, thevar, theatt)
void *therec;
NclQuark thevar;
NclQuark theatt;
#endif
{
	return(NULL);
}

static NclFVarRec *GribGetCoordInfo
#if	NhlNeedProto
(void* therec, NclQuark thevar)
#else
(therec, thevar)
void* therec;
NclQuark thevar;
#endif
{
	return(NULL);
}


static void *GribReadVar
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
	return(NULL);
}

static void *GribReadCoord
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
	return(NULL);
}


static void *GribReadAtt
#if	NhlNeedProto
(void *therec,NclQuark theatt,void* storage)
#else
(therec,theatt,storage)
void *therec;
NclQuark theatt;
void* storage;
#endif
{
	return(NULL);
}

static void *GribReadVarAtt
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
	return(NULL);
}








NclFormatFunctionRec GribRec = {
/* NclCreateFileRecFunc	   create_file_rec; */		GribCreateFileRec,
/* NclGetFileRecFunc       get_file_rec; */		GribGetFileRec,
/* NclFreeFileRecFunc      free_file_rec; */		GribFreeFileRec,
/* NclGetVarNamesFunc      get_var_names; */		GribGetVarNames,
/* NclGetVarInfoFunc       get_var_info; */		GribGetVarInfo,
/* NclGetDimNamesFunc      get_dim_names; */		GribGetDimNames,
/* NclGetDimInfoFunc       get_dim_info; */		GribGetDimInfo,
/* NclGetAttNamesFunc      get_att_names; */		GribGetAttNames,
/* NclGetAttInfoFunc       get_att_info; */		GribGetAttInfo,
/* NclGetVarAttNamesFunc   get_var_att_names; */	GribGetVarAttNames,
/* NclGetVarAttInfoFunc    get_var_att_info; */		GribGetVarAttInfo,
/* NclGetCoordInfoFunc     get_coord_info; */		GribGetCoordInfo,
/* NclReadCoordFunc        read_coord; */		GribReadCoord,
/* NclReadCoordFunc        read_coord; */		NULL,
/* NclReadVarFunc          read_var; */			GribReadVar,
/* NclReadVarFunc          read_var; */			NULL,
/* NclReadAttFunc          read_att; */			GribReadAtt,
/* NclReadVarAttFunc       read_var_att; */		GribReadVarAtt,
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
/* NclMapFormatTypeToNcl   map_format_type_to_ncl; */	GribMapToNcl,
/* NclMapNclTypeToFormat   map_ncl_type_to_format; */	GribMapFromNcl
};
NclFormatFunctionRecPtr GribAddFileFormat 
#if	NhlNeedProto
(void)
#else 
()
#endif
{
	
	return(&GribRec);
}
