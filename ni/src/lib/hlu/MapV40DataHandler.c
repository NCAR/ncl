/*
 *      $Id: MapV40DataHandler.c,v 1.14 2008-09-12 20:22:46 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Apr 23 12:01:04 MDT 1998
 *
 *	Description:	
 */
/* Uncomment this to write out tables of the recognized map boundaries, with area id info, etc. in html  */
/*#define HLU_WRITE_TABLES*/


#include <ncarg/hlu/MapV40DataHandlerP.h>
#include <ctype.h>

static NhlErrorTypes MapV40DHClassPartInit(
#if	NhlNeedProto
	NhlClass	lc
#endif
);


static NhlErrorTypes MapV40DHInitialize(
#if	NhlNeedProto
        NhlClass	class,
        NhlLayer	req,
        NhlLayer	new,
        _NhlArgList	args,
        int             num_args
#endif
);

static NhlErrorTypes  MapV40DHSetValues(
#if	NhlNeedProto
        NhlLayer	old,
        NhlLayer	reference,
        NhlLayer	new,
        _NhlArgList	args,
        int             num_args
#endif
);

static NhlErrorTypes    MapV40DHGetValues(
#if	NhlNeedProto
	NhlLayer        l,
	_NhlArgList     args,
	int             num_args
#endif
);

static NhlErrorTypes    MapV40DHDestroy(
#if	NhlNeedProto
	NhlLayer        l
#endif
);

static NhlErrorTypes MapV40DHUpdateDrawList(
#if	NhlNeedProto
	NhlLayer		instance,
        NhlBoolean  		init,
        NhlMapPlotLayer 	newmp,
        NhlMapPlotLayer 	oldmp,
        _NhlArgList		args,
        int             	num_args
#endif
);

static NhlErrorTypes MapV40DHDrawMapList(
#if	NhlNeedProto
	NhlLayer		instance,
        NhlMapPlotLayer 	mp,
        mpDrawOp		draw_op,
	NhlBoolean		init_draw
#endif
);


static void   load_hlumap_routines(
#if	NhlNeedProto
	NhlBoolean	flag
#endif
);

void   (_NHLCALLF(hlumapeod,HLUMAPEOD))(
#if	NhlNeedProto
	int *nout,
	int *nseg,
	int *idls,
	int *idrs,
	int *npts,
	float *pnts
#endif
);

NhlMapV40DataHandlerClassRec NhlmapV40DataHandlerClassRec = {
	{
/* class_name 		*/      "mapV40DataHandlerClass",
/* nrm_class 		*/      NrmNULLQUARK,
/* layer_size 		*/      sizeof(NhlMapV40DataHandlerLayerRec),
/* class_inited 	*/	False,
/* superclass		*/      (NhlClass)&NhlmapDataHandlerClassRec,
/* cvt_table		*/	NULL,

/* layer_resources 	*/   	NULL,
/* num_resources 	*/     	0,
/* all_resources 	*/	NULL,
/* callbacks		*/	NULL,
/* num_callbacks	*/	0,
/* class_callbacks	*/	NULL,
/* num_class_callbacks	*/	0,

/* class_part_initialize */     MapV40DHClassPartInit,
/* class_initialize 	*/  	NULL,
/* layer_initialize 	*/  	MapV40DHInitialize,
/* layer_set_values 	*/  	MapV40DHSetValues,
/* layer_set_values_hook */  	NULL,
/* layer_get_values 	*/  	MapV40DHGetValues,
/* layer_reparent 	*/  	NULL,
/* layer_destroy 	*/    	MapV40DHDestroy,
	},
	{
/* update_map_draw_list */	MapV40DHUpdateDrawList,
/* draw_map_list        */      MapV40DHDrawMapList
	}
};

NhlClass NhlmapV40DataHandlerClass = (NhlClass)&NhlmapV40DataHandlerClassRec;

static char *BGroup_Names[] = { 
	NhlmpNULLAREA,
	NhlmpALLNATIONAL,
	NhlmpALLGEOPHYSICAL,
	NhlmpLAND,
	NhlmpWATER,
	NhlmpINLANDWATER,
	NhlmpOCEANS,
	NhlmpCONTINENTS,
	NhlmpISLANDS,
	NhlmpLARGEISLANDS,
	NhlmpSMALLISLANDS,
	NhlmpALLUSSTATES,
	NhlmpUSSTATESLAND,
	NhlmpUSSTATESWATER };

static NrmQuark Qstring = NrmNULLQUARK;
static NrmQuark Qarea_names = NrmNULLQUARK;
static NrmQuark Qarea_types = NrmNULLQUARK;
static NrmQuark Qdynamic_groups = NrmNULLQUARK;
static NrmQuark Qfixed_groups = NrmNULLQUARK;

static NhlLayer Last_Instance = NULL;
static int Point_Count = 0;
static NhlMapPlotLayer Mpl;
static NhlMapPlotLayerPart *Mpp;
static NhlMapV40DataHandlerClassPart	*Mv4cp;
static NhlMapV40DataHandlerLayerPart	*Mv40p;
static NhlBoolean Grid_Setup;
static mpDrawOp Draw_Op;
static mpOutlineSet Outline_Set;

static mpDrawIdRec *DrawIds = NULL;
static short DrawId_Count = 600;

static short Id_Offset[4] = { 1,1004,223,440};
static short Id_End_Reg[4] = { 222,1361,439,1003 };
static short Draw_Special_Check[] = { 2,1005,223,441 };
static short Draw_Check;
static short US_Border;
static short US_Index;

#define mpUS_PO_IX	1031
#define mpUS_PS_IX	1015

/*
 * The following areas representing the US and the Great Lakes must
 * be excluded from the national and USState set when the USState set is used
 * with the National set.
 */

static short USState_Excludes[] = { 223 };

/*
 * The following area groupngs allow areas from the Geophysical map set
 * (the Ezmap 'CO' set) to be simulated using the National set (Ezmap 'PO').
 */

static short NSAmerica[] = { /* 1362 */
	1014,1026,1031,1037,1057,1061,1063,1064,1066,1068,1072,1074,1076,
	1077,1082,1090,1094,1097,1098,1101,1109,1113,1119,1120,1122 };

static short Tierra_Del_Fuego[] = { /* 1363 */
	1091,1103};

static short Dominican_Republic_and_Haiti[] = {  /* 1364 */
	1092,1100 };

static short Africa_Eurasia[] = { /* 1365 */
	1006,1127,1128,1129,1131,1132,1134,1137,1138,1139,1140,1142,1143,
	1144,1145,1151,1153,1154,1155,1156,1157,1158,1160,1161,1162,1163,
	1164,1165,1166,1167,1168,1169,1171,1174,1175,1176,1177,1178,1180,
	1184,1185,1186,1187,1188,1189,1190,1193,1194,1195,1196,1197,1198,
	1200,1201,1202,1203,1204,1205,1208,1209,1211,1213,1214,1215,1216,
	1219,1221,1222,1223,1225,1226,1229,1230,1231,1233,1236,1237,1238,
	1239,1240,1241,1242,1243,1245,1246,1248,1251,1252,1254,1260,1264,
	1265,1267,1270,1272,1274,1275,1276,1277,1279,1282,1285,1286,1287,
	1314,1317,1361 };

static short England_Scotland_Wales[] = { /* 1366 */
	1149,1150,1152 };

static short Ireland[] = { /* 1367 */
	1141,1146 };

static short Borneo[] = { /* 1368 */
	1293,1294,1298 };

static short New_Guinea[] = { /* 1369 */
	1324,1324,1336 };

static short PS_NSAmerica[] = { /* 1004 */
	450,462,1015,491,549,567,572,582,588,596,617,630,632,647,
	693,705,710,713,734,751,755,761,762,764};

static short PS_Tierra_Del_Fuego[] = {  /* 1005 */
	699,739};

static short PS_Dominican_Republic_and_Haiti[] = {  /* 1006 */
	700,724};

static short PS_Africa_Eurasia[] = { /* 1007 */
	442,769,770,771,773,774,776,779,780,781,782,784,785,786,787,793,
	795,796,797,798,799,800,802,803,804,805,806,807,808,809,810,811,
	813,816,817,818,819,820,822,826,827,828,829,830,831,832,835,836,
	837,838,839,840,842,843,848,845,846,847,850,851,853,855,856,857,
	858,861,863,864,865,867,868,871,872,873,875,878,879,880,881,882,
	883,884,885,887,888,890,893,894,896,902,906,907,909,912,914,916,
	917,918,919,921,924,927,928,929,956,959,1003};

static short PS_England_Scotland_Wales[] = { /* 1008 */
	791,792,794};

static short PS_Ireland[] = {  /* 1009 */
	783,788};

static short PS_Borneo[] = { /* 1010 */
	935,936,940};

static short PS_New_Guinea[] = {  /* 1011 */
	966,966,978};

static short PS_Bahamas_1[] = { /* 1012 */
	649,652,653,655,658 };

static short PS_Bahamas_2[] = { /* 1013 */
	687,663,664,665 };

static short PS_Bahamas_3[] = {  /* 1014 */
	648,656,657,654,659,660 };

static short *Exp_Ids[][2] = { 
	{NSAmerica,PS_NSAmerica},
	{Tierra_Del_Fuego,PS_Tierra_Del_Fuego},
	{Dominican_Republic_and_Haiti,PS_Dominican_Republic_and_Haiti},
	{Africa_Eurasia,PS_Africa_Eurasia},
	{England_Scotland_Wales,PS_England_Scotland_Wales},
	{Ireland,PS_Ireland},
	{Borneo,PS_Borneo},
	{New_Guinea,PS_New_Guinea},
	{NULL,PS_Bahamas_1},
	{NULL,PS_Bahamas_2},
	{NULL,PS_Bahamas_3},
	{NULL,NULL} };

static short Exp_Ids_Count[][2] = { 
	{NhlNumber(NSAmerica),NhlNumber(PS_NSAmerica)},
	{NhlNumber(Tierra_Del_Fuego),NhlNumber(PS_Tierra_Del_Fuego)},
	{NhlNumber(Dominican_Republic_and_Haiti),
	 NhlNumber(PS_Dominican_Republic_and_Haiti)},
	{NhlNumber(Africa_Eurasia),NhlNumber(PS_Africa_Eurasia)},
	{NhlNumber(England_Scotland_Wales),
	 NhlNumber(PS_England_Scotland_Wales)},
	{NhlNumber(Ireland),NhlNumber(PS_Ireland)},
	{NhlNumber(Borneo),NhlNumber(PS_Borneo)},
	{NhlNumber(New_Guinea),NhlNumber(PS_New_Guinea)},
	{0,NhlNumber(PS_Bahamas_1)},
	{0,NhlNumber(PS_Bahamas_2)},
	{0,NhlNumber(PS_Bahamas_3)},
	{0,0} };

static void mpLowerCase(char *string)
{
	char *cp = string;

	while (*cp != '\0') {
		*cp = tolower(*cp);
		cp++;
	}
}

#if 0

static void GetOutRecsByName(mpOutlineRec *orp, int nrecs, NhlString name)
{
	mpOutlineRec *lorp = orp;
	int i;
	int len = strlen(name) - 1;
	char tname[128];

	strcpy(tname,name);
	mpLowerCase(tname);
	if (name[len] == '*') {
		for (i = 0; i < nrecs; i++) {
			if (! strncmp(lorp->name,tname,len)) {
				printf("%s -- %d, %d\n",
				       lorp->name,lorp->id[0],lorp->id[1]);
			}
			lorp++;
		}
	}
	else {
		for (i = 0; i < nrecs; i++) {
			if (! strcmp(lorp->name,tname)) {
				printf("%s -- %d, %d\n",
				       lorp->name,lorp->id[0],lorp->id[1]);
			}
			lorp++;
		}
	}
}

static void GetOutRecsByType(mpOutlineRec *orp, int nrecs, mpOutlineType type)
{
	mpOutlineRec *lorp = orp;
	int i;

	for (i = 0; i < nrecs; i++) {
		if (lorp->type == type)
			printf("%s -- %d, %d\n",
			       lorp->name,lorp->id[0],lorp->id[1]);
		lorp++;
	}
}

void mpprintids(FILE *fp,short *idlist, int count)
{
	int i, j;
	for (i = 0; i < count; i++) 
		for (j = 0; j < mdhcp->outline_rec_count; j++)
			if (OutRecs[j].id[1] == idlist[i]) {
				fprintf(fp,"%d,",OutRecs[j].id[2]);
				break;
			}
}
#endif

static NhlErrorTypes Init_Outline_Recs
#if	NhlNeedProto
(
	NhlMapV40DataHandlerClass	mdhc,
        NhlString			entry_name
)
#else
(mdhc,entry_name)
	NhlMapV40DataHandlerClass	mdhc;
        NhlString			entry_name;
#endif
{
        NhlMapV40DataHandlerClassPart *mdhcp = &mdhc->mapv40dh_class;
	char *e_text;
	FILE *fp;
	char buf[256],name[128];
	int id0,id1,id2,cix0,cix1,type;
	mpOutlineRec *orp;
	int nalloced = 0;
	int bytes = 0, count;
	mpOutlineType last_type;
	Const char *db_path;
	char *full_name;

	if ((db_path = GetNCARGPath("database")) == NULL) {
		e_text = "%s: cannot find path to NCARG database";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	if ((full_name = NhlMalloc(strlen(db_path) + 
				   strlen(Nhl_mpMAPDATAFILE) + 2)) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	strcpy(full_name,db_path);
	strcat(full_name,"/");
	strcat(full_name,Nhl_mpMAPDATAFILE);
		      
	if ((fp = fopen(full_name,"r")) == NULL) {
		e_text = "%s: cannot open map data file: %s";
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  e_text,entry_name,Nhl_mpMAPDATAFILE);
		return NhlFATAL;
	}

	if ((mdhcp->outline_recs = (mpOutlineRec *) 
		NhlMalloc(mpALLOC_UNIT * sizeof(mpOutlineRec))) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	nalloced = mpALLOC_UNIT;

	last_type = mpOcean;
	mdhcp->outline_type_start_ix[mpOcean] = 0;
	while (fgets(buf,128,fp) != NULL) {
		sscanf(buf,"%d:%d:%d:%d:%d:%d%s",&type,
		       &id0,&cix0,&id1,&cix1,&id2,name);

		if (mdhcp->outline_rec_count == nalloced) {
			if ((mdhcp->outline_recs = (mpOutlineRec *)
			     NhlRealloc(mdhcp->outline_recs,
                                        (nalloced + mpALLOC_UNIT) 
					* sizeof(mpOutlineRec))) == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return NhlFATAL;
			}
			nalloced += mpALLOC_UNIT;
		}

		orp = &mdhcp->outline_recs[mdhcp->outline_rec_count];

		orp->id[0] = id0;
		orp->cix[0] = cix0;
		orp->id[1] = id1;
		orp->cix[1] = cix1;
		orp->id[2] = id2;
		orp->type = (mpOutlineType) type;
#ifndef HLU_WRITE_TABLES 
		mpLowerCase(name);
#endif
		count = strlen(name) + 1;
		orp->name = (char *) malloc(count);
		bytes += count;
		strcpy(orp->name,name);
		if (orp->type != last_type) {
			mdhcp->outline_type_start_ix[orp->type] =
                                mdhcp->outline_rec_count;
			last_type = orp->type;
		}
		mdhcp->outline_rec_count++;
	}
	mdhcp->outline_type_start_ix[last_type+1] = mdhcp->outline_rec_count; 
	fclose(fp);
	NhlFree(full_name);

#ifdef HLU_WRITE_TABLES 

	printf("<table border=1 width=100%%>\n");
	printf("<tr><th>Element<br>Index</th><th>Type</th><th>Fixed<br> Group</th><th>Dynamic<br>Group</th><th>Area Name</th><tr>\n");
        for (i = 0; i < mdhcp->outline_rec_count; i++) {
		printf("<tr>\n");
		printf("<td>%d</td><td>%d</td><td>%d</td><td>%d</td><td>%s</td>\n",
		       i,
		       (int)mdhcp->outline_recs[i].type,
		       mdhcp->outline_recs[i].cix[0],
		       mdhcp->outline_recs[i].cix[1],
		       mdhcp->outline_recs[i].name);
		printf("</tr>\n");
	}
	printf("</table>\n");

	exit(1);

#endif

#if 0        
	printf("records read %d, bytes %d + record size %d\n",
               mdhcp->outline_rec_count, 
	       bytes,mdhcp->outline_rec_count * sizeof(mpOutlineRec));

	GetOutRecsByType(mdhcp->outline_recs,
                         mdhcp->outline_rec_count,mpInlandWater);
	GetOutRecsByName(mdhcp->outline_recs,
                         mdhcp->outline_rec_count,"CANAD*");
#endif
	return NhlNOERROR;
}    
        

static NhlErrorTypes
MapV40DHClassPartInit
#if	NhlNeedProto
(
	NhlClass	lc
)
#else
(lc)
	NhlClass	lc;
#endif
{
	NhlMapV40DataHandlerClass	mdhc = (NhlMapV40DataHandlerClass)lc;
	NhlErrorTypes		ret = NhlNOERROR;
        NhlString		entry_name = "MapV40DHClassPartInit";
        
	Qstring = NrmStringToQuark(NhlTString);
	Qarea_names = NrmStringToQuark(NhlNmpAreaNames);
	Qarea_types = NrmStringToQuark(NhlNmpAreaTypes);
	Qdynamic_groups = NrmStringToQuark(NhlNmpDynamicAreaGroups);
	Qfixed_groups = NrmStringToQuark(NhlNmpFixedAreaGroups);
        
        ret = Init_Outline_Recs(mdhc,entry_name);
        if (ret < NhlWARNING) {
                char e_text[] = "%s: error initializing map outline records";
                NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
                return(ret);
        }
        
	load_hlumap_routines(False);
        
	return NhlNOERROR;
}

/*
 * Function:  mdhManageDynamicArrays
 *
 * Description: Creates and manages internal copies of each of the 
 *	MapDataHandler GenArrays. 
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: 
 */

/*ARGSUSED*/
static NhlErrorTypes    mdhManageDynamicArrays
#if	NhlNeedProto
(
	NhlMapV40DataHandlerLayer	mv4new, 
	NhlMapV40DataHandlerLayer	mv4old,
	NhlBoolean			init,
	_NhlArgList			args,
	int				num_args)
#else
(mdhnew,mdhold,init,args,num_args)
	NhlMapV40DataHandlerLayer	mv4new;
	NhlMapV40DataHandlerLayer	mv4old;
	NhlBoolean			init;
	_NhlArgList			args;
	int				num_args;
#endif

{
	NhlMapDataHandlerLayerPart	*mdhp = &(mv4new->mapdh);
	NhlMapDataHandlerLayerPart	*omdhp = &(mv4old->mapdh);
        NhlMapV40DataHandlerClassPart	*mv4cp = 
                &((NhlMapV40DataHandlerClass)
                  mv4new->base.layer_class)->mapv40dh_class;
        
	NhlErrorTypes ret = NhlNOERROR;
	char *entry_name;
	char *e_text;
	NhlGenArray ga;
	int i;
	int *ip;
	NhlString *sp;
	NhlBoolean need_check;
	NhlBoolean use_default;
        int outline_rec_count;
        mpOutlineRec *outline_recs;

	entry_name =  init ?
                "MapDataHandlerInitialize" : "MapDataHandlerSetValues";

        outline_rec_count = mv4cp->outline_rec_count;
        outline_recs = mv4cp->outline_recs;
        
/*
 * Area Name specifiers
 */
	ga = init ? NULL : omdhp->area_names;

	if (ga != mdhp->area_names) {
		if (! mdhp->area_names ||
                    mdhp->area_names->num_elements != outline_rec_count) {
			e_text = 
			  "%s: %s GenArray must contain %d elements: ignoring";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  NhlNmpAreaNames,outline_rec_count);
			ret = MIN(NhlWARNING,ret);
			mdhp->area_names = ga;
		}
		else {
			NhlFreeGenArray(ga);
			if ((ga = _NhlCopyGenArray(mdhp->area_names,
						   True)) == NULL) {
				e_text = "%s: error copying %s GenArray";
				NhlPError(NhlFATAL,
					  NhlEUNKNOWN,e_text,entry_name,
					  NhlNmpAreaNames);
				return NhlFATAL;
			}
			omdhp->area_names = NULL;
			mdhp->area_names = ga;
			/* Check elements for null strings */
			sp = (NhlString *) mdhp->area_names->data;
			for (i = 0; i < mdhp->area_names->num_elements; i++) {
				if (sp[i] == NULL || strlen(sp[i]) == 0) {
					e_text = 
		 "%s: Null or zero length %s string for index %d: defaulting";
					NhlPError(NhlWARNING,NhlEUNKNOWN,
						  e_text,entry_name,
						  NhlNmpAreaNames,i);
					ret = MIN(ret,NhlWARNING);
					if (sp[i] != NULL) NhlFree(sp[i]);
					sp[i] = NhlMalloc(
					    strlen(outline_recs[i].name) + 1);
					if (sp[i] == NULL) {
						e_text = 
				       "%s: dynamic memory allocation error";
						NhlPError(NhlFATAL,NhlEUNKNOWN,
							  e_text,entry_name);
						return NhlFATAL;
					}
					strcpy(sp[i],outline_recs[i].name);
				}
			}
		}
	}

/*
 * The dynamic area Groups
 */
	ga = init ? NULL : omdhp->dynamic_groups;

	need_check = False;
	if (ga != mdhp->dynamic_groups) {
		if (! mdhp->dynamic_groups) {
                        NhlFreeGenArray(ga);
			ga = NULL;
                }
		else if (mdhp->dynamic_groups->num_elements != outline_rec_count) {
			e_text = 
			  "%s: %s GenArray must contain %d elements: ignoring";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  NhlNmpDynamicAreaGroups,outline_rec_count);
			ret = MIN(NhlWARNING,ret);
		}
		else {
			NhlFreeGenArray(ga);
			if ((ga = _NhlCopyGenArray(mdhp->dynamic_groups,
 						   True)) == NULL) {
				e_text = "%s: error copying %s GenArray";
				NhlPError(NhlFATAL,
					  NhlEUNKNOWN,e_text,entry_name,
					  NhlNmpAreaNames);
				return NhlFATAL;
			}
			need_check = True;
			omdhp->dynamic_groups = NULL;
		}
		mdhp->dynamic_groups = ga;

	}
	if (ga && 
	    (need_check || mdhp->area_group_count < omdhp->area_group_count)) {
		ip = (int *) ga->data;
		for (i=0; i < ga->num_elements; i++) {
			use_default = False;
			if (ip[i] < NhlmpOCEANGROUPINDEX)
				use_default = True;
			else if (ip[i] > mdhp->area_group_count - 1) {
				e_text =
	         "%s: %s index %d holds an invalid fill group id: defaulting";
				NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
					  entry_name,
					  NhlNmpDynamicAreaGroups,i);
				ret = MIN(ret, NhlWARNING);
				use_default = True;
			}
			if (use_default)
				ip[i] = outline_recs[i].cix[1];
		}
	}

/*
 * The fixed area groups are read only
 */
	ga = init ? NULL : omdhp->fixed_groups;

	if (ga != mdhp->fixed_groups) {
		e_text = "%s: attempt to set read-only resource %s ignored";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
			  NhlNmpFixedAreaGroups);
		ret = MIN(ret, NhlWARNING);
		mdhp->fixed_groups = ga;
	}


/*
 * Area types are read only
 */
	ga = init ? NULL : omdhp->area_types;

	if (ga != mdhp->area_types) {
		e_text = "%s: attempt to set read-only resource %s ignored";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
			  NhlNmpAreaTypes);
		ret = MIN(ret, NhlWARNING);
		mdhp->area_types = ga;
	}

	return ret;
}

/*
 * Function:	MapV40DHInitialize
 *
 * Description: 
 *
 * In Args: 	class	objects layer_class
 *		req	instance record of requested values
 *		new	instance record of new object
 *		args	list of resources and values for reference
 *		num_args 	number of elements in args.
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:
 */
/*ARGSUSED*/
static NhlErrorTypes
MapV40DHInitialize
#if	NhlNeedProto
(
	NhlClass	class,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		num_args
)
#else
(class,req,new,args,num_args)
        NhlClass   class;
        NhlLayer        req;
        NhlLayer        new;
        _NhlArgList     args;
        int             num_args;
#endif
{
        NhlMapV40DataHandlerLayer mv40l = (NhlMapV40DataHandlerLayer) new;
        NhlMapV40DataHandlerLayerPart *mv40p = &mv40l->mapv40dh;
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*entry_name = "MapV40DHInitialize";
	char			*e_text;

        mv40p->new_co_amap_req = True;
	mv40p->co_aws_id = -1;
        mv40p->new_us_amap_req = True;
	mv40p->us_aws_id = -1;
        
	mv40p->fill_rec_alloc = 0;
	mv40p->fill_rec_count = 0;
	mv40p->fill_recs = NULL;
	mv40p->outline_rec_alloc = 0;
	mv40p->outline_rec_count = 0;
	mv40p->outline_recs = NULL;
        

/* Manage the dynamic arrays */

	subret = mdhManageDynamicArrays((NhlMapV40DataHandlerLayer)new,
                                        (NhlMapV40DataHandlerLayer)req,
                                        True,args,num_args);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error managing dynamic arrays";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

        return ret;
}

/*
 * Function:	MapV40DHSetValues
 *
 * Description: 
 *
 * In Args:	old	copy of old instance record
 *		reference	requested instance record
 *		new	new instance record	
 *		args 	list of resources and values for reference
 *		num_args	number of elements in args.
 *
 * Out Args:	NONE
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:
 */
/*ARGSUSED*/
static NhlErrorTypes MapV40DHSetValues
#if	NhlNeedProto
(
	NhlLayer	old,
	NhlLayer	reference,
	NhlLayer	new,
	_NhlArgList	args,
	int		num_args
)
#else
(old,reference,new,args,num_args)
	NhlLayer	old;
	NhlLayer	reference;
	NhlLayer	new;
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*entry_name = "MapV40DHSetValues";
	char			*e_text;

/* Manage the dynamic arrays */

	subret = mdhManageDynamicArrays((NhlMapV40DataHandlerLayer)new,
                                        (NhlMapV40DataHandlerLayer)old,
                                        False,args,num_args);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error managing dynamic arrays";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

        return ret;
}

/*
 * Function:  mpGenArraySubsetCopy
 *
 * Description: Since the internal GenArrays maintained by the MapPlot object
 *      may be bigger than the size currently in use, this function allows
 *      a copy of only a portion of the array to be created. This is for
 *      use by the GetValues routine when returning GenArray resources to
 *      the user level. The array is assumed to be valid. The only pointer
 *      type arrays that the routine can handle are NhlString arrays.
 *      Note: this might be another candidate for inclusion as a global
 *      routine.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */

static NhlGenArray mdhGenArraySubsetCopy
#if	NhlNeedProto
        (NhlGenArray    ga,
        int             length)
#else
(ga,length)
        NhlGenArray     ga;
        int             length;
#endif
{
        NhlGenArray gto;

        if (length > ga->num_elements)
                return NULL;

        if ((gto = _NhlCopyGenArray(ga,False)) == NULL) {
                return NULL;
        }
        if ((gto->data = (NhlPointer) NhlMalloc(length * ga->size)) == NULL) {
                return NULL;
        }
        if (ga->typeQ != Qstring) {
                memcpy((void *)gto->data, (Const void *) ga->data,
                       length * ga->size);
        }
        else {
                NhlString *cfrom = (NhlString *) ga->data;
                NhlString *cto = (NhlString *) gto->data;
                int i;
                for (i=0; i<length; i++) {
                        if ((*cto = (char *)
                             NhlMalloc(strlen(*cfrom)+1)) == NULL) {
                                return NULL;
                        }
                        strcpy(*cto++,*cfrom++);
                }
        }
        gto->num_elements = length;
	gto->my_data = True;
        return gto;
}

/*
 * Function:	mdhGetNewGenArray
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlGenArray mdhGetNewGenArray
#if	NhlNeedProto
(
 	NhlMapV40DataHandlerLayer       mv40l,
	NrmQuark			quark,
        NhlString			entry_name
)
#else
(mv40l,quark,entry_name)
	NhlMapV40DataHandlerLayer       mv40l;
	NrmQuark			quark;
        NhlString			entry_name;
#endif
{
        NhlMapV40DataHandlerClassPart	*mv4cp = 
                &((NhlMapV40DataHandlerClass)
                  mv40l->base.layer_class)->mapv40dh_class;
	char *e_text;
	ng_size_t i,len;
	NhlGenArray ga;
        int outline_rec_count;
        mpOutlineRec *outline_recs;

        outline_rec_count = mv4cp->outline_rec_count;
        outline_recs = mv4cp->outline_recs;
        
	if (quark == Qarea_names) {
		NhlString	*sp;
		len = outline_rec_count;
		if ((sp = NhlMalloc(sizeof(NhlString)*len)) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,
				  NhlEUNKNOWN,e_text,entry_name);
			return NULL;
		}
		for (i = 0; i < len; i++) {
			if ((sp[i] = NhlMalloc
                             (strlen(outline_recs[i].name) + 1)) == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return NULL;
			}
			strcpy(sp[i],outline_recs[i].name);
		}
		if ((ga = NhlCreateGenArray(sp,NhlTString,sizeof(NhlString),
					    1,&len)) == NULL) {
			e_text = "%s: error creating gen array";
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  e_text,entry_name);
			return NULL;
		}
                ga->my_data = True;
		return ga;
	}
	else if (quark == Qarea_types) {
		int	*ip;
		len = outline_rec_count;
		if ((ip = NhlMalloc(sizeof(int)*len)) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,
				  NhlEUNKNOWN,e_text,"MapPlotGetValues");
			return NULL;
		}
		for (i = 0; i < len; i++) {
			ip[i] = (int) outline_recs[i].type;
		}
		if ((ga = NhlCreateGenArray(ip,NhlTInteger,sizeof(int),
					    1,&len)) == NULL) {
			e_text = "%s: error creating gen array";
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  e_text,entry_name);
			return NULL;
		}
                ga->my_data = True;
		return ga;

	}
	else if (quark == Qdynamic_groups || quark == Qfixed_groups) {
		int	*ip;
		int	index = quark == Qdynamic_groups ? 1 : 0;

		len = outline_rec_count;
		if ((ip = NhlMalloc(sizeof(int)*len)) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,
				  NhlEUNKNOWN,e_text,"MapPlotGetValues");
			return NULL;
		}
		for (i = 0; i < len; i++) {
			ip[i] = (int) outline_recs[i].cix[index];
		}
		if ((ga = NhlCreateGenArray(ip,NhlTInteger,sizeof(int),
					    1,&len)) == NULL) {
			e_text = "%s: error creating gen array";
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  e_text,"MapPlotGetValues");
			return NULL;
		}
                ga->my_data = True;
		return ga;
	}
	return NULL;
}

/*
 * Function:    MapV40DHGetValues
 *
 * Description: Retrieves the current setting of MapV40DataHandler resources.
 *      Actually the resources belong to the superclass MapDataHandler --
 *      but they get their contents from the subclass.
 *
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 *      Memory is allocated when any of the following resources are retrieved:
 *		NhlNmpAreaNames
 *		NhlNmpAreaTypes
 *		NhlNmpDynamicAreaGroups
 *		NhlNmpSpecifiedFillColors
 *
 *      The caller is responsible for freeing this memory.
 */

static NhlErrorTypes    MapV40DHGetValues
#if	NhlNeedProto
(NhlLayer l, _NhlArgList args, int num_args)
#else
(l,args,num_args)
        NhlLayer        l;
        _NhlArgList     args;
        int     	num_args;
#endif
{
        NhlMapV40DataHandlerLayer mv40l = (NhlMapV40DataHandlerLayer) l;
        NhlMapDataHandlerLayerPart *mdhp = &mv40l->mapdh;
        NhlGenArray ga;
        NhlString e_text,entry_name = "MapV40DHGetValues";
        int i, count = 0;
	NhlBoolean create_it;

        for (i = 0; i < num_args; i++ ) {

		create_it = False;
                ga = NULL;
                
                if (args[i].quark == Qarea_names) {
			create_it = True;
                        ga = mdhp->area_names;
                        count = ga ? ga->num_elements : 0;
                }
                else if (args[i].quark == Qarea_types) {
			create_it = True;
                        ga = mdhp->area_types;
                        count = ga ? ga->num_elements : 0;
                }
                else if (args[i].quark == Qdynamic_groups) {
			create_it = True;
                        ga = mdhp->dynamic_groups;
                        count = ga ? ga->num_elements : 0;
                }
                else if (args[i].quark == Qfixed_groups) {
			create_it = True;
                        ga = mdhp->fixed_groups;
                        count = ga ? ga->num_elements : 0;
                }
                if (ga != NULL) {
                        if ((ga = mdhGenArraySubsetCopy(ga, count)) == NULL) {
                                e_text = "%s: error copying %s GenArray";
                                NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
                                          entry_name,
					  NrmQuarkToString(args[i].quark));
                                return NhlFATAL;
                        }
                        *((NhlGenArray *)(args[i].value.ptrval)) = ga;
                }
		else if (create_it) {
			if ((ga = mdhGetNewGenArray(mv40l,args[i].quark,
                                                    entry_name)) 
			    == NULL) {
				e_text = "%s: error getting %s GenArray";
                                NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
                                          entry_name,
					  NrmQuarkToString(args[i].quark));
                                return NhlFATAL;
                        }
                        *((NhlGenArray *)(args[i].value.ptrval)) = ga;

		}
        }

        return(NhlNOERROR);

}


/*
 * Function:    MapV40DHDestroy
 *
 * Description: Retrieves the current setting of MapV40DataHandler resources.
 *      Actually the resources belong to the superclass MapDataHandler --
 *      but they get their contents from the subclass.
 *
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 *      Memory is allocated when any of the following resources are retrieved:
 *		NhlNmpAreaNames
 *		NhlNmpAreaTypes
 *		NhlNmpDynamicAreaGroups
 *		NhlNmpSpecifiedFillColors
 *
 *      The caller is responsible for freeing this memory.
 */

static NhlErrorTypes    MapV40DHDestroy
#if	NhlNeedProto
(
        NhlLayer l
        )
#else
(l)
        NhlLayer        l;
#endif
{
        NhlMapV40DataHandlerLayer mv40l = (NhlMapV40DataHandlerLayer) l;
        NhlMapV40DataHandlerLayerPart *mv40p = &mv40l->mapv40dh;

	if (mv40p->fill_recs != NULL)
		NhlFree(mv40p->fill_recs);
	if (mv40p->outline_recs != NULL)
		NhlFree(mv40p->outline_recs);
	if (mv40p->co_aws_id > 0)
		_NhlFreeWorkspace(mv40p->co_aws_id);
	if (mv40p->us_aws_id > 0)
		_NhlFreeWorkspace(mv40p->us_aws_id);

        return NhlNOERROR;
}

/*
 * Routines called by MapV40DHUpdateDrawList to set up the list of areas
 * to be drawn -- there are two lists, one for fill and masking, the other
 * for outlines.
 */

/*
 * Function:  mdhUpdateDrawGroups
 *
 * Description: 
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: 
 */


/*ARGSUSED*/
static NhlErrorTypes    mdhUpdateDrawGroups
#if	NhlNeedProto
(
        NhlMapV40DataHandlerLayer 	mv40l,
	NhlMapPlotLayerPart	*mpp,
	mpDrawOp		draw_op,
	int			draw_mode,
	int			index,
	int			group_ix,
	char			*entry_name
)
#else
(mv40l,draw_op,draw_mode,index,group_ix,entry_name)
	NhlMapV40DataHandlerLayer 	mv40l;
	NhlMapPlotLayerPart	*mpp;
	mpDrawOp		draw_op;
	int			draw_mode;
	int			index;
	int			group_ix;
	char			*entry_name;

#endif
{
        NhlMapV40DataHandlerLayerPart *mv40p = &mv40l->mapv40dh;
	char		*e_text;
	mpNameRec	*groups;
	mpNameRec	tmpl;
	mpGlobalSetMode *gmode;
	mpStateSetMode	*smode;
	int		list[NhlmpOUTLINE_TYPE_COUNT];
	int		i,count = 0;

	switch (draw_op) {
	case mpDRAWFILL:
		groups = mv40p->fill_groups;
		gmode = &mv40p->global_fill_mode;
		smode = &mv40p->usstates_fill_mode;
		break;
	case mpDRAWOUTLINE:
		groups = mv40p->outline_groups;
		gmode = &mv40p->global_outline_mode;
		smode = &mv40p->usstates_outline_mode;
		break;
	default:
		e_text = "%s: internal enumeration error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}

	switch ((mpBGroups) group_ix) {
	case mpNULLAREA:
		break;
	case mpALLNATIONAL:
		list[0] = mpNational;
		list[1] = mpOcean;
		list[2] = mpContinent;
		list[3] = mpLargeIsland;
		list[4] = mpSmallIsland;
		list[5] = mpInlandWater;
		count = 6;
		*gmode = mpNAT;
		break;
	case mpALLGEOPHYSICAL:
		list[0] = mpOcean;
		list[1] = mpContinent;
		list[2] = mpLargeIsland;
		list[3] = mpSmallIsland;
		list[4] = mpInlandWater;
		count = 5;
		*gmode = MAX(*gmode,mpGEO);
		break;
	case mpLAND:
		list[0] = mpContinent;
		list[1] = mpLargeIsland;
		list[2] = mpSmallIsland;
		count = 3;
		*gmode = MAX(*gmode,mpGEO);
		break;
	case mpWATER:
		list[0] = mpOcean;
		list[1] = mpInlandWater;
		count = 2;
		*gmode = MAX(*gmode,mpGEO);
		break;
	case mpINLANDWATER:
		list[0] = mpInlandWater;
		count = 1;
		*gmode = MAX(*gmode,mpGEO);
		break;
	case mpOCEANS:
		list[0] = mpOcean;
		count = 1;
		*gmode = MAX(*gmode,mpGEO);
		break;
	case mpCONTINENTS:
		list[0] = mpContinent;
		count = 1;
		*gmode = MAX(*gmode,mpGEO);
		break;
	case mpISLANDS:
		list[0] = mpLargeIsland;
		list[1] = mpSmallIsland;
		count = 2;
		*gmode = MAX(*gmode,mpGEO);
		break;
	case mpLARGEISLANDS:
		list[0] = mpLargeIsland;
		count = 1;
		*gmode = MAX(*gmode,mpGEO);
		break;
	case mpSMALLISLANDS:
		list[0] = mpSmallIsland;
		count = 1;
		*gmode = MAX(*gmode,mpGEO);
		break;
	case mpALLUSSTATES:
		list[0] = mpUSStateLand;
		list[1] = mpUSStateWater;
		count = 2;
		*smode = mpSET;
		break;
	case mpUSSTATESLAND:
		list[0] = mpUSStateLand;
		count = 1;
		*smode = mpSET;
		break;
	case mpUSSTATESWATER:
		list[0] = mpUSStateWater;
		count = 1;
		*smode = MAX(*smode,mpIMPLIED_SET);
		break;
	default:
		e_text = "%s: internal enumeration error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}

	tmpl.u.f.s_col = 0;
	tmpl.u.f.s_pat = 0;
	tmpl.u.f.s_scl = 0;
	tmpl.u.f.draw_mode = draw_mode;
	if (index != mpNOINDEX) {
		if (mpp->spec_fill_color_count > index) {
			int *ip = (int *) mpp->spec_fill_colors->data;
			if (ip[index] != NhlUNSPECIFIEDCOLOR)
				tmpl.u.f.s_col =  1;
		}
		if (mpp->spec_fill_pattern_count > index) {
			int *ip = (int *) mpp->spec_fill_patterns->data;
			if (ip[index] != NhlUNSPECIFIEDFILL)
				tmpl.u.f.s_pat =  1;
		}
		if (mpp->spec_fill_scale_count > index) {
			float *fp = (float *) mpp->spec_fill_scales->data;
			if (fp[index] != NhlmpUNSETFILLSCALE )
				tmpl.u.f.s_scl =  1;
		}
	}
	for (i = 0; i < count; i++) {
		groups[list[i]].s_ix = index;
		groups[list[i]].u.flags = tmpl.u.flags;
	}

	return NhlNOERROR;
}

/*
 * Function:	mpSetFlags
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes mpSetFlags
#if	NhlNeedProto
(
        NhlMapV40DataHandlerLayer 	mv40l,
	NhlMapPlotLayerPart		*mpp,
	mpNameRec			*nrec,
	NhlBoolean			invert,
	int				index
)
#else
(mv40l,mpp,nrec,orec,invert,index)
        NhlMapV40DataHandlerLayer 	mv40l;
	NhlMapPlotLayerPart		*mpp;
	mpNameRec			*nrec;
	NhlBoolean			invert;
	int				index;
#endif
{
        NhlMapDataHandlerLayerPart *mdhp = &mv40l->mapdh;
        NhlMapV40DataHandlerClassPart *mdhcp =
         &((NhlMapV40DataHandlerClass)mv40l->base.layer_class)->mapv40dh_class;

	nrec->s_ix = index;
	nrec->u.f.s_col = 0;
	nrec->u.f.s_pat = 0;
	nrec->u.f.s_scl = 0;
	
	if (index != mpNOINDEX) {
		if (mpp->spec_fill_color_count > index) {
			int *ip = (int *) mpp->spec_fill_colors->data;
			if (ip[index] != NhlUNSPECIFIEDCOLOR)
				nrec->u.f.s_col =  1;
		}
		if (mpp->spec_fill_pattern_count > index) {
			int *ip = (int *) mpp->spec_fill_patterns->data;
			if (ip[index] != NhlUNSPECIFIEDFILL)
				nrec->u.f.s_pat =  1;
		}
		if (mpp->spec_fill_scale_count > index) {
			float *fp = (float *) mpp->spec_fill_scales->data;
			if (fp[index] > NhlmpUNSETFILLSCALE)
				nrec->u.f.s_scl =  1;
		}
	}
	if (mpp->spec_fill_priority == NhlGEOPHYSICALPRIORITY) {
		if (! invert)
			nrec->ix = mdhcp->outline_recs[nrec->name_ix].cix[0];
		else if (mdhp->dynamic_groups != NULL) {
			int *dyn_grps = (int *) mdhp->dynamic_groups->data;
			nrec->ix = dyn_grps[nrec->name_ix];
		}
		else 
			nrec->ix = mdhcp->outline_recs[nrec->name_ix].cix[1];
	}
	else {
		if (invert)
			nrec->ix = mdhcp->outline_recs[nrec->name_ix].cix[0];
		else if (mdhp->dynamic_groups != NULL) {
			int *dyn_grps = (int *) mdhp->dynamic_groups->data;
			nrec->ix = dyn_grps[nrec->name_ix];
		}
		else 
			nrec->ix = mdhcp->outline_recs[nrec->name_ix].cix[1];
	}
	return NhlNOERROR;
}
/*
 * Function:	mdhUpdateNameRecs
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes mdhUpdateNameRecs
#if	NhlNeedProto
(
        NhlMapV40DataHandlerLayer mv40l,
        NhlMapPlotLayerPart	*mpp,
	mpDrawOp		draw_op,
	NhlString		name,
	int			draw_mode,
	int			index,
	NhlString		entry_name
)
#else
(mv40l,mpp,draw_op,name,draw_mode,index,entry_name)
        NhlMapV40DataHandlerLayer mv40l;
	NhlMapPlotLayerPart	*mpp;
	mpDrawOp		draw_op;
	NhlString		name;
	int			draw_mode;
	int			index;
	NhlString		entry_name;
#endif
{
        NhlMapV40DataHandlerLayerPart *mv40p = &mv40l->mapv40dh;
        NhlMapV40DataHandlerClassPart *mdhcp =
         &((NhlMapV40DataHandlerClass)mv40l->base.layer_class)->mapv40dh_class;
	char		*e_text;
	int		i;
	int		len = strlen(name);
	NhlBoolean	found = False, found_all = False, found_one = False;
	NhlString	*names = NULL;
	char		*cp;
	NhlBoolean	invert = False;
	NhlString	comp_name;
	mdhCompType comp_type = mdhSTRCMP;
	mpNameRec	**nrecs;
	int		*alloc;
	int		*count;
	mpGlobalSetMode *gmode;
	mpStateSetMode	*smode;
	char		buf[256];
	char		*np = buf;

	strcpy(buf,name);

	switch (draw_op) {
	case mpDRAWFILL:
		nrecs = &mv40p->fill_recs;
		alloc = &mv40p->fill_rec_alloc;
		count = &mv40p->fill_rec_count;
		gmode = &mv40p->global_fill_mode;
		smode = &mv40p->usstates_fill_mode;
		break;
	case mpDRAWOUTLINE:
		nrecs = &mv40p->outline_recs;
		alloc = &mv40p->outline_rec_alloc;
		count = &mv40p->outline_rec_count;
		gmode = &mv40p->global_outline_mode;
		smode = &mv40p->usstates_outline_mode;
		break;
	default:
		e_text = "%s: internal enumeration error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}

	if (mpp->area_names != NULL)
		names = (NhlString *) mpp->area_names->data;

	if (*nrecs == NULL) {
		if ((*nrecs = (mpNameRec *) 
		     NhlMalloc(sizeof(mpNameRec) * mpALLOC_UNIT)) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		*alloc = mpALLOC_UNIT;
	}

	if (np[len - 1] == '*') {
		comp_type = mdhSTRNCMP;
		np[len - 1] = '\0';
		len --;
	}

	if (np[0] == '!') {
		invert = True;
		len --;
		np++;
	}
	if (np[0] == '*') {
		if (comp_type == mdhSTRNCMP)
			comp_type = mdhSTRSTR;
		else
			comp_type = mdhSTRSTREND;
		len--;
		np++;
	}
	
	for (i = 0; i < mdhcp->outline_rec_count; i++) {
		comp_name = names ? names[i] : mdhcp->outline_recs[i].name; 
		if (*count == *alloc) {
			if ((*nrecs = (mpNameRec *) 
			     NhlRealloc(*nrecs,sizeof(mpNameRec)
					* (*alloc + mpALLOC_UNIT))) == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return NhlFATAL;
			}
			*alloc += mpALLOC_UNIT;
		}
		switch (comp_type) {
		case mdhSTRCMP:
			if (! strcmp(comp_name,np)) {
				found = True;
				found_one = True;
				found_all = True;
			}
			break;
		case mdhSTRNCMP:
			if (! strncmp(comp_name,np,len)) {
				found_one = True;
				found = True;
			}
			break;
		case mdhSTRSTR:
			if (strstr(comp_name,np) != NULL) {
				found_one = True;
				found = True;
			}
			break;
		case mdhSTRSTREND:
			if ((cp = strstr(comp_name,np)) != NULL &&
			    (comp_name + strlen(comp_name) - cp == len)) {
				found_one = True;
				found = True;
			}
			break;
		}
		if (found) {
			(*nrecs)[*count].name_ix = i;
			(*nrecs)[*count].u.f.draw_mode = draw_mode;
			mpSetFlags(mv40l,mpp,&((*nrecs)[*count]),invert,index);

			if (mdhcp->outline_recs[i].type == mpNational) {
				*gmode = MAX(*gmode,mpIMPLIED_NAT);
			}
			else if (mdhcp->outline_recs[i].type ==
                                 mpUSStateLand ||
                                 mdhcp->outline_recs[i].type ==
                                 mpUSStateWater) {
				*smode = MAX(*smode,mpIMPLIED_SET);
			}
			else {
				*gmode = MAX(*gmode,mpGEO);
			}

			found = False;
			(*count)++;
		}
		if (found_all)
			break;
	}

	if (! found_one) {
		e_text = "%s: invalid boundary specification string: \"%s\"";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,name);
		return NhlWARNING;
	}
	return NhlNOERROR;
}

/*
 * Function:  mdhBuildFillDrawList
 *
 * Description: 
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: 
 */


/*ARGSUSED*/
static NhlErrorTypes    mdhBuildFillDrawList
#if	NhlNeedProto
(
        NhlMapV40DataHandlerLayer mv40l,
	NhlMapPlotLayer	mpnew, 
	NhlMapPlotLayer	mpold,
	NhlBoolean	init,
	_NhlArgList	args,
	int		num_args,
        NhlString	entry_name)
#else
(mv40l,mpnew,mpold,init,args,num_args,entry_name)
	NhlMapV40DataHandlerLayer mv40l;
	NhlMapPlotLayer	mpnew;
	NhlMapPlotLayer	mpold;
	NhlBoolean	init;
	_NhlArgList	args;
	int		num_args;
        NhlString	entry_name;
#endif
{
        NhlMapV40DataHandlerLayerPart *mv40p = &mv40l->mapv40dh;
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlMapPlotLayerPart	*mpp = &(mpnew->mapplot);
	int			i,j;
	NhlString		*sp;
	NhlBoolean		found;
	int			*clrs;

/*
 * If the National set is specified all countries are drawn using the 
 * national color scheme. If however, the geophysical set is specified along
 * with one or more explicitly specified national areas, the national set
 * must be used, but areas not explicitly specified are drawn using the
 * geophysical set generic land color. In this case the National set is
 * specified implicitly.
 */

/*
 * don't go through all this if nothing has changed
 */

	clrs = (int *) mpp->fill_colors->data;
	for (i = 0; i < NhlNumber(mv40p->fill_groups); i++) {
		mv40p->fill_groups[i].u.flags = 0;
		mv40p->fill_groups[i].ix = 0;
		mv40p->fill_groups[i].s_ix = (unsigned char) -1;
	}
	mv40p->global_fill_mode = mpNONE;
	mv40p->usstates_fill_mode = mpNOSET;
	mv40p->fill_rec_count = 0;

/* 
 * Determine which map element types are required and consequently which
 * outline sets to use; first examine the boundary sets enumerative, then
 * the include and exclude lists.
 */
	switch (mpp->fill_boundaries) {
	case NhlNOBOUNDARIES:
	default:
		break;
	case NhlGEOPHYSICAL:
		subret = mdhUpdateDrawGroups(mv40l,mpp,mpDRAWFILL,mpDRAW,
					    mpNOINDEX,
					    mpALLGEOPHYSICAL,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		break;
	case NhlNATIONAL:
		subret = mdhUpdateDrawGroups(mv40l,mpp,mpDRAWFILL,mpDRAW,
                                             mpNOINDEX,
                                             mpALLNATIONAL,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		break;
	case NhlUSSTATES:
		subret = mdhUpdateDrawGroups(mv40l,mpp,mpDRAWFILL,mpDRAW,
                                             mpNOINDEX,
                                             mpALLUSSTATES,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		break;
	case NhlGEOPHYSICALANDUSSTATES:
		subret = mdhUpdateDrawGroups(mv40l,mpp,mpDRAWFILL,mpDRAW,
                                             mpNOINDEX,
                                             mpALLGEOPHYSICAL,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		subret = mdhUpdateDrawGroups(mv40l,mpp,mpDRAWFILL,mpDRAW,
                                             mpNOINDEX,
                                             mpALLUSSTATES,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		break;
	case NhlALLBOUNDARIES:
		subret = mdhUpdateDrawGroups(mv40l,mpp,mpDRAWFILL,mpDRAW,
                                             mpNOINDEX,
                                             mpALLNATIONAL,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		subret = mdhUpdateDrawGroups(mv40l,mpp,mpDRAWFILL,mpDRAW,
                                             mpNOINDEX,
                                             mpALLUSSTATES,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		break;
	}

	if (mpp->fill_area_specs != NULL) {
		sp = (NhlString *) mpp->fill_area_specs->data;
		for (i = 0; i < mpp->fill_area_specs->num_elements; i++) {
			found = False;
			mpLowerCase(sp[i]);
			for (j = 0; j < NhlNumber(BGroup_Names); j++) {
				if (! strcmp(sp[i],BGroup_Names[j])) {
					subret = mdhUpdateDrawGroups
                                                (mv40l,mpp,mpDRAWFILL,mpDRAW,
                                                 i,j,entry_name);
					if ((ret = MIN(ret,subret)) < 
					    NhlWARNING)
						return ret;
					found = True;
					break;
				}
			}   
			if (! found) 
				subret = mdhUpdateNameRecs
                                        (mv40l,mpp,mpDRAWFILL,sp[i],
                                         mpDRAW,i,entry_name);
			if ((ret = MIN(ret,subret)) < NhlWARNING)
				return ret;

		}
	}

	if (mpp->area_masking_on && mpp->mask_area_specs != NULL) {
		sp = (NhlString *) mpp->mask_area_specs->data;
		for (i = 0; i < mpp->mask_area_specs->num_elements; i++) {
			found = False;
			mpLowerCase(sp[i]);
			for (j = 0; j < NhlNumber(BGroup_Names); j++) {
				if (! strcmp(sp[i],BGroup_Names[j])) {
					subret = mdhUpdateDrawGroups
                                                (mv40l,mpp,mpDRAWFILL,mpMASK,
                                                 mpNOINDEX,j,entry_name);
					if ((ret = MIN(ret,subret)) < 
					    NhlWARNING)
						return ret;
					found = True;
					break;
				}
			}   
			if (! found) 
				subret = mdhUpdateNameRecs
                                        (mv40l,mpp,mpDRAWFILL,sp[i],
                                         mpMASK,mpNOINDEX,entry_name);
			if ((ret = MIN(ret,subret)) < NhlWARNING)
				return ret;
		}
	}
/*
 * If usstates are to be used promote to the global set if necessary
 */
	if (mv40p->usstates_fill_mode > mpNOSET && 
	    mv40p->global_fill_mode == mpGEO) {
		mv40p->global_fill_mode = mpIMPLIED_NAT;
	}

	return ret;
}

/*
 * Function:  mdhBuildOutlineDrawList
 *
 * Description: 
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: 
 */


/*ARGSUSED*/
static NhlErrorTypes    mdhBuildOutlineDrawList
#if	NhlNeedProto
(
        NhlMapV40DataHandlerLayer mv40l,
	NhlMapPlotLayer	mpnew, 
	NhlMapPlotLayer	mpold,
	NhlBoolean	init,
	_NhlArgList	args,
	int		num_args,
        NhlString	entry_name)
#else
(mv40l,mpnew,mpold,init,args,num_args,entry_name)
	NhlMapV40DataHandlerLayer mv40l;
	NhlMapPlotLayer	mpnew;
	NhlMapPlotLayer	mpold;
	NhlBoolean	init;
	_NhlArgList	args;
	int		num_args;
        NhlString	entry_name;
#endif
{
        NhlMapV40DataHandlerLayerPart *mv40p = &mv40l->mapv40dh;
	NhlErrorTypes ret = NhlNOERROR, subret = NhlNOERROR;
	NhlMapPlotLayerPart *mpp = &(mpnew->mapplot);
	int i,j;
	NhlString	*sp;
	NhlBoolean	found;

/*
 * don't go through all this if nothing has changed
 */
        
	for (i = 0; i < NhlNumber(mv40p->outline_groups); i++) {
		mv40p->outline_groups[i].u.flags = 0;
		mv40p->outline_groups[i].ix = mpNOINDEX;
		mv40p->outline_groups[i].s_ix = (unsigned char) -1;
	}
	mv40p->global_outline_mode = mpNONE;
	mv40p->usstates_outline_mode = mpNOSET;
	mv40p->outline_rec_count = 0;

/* 
 * Determine which map element types are required and consequently which
 * outline sets to use; first examine the boundary sets enumerative, then
 * the include and exclude lists.
 */
	switch (mpp->outline_boundaries) {
	case NhlNOBOUNDARIES:
	default:
		break;
	case NhlGEOPHYSICAL:
		subret = mdhUpdateDrawGroups(mv40l,mpp,mpDRAWOUTLINE,mpDRAW,
                                             mpNOINDEX,
                                             mpALLGEOPHYSICAL,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		break;
	case NhlNATIONAL:
		subret = mdhUpdateDrawGroups(mv40l,mpp,mpDRAWOUTLINE,mpDRAW,
                                             mpNOINDEX,
                                             mpALLNATIONAL,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		break;
	case NhlUSSTATES:
		subret = mdhUpdateDrawGroups(mv40l,mpp,mpDRAWOUTLINE,mpDRAW,
                                             mpNOINDEX,
                                             mpALLUSSTATES,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		break;
	case NhlGEOPHYSICALANDUSSTATES:
		subret = mdhUpdateDrawGroups(mv40l,mpp,mpDRAWOUTLINE,mpDRAW,
                                             mpNOINDEX,
                                             mpALLGEOPHYSICAL,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		subret = mdhUpdateDrawGroups(mv40l,mpp,mpDRAWOUTLINE,mpDRAW,
                                             mpNOINDEX,
                                             mpALLUSSTATES,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		break;
	case NhlALLBOUNDARIES:
		subret = mdhUpdateDrawGroups(mv40l,mpp,mpDRAWOUTLINE,mpDRAW,
                                             mpNOINDEX,
                                             mpALLNATIONAL,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		subret = mdhUpdateDrawGroups(mv40l,mpp,mpDRAWOUTLINE,mpDRAW,
                                             mpNOINDEX,
                                             mpALLUSSTATES,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
		break;
	}

	if (mpp->outline_specs != NULL) {
		sp = (NhlString *) mpp->outline_specs->data;
		for (i = 0; i < mpp->outline_specs->num_elements; i++) {
			found = False;
			mpLowerCase(sp[i]);
			for (j = 0; j < NhlNumber(BGroup_Names); j++) {
				if (! strcmp(sp[i],BGroup_Names[j])) {
					subret = mdhUpdateDrawGroups
                                                (mv40l,mpp,mpDRAWOUTLINE,
                                                 mpDRAW,
						 mpNOINDEX,j,entry_name);
					if ((ret = MIN(ret,subret)) < 
					    NhlWARNING)
						return ret;
					found = True;
					break;
				}
			}   
			if (! found) 
				subret = mdhUpdateNameRecs
                                        (mv40l,mpp,mpDRAWOUTLINE,
                                         sp[i],mpDRAW,mpNOINDEX,entry_name);
			if ((ret = MIN(ret,subret)) < NhlWARNING)
				return ret;

		}
	}

	if (mpp->outline_masking_on && mpp->mask_outline_specs != NULL) {
		sp = (NhlString *) mpp->mask_outline_specs->data;
		for (i = 0; i < mpp->mask_outline_specs->num_elements; i++) {
			found = False;
			mpLowerCase(sp[i]);
			for (j = 0; j < NhlNumber(BGroup_Names); j++) {
				if (! strcmp(sp[i],BGroup_Names[j])) {
					subret = mdhUpdateDrawGroups
                                                (mv40l,mpp,mpDRAWOUTLINE,mpMASK,
                                                 mpNOINDEX,j,entry_name);
					if ((ret = MIN(ret,subret)) < 
					    NhlWARNING)
						return ret;
					found = True;
					break;
				}
			}   
			if (! found) 
				subret = mdhUpdateNameRecs
                                        (mv40l,mpp,mpDRAWOUTLINE,sp[i],
                                         mpMASK,mpNOINDEX,entry_name);
			if ((ret = MIN(ret,subret)) < NhlWARNING)
				return ret;
		}
	}
/*
 * If usstates are to be used promote the global set if necessary
 */
	if (mv40p->usstates_outline_mode > mpNOSET && 
	    mv40p->global_outline_mode == mpGEO) {
		mv40p->global_outline_mode = mpIMPLIED_NAT;
	}

	return ret;
}

static NhlErrorTypes MapV40DHUpdateDrawList
#if	NhlNeedProto
(
	NhlLayer	instance,
        NhlBoolean  	init,
        NhlMapPlotLayer newmp,
        NhlMapPlotLayer oldmp,
        _NhlArgList	args,
        int             num_args
        )
#else
(instance,newmp,oldmp,args,num_args)
	NhlLayer	instance;
        NhlBoolean  	init;
        NhlMapPlotLayer newmp;
        NhlMapPlotLayer oldmp;
        _NhlArgList	args;
        int             num_args;
#endif
{
        NhlMapV40DataHandlerLayer mv40l = (NhlMapV40DataHandlerLayer) instance;
        NhlMapV40DataHandlerLayerPart *mv40p = &mv40l->mapv40dh;
	NhlMapPlotLayerPart	*mpp = &(newmp->mapplot);
	NhlString entry_name = "MapV40DHUpdateDrawList";
        NhlErrorTypes ret = NhlNOERROR,subret = NhlNOERROR;
        NhlBoolean build_fill_list = False, build_outline_list = False;

	if (init) {
               mv40p->new_co_amap_req = True;
               mv40p->new_us_amap_req = True;
               build_fill_list = True;
               build_outline_list = True;
        }
        else {
                NhlMapPlotLayerPart	*ompp = &(oldmp->mapplot);
                
                if (mpp->database_version != ompp->database_version ||
                    mpp->fill_boundaries != ompp->fill_boundaries ||
                    mpp->fill_area_specs != ompp->fill_area_specs ||
                    mpp->mask_area_specs != ompp->mask_area_specs ||
                    mpp->spec_fill_colors != ompp->spec_fill_colors ||
                    mpp->spec_fill_patterns != ompp->spec_fill_patterns ||
                    mpp->spec_fill_scales != ompp->spec_fill_scales ||
                    mpp->spec_fill_direct != ompp->spec_fill_direct ||
                    mpp->area_names != ompp->area_names ||
                    mpp->dynamic_groups != ompp->dynamic_groups ||
                    mpp->area_group_count != ompp->area_group_count ||
                    mpp->area_masking_on != ompp->area_masking_on ||
                    mpp->spec_fill_priority != ompp->spec_fill_priority)
                        build_fill_list = True;
                
                if (mpp->database_version != ompp->database_version ||
                    mpp->outline_boundaries != ompp->outline_boundaries ||
                    mpp->outline_specs != ompp->outline_specs ||
                    mpp->mask_outline_specs != ompp->mask_outline_specs ||
                    mpp->outline_masking_on != ompp->outline_masking_on)
                        build_outline_list = True;

                if (build_fill_list || mpp->view_changed ||
                    mpp->trans_change_count != ompp->trans_change_count) {
                        mv40p->new_co_amap_req = True;
                        mv40p->new_us_amap_req = True;
                }
        }
        
        if (build_fill_list) {
                subret = mdhBuildFillDrawList(mv40l,newmp,oldmp,init,
                                              args,num_args,entry_name);
                if ((ret = MIN(ret,subret)) < NhlWARNING)
                        return NhlFATAL;
        }
        
        if (build_outline_list) {
                subret = mdhBuildOutlineDrawList(mv40l,newmp,oldmp,init,
                                                 args,num_args,entry_name);
                if ((ret = MIN(ret,subret)) < NhlWARNING)
                        return NhlFATAL;
        }
        
        return ret;
}


/*
 * Routines called by MapV40DHDrawMapList to perform map fill, outline,
 * and grid using the Version 4.0 database.
 */

/*
 * Function:	mpExpandId
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes mpExpandId
#if	NhlNeedProto
(
        NhlMapV40DataHandlerLayer 	mv40l,
	NhlMapPlotLayerPart		*mpp,
	int				id,
	mpFlags				uflags,
	int				index,
	unsigned char			s_index,
 	NhlString			entry_name
)
#else
(mv40l,mpp,id,uflags,index,s_index,entry_name)
        NhlMapV40DataHandlerLayer 	mv40l;
	NhlMapPlotLayerPart		*mpp;
	int				id;
	mpFlags				uflags;
	int				index;
	unsigned char			s_index;
	NhlString			entry_name;
#endif
{
	NhlMapDataHandlerLayerPart	*mdhp = &(mv40l->mapdh);
	char 	*e_text;
	short	*idp;
	int	count,ix,i,set;
	NhlBoolean do_us = False;
	int		*dyn_grps = NULL;
	NhlBoolean	use_dyn_grps = False;
        mpOutlineRec *outline_recs = Mv4cp->outline_recs;
        int *otype_start_ix = Mv4cp->outline_type_start_ix;
        
	count = NhlNumber(Exp_Ids);
	if (id >= count) {
		e_text = "%s: invalid expansion id";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}

	set = Outline_Set == mpPO ? 0 : 1;
	idp = Exp_Ids[id][set];
	count = Exp_Ids_Count[id][set];

	if (id == 11 && set == 1) {	/* US outline */
		US_Border = 2;
		if (mdhp->dynamic_groups != NULL) {
			dyn_grps = (int *) mdhp->dynamic_groups->data;
			use_dyn_grps = True;
		}
		US_Index = index;
		do_us = True;
	}
	else if (id == 0 && set == 1)  { /* North-and-South-America */
		US_Border = 1;
		US_Index = index;
		do_us = True;
	}
	else if (idp == NULL) {
		e_text = "%s: invalid expansion id";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}

	if (idp != NULL) {
		for (i = 0; i < count; i++) {
			ix = idp[i] - Id_Offset[Outline_Set];
			DrawIds[ix].u.flags = uflags.flags;
			DrawIds[ix].ix = index;
			DrawIds[ix].s_ix = s_index;
		}
	}
	if (do_us) {
		for (i = otype_start_ix[mpUSStateLand]; 
		     i < otype_start_ix[mpUSStateLand + 1]; i++) {
			ix = outline_recs[i].id[2] - Id_Offset[Outline_Set];
			DrawIds[ix].u.flags = uflags.flags;
			DrawIds[ix].ix = index;
			DrawIds[ix].s_ix = s_index;
		}
		for (i = otype_start_ix[mpUSStateWater]; 
		     i < otype_start_ix[mpUSStateWater + 1]; i++) {
			ix = outline_recs[i].id[2] - Id_Offset[Outline_Set];
			DrawIds[ix].u.flags = uflags.flags;
			DrawIds[ix].s_ix = s_index;
			DrawIds[ix].ix = use_dyn_grps ?
				dyn_grps[i] : outline_recs[i].cix[US_Border-1];
		}
	}
	return NhlNOERROR;
}

/*
 * Function:	mpSetUpStateDrawIds
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes mpSetUpStateDrawIds
#if	NhlNeedProto
(
        NhlMapV40DataHandlerLayer 	mv40l,
	NhlMapPlotLayerPart		*mpp,
	mpDrawOp			draw_op,
	NhlString			entry_name
)
#else
(mv40l,mpp,draw_op,entry_name)
        NhlMapV40DataHandlerLayer 	mv40l;
	NhlMapPlotLayerPart		*mpp;
	mpDrawOp			draw_op;
	NhlString			entry_name;
#endif
{
	NhlMapDataHandlerLayerPart	*mdhp = &(mv40l->mapdh);
        NhlMapV40DataHandlerLayerPart 	*mv40p = &mv40l->mapv40dh;
	NhlErrorTypes	ret = NhlNOERROR;
	char		*e_text;
	int		offset,ix,i,j;
	mpNameRec	*land_group,*water_group;
	mpNameRec	*nrecs;
	int		count;
	mpFlags		uflags;
	mpGlobalSetMode gmode;
	mpStateSetMode	smode;
	int		*dyn_grps = NULL;
	NhlBoolean	use_dyn_grps = False;
        mpOutlineRec *outline_recs = Mv4cp->outline_recs;
        int *otype_start_ix = Mv4cp->outline_type_start_ix;

	switch (draw_op) {
	case mpDRAWFILL:
		land_group = &mv40p->fill_groups[mpUSStateLand];
		water_group = &mv40p->fill_groups[mpUSStateWater];
		nrecs = mv40p->fill_recs;
		count = mv40p->fill_rec_count;
		gmode = mv40p->global_fill_mode;
		smode = mv40p->usstates_fill_mode;
		break;
	case mpDRAWOUTLINE:
		land_group = &mv40p->outline_groups[mpUSStateLand];
		water_group = &mv40p->outline_groups[mpUSStateWater];
		nrecs = mv40p->outline_recs;
		count = mv40p->outline_rec_count;
		gmode = mv40p->global_outline_mode;
		smode = mv40p->usstates_outline_mode;
		break;
	default:
		e_text = "%s: internal enumeration error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}

	c_mpsetc("OU","US");
	_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	Outline_Set = mpUS;
	offset = Id_Offset[2];
	if (gmode == mpNONE || US_Border == 2)
		Draw_Check = Draw_Special_Check[Outline_Set];
	else
		Draw_Check = -999;

	uflags.flags = 0;
	for (i = 0; i < DrawId_Count; i++) {
		DrawIds[i].u.flags = 0;
		DrawIds[i].s_ix = (unsigned char) -1;
		DrawIds[i].ix = NhlmpDEFAULTGROUPINDEX;
	}
	if (mdhp->dynamic_groups != NULL) {
		dyn_grps = (int *) mdhp->dynamic_groups->data;
		use_dyn_grps = True;
	}
	
	if (land_group->u.f.draw_mode > mpBACKGROUND) {
		uflags.flags = land_group->u.flags;
		for (j = otype_start_ix[mpUSStateLand]; 
		     j < otype_start_ix[mpUSStateLand + 1]; j++) {
			ix = outline_recs[j].id[1] - offset;
			DrawIds[ix].s_ix = land_group->s_ix;
			if (smode == mpIMPLIED_SET && US_Index > 0) {
				DrawIds[ix].u.flags = 0;
				DrawIds[ix].ix = (unsigned char) US_Index;
			}
			else {
				DrawIds[ix].u.flags = uflags.flags;
				DrawIds[ix].ix = (unsigned char)use_dyn_grps ?
					dyn_grps[j] : outline_recs[j].cix[1];
			}
			DrawIds[ix].u.f.draw_mode = uflags.f.draw_mode;
		}
	}

	if (water_group->u.f.draw_mode > mpBACKGROUND) {
		for (j = otype_start_ix[mpUSStateWater]; 
		     j < otype_start_ix[mpUSStateWater + 1]; j++) {
			ix = outline_recs[j].id[1] - offset;
			DrawIds[ix].s_ix = water_group->s_ix;
			DrawIds[ix].u.flags = water_group->u.flags;
			DrawIds[ix].ix = (unsigned char)use_dyn_grps ?
				dyn_grps[j] : outline_recs[j].cix[1];
		}
	}

	for (i = 0; i < count; i++) {
		j = nrecs[i].name_ix;
		if (outline_recs[j].type < mpUSStateLand)
			continue;
		ix = outline_recs[j].id[1] - offset;
		DrawIds[ix].s_ix = nrecs[i].s_ix;
		DrawIds[ix].u.flags = nrecs[i].u.flags;
		DrawIds[ix].ix = nrecs[i].ix;
	}

	if (draw_op != mpDRAWOUTLINE) {
                unsigned char draw_mode = gmode == mpNONE ?
                        mpBACKGROUND : mpSYSEXCLUDE;
                
		for (i = 0; i < NhlNumber(USState_Excludes); i++) {
			DrawIds[USState_Excludes[i]-offset].u.f.draw_mode = 
				draw_mode;
		}
	}

	return ret;
}

/*
 * Function:	mpSetUpDrawIds
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes mpSetUpDrawIds
#if	NhlNeedProto
(
        NhlMapV40DataHandlerLayer 	mv40l,
	NhlMapPlotLayerPart		*mpp,
	mpDrawOp			draw_op,
	NhlString			entry_name
)
#else
(mv40l,mpp,draw_op,entry_name)
	NhlMapV40DataHandlerLayer 	mv40l;
	NhlMapPlotLayerPart		*mpp;
	mpDrawOp			draw_op;
	NhlString			entry_name;
#endif
{
 	NhlMapDataHandlerLayerPart	*mdhp = &(mv40l->mapdh);
        NhlMapV40DataHandlerLayerPart *mv40p = &mv40l->mapv40dh;
	NhlErrorTypes	ret = NhlNOERROR, subret = NhlNOERROR;
	char		*e_text;
	int		id_ix,cix_ix,start_ix,end_ix,ix,i,j;
	mpNameRec	*groups;
	mpNameRec	*nrecs;
	int		count;
	mpFlags		uflags;
	mpGlobalSetMode gmode;
	mpStateSetMode	smode;
	int		index;
	unsigned char	s_index;
	int		*dyn_grps = NULL;
	NhlBoolean	use_dyn_grps = False;
	int		us_ix = -999;
        mpOutlineRec *outline_recs = Mv4cp->outline_recs;
        int *otype_start_ix = Mv4cp->outline_type_start_ix;

	uflags.flags = 0;
	US_Border = 0;
	US_Index = -999;
	for (i = 0; i < DrawId_Count; i++) {
		DrawIds[i].u.flags = 0;
		DrawIds[i].s_ix = (unsigned char) -1;
		DrawIds[i].ix = NhlmpDEFAULTGROUPINDEX;
	}

	switch (draw_op) {
	case mpDRAWFILL:
		groups = mv40p->fill_groups;
		nrecs = mv40p->fill_recs;
		count = mv40p->fill_rec_count;
		gmode = mv40p->global_fill_mode;
		smode = mv40p->usstates_fill_mode;
		break;
	case mpDRAWOUTLINE:
		groups = mv40p->outline_groups;
		nrecs = mv40p->outline_recs;
		count = mv40p->outline_rec_count;
		gmode = mv40p->global_outline_mode;
		smode = mv40p->usstates_outline_mode;
		break;
	default:
		e_text = "%s: internal enumeration error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}

	switch (gmode) {
	case mpGEO:
		c_mpsetc("OU","CO");
		_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
		Outline_Set = mpCO;
		id_ix = 0;
		cix_ix = 0;
		break;
	case mpNAT:
		if (smode) {
			c_mpsetc("OU","PS");
			_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
			Outline_Set = mpPS;
			id_ix = 2;
		}
		else {
			c_mpsetc("OU","PO");
			_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
			Outline_Set = mpPO;
			id_ix = 1;
		}
		cix_ix = 1;
		if (mdhp->dynamic_groups != NULL) {
			dyn_grps = (int *) mdhp->dynamic_groups->data;
			use_dyn_grps = True;
		}
		break;
	case mpIMPLIED_NAT:
		if (smode) {
			c_mpsetc("OU","PS");
			_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
			Outline_Set = mpPS;
			id_ix = 2;
		}
		else {
			c_mpsetc("OU","PO");
			_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
			Outline_Set = mpPO;
			id_ix = 1;
		}
		cix_ix = 0;
		break;
	default:
		return ret;
	}
	start_ix = Id_Offset[Outline_Set];
	end_ix = Id_End_Reg[Outline_Set];
	Draw_Check = Draw_Special_Check[Outline_Set];


/*
 * Set Up the Draw List for the map boundary catagories. The specification
 * lists are parsed later and thus overrride the general catagories.
	for (i = 0; i < NhlmpOUTLINE_TYPE_COUNT - 2; i++) {
 */
	for (i = 0; i < NhlmpOUTLINE_TYPE_COUNT; i++) {
		if (groups[i].u.f.draw_mode > mpBACKGROUND) {
			uflags.flags = groups[i].u.flags;
			s_index = groups[i].s_ix;
			for (j = otype_start_ix[i];
                             j < otype_start_ix[i+1]; j++) {
				ix = outline_recs[j].id[id_ix];
				if (use_dyn_grps)
					index = dyn_grps[j];
				else {
					index = outline_recs[j].cix[cix_ix];
				}
				if (ix > end_ix) {
					if (draw_op == mpDRAWOUTLINE &&
					    uflags.f.draw_mode == mpDRAW)
					    uflags.f.draw_mode = mpDRAWSPECIAL;
					subret = mpExpandId
                                                (mv40l,mpp,ix-end_ix-1,uflags,
                                                 index,s_index,entry_name);
					if ((ret = MIN(ret,subret))
					     < NhlWARNING) return ret;
				}
				else {
					ix -= start_ix;
					DrawIds[ix].u.flags = uflags.flags;
					DrawIds[ix].ix = index;
					DrawIds[ix].s_ix = s_index;
				}
			}
		}
	}
/*
 * Parse the spec lists twice. The first time only set the composite ids.
 * Thus in the second parse the specific ids will override the composites.
 * The US outline needs special treatment, since its border is drawn using
 * the PS outline while the interior state borders are drawn using the US
 * outline set. Also its setting must override the N+S-America setting, but
 * in turn individual state settings must override it. Therefore set it
 * in between the two loops if it appears.
 */
	for (i = 0; i < count; i++) {
		j = nrecs[i].name_ix;
		ix = outline_recs[j].id[id_ix];

		if (ix <= end_ix)
			continue;
		else if (ix == mpUS_PS_IX && Outline_Set == mpPS) {
			us_ix = i;
			continue;
		}
		uflags.flags = nrecs[i].u.flags;
		index = nrecs[i].ix;
		s_index = nrecs[i].s_ix;
		if (draw_op == mpDRAWOUTLINE && uflags.f.draw_mode == mpDRAW)
			uflags.f.draw_mode = mpDRAWSPECIAL;
		subret = mpExpandId(mv40l,mpp,ix-end_ix-1,
				    uflags,index,s_index,entry_name);
		if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
	}
	if (us_ix >= 0) {
		j = nrecs[us_ix].name_ix;
		ix = outline_recs[j].id[id_ix];
		uflags.flags = nrecs[us_ix].u.flags;
		index = nrecs[us_ix].ix;
		s_index = nrecs[us_ix].s_ix;
		if (uflags.f.draw_mode == mpMASK) {
			subret = mpExpandId(mv40l,mpp,ix-end_ix-1,uflags,
					    index,s_index,entry_name);
			if ((ret = MIN(ret,subret)) < NhlWARNING) return ret;
			groups[mpUSStateLand].u.flags = uflags.flags;
		}
		else {
			US_Border = 2;
			US_Index = index;
		}
	}

	for (i = 0; i < count; i++) {
		j = nrecs[i].name_ix;
		ix = outline_recs[j].id[id_ix];
		if (outline_recs[j].type > mpNational && 
		    nrecs[i].u.f.draw_mode != mpMASK)
			continue;
		ix -= start_ix;
		DrawIds[ix].s_ix = nrecs[i].s_ix;
		DrawIds[ix].u.flags = nrecs[i].u.flags;
		DrawIds[ix].ix = nrecs[i].ix;
	}
/*
 * If the US set is in implied set mode, set the draw mode for the groups
 * to the color that has been set up for the US border. Is the DrawId for
 * this index always set????
 */
	if (smode == mpIMPLIED_SET) {
		us_ix = (Outline_Set == mpPO ?
			 mpUS_PO_IX : mpUS_PS_IX) - start_ix;
		uflags = DrawIds[us_ix].u;
		if (draw_op == mpDRAWOUTLINE && uflags.f.draw_mode == mpDRAW)
			uflags.f.draw_mode = mpDRAWSPECIAL;
		groups[mpUSStateLand].u.flags = uflags.flags;
	}

	return ret;
}

/*
 * Function:	mpSetUpAreamap
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes mpSetUpAreamap
#if	NhlNeedProto
(
        NhlMapV40DataHandlerLayer 	mv40l,
	NhlMapPlotLayer			mpl,
	NhlWorkspace			**aws,
	int 				amap_type,
	NhlString			entry_name
)
#else
(mv401,mpl,aws,amap_type,entry_name)
	NhlMapV40DataHandlerLayer 	mv40l;
        NhlMapPlotLayer 		mpl;
	NhlWorkspace			**aws;
	int				amap_type;
	NhlString			entry_name;
#endif
{
        NhlMapV40DataHandlerLayerPart *mv40p = &mv40l->mapv40dh;
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	NhlMapPlotLayerPart	*mpp = &(mpl->mapplot);
	int			aws_id;
	NhlBoolean		new_amap_req;

	c_arseti("RC",1);
	switch (amap_type) {
	case mpGLOBAL_AMAP:
		aws_id = mv40p->co_aws_id;
                new_amap_req = mv40p->new_co_amap_req;
		break;
	case mpUSSTATES_AMAP:
		aws_id = mv40p->us_aws_id;
                new_amap_req = mv40p->new_us_amap_req;
		break;
	}

	if (aws_id < 1) {
		aws_id = _NhlNewWorkspace(NhlwsAREAMAP,NhlwsNONE,
                                          mpWORKSPACE_SIZE_REQ);
		if (aws_id < 1) 
			return MIN(ret,(NhlErrorTypes)aws_id);
	}
	if ((*aws = _NhlUseWorkspace(aws_id)) == NULL) {
		e_text = "%s: error reserving area map workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}
        
	switch (amap_type) {
	case mpGLOBAL_AMAP:
		subret = mpSetUpDrawIds(mv40l,mpp,mpDRAWFILL,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
		break;
	case mpUSSTATES_AMAP:
		subret = mpSetUpStateDrawIds(mv40l,mpp,mpDRAWFILL,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
		break;
	}
	if (! _NhlWorkspaceDataIntact(aws_id) || new_amap_req) {
		c_mpseti("VS",0);
		c_mpseti("G2",2);
		c_mpseti("G1",1);
		_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
		subret = _NhlArinam(*aws,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

		subret = _NhlMapbla(*aws,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

		if (mpp->dump_area_map)
			_NhlDumpAreaMap(*aws,entry_name);
	}
	switch (amap_type) {
	case mpGLOBAL_AMAP:
		mv40p->co_aws_id = aws_id;
                mv40p->new_co_amap_req = False;
		break;
	case mpUSSTATES_AMAP:
		mv40p->us_aws_id = aws_id;
                mv40p->new_us_amap_req = False;
		break;
	}
	return ret;
}

/*
 * Function:  hlumapfill
 *
 * Description: C version of APR user routine called from within ARSCAM 
 *		to fill areas based on the area ID.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: 
 */

/*ARGSUSED*/
static int (_NHLCALLF(hlumapfill,HLUMAPFILL))
#if	NhlNeedProto
(
	float *xcs, 
	float *ycs, 
	int *ncs, 
	int *iai, 
	int *iag, 
	int *nai
)
#else
(xcs,ycs,ncs,iai,iag,nai)
	float *xcs; 
	float *ycs; 
	int *ncs; 
	int *iai; 
	int *iag; 
	int *nai;
#endif
{
	int ix, pat_ix, col_ix, id;
	float fscale;
	unsigned char s_ix;
	int *fcp;
	int *fpp;
	float *fsp;

	if (Mpp == NULL) return 0;

	fcp = (int *) Mpp->fill_colors->data;
	fpp = (int *) Mpp->fill_patterns->data;
	fsp = (float *) Mpp->fill_scales->data;

	if (*iai < 1) return 0;
	id = *iai - Id_Offset[Outline_Set];
#if 0
	printf("iai %d id %d use %d color %d\n", *iai,id,
	       DrawIds[id].u.f.draw_mode,DrawIds[id].cix);
#endif
	if (id < 0 || DrawIds[id].u.f.draw_mode > mpDRAW)
		return 0;

	ix = DrawIds[id].ix;
	s_ix = DrawIds[id].s_ix;

	if (! DrawIds[id].u.f.s_col) {
		if (Mpp->mono_fill_color)
			col_ix = Mpp->fill_color;
		else
			col_ix = fcp[ix];
	}
	else if (Mpp->spec_fill_direct) {
		int *sfcp = (int *) Mpp->spec_fill_colors->data;
		col_ix = sfcp[s_ix];
	}
	else {
		int *sfcp = (int *) Mpp->spec_fill_colors->data;
		if (sfcp[s_ix] < Mpp->area_group_count)
			col_ix = fcp[sfcp[s_ix]];
	} 

	if (! DrawIds[id].u.f.s_pat) {
		if (Mpp->mono_fill_pattern)
			pat_ix = Mpp->fill_pattern;
		else
			pat_ix = fpp[ix];
	}
	else if (Mpp->spec_fill_direct) {
		int *sfpp = (int *) Mpp->spec_fill_patterns->data;
		pat_ix = sfpp[s_ix];
	}
	else {
		int *sfpp = (int *) Mpp->spec_fill_patterns->data;
		if (sfpp[s_ix] < Mpp->area_group_count)
			pat_ix = fpp[sfpp[s_ix]];
	}

	if (! DrawIds[id].u.f.s_scl) {
		if (Mpp->mono_fill_scale)
			fscale = Mpp->fill_scale;
		else
			fscale = fsp[ix];
	}
	else if (Mpp->spec_fill_direct) {
		float *sfsp = (float *) Mpp->spec_fill_scales->data;
		fscale = sfsp[s_ix];
	}
	else {
		float *sfsp = (float *) Mpp->spec_fill_scales->data;
		if ((int)sfsp[s_ix] < Mpp->area_group_count)
			fscale = fsp[(int)sfsp[s_ix]];
	}

	NhlVASetValues(Mpl->base.wkptr->base.id,
		       _NhlNwkFillBackground, Mpp->fill_pattern_background,
		       _NhlNwkFillIndex, pat_ix,
		       _NhlNwkFillColor, col_ix,
		       _NhlNwkFillScaleFactorF,fscale,
		       _NhlNwkFillDotSizeF,Mpp->fill_dot_size,
		       _NhlNwkEdgesOn,0,
		       NULL);
	
	_NhlSetFillInfo(Mpl->base.wkptr, (NhlLayer) Mpl);
	_NhlWorkstationFill(Mpl->base.wkptr,xcs,ycs,*ncs);

	return 0;
}

/*
 * Function:	mpFill
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	


static NhlErrorTypes mpFill
#if	NhlNeedProto
(
        NhlMapV40DataHandlerLayer 	mv40l,
	NhlMapPlotLayer			mpl,
	NhlString			entry_name
)
#else
(mv40l,mpl,entry_name)
        NhlMapV40DataHandlerLayer 	mv40l;
        NhlMapPlotLayer 		mpl;
	NhlString			entry_name;
#endif
{
        NhlMapV40DataHandlerLayerPart *mv40p = &mv40l->mapv40dh;
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
        NhlWorkspace		*aws = NULL, *us_aws = NULL;

/*
 * for efficiency if the ustates are drawn they go into a separate 
 * area map. Eventually there may be a number of sub-area maps.
 */
#if 0
        float flx,frx,fby,fuy,wlx,wrx,wby,wuy; int ll;
        c_getset(&flx,&frx,&fby,&fuy,&wlx,&wrx,&wby,&wuy,&ll);
        printf("getset - %f,%f,%f,%f,%f,%f,%f,%f\n",
               flx,frx,fby,fuy,wlx,wrx,wby,wuy); 
#endif

	if (mv40p->global_fill_mode != mpNONE ||
	    mv40p->usstates_fill_mode == mpNOSET) {

		subret = mpSetUpAreamap(mv40l,mpl,&aws,
                                        mpGLOBAL_AMAP,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) goto error_ret;
#if 0
		switch (Outline_Set) {
		case mpCO:
			printf("using CO for fill\n");
			break;
		case mpPO:
			printf("using PO for fill\n");
			break;
		case mpPS:
			printf("using PS for fill\n");
			break;
		default:
			printf("outline set not correct\n");
		}
#endif
		subret = _NhlArpram(aws,0,0,0,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) goto error_ret;

		subret = _NhlArscam(aws,(_NHLCALLF(hlumapfill,HLUMAPFILL)),
				    entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) goto error_ret;

	}

        if (mv40p->usstates_fill_mode != mpNOSET) {

		subret = mpSetUpAreamap(mv40l,mpl,&us_aws,
                                        mpUSSTATES_AMAP,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) goto error_ret;
#if 0
		printf("using US for fill\n");
#endif
		subret = _NhlArpram(us_aws,0,0,0,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) goto error_ret;

		subret = _NhlArscam(us_aws,(_NHLCALLF(hlumapfill,HLUMAPFILL)),
				    entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) goto error_ret;

        }

 error_ret:

	if (aws) {
		subret = _NhlIdleWorkspace(aws);
		ret = MIN(subret,ret);
	}
	if (us_aws) {
		subret = _NhlIdleWorkspace(us_aws);
		ret = MIN(subret,ret);
	}
		
	return ret;


}


/*
 * Function:	mpOutline
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes mpOutline
#if	NhlNeedProto
(
        NhlMapV40DataHandlerLayer 	mv40l,
	NhlMapPlotLayer			mpl,
	NhlString			entry_name
)
#else
(mv40l,mp,entry_name)
        NhlMapV40DataHandlerLayer 	mv40l;
        NhlMapPlotLayer 		mpl;
	NhlString			entry_name;
#endif
{
        NhlMapV40DataHandlerLayerPart *mv40p = &mv40l->mapv40dh;
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlMapPlotLayerPart	*mpp = &(mpl->mapplot);

	c_mpseti("C5",mpp->geophysical.gks_color);
	_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	c_mpseti("C6",mpp->us_state.gks_color);
	_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	c_mpseti("C7",mpp->national.gks_color);
	_NhlLLErrCheckPrnt(NhlWARNING,entry_name);

	if (mv40p->global_outline_mode != mpNONE) {
		subret = mpSetUpDrawIds(mv40l,mpp,mpDRAWOUTLINE,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
#if 0
		switch (Outline_Set) {
		case mpCO:
			printf("using CO for outlines\n");
			break;
		case mpPO:
			printf("using PO for outlines\n");
			break;
		case mpPS:
			printf("using PS for outlines\n");
			break;
		default:
			printf("outline set not correct\n");
		}
#endif
		c_maplot();
		_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	}

        if (mv40p->usstates_outline_mode != mpNOSET) {
		subret = mpSetUpStateDrawIds(mv40l,mpp,
                                             mpDRAWOUTLINE,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
#if 0
		printf("using US for outlines\n");
#endif
		c_maplot();
		_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	}

	return ret;
}

/*
 * Function:  hlumaskgrid
 *
 * Description: C version of ULPR user routine called from within MAPGRM 
 *		to mask a grid based on an areamap.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: 
 */

/*ARGSUSED*/

static int (_NHLCALLF(hlumaskgrid,HLUMASKGRID))
#if	NhlNeedProto
(
	float *xcra, 
	float *ycra, 
	int *ncra, 
	int *iai, 
	int *iag, 
	int *nai
)
#else
(xcra,ycra,ncra,iai,iag,nai)
	float *xcra; 
	float *ycra; 
	int *ncra; 
	int *iai; 
	int *iag; 
	int *nai;
#endif
{
	NhlBoolean draw_line = False;
	int i,id,ix = 0;
	int type;

	if (Mpp == NULL) return 0;

	id = *iai - Id_Offset[Outline_Set];
	if (id < 0)
		return 0;
        
        if (DrawIds[id].u.f.draw_mode == mpBACKGROUND)
                draw_line = True;
        else {
                switch (Mpp->grid_mask_mode) {
                    case NhlMASKNONE:
                    default:
                            draw_line = True;
                            break;
                    case NhlMASKOCEAN:
                            switch (Outline_Set) {
                                case mpEMPTY:
                                        draw_line = True;
                                        break;
                                case mpCO:
                                case mpPO:
                                case mpPS:
                                        if (id != 1)
                                                draw_line = True;
                                        break;
                                case mpUS:
                                        if (id > 0)
                                                draw_line = True;
                                        break;
                            }
                            break;
                    case NhlMASKNOTOCEAN:
                            switch (Outline_Set) {
                                case mpEMPTY:
                                        draw_line = True;
                                        break;
                                case mpCO:
                                case mpPO:
                                case mpPS:
                                        if (id == 1)
                                                draw_line = True;
                                        break;
                                case mpUS:
                                        break;
                            }
                            break;
                    case NhlMASKLAND:
                            switch (Outline_Set) {
                                case mpEMPTY:
                                        draw_line = True;
                                        break;
                                case mpCO:
                                        ix = 0;
                                        type = mpInlandWater;
                                        if (id == 1)
                                                draw_line = True;
                                        break;
                                case mpPO:
                                        ix = 1;
                                        if (id == 1)
                                                draw_line = True;
                                        type = mpInlandWater;
                                        break;
                                case mpUS:
                                        ix = 1;
                                        type = mpUSStateWater;
                                        break;
                                case mpPS:
                                        ix = 2;
                                        if (id == 1)
                                                draw_line = True;
                                        type = mpInlandWater;
                                        break;
                            }
                            if (! draw_line) {
                                    mpOutlineRec *outline_recs =
                                            Mv4cp->outline_recs;
                                    int *otype_start_ix =
                                            Mv4cp->outline_type_start_ix;
                                    for
                                      (i = otype_start_ix[type]; 
                                       i < otype_start_ix[type+1];i++) {
                                            if (id == outline_recs[i].id[ix] -
                                                Id_Offset[Outline_Set]) {
                                                    draw_line = True;
                                                    break;
                                            }
                                    }
                            }
                            break;
                    case NhlMASKNOTLAND:
                            draw_line = True;
                            switch (Outline_Set) {
                                case mpEMPTY:
                                        draw_line = True;
                                        break;
                                case mpCO:
                                        ix = 0;
                                        if (id == 1)
                                                draw_line = False;
                                        type = mpInlandWater;
                                        break;
                                case mpPO:
                                        ix = 1;
                                        if (id == 1)
                                                draw_line = False;
                                        type = mpInlandWater;
                                        break;
                                case mpUS:
                                        ix = 0;
                                        if (id <= 0)
                                                draw_line = False;
                                        type = mpUSStateWater;
                                        break;
                                case mpPS:
                                        ix = 2;
                                        if (id == 1)
                                                draw_line = False;
                                        type = mpInlandWater;
                                        break;
                            }

                            if (draw_line) {
                                    mpOutlineRec *outline_recs =
                                            Mv4cp->outline_recs;
                                    int *otype_start_ix =
                                            Mv4cp->outline_type_start_ix;
                                    for
                                      (i = otype_start_ix[type]; 
                                       i < otype_start_ix[type+1];i++) {
                                            if (id == outline_recs[i].id[ix] -
                                                Id_Offset[Outline_Set]) {
                                                    draw_line = False;
                                                    break;
                                            }
                                    }
                            }
                            break;
                    case NhlMASKFILLAREA:
                            break;
                    case NhlMASKMASKAREA:
                            if (DrawIds[id].u.f.draw_mode < mpMASK) {
                                    draw_line = True;
                            }
                            break;
                }
        }
	if (! draw_line)
		return 0;
		
	if (! Grid_Setup) {
		NhlVASetValues(Mpl->base.wkptr->base.id,
			       _NhlNwkLineLabel,"",
			       _NhlNwkDashPattern,Mpp->grid.dash_pat,
			       _NhlNwkLineDashSegLenF,Mpp->grid.dash_seglen,
			       _NhlNwkLineThicknessF,Mpp->grid.thickness,
			       _NhlNwkLineColor,Mpp->grid.color, 
			       NULL);

		_NhlSetLineInfo(Mpl->base.wkptr,(NhlLayer) Mpl);
		Grid_Setup = True;
	}
	_NhlWorkstationLineTo(Mpl->base.wkptr, 
			      xcra[0],ycra[0],1);
	for (i = 1; i < *ncra; i++)
		_NhlWorkstationLineTo(Mpl->base.wkptr, 
				      xcra[i],ycra[i],0);
	_NhlWorkstationLineTo(Mpl->base.wkptr,0.0,0.0,1);

	return 0;
}

/*
 * Function:	mpGrid
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes mpGrid
#if	NhlNeedProto
(
        NhlMapV40DataHandlerLayer 	mv40l,
	NhlMapPlotLayer			mpl,
	NhlString			entry_name
)
#else
(mv40l,mpl,entry_name)
        NhlMapV40DataHandlerLayer 	mv40l;
        NhlMapPlotLayer 		mpl;
	NhlString			entry_name;
#endif
{
        NhlMapV40DataHandlerLayerPart *mv40p = &mv40l->mapv40dh;
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlMapPlotLayerPart	*mpp = &(mpl->mapplot);
	NhlWorkspace		*aws = NULL, *us_aws = NULL;
        float pole_param;

	Grid_Setup = False;
	c_mpseti("C2",mpp->grid.gks_color);
	_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	c_mpseti("C4", mpp->limb.gks_color);
	_NhlLLErrCheckPrnt(NhlWARNING,entry_name);

	c_mpsetr("GT",mpp->grid_lat_spacing);
	_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	c_mpsetr("GN",mpp->grid_lon_spacing);
	_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
        pole_param = 1000.0 * mpp->grid_max_lat + mpp->grid_polar_lon_spacing;
        c_mpsetr("GP",pole_param);
	_NhlLLErrCheckPrnt(NhlWARNING,entry_name);

	if (mpp->grid_mask_mode == NhlMASKNONE) {
		c_mapgrd();
		_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
		return ret;
	}
	if (mv40p->global_fill_mode > mpNONE ||
            mv40p->usstates_fill_mode == mpNOSET) {
		
		subret = mpSetUpAreamap(mv40l,mpl,&aws,
                                        mpGLOBAL_AMAP,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) goto error_ret;
#if 0
		switch (Outline_Set) {
		case mpCO:
			printf("using CO for grid\n");
			break;
		case mpPO:
			printf("using PO for grid\n");
			break;
		case mpPS:
			printf("using PS for grid\n");
			break;
		default:
			printf("outline set not correct\n");
		}
#endif
		subret = _NhlMapgrm(aws,
				    (_NHLCALLF(hlumaskgrid,HLUMASKGRID)),
				    entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) goto error_ret;
		_NHLCALLF(mdplmb,MDPLMB)();
		_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	}
	if (mv40p->usstates_fill_mode != mpNOSET) {

		subret = mpSetUpAreamap(mv40l,mpl,&aws,
                                        mpUSSTATES_AMAP,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) goto error_ret;
#if 0
		printf("using US for grid\n");
#endif
		subret = _NhlMapgrm(aws,
				    (_NHLCALLF(hlumaskgrid,HLUMASKGRID)),
				    entry_name);
		_NHLCALLF(mdplmb,MDPLMB)();
		_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
		
		if ((ret = MIN(subret,ret)) < NhlWARNING) goto error_ret;

	}
 error_ret:

	if (aws) {
		subret = _NhlIdleWorkspace(aws);
		ret = MIN(subret,ret);
	}
	if (us_aws) {
		subret = _NhlIdleWorkspace(us_aws);
		ret = MIN(subret,ret);
	}
	return ret;
}


static NhlErrorTypes MapV40DHDrawMapList
#if	NhlNeedProto
(
	NhlLayer		instance,
        NhlMapPlotLayer 	mpl,
        mpDrawOp		draw_op,
	NhlBoolean		init_draw
        )
#else
(instance,mpl,draw_op,init_draw)
	NhlLayer		instance;
        NhlMapPlotLayer 	mpl;
        mpDrawOp		draw_op;
	NhlDrawOrder		init_draw;
#endif
{
        NhlMapV40DataHandlerLayer mv40l = (NhlMapV40DataHandlerLayer) instance;
	NhlMapPlotLayerPart	  *mpp = &mpl->mapplot;
	NhlString entry_name = "MapV40DHDrawMapList";
        NhlErrorTypes ret = NhlNOERROR;
        

        if (! DrawIds) {
                DrawIds = NhlMalloc(DrawId_Count *  sizeof(mpDrawIdRec));
		memset(DrawIds,-1,DrawId_Count *  sizeof(mpDrawIdRec));
                if (! DrawIds) {
                        NHLPERROR((NhlFATAL,ENOMEM,NULL));
                        return NhlFATAL;
                }
        }

        Mv4cp = &((NhlMapV40DataHandlerClass)
                  mv40l->base.layer_class)->mapv40dh_class;
	Mv40p = &(mv40l->mapv40dh);
	Mpp = mpp;
	Mpl = mpl;

        Last_Instance = instance;
        Point_Count = 0;
	Draw_Op = draw_op;
                
        switch (draw_op) {
	case mpDRAWFILL:
		ret =  mpFill(mv40l,mpl,entry_name);
		break;
	case mpDRAWOUTLINE:
		ret =  mpOutline(mv40l,mpl,entry_name);
		break;
	case mpDRAWGRID:
		ret =  mpGrid(mv40l,mpl,entry_name);
		break;
	default:
		break;
        }
                    
	Mpp = NULL;
	Mpl = NULL;
        Mv4cp = NULL;
	Mv40p = NULL;

        return ret;
}

/*
 * Function:  hlumapeod
 *
 * Description: 
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: 
 */

/*ARGSUSED*/
void   (_NHLCALLF(hlumapeod,HLUMAPEOD))
#if	NhlNeedProto
(
	int *nout,
	int *nseg,
	int *idls,
	int *idrs,
	int *npts,
	float *pnts
)

#else
(nout,nseg,idls,idrs,npts,pnts)
	int *nout;
	int *nseg;
	int *idls;
	int *idrs;
	int *npts;
	float *pnts;
#endif
        
{
	int ir,il;
	NhlBoolean keep = False;

	if (Mpp == NULL) {
		_NHLCALLF(mapeod,MAPEOD)(nout,nseg,idls,idrs,npts,pnts);
		return;
	}

#if 0
	printf("nseg %d idls %d, idrs %d, npts %d\n", 
	       *nseg,*idls,*idrs,*npts);
	for (i = 0; i < *npts; i++) {
		printf("x %f y %f",pnts[2*i],pnts[2*i+1]);
		if (*npts % 3 == 0)
			printf("\n");
	}
#endif
        
	switch (Draw_Op) {
	case mpDRAWOUTLINE:
		il = *idls - Id_Offset[Outline_Set];
		ir = *idrs - Id_Offset[Outline_Set];
		if (DrawIds[il].u.f.draw_mode == mpMASK || 
		    DrawIds[ir].u.f.draw_mode == mpMASK) {
			keep = False;
		}
		else if ((Mv40p->outline_groups[mpSmallIsland].u.f.draw_mode != mpDRAW) &&
			 (Outline_Set != mpUS && il == 1 && ir == 1)) 
			/* 
			 * this gets rid of line segments representing islands that have no area,
			 * and therefore no area id.
			 */
			keep = False;
		else {
			if (il >= 0) {
				switch (DrawIds[il].u.f.draw_mode) {
				case mpDRAW:
					keep = True;
					break;
				case mpDRAWSPECIAL:
				/* 
				 * this is for simulating large areas from contiguous smaller areas: 
				 * only draw a segment if it touches the larger border
				 */
                                   
					if (*idrs == Draw_Check)
						keep = True;
					break;
				default:
					break;
				}
			}
			if (ir >= 0) {
				switch (DrawIds[ir].u.f.draw_mode) {
				case mpDRAW:
					keep = True;
					break;
				case mpDRAWSPECIAL:
				/* 
				 * this is for simulating large areas from contiguous smaller areas: 
				 * only draw a segment if it touches the larger border
				 */
					if (*idls == Draw_Check)
						keep = True;
					break;
				default:
					break;
				}
			}
		}
		if (! keep)
			*npts = 0;
#if 0
		else {
			printf("nseg %d il %d, ir %d, npts %d idls %d idrs %d\n", 
			       *nseg,il,ir,*npts,*idls,*idrs);
		}
#endif
		break;
	case mpDRAWFILL:
		il = *idls - Id_Offset[Outline_Set];
		ir = *idrs - Id_Offset[Outline_Set];
		if (il >= 0 && DrawIds[il].u.f.draw_mode != mpBACKGROUND)
			keep = True;
		else if (ir >= 0 && DrawIds[ir].u.f.draw_mode != mpBACKGROUND)
			keep = True;
#if 0
		else if (il == 1 || ir == 1)
			keep = True;
#endif

		if (! keep)
			*npts = 0;
		else if ((DrawIds[ir].u.flags == DrawIds[il].u.flags) &&
			 (DrawIds[ir].ix == DrawIds[il].ix)) {
			*npts = 0;
		}

		break;
	default:
		break;                
	}
	if (*npts)
		Point_Count += *npts;
	return;
}

/*
 * Function:  load_hlumap_routines
 *
 * Description: Forces the hlumap... routines to load from the HLU library
 *
 * In Args:   NhlBoolean flag - should always be False - dont actually
 *			        want to call the routines.
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: 
 */

/*ARGSUSED*/
static void   load_hlumap_routines
#if	NhlNeedProto
(
	NhlBoolean	flag
)
#else
(flag)
	NhlBoolean	flag;
#endif
{
	int idum;
	float fdum;


	if (flag) {
		_NHLCALLF(hlumapeod,HLUMAPEOD)
			(&idum,&idum,&idum,&idum,&idum,&fdum);
	}
	return;
}
