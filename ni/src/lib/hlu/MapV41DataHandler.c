/*
 *      $Id: MapV41DataHandler.c,v 1.28 2008-09-12 20:22:46 dbrown Exp $
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

#include <ncarg/hlu/MapV41DataHandlerP.h>
#include <ctype.h>

#define Oset(field)	NhlOffset(NhlMapV41DataHandlerLayerRec,mapv41dh.field)
static NhlResource resources[] = {
	{NhlNmpDataSetName,NhlCmpDataSetName,NhlTString,
		 sizeof(NhlString),Oset(data_set_name),NhlTImmediate,
		 _NhlUSET((NhlPointer) NULL),0,NULL}
};

static NhlErrorTypes MapV41DHClassPartInit(
#if	NhlNeedProto
	NhlClass	lc
#endif
);


static NhlErrorTypes MapV41DHInitialize(
#if	NhlNeedProto
        NhlClass	class,
        NhlLayer	req,
        NhlLayer	new,
        _NhlArgList	args,
        int             num_args
#endif
);

static NhlErrorTypes  MapV41DHSetValues(
#if	NhlNeedProto
        NhlLayer	old,
        NhlLayer	reference,
        NhlLayer	new,
        _NhlArgList	args,
        int             num_args
#endif
);

static NhlErrorTypes    MapV41DHGetValues(
#if	NhlNeedProto
	NhlLayer        l,
	_NhlArgList     args,
	int             num_args
#endif
);

static NhlErrorTypes    MapV41DHDestroy(
#if	NhlNeedProto
	NhlLayer        l
#endif
);

static NhlErrorTypes MapV41DHUpdateDrawList(
#if	NhlNeedProto
	NhlLayer		instance,
        NhlBoolean  		init,
        NhlMapPlotLayer 	newmp,
        NhlMapPlotLayer 	oldmp,
        _NhlArgList		args,
        int             	num_args
#endif
);

static NhlErrorTypes MapV41DHDrawMapList(
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

NhlMapV41DataHandlerClassRec NhlmapV41DataHandlerClassRec = {
	{
/* class_name 		*/      "mapV41DataHandlerClass",
/* nrm_class 		*/      NrmNULLQUARK,
/* layer_size 		*/      sizeof(NhlMapV41DataHandlerLayerRec),
/* class_inited 	*/	False,
/* superclass		*/      (NhlClass)&NhlmapDataHandlerClassRec,
/* cvt_table		*/	NULL,

/* layer_resources 	*/   	resources,
/* num_resources 	*/     	NhlNumber(resources),
/* all_resources 	*/	NULL,
/* callbacks		*/	NULL,
/* num_callbacks	*/	0,
/* class_callbacks	*/	NULL,
/* num_class_callbacks	*/	0,

/* class_part_initialize */     MapV41DHClassPartInit,
/* class_initialize 	*/  	NULL,
/* layer_initialize 	*/  	MapV41DHInitialize,
/* layer_set_values 	*/  	MapV41DHSetValues,
/* layer_set_values_hook */  	NULL,
/* layer_get_values 	*/  	MapV41DHGetValues,
/* layer_reparent 	*/  	NULL,
/* layer_destroy 	*/    	MapV41DHDestroy,
	},
	{
/* update_map_draw_list */	MapV41DHUpdateDrawList,
/* draw_map_list        */      MapV41DHDrawMapList
	}
};

NhlClass NhlmapV41DataHandlerClass = (NhlClass)&NhlmapV41DataHandlerClassRec;

static int Level;
static int Color,Dash_Pattern;
static float Dash_SegLen,Thickness;

static NrmQuark Qstring = NrmNULLQUARK;
static NrmQuark Qarea_names = NrmNULLQUARK;
static NrmQuark Qarea_types = NrmNULLQUARK;
static NrmQuark Qdynamic_groups = NrmNULLQUARK;
static NrmQuark Qfixed_groups = NrmNULLQUARK;
static NrmQuark Qdata_set_name = NrmNULLQUARK;

static NhlMapPlotLayer Mpl;
static NhlMapPlotLayerPart *Mpp;
static NhlMapV41DataHandlerClassPart	*Mv41cp;
static NhlMapV41DataHandlerLayerPart	*Mv41p;
static NhlMapDataHandlerLayerPart *Mdhp;
static NhlBoolean Grid_Setup;
static NhlBoolean Count_Points_Only = False;

static mpDrawIdRec *DrawIds = NULL;
static int	    DrawIdCount = 0;

static int UsIds[3];
static int UsIdCount;
static int LandId,WaterId,OceanId;
static char OutBuf[512];

static NhlString DefDataSetName = "Earth..2";


static char *BorderWater_1[] = {
"ocean",
"aral sea (eurasia)",
"azov sea (eurasia)",
"black sea (eurasia)",
"caspian sea (eurasia)",
"lake albert (africa)",
"lake chad (africa)",
"lake kariba (africa)",
"lake malawi (africa)",
"lake tanganyika (africa)",
"lake titicaca (south america)",
"lake victoria (africa)",

"lake champlain (north america)",
"lake erie (north america)",
"lake george (north america)",
"lake of the woods (north america)",
"lake ontario (north america)",
"lake saint clair (north america)",
"lake superior (north america)",
"lakes michigan and huron (north america)",
"namakan lake (north america)",
"rainy lake (north america)",
"sault sainte marie (north america)",  /* national count */

"clark hill reservoir (north america)",
"lake mead (north america)",
"lake seminole (north america)",
"lake tahoe (north america)",
"lake texoma (north america)",
"sabine lake (north america)",     /* state count */

"great salt lake (north america)",
"lake maurepas (north america)",
"lake okeechobee (north america)",
"lake pontchartrain (north america)"

};

static char *BorderWater_4[] = {

"ocean",

"aral sea",
"azov sea",
"black sea",
"caspian sea",
"lake albert",
"lake chad",
"lake kariba",
"lake malawi",
"lake tanganyika",
"lake titicaca",
"lake victoria",

"lake champlain",
"lake erie",
"lake george",
"lake of the woods",
"lake ontario",
"lake saint clair",
"lake superior",
"lakes michigan and huron",
"namakan lake",
"rainy lake",
"sault sainte marie",  /* national count */

"clark hill reservoir",
"lake mead",
"lake seminole",
"lake tahoe",
"lake texoma",
"sabine lake",     /* state count */

"great salt lake",
"lake maurepas",
"lake okeechobee",
"lake pontchartrain"
};

static char **BorderWater;
static int GeoBorderWaterCount = 1;
static int NatBorderWaterCount = 23;
static int StateBorderWaterCount = 29;
static int CountyBorderWaterCount = 33;
static int USStartIndex = 13; /* this is for  NhlGEOPHYSICALANDUSSTATES */
static int BorderWaterEids[33];

static NrmQuark RDatasets[5];

/*
 * special entity recs for broad subcategories
 * all ids are -1 because these are not part of the regular database
 */

static v41EntityRec SubCatRecs[] = {
{ 4, -1, 2, -1, -1, 0, "states" },
{ 4, -1, 2, -1, -1, 0, "provinces" },
{ 5, -1, 2, -1, -1, 0, "counties" }
};

/*
 * The following string manipulation routines help deal with Map entity
 * names in various formats. The first three do not alter the input string
 * but put the modified output into a single static buffer. Therefore you must
 * save the output if you want to preserve it before calling any one of the
 * routines again. They are designed to work if the static buffer itself
 * is the input string.
 */

static char *mpLowerCase(char *string)
{
	register char *cp = string;
	register char *bp = OutBuf;

	while (*cp != '\0') {
                *(bp++) = tolower(*(cp++));
	}
	*bp = '\0';
	return OutBuf;
}

/*
 * Either ':' or '.' indicate parent-child boundaries in the spec string.
 * To simplify the search, change '.' to ':'. Also in order to allow
 * some compatibility with the 4.0 database, '-' used as a word separator is
 * replaced with a space. However, we must be careful not to replace '-'
 * when it is part of the 4.1 name. In this case the '-' should always be
 * preceded and followed by a space (except possibly when a wild card is
 * involved, but this is not currently handled).
 * NOTE: actually it is not the case that 4.1 names never have a '-'
 * without spaces between them, so I am not going to do any processing
 * for now.
 */

static char *PrepareSpecString(char *string)
{
	char *cp = string;
        char *bp = OutBuf;
        NhlBoolean last_space = False;

	while (*cp != '\0') {
                switch (*cp) {
                    case '.':
                            *bp = ':';
                            last_space = False;
                            break;
#if 0                            
                    case ' ':
                            *bp = ' ';
                            last_space = True;
                            break;
                    case '-':
                            *bp = last_space ? '-' : ' ';
                            last_space = False;
                            break;
#endif                            
                    default:
                            *bp = tolower(*cp);
                            last_space = False;
                            break;
                }
		cp++,bp++;
	}
        *bp = '\0';
        
        return OutBuf;
}

/*
 * The original string is not modified, but copied into a static output
 * buffer. Therefore you can only work on one string at a time.
 * This routine simply substitutes ':' whereever it sees a '.'. This is
 * only for processing new mpAreaName arrays -- where only the simple
 * entity name is preserved, but the string is otherwise unaltered -- in
 * particular it is not lower-cased or have its '-'s removed.
 */
static char *SimplifyString(char *string)
{
	char *cp = string;
        char *bp = OutBuf;

	while (*cp != '\0') {
                switch (*cp) {
                    case '.':
                            *bp = ':';
                            break;
                    default:
                            *bp = *cp;
                }
                cp++,bp++;
	}
        *bp = '\0';
        
        return OutBuf;
}

/*
 * This function assumes a that the input string does not need to be
 * preserved. For each call it returns the next higher name in partially
 * or fully qualified entity name. That is, it starts with the leaf node
 * child, and each successive call returns the next immediate ancestor.
 */

static char *UpNameHierarchy
#if	NhlNeedProto
(
        char	*string
        )
#else
(string)
	char	*string;
#endif
{
        char *cp,*bcp;

        cp = string;
        
        cp = strrchr(cp,':');
        if (cp > string) {
                *cp = '\0';
                bcp = cp - 1;
                while (isspace(*bcp))
                        *(bcp--) = '\0';
                cp++;
        }
        else
                cp = string;
        
        while (isspace(*cp))
                cp++;

        return cp;
}

static NhlErrorTypes
MapV41DHClassPartInit
#if	NhlNeedProto
(
	NhlClass	lc
)
#else
(lc)
	NhlClass	lc;
#endif
{
	NhlMapV41DataHandlerClass	mdhc = (NhlMapV41DataHandlerClass)lc;
        
	Qstring = NrmStringToQuark(NhlTString);
	Qarea_names = NrmStringToQuark(NhlNmpAreaNames);
	Qarea_types = NrmStringToQuark(NhlNmpAreaTypes);
	Qdynamic_groups = NrmStringToQuark(NhlNmpDynamicAreaGroups);
	Qfixed_groups = NrmStringToQuark(NhlNmpFixedAreaGroups);
	Qdata_set_name = NrmStringToQuark(NhlNmpDataSetName);
        
        Mv41cp = &mdhc->mapv41dh_class;
        Mv41cp->entity_rec_count = 0;
        Mv41cp->entity_recs = NULL;

	load_hlumap_routines(False);
        
	return NhlNOERROR;
}

static int alpha_sort
(
        const void *p1,
        const void *p2
)
{
        v41EntityRec *erec1 = *(v41EntityRec **) p1;
        v41EntityRec *erec2 = *(v41EntityRec **) p2;
        int ret;

        ret =  strcmp(erec1->name,erec2->name);
        if (! ret) {
                int ix1 = erec1->eid;
                int ix2 = erec2->eid;
                while (! ret) {
                        ix1 = c_mpipar(ix1);
                        ix2 = c_mpipar(ix2);
			if (! (ix1 && ix2))
				return 0;
                        ret = strcmp(Mv41p->entity_recs[ix1-1].name,
                                     Mv41p->entity_recs[ix2-1].name);
                }
        }
        return ret;
}

typedef struct _LongNameRec 
{
        char *lname;
        v41EntityRec *erec;
} LongNameRec;

static int long_alpha_sort
(
        const void *p1,
        const void *p2
)
{
        LongNameRec lrec1 = *(LongNameRec *) p1;
        LongNameRec lrec2 = *(LongNameRec *) p2;

        return strcmp(lrec1.lname,lrec2.lname);
}

static NhlErrorTypes Init_Entity_Recs
#if	NhlNeedProto
(
	NhlMapV41DataHandlerLayer	mv41l,
        NhlString			entry_name
)
#else
(mv41l,entry_name)
	NhlMapV41DataHandlerLayer	mv41l;
        NhlString			entry_name;
#endif
{
        NhlMapV41DataHandlerLayerPart *mv41p = &mv41l->mapv41dh;
	char *e_text;
        int i,j;
        LongNameRec *lname_recs;
        char lname_buf[256];
	NrmQuark cur_dataset_q;

	RDatasets[0] = NrmStringToQuark("Earth..1");
	RDatasets[1] = NrmStringToQuark("Earth..2");
	RDatasets[2] = NrmStringToQuark("Earth..3");
	RDatasets[3] = NrmStringToQuark("Earth..4");
	RDatasets[4] = NrmNULLQUARK;

        mv41p->entity_recs = NhlMalloc
                (sizeof(v41EntityRec) * mv41p->entity_rec_count);
        mv41p->alpha_recs = NhlMalloc
                (sizeof(v41EntityRec *) * mv41p->entity_rec_count);
        mv41p->long_alpha_recs = NhlMalloc
                (sizeof(v41EntityRec *) * mv41p->entity_rec_count);
        lname_recs = NhlMalloc
                (sizeof(LongNameRec) * mv41p->entity_rec_count);
        if (! mv41p->entity_recs || ! mv41p->alpha_recs ||
            !mv41p->long_alpha_recs || ! lname_recs) {
                e_text = "%s: dynamic memory allocation error";
                NhlPError(NhlFATAL,ENOMEM,e_text,entry_name);
                return NhlFATAL;
        }
        
        for (i = 0; i < mv41p->entity_rec_count; i++) {
                v41EntityRec *erec = &mv41p->entity_recs[i];
                char *buf;
                int j,pix[32],pcount,ix = i + 1;
                int type,lasttype = -1;
                
                mv41p->alpha_recs[i] = erec;
                mv41p->long_alpha_recs[i] = erec;
                erec->level = (int) NGCALLF(mpiaty,MPIATY)(&ix);
                erec->eid = ix;
                buf = c_mpname(ix);
                erec->name = NhlMalloc(strlen(buf)+1);
                if (!erec->name) {
                        e_text = "%s: dynamic memory allocation error";
                        NhlPError(NhlFATAL,ENOMEM,e_text,entry_name);
                        return NhlFATAL;
                }
#ifdef HLU_WRITE_TABLES
		strcpy(erec->name,buf);
#else
                strcpy(erec->name,mpLowerCase(buf));
#endif

                if (ix == LandId) {
                        erec->dynamic_gid = erec->fixed_gid =
                                NhlmpLANDGROUPINDEX;
                }
                else if (NGCALLF(mpipai,MPIPAI)(&ix,&LandId)) {
                        erec->fixed_gid = NhlmpLANDGROUPINDEX;
                        erec->dynamic_gid = NGCALLF(mpisci,MPISCI)(&ix) + 2;
                }
                else if (ix == OceanId || ix == WaterId)
                        erec->dynamic_gid = erec->fixed_gid
                                = NhlmpOCEANGROUPINDEX;
                else 
                        erec->dynamic_gid = erec->fixed_gid
                                = NhlmpINLANDWATERGROUPINDEX;

                pix[0] = ix;
                for (j = 0; pix[j] != 0; j++)
                        pix[j+1] = c_mpipar(pix[j]);
                pcount = j;

                lname_buf[0] = '\0';
                for (j = pcount-1; j >=0; j--) {
                        type = c_mpiaty(pix[j]);
                        if (lasttype > -1) {
                                if (type != lasttype)
                                        strcat(lname_buf," : ");
                                else
                                        strcat(lname_buf," . ");
                        }
                        strcat(lname_buf,c_mpname(pix[j]));
                        lasttype = type;
                }
                lname_recs[i].lname = NhlMalloc(strlen(lname_buf) + 1);
                strcpy(lname_recs[i].lname,lname_buf);
                lname_recs[i].erec = erec;
                
        }
        qsort(mv41p->alpha_recs,mv41p->entity_rec_count,
              sizeof(v41EntityRec *),alpha_sort);
        qsort(lname_recs,mv41p->entity_rec_count,
              sizeof(LongNameRec),long_alpha_sort);

        for (i = 0; i < mv41p->entity_rec_count; i++) {
                mv41p->alpha_recs[i]->unique = True;
                if (i < mv41p->entity_rec_count - 1) {
                        if (! strcmp(mv41p->alpha_recs[i]->name,
                              mv41p->alpha_recs[i+1]->name))
                                mv41p->alpha_recs[i]->unique = False;
                }
                if (i > 0) {
                        if (! strcmp(mv41p->alpha_recs[i]->name,
                              mv41p->alpha_recs[i-1]->name))
                                mv41p->alpha_recs[i]->unique = False;
                }
                mv41p->long_alpha_recs[i] = lname_recs[i].erec;
		mv41p->long_alpha_recs[i]->canonical_ix = i;
	}
#ifdef HLU_WRITE_TABLES                
	printf("<table border=1 width=100%%>\n");
	printf("<tr><th>Element<br>Index</th><th>Type</th><th>Fixed<br> Group</th><th>Dynamic<br>Group</th><th>Area Name</th><tr>\n");
        for (i = 0; i < mv41p->entity_rec_count; i++) {
                char buf[256];
		char *sub,*sub2;
                if (mv41p->long_alpha_recs[i]->unique) {
                        strcpy(buf,mv41p->long_alpha_recs[i]->name);
                }
                else {
                        int pix = c_mpipar(mv41p->long_alpha_recs[i]->eid);
                        int ptype = c_mpiaty(pix);
                        int etype = c_mpiaty(mv41p->long_alpha_recs[i]->eid);
                        
                        strcpy(buf,mv41p->entity_recs[pix-1].name);
                        if (ptype == etype)
                                strcat(buf," . ");
                        else 
                                strcat(buf," : ");
                        strcat(buf,mv41p->long_alpha_recs[i]->name);
                }
		printf("<tr>\n");
		sub = strstr(lname_recs[i].lname,buf);
		sub2 = sub;
		while (sub2) {
			sub2 = strstr(sub+1,buf);
			if (sub2) sub = sub2;
		}
		if (sub) {
			*sub = '\0';
			printf("<td>%d</td><td>%d</td><td>%d</td><td>%d</td><td>%s<b>%s</b></td>\n",
			       i,
			       mv41p->long_alpha_recs[i]->level,
			       mv41p->long_alpha_recs[i]->fixed_gid,
			       mv41p->long_alpha_recs[i]->dynamic_gid,
			       lname_recs[i].lname,
			       buf);
		}
		else {
			printf("error in %s\n",buf);
		}
		printf("</tr>\n");
	}
	printf("</table>\n");
/*
	printf("=========================================================================\n");
        for (i = 0; i < mv41p->entity_rec_count; i++) {
		printf("%d\t%s\n",i+1,lname_recs[i].lname);
        }
*/
        NhlFree(lname_recs);
	exit(1);
#endif
        
#if 0        
        for (i = 1; i <= mv41p->entity_rec_count; i++) {
                if (NGCALLF(mpipai,MPIPAI)(&i,&UsIds[0])) {
                        printf("%d -- child type %d name %s\n",UsIds[0],
                               NGCALLF(mpiaty,MPIATY)(&i),
                               c_mpname(i));
                        us_child_count[0]++;
                }
                if (NGCALLF(mpipai,MPIPAI)(&i,&UsIds[1])) {
                        printf("%d -- child type %d name %s\n",UsIds[1],
                               NGCALLF(mpiaty,MPIATY)(&i),
                               c_mpname(i));
                        us_child_count[1]++;
                }
                if (NGCALLF(mpipai,MPIPAI)(&i,&UsIds[2])) {
                        printf("%d -- child type %d name %s\n",UsIds[2],
                               NGCALLF(mpiaty,MPIATY)(&i),
                               c_mpname(i));
                        us_child_count[2]++;
                }
        }
        printf("%s: %d children, %s: %d children, %s: %d children\n",
               c_mpname(UsIds[0]),us_child_count[0],
               c_mpname(UsIds[1]),us_child_count[1],
               c_mpname(UsIds[2]),us_child_count[2]);
#endif                

	cur_dataset_q = NrmStringToQuark(mv41p->data_set_name);
	for (i = 0; RDatasets[i] != NrmNULLQUARK; i++) {
		if (cur_dataset_q == RDatasets[i]) {
			if (i == 3)
				BorderWater = BorderWater_4;
			else
				BorderWater = BorderWater_1;
			break;
		}
	}
	if (RDatasets[i] == NrmNULLQUARK) {
		if (lname_recs != NULL) {
			for (i = 0; i < mv41p->entity_rec_count; i++) {
				NhlFree(lname_recs[i].lname);
			}
			NhlFree(lname_recs);
		}
		return (NhlNOERROR);
	}
#if 0
	printf("initializing dataset %s\n",mv41p->data_set_name);
#endif
		
	memset(BorderWaterEids,0,sizeof(int) * CountyBorderWaterCount);
	j = 0;
	for (i = 0; i < CountyBorderWaterCount; i++) {
		int found = 0;
		while (! found) {
			if (j == mv41p->entity_rec_count) {
				j = 0;
#if 0				
				printf("recycling j at i=%d, \n",i);
#endif
			}
			if (! strcmp(mv41p->alpha_recs[j]->name,BorderWater[i])) {
				if (c_mpiola(mv41p->alpha_recs[j]->eid,1) == Mv41p->basic_ids.water_id) {
					BorderWaterEids[i] = mv41p->alpha_recs[j]->eid;
#if 0
					printf("initializing %d with alpha rec %d\n",i,j);
#endif
					found = 1;
				}
			}
			j++;
		}
	}
        for (i = 0; i < mv41p->entity_rec_count; i++) {
		NhlFree(lname_recs[i].lname);
	}
	NhlFree(lname_recs);
		
	return NhlNOERROR;
}    
        


static NhlErrorTypes SetUpEntityRecs
#if	NhlNeedProto
(
	NhlMapV41DataHandlerLayer	mv41l,
        NhlString			entry_name
)
#else
(mv41l,entry_name)
	NhlMapV41DataHandlerLayer	mv41l;
        NhlString			entry_name;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	NhlMapDataHandlerLayerPart *mdhp = &(mv41l->mapdh);
	char *e_text;
        int i,us_ix=0;

        Mv41p = &mv41l->mapv41dh;
	if (Mv41p->entity_recs &&
	    Mv41p->entity_recs != Mv41cp->entity_recs) {
		for (i = 0; i < Mv41p->entity_rec_count; i++) {
			NhlFree(Mv41p->entity_recs[i].name);
		}
		NhlFree(Mv41p->entity_recs);
		Mv41p->entity_recs = NULL;
	}
	if (Mv41p->alpha_recs &&
	    Mv41p->alpha_recs != Mv41cp->alpha_recs) {
		NhlFree(Mv41p->alpha_recs);
		Mv41p->alpha_recs = NULL;
	}
	if (Mv41p->long_alpha_recs &&
	    Mv41p->long_alpha_recs != Mv41cp->long_alpha_recs) {
		NhlFree(Mv41p->long_alpha_recs);
		Mv41p->long_alpha_recs = NULL;
	}
	Mv41p->data_set_point_count = 0;
	Mv41p->entity_rec_count = 0;
	Mv41p->basic_ids.land_id = LandId = -1;
	Mv41p->basic_ids.water_id = WaterId = -1;
	Mv41p->basic_ids.ocean_id = OceanId = -1;

	c_mplnri(Mv41p->data_set_name);
        
	for (i = 1; ;i++) {
		int type = NGCALLF(mpiaty,MPIATY)(&i);
		if (type == 0)
			break;
		if (!strcmp(c_mdname(i),"United States")) {
#if 0
			printf("us id %d\n",i);
#endif
			Mv41p->basic_ids.us_ids[us_ix] = UsIds[us_ix] = i;
			us_ix++;
		}
		if (!strcmp(c_mdname(i),"Land") && c_mdipar(i) == 0) {
#if 0
			printf("land id %d\n",i);
#endif
			Mv41p->basic_ids.land_id = LandId = i;
		}
		if (!strcmp(c_mdname(i),"Water")&& c_mdipar(i) == 0) {
#if 0
			printf("water id %d\n",i);
#endif
			Mv41p->basic_ids.water_id = WaterId = i;
		}
		if (!strcmp(c_mdname(i),"Ocean") &&
			!strcmp(c_mdname(c_mdipar(i)),"Water")) {
#if 0
			printf("ocean id %d\n",i);
#endif
			Mv41p->basic_ids.ocean_id = OceanId = i;
		}
	}
	Mv41p->basic_ids.us_id_count = UsIdCount = us_ix;
	Mv41p->entity_rec_count = Mv41p->outline_rec_count = i-1;

	if (! strcmp(Mv41p->data_set_name,DefDataSetName)) {
		if (! Mv41cp->entity_rec_count) {
			ret = Init_Entity_Recs(mv41l,entry_name);
			if (ret < NhlWARNING) {
				e_text	 =
                                "%s: error initializing map outline records";
				NhlPError(NhlFATAL,
					  NhlEUNKNOWN,e_text,entry_name);
				return(ret);
			}
			Mv41cp->entity_rec_count = Mv41p->entity_rec_count;
			Mv41cp->entity_recs = Mv41p->entity_recs;
			Mv41cp->alpha_recs = Mv41p->alpha_recs;
			Mv41cp->long_alpha_recs = Mv41p->long_alpha_recs;
		}
		else {
			Mv41p->entity_rec_count = Mv41cp->entity_rec_count;
			Mv41p->entity_recs = Mv41cp->entity_recs;
			Mv41p->alpha_recs = Mv41cp->alpha_recs;
			Mv41p->long_alpha_recs = Mv41cp->long_alpha_recs;
		}
	}
	else {
		ret = Init_Entity_Recs(mv41l,entry_name);
		if (ret < NhlWARNING) {
			e_text = "%s: error initializing map outline records";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return(ret);
		}
	}
	
	return ret;
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
	NhlMapV41DataHandlerLayer	mv4new, 
	NhlMapV41DataHandlerLayer	mv4old,
	NhlBoolean			init,
	_NhlArgList			args,
	int				num_args)
#else
(mdhnew,mdhold,init,args,num_args)
	NhlMapV41DataHandlerLayer	mv4new;
	NhlMapV41DataHandlerLayer	mv4old;
	NhlBoolean			init;
	_NhlArgList			args;
	int				num_args;
#endif

{
	NhlMapDataHandlerLayerPart	*mdhp = &(mv4new->mapdh);
	NhlMapDataHandlerLayerPart	*omdhp = &(mv4old->mapdh);
        NhlMapV41DataHandlerLayerPart	*mv41p = &mv4new->mapv41dh;
        
	NhlErrorTypes ret = NhlNOERROR;
	char *entry_name;
	char *e_text;
	NhlGenArray ga;
	int i;
	int *ip;
	NhlString *sp;
	NhlBoolean need_check;
	NhlBoolean use_default;
        int entity_rec_count;
        v41EntityRec *entity_recs;
        NhlBoolean new_area_names = False;

	entry_name =  init ?
                "MapDataHandlerInitialize" : "MapDataHandlerSetValues";

        entity_rec_count = mv41p->entity_rec_count;
        entity_recs = mv41p->entity_recs;
        
/*
 * Area Name specifiers
 */
	ga = init ? NULL : omdhp->area_names;

	if (ga != mdhp->area_names) {
		if (! mdhp->area_names) {
                        NhlFreeGenArray(ga);
                }
                else if (mdhp->area_names->num_elements != entity_rec_count) {
			e_text = 
			  "%s: %s GenArray must contain %d elements: ignoring";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  NhlNmpAreaNames,entity_rec_count);
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
                        new_area_names = True;
		}
	}
        if (new_area_names) {
                        
/* Check elements for null strings and replace full specs with simple names */
                
                sp = (NhlString *) mdhp->area_names->data;
                for (i = 0; i < mdhp->area_names->num_elements; i++) {
                        NhlString spfull,spout;
                        if (sp[i] == NULL || strlen(sp[i]) == 0) {
                                char *buf = c_mdname(i+1);
                                e_text = 
                  "%s: Null or zero length %s string for index %d: defaulting";
                                NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
                                          entry_name,NhlNmpAreaNames,i);
                                ret = MIN(ret,NhlWARNING);
                                if (sp[i] != NULL) NhlFree(sp[i]);
                                        
                                sp[i] = NhlMalloc(strlen(buf) + 1);
                                if (! sp[i]) {
                                        e_text = 
                                        "%s: dynamic memory allocation error";
                                        NhlPError(NhlFATAL,ENOMEM,
                                                  e_text,entry_name);
                                        return NhlFATAL;
                                }
                                strcpy(sp[i],buf);
                        }
                        spfull = SimplifyString(sp[i]);
                        spout = UpNameHierarchy(spfull);
                        if (strcmp(spout,sp[i])) {
                                NhlFree(sp[i]);
                                sp[i] = NhlMalloc(strlen(spout) + 1);
                                if (! sp[i]) {
                                        e_text = 
                                        "%s: dynamic memory allocation error";
                                        NhlPError(NhlFATAL,ENOMEM,
                                                  e_text,entry_name);
                                        return NhlFATAL;
                                }
                                strcpy(sp[i],spout);
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
                else if (mdhp->dynamic_groups->num_elements
                         != entity_rec_count) {
			e_text = 
			  "%s: %s GenArray must contain %d elements: ignoring";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  NhlNmpDynamicAreaGroups,entity_rec_count);
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
				ip[i] =
                                       mv41p->long_alpha_recs[i]->dynamic_gid;
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
 * Function:	MapV41DHInitialize
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
MapV41DHInitialize
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
        NhlMapV41DataHandlerLayer mv41l = (NhlMapV41DataHandlerLayer) new;
	NhlMapDataHandlerLayerPart *mdhp = &(mv41l->mapdh);
        NhlMapV41DataHandlerLayerPart *mv41p = &mv41l->mapv41dh;
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*entry_name = "MapV41DHInitialize";
	char			*e_text;
	char	*dsname = NULL;
        
	mv41p->aws_id = -1;
	mv41p->fill_rec_alloc = 0;
	mv41p->fill_rec_count = 0;
	mv41p->fill_recs = NULL;
	mv41p->outline_rec_alloc = 0;
	mv41p->outline_rec_count = 0;
	mv41p->outline_recs = NULL;
	mv41p->data_set_point_count = 0;
	mv41p->entity_rec_count = 0;
	mv41p->entity_recs = NULL;
	mv41p->alpha_recs = NULL;
	mv41p->long_alpha_recs = NULL;

	if (mv41p->data_set_name) {
		dsname = (char*)_NGResolvePath(mv41p->data_set_name);
		if(!dsname){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
		"%s:Unable to resolve path name for \"%s\", defaulting %s",
				  entry_name,mv41p->data_set_name,
				  NhlNmpDataSetName);
			ret = NhlWARNING;
		}
	}
	if (! dsname) {
		dsname = DefDataSetName;
	}
	mv41p->data_set_name = NhlMalloc(strlen(dsname)+1);
	if(! mv41p->data_set_name){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NhlFATAL;
	}
	strcpy(mv41p->data_set_name,dsname);

	SetUpEntityRecs(mv41l,entry_name);
        
/* Manage the dynamic arrays */

	subret = mdhManageDynamicArrays((NhlMapV41DataHandlerLayer)new,
                                        (NhlMapV41DataHandlerLayer)req,
                                        True,args,num_args);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error managing dynamic arrays";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

        return ret;
}

/*
 * Function:	MapV41DHSetValues
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
static NhlErrorTypes MapV41DHSetValues
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
        NhlMapV41DataHandlerLayer mv41l = (NhlMapV41DataHandlerLayer) new;
	NhlMapDataHandlerLayerPart *mdhp = &(mv41l->mapdh);
	NhlMapV41DataHandlerLayerPart *mv41p = &(mv41l->mapv41dh);
        NhlMapV41DataHandlerLayer omv41l = (NhlMapV41DataHandlerLayer) old;
	NhlMapDataHandlerLayerPart *omdhp = &(omv41l->mapdh);
	NhlMapV41DataHandlerLayerPart *omv41p = &(omv41l->mapv41dh);
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*entry_name = "MapV41DHSetValues";
	char			*e_text;
	char			*dsname = NULL;

	if (mv41p->data_set_name != omv41p->data_set_name) {
		if (mv41p->data_set_name) {
			dsname = (char*)_NGResolvePath(mv41p->data_set_name);
			if(!dsname){
				NhlPError(NhlWARNING,NhlEUNKNOWN,
		"%s:Unable to resolve path name for \"%s\", defaulting %s",
					  entry_name,mv41p->data_set_name,
					  NhlNmpDataSetName);
				ret = NhlWARNING;
				dsname = omv41p->data_set_name;
			}
		}
		if (! dsname)
			dsname = DefDataSetName;
		mv41p->data_set_name = NhlMalloc(strlen(dsname)+1);
		if(!mv41p->data_set_name){
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return NhlFATAL;
		}
		strcpy(mv41p->data_set_name,dsname);
		if (omv41p->data_set_name) {
			NhlFree(omv41p->data_set_name);
			omv41p->data_set_name = NULL;
		}
		SetUpEntityRecs(mv41l,entry_name);
	}
	else {
		/*
		 * this is necessary to ensure that Ezmap is using the
		 * correct data set.
		 */
		c_mplnri(mv41p->data_set_name);
	}

		

/* Manage the dynamic arrays */

	subret = mdhManageDynamicArrays((NhlMapV41DataHandlerLayer)new,
                                        (NhlMapV41DataHandlerLayer)old,
                                        False,args,num_args);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error managing dynamic arrays";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

        return ret;
}

/*
 * Function:	BuildAreaNamesGenArray
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

static NhlGenArray BuildAreaNamesGenArray
#if	NhlNeedProto
(
 	NhlMapV41DataHandlerLayer       mv41l,
        NhlString			entry_name
)
#else
(mv41l,entry_name)
	NhlMapV41DataHandlerLayer       mv41l;
        NhlString			entry_name;
#endif
{
        NhlMapDataHandlerLayerPart 	*mdhp = &mv41l->mapdh;
        NhlMapV41DataHandlerLayerPart	*mv41p = &mv41l->mapv41dh;
	char *e_text;
	NhlGenArray ga;
        ng_size_t i, entity_rec_count;
        v41EntityRec *entity_recs;
        v41EntityRec **long_alpha_recs;
        char lbuf[512];
        NhlString	*sp, *anames = (NhlString *)mdhp->area_names->data;

        entity_rec_count = mv41p->entity_rec_count;
        entity_recs = mv41p->entity_recs;
        long_alpha_recs = mv41p->long_alpha_recs;

        e_text = "%s: dynamic memory allocation error";

        if ((sp = NhlMalloc(sizeof(NhlString)*entity_rec_count)) == NULL) {
			NhlPError(NhlFATAL,
				  NhlEUNKNOWN,e_text,entry_name);
			return NULL;
        }
        for (i = 0; i < entity_rec_count; i++) {
                int eid = long_alpha_recs[i]->eid;
                if (long_alpha_recs[i]->unique) {
                        char *buf = anames[i];
                        if ((sp[i] = NhlMalloc(strlen(buf) + 1)) == NULL) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN,
                                          e_text,entry_name);
                                return NULL;
                        }
                        strcpy(sp[i],buf);
                }
                else {
                        int pix = c_mpipar(eid);
                        int ptype = c_mpiaty(pix);
                        int etype = c_mpiaty(eid);
                        int pcix = mv41p->entity_recs[pix-1].canonical_ix;
                        
                        strcpy(lbuf,anames[pcix]);
                        if (ptype == etype)
                                strcat(lbuf," . ");
                        else 
                                strcat(lbuf," : ");
                        strcat(lbuf,anames[i]);

                        if ((sp[i] = NhlMalloc(strlen(lbuf) + 1)) == NULL) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN,
                                          e_text,entry_name);
                                return NULL;
                        }
                        strcpy(sp[i],lbuf);
                }
        }
        if ((ga = NhlCreateGenArray(sp,NhlTString,sizeof(NhlString),
                                    1,&entity_rec_count)) == NULL) {
                e_text = "%s: error creating gen array";
                NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
                return NULL;
        }
        ga->my_data = True;
        return ga;
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
 	NhlMapV41DataHandlerLayer       mv41l,
	NrmQuark			quark,
        NhlString			entry_name
)
#else
(mv41l,quark,entry_name)
	NhlMapV41DataHandlerLayer       mv41l;
	NrmQuark			quark;
        NhlString			entry_name;
#endif
{
        NhlMapDataHandlerLayerPart 	*mdhp = &mv41l->mapdh;
        NhlMapV41DataHandlerLayerPart	*mv41p = &mv41l->mapv41dh;
	char *e_text;
	int i;
	ng_size_t len;
	NhlGenArray ga;
        ng_size_t entity_rec_count;
        v41EntityRec *entity_recs;
        v41EntityRec **long_alpha_recs;
        char lbuf[512];

        entity_rec_count = mv41p->entity_rec_count;
        entity_recs = mv41p->entity_recs;
        long_alpha_recs = mv41p->long_alpha_recs;
        
	if (quark == Qarea_names) {
		NhlString	*sp;
                
                if (mdhp->area_names) {
                        return BuildAreaNamesGenArray(mv41l,entry_name);
                }
                
		len = entity_rec_count;
                e_text = "%s: dynamic memory allocation error";
		if ((sp = NhlMalloc(sizeof(NhlString)*len)) == NULL) {
			NhlPError(NhlFATAL,
				  NhlEUNKNOWN,e_text,entry_name);
			return NULL;
		}
                
                for (i = 0; i < len; i++) {
                        int eid = long_alpha_recs[i]->eid;
                        if (long_alpha_recs[i]->unique) {
                                char *buf = c_mdname(eid);

                                if ((sp[i] = NhlMalloc
                                     (strlen(buf) + 1)) == NULL) {
                                        NhlPError(NhlFATAL,NhlEUNKNOWN,
                                                  e_text,entry_name);
                                        return NULL;
                                }
                                strcpy(sp[i],buf);
                        }
                        else {
                                int pix = c_mpipar(eid);
                                int ptype = c_mpiaty(pix);
                                int etype = c_mpiaty(eid);
                        
                                strcpy(lbuf,c_mdname(pix));
                                if (ptype == etype)
                                        strcat(lbuf," . ");
                                else 
                                        strcat(lbuf," : ");
                                strcat(lbuf,c_mdname(eid));

                                if ((sp[i] = NhlMalloc
                                     (strlen(lbuf) + 1)) == NULL) {
                                        NhlPError(NhlFATAL,NhlEUNKNOWN,
                                                  e_text,entry_name);
                                        return NULL;
                                }
                                strcpy(sp[i],lbuf);
                        }
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
		len = entity_rec_count;
		if ((ip = NhlMalloc(sizeof(int)*len)) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,
				  NhlEUNKNOWN,e_text,"MapPlotGetValues");
			return NULL;
		}
		for (i = 0; i < len; i++) {
			ip[i] = long_alpha_recs[i]->level;
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
        else if (quark == Qfixed_groups) {
		int	*ip;
		len = entity_rec_count;
		if ((ip = NhlMalloc(sizeof(int)*len)) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,
				  NhlEUNKNOWN,e_text,"MapPlotGetValues");
			return NULL;
		}
		for (i = 0; i < len; i++) {
                        ip[i] = long_alpha_recs[i]->fixed_gid;
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
	else if (quark == Qdynamic_groups) {
		int	*ip;

		len = entity_rec_count;
		if ((ip = NhlMalloc(sizeof(int)*len)) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(NhlFATAL,
				  NhlEUNKNOWN,e_text,"MapPlotGetValues");
			return NULL;
		}
                for (i = 0; i < len; i++) {
                        ip[i] = long_alpha_recs[i]->dynamic_gid;
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
 * Function:    MapV41DHGetValues
 *
 * Description: Retrieves the current setting of MapV41DataHandler resources.
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

static NhlErrorTypes    MapV41DHGetValues
#if	NhlNeedProto
(NhlLayer l, _NhlArgList args, int num_args)
#else
(l,args,num_args)
        NhlLayer        l;
        _NhlArgList     args;
        int     	num_args;
#endif
{
        NhlMapV41DataHandlerLayer mv41l = (NhlMapV41DataHandlerLayer) l;
        NhlMapDataHandlerLayerPart *mdhp = &mv41l->mapdh;
        NhlMapV41DataHandlerLayerPart	*mv41p = &mv41l->mapv41dh;
        NhlGenArray ga;
	NhlString ts;
        NhlString e_text,entry_name = "MapV41DHGetValues";
        int i, count = 0;
	NhlBoolean create_it;

        for (i = 0; i < num_args; i++ ) {

		create_it = False;
                ga = NULL;
                
                if (args[i].quark == Qarea_names) {
/*
 * Since the stored form of the mpAreaNames is not the same as what the
 * user should see, it is always necessary to build a new array from
 * scratch when this resource is retrieved.
 */
			create_it = True;
                        ga = NULL;
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
			if ((ga = mdhGetNewGenArray(mv41l,args[i].quark,
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
		ts = NULL;
		if(args[i].quark == Qdata_set_name){
			ts = mv41p->data_set_name;
		}
                if (ts != NULL) {
			*((NhlString*)(args[i].value.ptrval)) =
				NhlMalloc(strlen(ts)+1);
			if(!*((NhlString*)(args[i].value.ptrval))){
                                e_text = "%s: error copying String";
                                NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
					  entry_name);
                                return NhlFATAL;
                        }
			strcpy(*((NhlString*)(args[i].value.ptrval)),ts);
			continue;
                }
        }

        return(NhlNOERROR);

}


/*
 * Function:    MapV41DHDestroy
 *
 * Description: Destroys memory specifically the responsibility of the V41 Map DataHandler
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
 */

static NhlErrorTypes    MapV41DHDestroy
#if	NhlNeedProto
(
        NhlLayer l
        )
#else
(l)
        NhlLayer        l;
#endif
{
        NhlMapV41DataHandlerLayer mv41l = (NhlMapV41DataHandlerLayer) l;
        NhlMapV41DataHandlerLayerPart *mv41p = &mv41l->mapv41dh;
	int i;

	if (mv41p->fill_recs != NULL)
		NhlFree(mv41p->fill_recs);
	if (mv41p->outline_recs != NULL)
		NhlFree(mv41p->outline_recs);
	if (mv41p->aws_id > 0)
		_NhlFreeWorkspace(mv41p->aws_id);

	if (mv41p->entity_recs &&
	    mv41p->entity_recs != Mv41cp->entity_recs) {
		for (i = 0; i < mv41p->entity_rec_count; i++) {
			NhlFree(mv41p->entity_recs[i].name);
		}
		NhlFree(mv41p->entity_recs);
	}
	if (mv41p->alpha_recs &&
	    mv41p->alpha_recs != Mv41cp->alpha_recs) {
		NhlFree(mv41p->alpha_recs);
	}
	if (mv41p->long_alpha_recs &&
	    mv41p->long_alpha_recs != Mv41cp->long_alpha_recs) {
		NhlFree(mv41p->long_alpha_recs);
	}

	if (mv41p->data_set_name) NhlFree(mv41p->data_set_name);


        return NhlNOERROR;
}

/*
 * Routines called by MapV41DHUpdateDrawList to set up the list of areas
 * to be drawn -- there are two lists, one for fill and masking, the other
 * for outlines.
 */

static int fill_sort
(
        const void *p1,
        const void *p2
)
{
        v41SpecFillRec frec1 = *(v41SpecFillRec *) p1;
        v41SpecFillRec frec2 = *(v41SpecFillRec *) p2;
        int lev1,lev2;
        int pix;
/*
 * The draw id records are set up based on the specified fill records -
 * Later entries in the list modify the list last, and therefore override
 * earlier entries. So higher priority items are moved to the end of the
 * list.
 */

/*
 * sort first by level
 */
        lev1 = c_mpiaty(frec1.eid);
        lev2 = c_mpiaty(frec2.eid);
        if (lev1 != lev2)
                return (lev1 - lev2);
/*
 * next by parent child relationships within the same level
 */
        pix = frec1.eid;
        while (pix) {
                pix = c_mpipar(pix);
                if (pix == frec2.eid)
                        return 1;
        }
        pix = frec2.eid;
        while (pix) {
                pix = c_mpipar(pix);
                if (pix == frec1.eid)
                        return -1;
        }
/*
 * next sort by draw mode, masking follows fill, causing it to override
 */
        if (frec1.draw_mode != frec2.draw_mode)
                return frec1.draw_mode - frec2.draw_mode;

/*
 * Finally sort by entity id (not really necessary)
 */
        return (frec1.eid - frec2.eid);
}

/*
 * Function:  ExpandSpecFillRecord
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
static NhlErrorTypes    ExpandSpecFillRecord
#if	NhlNeedProto
(
        NhlMapV41DataHandlerLayerPart  *mv41p,
	v41EntityRec                   *srec,
	int                            level,
	int                            spec_fill_index,
        unsigned char   	       draw_mode,
        NhlString		       entry_name
)
#else
(mv41p,erec,level,spec_fill_index,draw_mode,entry_name)
	NhlMapV41DataHandlerLayerPart  *mv41p;
	v41EntityRec                   *srec;
	int                            level;
	int                            spec_fill_index;
        unsigned char   	       draw_mode;
        NhlString		       entry_name;
#endif
{

        char *e_text;
	int eid = srec->eid;
	int i;

        for (i = 0; i < mv41p->entity_rec_count; i++) {
                int count = mv41p->fill_rec_count;
		v41EntityRec *erec = mv41p->alpha_recs[i];

		if (! c_mpipai(erec->eid,eid))
			continue;
		if (c_mpiaty(erec->eid) != level)
			continue;

		if (count == mv41p->fill_rec_alloc) {
                        mv41p->fill_rec_alloc *= 2;
                        mv41p->fill_recs = NhlRealloc
                                (mv41p->fill_recs,
				 sizeof(v41SpecFillRec) *
				 mv41p->fill_rec_alloc);
                        if (! mv41p->fill_recs) {
                                e_text = "%s: dynamic memory allocation error";
                                NhlPError(NhlFATAL,ENOMEM,e_text,entry_name);
                                return NhlFATAL;
                        }
                }
		mv41p->fill_recs[count].eid = erec->eid;
		mv41p->fill_recs[count].spec_ix = spec_fill_index;
		mv41p->fill_recs[count].draw_mode = draw_mode;
		mv41p->fill_recs[count].spec_col = 0;
		mv41p->fill_recs[count].spec_pat = 0;
		mv41p->fill_recs[count].spec_fscale = 0;
		mv41p->fill_recs[count].level = level;
		mv41p->fill_rec_count++;
	}
	return NhlNOERROR;
}


/*
 * Function:  UpdateSpecFillRecords
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
static NhlErrorTypes    UpdateSpecFillRecords
#if	NhlNeedProto
(
        NhlMapV41DataHandlerLayer	mv41l,
	NhlMapPlotLayerPart		*mpp,
        unsigned char			draw_mode,
        NhlString			spec_string,
        int				spec_fill_index,
        NhlString			entry_name)
#else
(mv41l,mpp,draw_mode,spec_string,spec_fill_index,entry_name)
	NhlMapV41DataHandlerLayer 	mv41l;
	NhlMapPlotLayerPart		*mpp;
        unsigned char			draw_mode;
        NhlString			spec_string;
        int				spec_fill_index;
        NhlString			entry_name;
#endif
{
        NhlMapDataHandlerLayerPart *mdhp = &mv41l->mapdh;
        NhlMapV41DataHandlerLayerPart *mv41p = &mv41l->mapv41dh;
	NhlErrorTypes		ret = NhlNOERROR;
        char *comp_string, *parent_string = NULL;
        char *e_text;
	NhlString *area_names = NULL;
        int i;
	NhlBoolean found = False;
	char spec_fill_level = -1;

        if (!strcmp(mpLowerCase(spec_string),NhlmpNULLAREA))
                return ret;
        
	if (mdhp->area_names) {
                area_names = (NhlString *) mdhp->area_names->data;
	}
        comp_string = UpNameHierarchy(spec_string);
/*
 * first see if the string is represents one of the broad subcategories
 */
	for (i = 0; i < NhlNumber(SubCatRecs); i++) {
		if (strcmp(comp_string,SubCatRecs[i].name))
			continue;
		 parent_string = UpNameHierarchy(spec_string);
		 if (parent_string == comp_string) {
			 e_text = "%s: invalid boundary specification string: \"%s\"";
			 NhlPError(NhlWARNING,
				   NhlEUNKNOWN,e_text,entry_name,spec_string);
			 return NhlWARNING;
		 }
		 spec_fill_level = SubCatRecs[i].level;
		 comp_string = parent_string;
		 parent_string = NULL;
		 break;
	}
		
        for (i = 0; i < mv41p->entity_rec_count; i++) {
                int count = mv41p->fill_rec_count;
		v41EntityRec *erec;
                NhlBoolean unique;

/*
 * If mpAreaNames resource has been set then it is necessary to compare
 * the spec list with these names rather than the default list. Since 
 * mpAreaNames is required to be in the canonical order, as defined by the
 * class variable "long_alpha_recs", the entity record is found by 
 * indexing into it. Otherwise just look at the regular sorted alpha_recs list.
 */
		if (area_names) {
			if (strcmp(comp_string,
                                   PrepareSpecString(area_names[i])))
				continue;
			erec = mv41p->long_alpha_recs[i];
		}
		else {
			  if (strcmp(comp_string,mv41p->alpha_recs[i]->name))
			  	continue;
			  erec = mv41p->alpha_recs[i];
		}
		if (spec_fill_level > 0 && spec_fill_level <= erec->level) {
			/* at least for now the subcategories have to be at
			 * higher level than the containing entity
			 * e.g.: this eliminates iowa county in wisconsin when
			 * you specify "iowa:counties"
			 */
			continue;
		}
		found = True;
                unique = erec->unique;
                if (! unique) {
                            /* see if a parent has been specified to
                             uniquely qualify the name */
                        
                        if (! parent_string)
                                parent_string = UpNameHierarchy(spec_string);
                        if (parent_string != comp_string) {
                                int peid;
                                peid = c_mpipar(erec->eid);
				if (area_names) {
					int ix = 
			 	      mv41p->entity_recs[peid-1].canonical_ix;
					if (strcmp(parent_string,
					    PrepareSpecString(area_names[ix])))
						continue;
				}
				else {
					if (strcmp(parent_string,
					     mv41p->entity_recs[peid-1].name))
                                        	continue;
				}
                                unique = True;
                        }
                }

                if (count == mv41p->fill_rec_alloc) {
			if (count == 0)
				mv41p->fill_rec_alloc = v41ALLOC_UNIT;
			else 
				mv41p->fill_rec_alloc *= 2;
                        mv41p->fill_recs = NhlRealloc
                                (mv41p->fill_recs,
				 sizeof(v41SpecFillRec) *
				 mv41p->fill_rec_alloc);
                        if (! mv41p->fill_recs) {
                                e_text = "%s: dynamic memory allocation error";
                                NhlPError(NhlFATAL,ENOMEM,e_text,entry_name);
                                return NhlFATAL;
                        }
                }
		if (spec_fill_level > 0) {
			/* what we need are all the children of this entity */
			ExpandSpecFillRecord(mv41p,erec,spec_fill_level,
					     spec_fill_index,draw_mode,entry_name);
		}
		else {
			mv41p->fill_recs[count].eid = erec->eid;
			mv41p->fill_recs[count].spec_ix = spec_fill_index;
			mv41p->fill_recs[count].draw_mode = draw_mode;
			mv41p->fill_recs[count].spec_col = 0;
			mv41p->fill_recs[count].spec_pat = 0;
			mv41p->fill_recs[count].spec_fscale = 0;
			mv41p->fill_recs[count].level = erec->level;
			mv41p->fill_rec_count++;
		}		
                if (unique)
                        break;
                    /* Otherwise add all matching items */
        }
	if (! found) {
		e_text = "%s: invalid boundary specification string: \"%s\"";
		NhlPError(NhlWARNING,
			  NhlEUNKNOWN,e_text,entry_name,spec_string);
		return NhlWARNING;
	}
        return ret;
        
}

/*
 * Function:  mv41BuildFillDrawList
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
static NhlErrorTypes    mv41BuildFillDrawList
#if	NhlNeedProto
(
        NhlMapV41DataHandlerLayer	mv41l,
	NhlMapPlotLayerPart		*mpp,
        NhlString			entry_name)
#else
(mv41l,mpp,entry_name)
	NhlMapV41DataHandlerLayer 	mv41l;
	NhlMapPlotLayerPart		*mpp;
        NhlString			entry_name;
#endif
{
        NhlMapDataHandlerLayerPart *mdhp = &mv41l->mapdh;
        NhlMapV41DataHandlerLayerPart *mv41p = &mv41l->mapv41dh;
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	int			i;
	NhlString		*sp;
        char			spec_string[512];
        
        mv41p->fill_rec_count = 0;
        mv41p->min_fill_level = 0;
/*
 * Note that the spec_string must have its own space; not just the static
 * string supplied by PrepareSpecString, since it will be called again
 * for the comparisions.
 */
	if (mpp->fill_area_specs != NULL) {
		sp = (NhlString *) mpp->fill_area_specs->data;
		for (i = 0; i < mpp->fill_area_specs->num_elements; i++) {
                        strcpy(spec_string,PrepareSpecString(sp[i]));
                        subret = UpdateSpecFillRecords
                                (mv41l,mpp,mpDRAW,spec_string,i,entry_name);
			if ((ret = MIN(ret,subret)) < NhlWARNING)
				return ret;

		}
	}
	if (mpp->area_masking_on && mpp->mask_area_specs != NULL) {
		sp = (NhlString *) mpp->mask_area_specs->data;
		for (i = 0; i < mpp->mask_area_specs->num_elements; i++) {
			strcpy(spec_string,PrepareSpecString(sp[i]));
                        subret = UpdateSpecFillRecords
                                (mv41l,mpp,mpMASK,spec_string,i,entry_name);
			if ((ret = MIN(ret,subret)) < NhlWARNING)
				return ret;
		}
	}
        if (mv41p->fill_rec_count) {
                qsort(mv41p->fill_recs,mv41p->fill_rec_count,
                      sizeof(v41SpecFillRec),fill_sort);
        
                mv41p->min_fill_level =
                        MAX(c_mpiaty
                            (mv41p->fill_recs[mv41p->fill_rec_count-1].eid),
                            mv41p->min_fill_level);
		for (i = 0; i < mv41p->fill_rec_count; i++) {
			mv41p->min_fill_level = 
				MAX(mv41p->min_fill_level,
				    mv41p->fill_recs[i].level);
		}
        }

	if (mdhp->dynamic_groups) {
		for (i = 0; i < mv41p->entity_rec_count; i++) {
			v41EntityRec *erec = &mv41p->entity_recs[i];
			int ix = i + 1;
			if (ix == LandId) {
				erec->dynamic_gid = erec->fixed_gid =
					NhlmpLANDGROUPINDEX;
			}
			else if (NGCALLF(mpipai,MPIPAI)(&ix,&LandId)) {
				erec->fixed_gid = NhlmpLANDGROUPINDEX;
				erec->dynamic_gid = ((int*)mdhp->dynamic_groups->data)[erec->canonical_ix];
			}
			else if (ix == OceanId || ix == WaterId)
				erec->dynamic_gid = erec->fixed_gid
					= NhlmpOCEANGROUPINDEX;
			else 
				erec->dynamic_gid = erec->fixed_gid
					= NhlmpINLANDWATERGROUPINDEX;
		}
	}
	else {
		for (i = 0; i < mv41p->entity_rec_count; i++) {
			v41EntityRec *erec = &mv41p->entity_recs[i];
			int ix = i + 1;
			if (ix == LandId) {
				erec->dynamic_gid = erec->fixed_gid =
					NhlmpLANDGROUPINDEX;
			}
			else if (NGCALLF(mpipai,MPIPAI)(&ix,&LandId)) {
				erec->fixed_gid = NhlmpLANDGROUPINDEX;
				erec->dynamic_gid = NGCALLF(mpisci,MPISCI)(&ix) + 2;
			}
			else if (ix == OceanId || ix == WaterId)
				erec->dynamic_gid = erec->fixed_gid
					= NhlmpOCEANGROUPINDEX;
			else 
				erec->dynamic_gid = erec->fixed_gid
					= NhlmpINLANDWATERGROUPINDEX;
		}
	}
        
        return ret;
        
}

static int outline_sort
(
        const void *p1,
        const void *p2
)
{
        v41SpecLineRec lrec1 = *(v41SpecLineRec *) p1;
        v41SpecLineRec lrec2 = *(v41SpecLineRec *) p2;
        int lev1,lev2;
        int pix;
/*
 * The draw id records are set up based on the specified outline records -
 * Later entries in the list modify the list last, and therefore override
 * earlier entries. So higher priority items are moved to the end of the
 * list.
 */
/*
 * sort first by level
 */
        lev1 = c_mpiaty(lrec1.eid);
        lev2 = c_mpiaty(lrec2.eid);
        if (lev1 != lev2)
                return (lev1 - lev2);
/*
 * next by parent child relationships within the same level
 */
        pix = lrec1.eid;
        while (pix) {
                pix = c_mpipar(pix);
                if (pix == lrec2.eid)
                        return 1;
        }
        pix = lrec2.eid;
        while (pix) {
                pix = c_mpipar(pix);
                if (pix == lrec1.eid)
                        return -1;
        }

/*
 * next sort by draw mode, masking follows outline, causing it to override
 */
        if (lrec1.draw_mode != lrec2.draw_mode)
                return lrec1.draw_mode - lrec2.draw_mode;

/*
 * Finally sort by entity id (not really necessary)
 */
        return (lrec1.eid - lrec2.eid);
}
 
/*
 * Function:  UpdateSpecLineRecords
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
static NhlErrorTypes    UpdateSpecLineRecords
#if	NhlNeedProto
(
        NhlMapV41DataHandlerLayer	mv41l,
	NhlMapPlotLayerPart		*mpp,
        unsigned char			draw_mode,
        NhlString			spec_string,
        int				spec_line_index,
        NhlString			entry_name)
#else
(mv41l,mpp,draw_mode,spec_string,spec_line_index,entry_name)
	NhlMapV41DataHandlerLayer 	mv41l;
	NhlMapPlotLayerPart		*mpp;
        unsigned char			draw_mode;
        NhlString			spec_string;
        int				spec_line_index;
        NhlString			entry_name;
#endif
{
        NhlMapV41DataHandlerLayerPart *mv41p = &mv41l->mapv41dh;
	NhlErrorTypes		ret = NhlNOERROR;
        char *comp_string, *parent_string = NULL;
	NhlString *area_names = NULL;
        char *e_text;
        int i;
	NhlBoolean found = False;
	char spec_line_level = -1;
	
        if (!strcmp(mpLowerCase(spec_string),NhlmpNULLAREA))
                return ret;
        
        comp_string = UpNameHierarchy(spec_string);
/*
 * first see if the string is represents one of the broad subcategories
 */
	for (i = 0; i < NhlNumber(SubCatRecs); i++) {
		if (strcmp(comp_string,SubCatRecs[i].name))
			continue;
		 parent_string = UpNameHierarchy(spec_string);
		 if (parent_string == comp_string) {
			 e_text = "%s: invalid boundary specification string: \"%s\"";
			 NhlPError(NhlWARNING,
				   NhlEUNKNOWN,e_text,entry_name,spec_string);
			 return NhlWARNING;
		 }
		 spec_line_level = SubCatRecs[i].level;
		 comp_string = parent_string;
		 parent_string = NULL;
		 break;
	}
		
        for (i = 0; i < mv41p->entity_rec_count; i++) {
                int count = mv41p->outline_rec_count;
		v41EntityRec *erec;
                NhlBoolean unique;
                
/*
 * If mpAreaNames resource has been set then it is necessary to compare
 * the spec list with these names rather than the default list. Since 
 * mpAreaNames is required to be in the canonical order, as defined by the
 * class variable "long_alpha_recs", the entity record is found by 
 * indexing into it. Otherwise just look at the regular sorted alpha_recs list.
 */
		if (area_names) {
			if (strcmp(comp_string,
                                   PrepareSpecString(area_names[i])))
				continue;
			erec = mv41p->long_alpha_recs[i];
		}
		else {
			  if (strcmp(comp_string,mv41p->alpha_recs[i]->name))
			  	continue;
			  erec = mv41p->alpha_recs[i];
		}
		if (spec_line_level > 0 && spec_line_level <= erec->level) {
			/* at least for now the subcategories have to be at
			 * higher level than the containing entity
			 * e.g.: this eliminates iowa county in wisconsin when
			 * you specify "iowa:counties"
			 */
			continue;
		}
		found = True;
                unique = erec->unique;
                if (! unique) {
                            /* see if a parent has been specified to
                             uniquely qualify the name */
                        
                        if (! parent_string)
                                parent_string = UpNameHierarchy(spec_string);
                        if (parent_string != comp_string) {
                                int peid;
                                peid = c_mpipar(erec->eid);
				if (area_names) {
					int ix = 
			 	      mv41p->entity_recs[peid-1].canonical_ix;
					if (strcmp(parent_string,
                                            PrepareSpecString(area_names[ix])))
						continue;
				}
				else {
					if (strcmp(parent_string,
					     mv41p->entity_recs[peid-1].name))
                                        	continue;
				}
                                unique = True;
                        }
                }
                
                if (count == mv41p->outline_rec_alloc) {
                        mv41p->outline_rec_alloc += v41ALLOC_UNIT;
                        mv41p->outline_recs = NhlRealloc
                                (mv41p->outline_recs, 
				 sizeof(v41SpecLineRec) *
				 mv41p->outline_rec_alloc);
                        if (! mv41p->outline_recs) {
                                e_text = "%s: dynamic memory allocation error";
                                NhlPError(NhlFATAL,ENOMEM,e_text,entry_name);
                                return NhlFATAL;
                        }
                }
                mv41p->outline_recs[count].eid = erec->eid;
                mv41p->outline_recs[count].draw_mode = draw_mode;
                mv41p->outline_recs[count].level = spec_line_level;
                mv41p->outline_recs[count].spec_ix = 0;
                mv41p->outline_recs[count].spec_col = 0;
                mv41p->outline_recs[count].spec_dpat = 0;
                mv41p->outline_recs[count].spec_thickness = 0;
                mv41p->outline_rec_count++;
                
                if (unique)
                        break;
                    /* Otherwise add all matching items */
        }
	if (! found) {
		e_text = "%s: invalid boundary specification string: \"%s\"";
		NhlPError(NhlWARNING,
			  NhlEUNKNOWN,e_text,entry_name,spec_string);
		return NhlWARNING;
	}
        return ret;
        
}

/*
 * Function:  mv41BuildOutlineDrawList
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
static NhlErrorTypes    mv41BuildOutlineDrawList
#if	NhlNeedProto
(
        NhlMapV41DataHandlerLayer	mv41l,
	NhlMapPlotLayerPart		*mpp,
        NhlString			entry_name)
#else
(mv41l,mpp,entry_name)
	NhlMapV41DataHandlerLayer 	mv41l;
	NhlMapPlotLayerPart		*mpp;
        NhlString			entry_name;
#endif
{
        NhlMapV41DataHandlerLayerPart *mv41p = &mv41l->mapv41dh;
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	int			i;
	NhlString		*sp;
        char spec_string[512];
        
        mv41p->outline_rec_count = 0;
        mv41p->min_outline_level = 0;
        
	if (mpp->outline_specs != NULL) {
		sp = (NhlString *) mpp->outline_specs->data;
		for (i = 0; i < mpp->outline_specs->num_elements; i++) {
                        strcpy(spec_string,PrepareSpecString(sp[i]));
                        subret = UpdateSpecLineRecords
                                (mv41l,mpp,mpDRAW,spec_string,i,entry_name);
			if ((ret = MIN(ret,subret)) < NhlWARNING)
				return ret;

		}
	}
	if (mpp->outline_masking_on && mpp->mask_outline_specs != NULL) {
		sp = (NhlString *) mpp->mask_outline_specs->data;
		for (i = 0; i < mpp->mask_outline_specs->num_elements; i++) {
			strcpy(spec_string,PrepareSpecString(sp[i]));
                        subret = UpdateSpecLineRecords
                                (mv41l,mpp,mpMASK,spec_string,i,entry_name);
			if ((ret = MIN(ret,subret)) < NhlWARNING)
				return ret;
		}
	}
        if (mv41p->outline_rec_count) {
                qsort(mv41p->outline_recs,mv41p->outline_rec_count,
                      sizeof(v41SpecLineRec),outline_sort);
        
                mv41p->min_outline_level =
                        MAX(c_mpiaty
                         (mv41p->outline_recs[mv41p->outline_rec_count-1].eid),
                            mv41p->min_outline_level);
		for (i = 0; i < mv41p->outline_rec_count; i++) {
			mv41p->min_outline_level = 
				MAX(mv41p->min_outline_level,
				    mv41p->outline_recs[i].level);
		}
        }
        
        return ret;
        
}

static NhlErrorTypes MapV41DHUpdateDrawList
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
        NhlMapV41DataHandlerLayer mv41l = 
		(NhlMapV41DataHandlerLayer) instance;
        NhlMapV41DataHandlerLayerPart *mv41p = &mv41l->mapv41dh;
	NhlMapPlotLayerPart	*mpp = &(newmp->mapplot);
	NhlString entry_name = "MapV41DHUpdateDrawList";
        NhlErrorTypes ret = NhlNOERROR,subret = NhlNOERROR;
        NhlBoolean build_fill_list = False, build_outline_list = False;
        
        
        if (init) {
               mv41p->new_amap_req = True;
               build_fill_list = True;
               build_outline_list = True;
        }
	else {
                NhlMapPlotLayerPart	*ompp = &(oldmp->mapplot);
                if (mpp->database_version != ompp->database_version ||
                    mpp->fill_boundaries != ompp->fill_boundaries ||
                    mpp->fill_area_specs != ompp->fill_area_specs ||
                    mpp->mask_area_specs != ompp->mask_area_specs ||
                    mpp->area_names != ompp->area_names ||
                    mpp->area_masking_on != ompp->area_masking_on ||
		    mpp->data_set_name != ompp->data_set_name ||
		    mpp->dynamic_groups != ompp->dynamic_groups ||
		    mpp->area_group_count != ompp->area_group_count)
                        build_fill_list = True;
                
                if (mpp->database_version != ompp->database_version ||
                    mpp->outline_boundaries != ompp->outline_boundaries ||
		    mpp->mask_outline_specs != ompp->mask_outline_specs ||
                    mpp->outline_masking_on != ompp->outline_masking_on ||
                    mpp->outline_specs != ompp->outline_specs ||
		    mpp->data_set_name != ompp->data_set_name)
                        build_outline_list = True;
                
                if (build_fill_list || mpp->view_changed ||
                    mpp->trans_change_count != ompp->trans_change_count)
                        mv41p->new_amap_req = True;
        }
        if (build_fill_list) {
                subret = mv41BuildFillDrawList(mv41l,mpp,entry_name);
                if ((ret = MIN(ret,subret)) < NhlWARNING)
                        return ret;
        }
        if (build_outline_list) {
                subret = mv41BuildOutlineDrawList(mv41l,mpp,entry_name);
                if ((ret = MIN(ret,subret)) < NhlWARNING)
                        return ret;
        }
        
                
        return ret;
}


/*
 * Routines called by MapV41DHDrawMapList to perform map fill, outline,
 * and grid using the Version 4.0 database.
 */

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
	int pat_ix, col_ix;
	float fscale;
        int i,gid,geo_ix = -1 ,vs_ix = -1;

	if (Mpp == NULL) return 0;

        for (i = 0; i < *nai; i++) {
                if (iag[i] == 1) geo_ix = iai[i];
                if (iag[i] == 2) vs_ix = iai[i];
        }
        if (geo_ix > 0 && vs_ix > -1) {
                int ix,level = Level;
                int spec_col_ix = -1,spec_pat_ix = -1, spec_fscale_ix = -1;
                
		if (Mpp->fill_boundaries == NhlGEOPHYSICALANDUSSTATES ||
		    Mpp->fill_boundaries == NhlUSSTATES) {
                        NhlBoolean found = False;
                        for (i = UsIdCount-1;i>-1;i--) {
                                if (NGCALLF(mpipai,MPIPAI)(&geo_ix,&UsIds[i])){
                                        found = True;
                                        break;
                                }
                        }
                        if (! found)
				level = Mpp->fill_boundaries == NhlUSSTATES ?
					0 : 2;
                }
                if (DrawIds[geo_ix-1].spec_rec) {
                        v41SpecFillRec *fill_rec =
                                (v41SpecFillRec *)DrawIds[geo_ix-1].spec_rec;
                        if (fill_rec->draw_mode > mpDRAW)
                                return 0;
                        level = c_mpiaty(fill_rec->eid);
                        if (fill_rec->spec_col)
                                spec_col_ix = fill_rec->spec_ix;
                        if (fill_rec->spec_pat)
                                spec_pat_ix = fill_rec->spec_ix;
                        if (fill_rec->spec_fscale)
                                spec_fscale_ix = fill_rec->spec_ix;
                }
                
		/* determine the group id */

		if (level == 0)
			gid = 0;
		else {
                	if (level == 2 &&
                            ! DrawIds[geo_ix-1].spec_rec &&
			    NGCALLF(mpipai,MPIPAI)(&geo_ix,&LandId))
				level = 1;
			ix = NGCALLF(mpiosa,MPIOSA)(&geo_ix,&level);
			if (level < 2)
				gid = Mv41p->entity_recs[ix-1].fixed_gid;
                        else
				gid = Mv41p->entity_recs[ix-1].dynamic_gid;
		}

                if (spec_col_ix < 0) {
                        if (Mpp->mono_fill_color) {
                                col_ix = Mpp->fill_color;
                        }
                        else {
                                int *fcp = (int*)Mpp->fill_colors->data;
                                col_ix = fcp[gid];
                        }
                }
                else {
                        int *sfcp = (int *) Mpp->spec_fill_colors->data;
                        if (Mpp->spec_fill_direct) {
                                col_ix = sfcp[spec_col_ix];
                        }
                        else if (sfcp[spec_col_ix] < Mpp->area_group_count) {
                                int *fcp = (int*)Mpp->fill_colors->data;
                                col_ix = fcp[sfcp[spec_col_ix]];
                        }
                }
                if (spec_pat_ix < 0) {
                        if (Mpp->mono_fill_pattern) {
                                pat_ix = Mpp->fill_pattern;
                        }
                        else {
                                int *fpp = (int*)Mpp->fill_patterns->data;
                                pat_ix = fpp[gid];
                        }
                }
                else {
                        int *sfpp = (int *) Mpp->spec_fill_patterns->data;
                        if (Mpp->spec_fill_direct) {
                                pat_ix = sfpp[spec_pat_ix];
                        }
                        else if (sfpp[spec_pat_ix] < Mpp->area_group_count) {
                                int *fpp = (int*)Mpp->fill_patterns->data;
                                pat_ix = fpp[sfpp[spec_pat_ix]];
                        }
                }
                if (spec_fscale_ix < 0) {
                        if (Mpp->mono_fill_scale) {
                                fscale = Mpp->fill_scale;
                        }
                        else {
                                int *fsp = (int*)Mpp->fill_scales->data;
                                fscale = fsp[gid];
                        }
                }
                else {
                        float *sfsp = (float *) Mpp->spec_fill_scales->data;
                        if (Mpp->spec_fill_direct) {
                                fscale = sfsp[spec_fscale_ix];
                        }
                        else if (sfsp[spec_fscale_ix] <Mpp->area_group_count) {
                                float *fsp = (float*)Mpp->fill_scales->data;
                                fscale = fsp[(int)sfsp[spec_fscale_ix]];
                        }
                }
                
                NhlVASetValues
                        (Mpl->base.wkptr->base.id,
                         _NhlNwkFillBackground, Mpp->fill_pattern_background,
                         _NhlNwkFillIndex, pat_ix,
                         _NhlNwkFillColor, col_ix,
                         _NhlNwkFillScaleFactorF,fscale,
			 _NhlNwkFillDotSizeF, Mpp->fill_dot_size,
                         _NhlNwkEdgesOn,0,
                         NULL);
                _NhlSetFillInfo(Mpl->base.wkptr, (NhlLayer) Mpl);
                _NhlWorkstationFill(Mpl->base.wkptr,xcs,ycs,*ncs);
                return 0;
        }
                
	return 0;
}

/*
 * Function:	mpSetUpFillDrawList
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

static NhlErrorTypes mpSetUpFillDrawList
#if	NhlNeedProto
(
        NhlMapV41DataHandlerLayer 	mv41l,
	NhlMapPlotLayer			mpl,
	NhlString			entry_name
)
#else
(mv411,mpl,entry_name)
	NhlMapV41DataHandlerLayer 	mv41l;
        NhlMapPlotLayer 		mpl;
	NhlString			entry_name;
#endif
{
        NhlMapV41DataHandlerLayerPart *mv41p = &mv41l->mapv41dh;
	NhlErrorTypes		ret = NhlNOERROR;
	NhlMapPlotLayerPart	*mpp = &(mpl->mapplot);
	int	i;

        switch (mpp->fill_boundaries) {
            default:
	    case NhlNOBOUNDARIES:
	    	    Level = 0;
		    break;
            case NhlGEOPHYSICAL:
                    Level = 2;
                    break;
            case NhlNATIONAL:
                    Level = 3;
                    break;
            case NhlUSSTATES:
            case NhlGEOPHYSICALANDUSSTATES:
                    Level = 4;
                    break;
            case NhlALLBOUNDARIES:
                    Level = 5;
                    break;
        }
        mv41p->min_fill_level = MAX(Level,mv41p->min_fill_level);
        
        memset(DrawIds,0,mv41p->entity_rec_count * sizeof(mpDrawIdRec));

        for (i = 0; i < mv41p->fill_rec_count; i++) {
                int eid = mv41p->fill_recs[i].eid;
                int spec_fill_index = mv41p->fill_recs[i].spec_ix;
                int j;
                
                mv41p->fill_recs[i].spec_col = 0;
                if (mpp->spec_fill_color_count > spec_fill_index) {
			int *ip = (int *) mpp->spec_fill_colors->data;
			if (ip[spec_fill_index] != NhlUNSPECIFIEDCOLOR)
				mv41p->fill_recs[i].spec_col =  1;
		}
                mv41p->fill_recs[i].spec_pat = 0;
		if (mpp->spec_fill_pattern_count > spec_fill_index) {
			int *ip = (int *) mpp->spec_fill_patterns->data;
			if (ip[spec_fill_index] != NhlUNSPECIFIEDFILL)
				mv41p->fill_recs[i].spec_pat =  1;
		}
                mv41p->fill_recs[i].spec_fscale = 0;
		if (mpp->spec_fill_scale_count > spec_fill_index) {
			float *fp = (float *) mpp->spec_fill_scales->data;
			if (fp[spec_fill_index] > NhlmpUNSETFILLSCALE)
				mv41p->fill_recs[i].spec_fscale =  1;
		}

                for (j = 1; j <= mv41p->entity_rec_count; j++) {
#if 0
                        if (c_mpiosa(j,spec_level) == eid) {
                                DrawIds[j-1].spec_rec =   
                                        (void *) &mv41p->fill_recs[i];
			}
#endif
                        if (c_mpipai(j,eid)) {
                                DrawIds[j-1].spec_rec =   
                                        (void *) &mv41p->fill_recs[i];
                        }
                }
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
        NhlMapV41DataHandlerLayer 	mv41l,
	NhlMapPlotLayer			mpl,
	NhlWorkspace			**aws,
	NhlString			entry_name
)
#else
(mv411,mpl,aws,entry_name)
	NhlMapV41DataHandlerLayer 	mv41l;
        NhlMapPlotLayer 		mpl;
	NhlWorkspace			**aws;
	NhlString			entry_name;
#endif
{
        NhlMapV41DataHandlerLayerPart *mv41p = &mv41l->mapv41dh;
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	NhlMapPlotLayerPart	*mpp = &(mpl->mapplot);
	int			aws_id = -1;
	int			last_fill_level;
	int			req_size;

	last_fill_level = mv41p->min_fill_level;

	mpSetUpFillDrawList(mv41l,mpl,entry_name);

	if (! mv41p->data_set_point_count || 
	    last_fill_level != mv41p->min_fill_level) {
		mv41p->data_set_point_count = 0;
		Count_Points_Only = True;
		c_mplndr(mv41p->data_set_name,mv41p->min_fill_level);
		Count_Points_Only = False;
	}

	c_arseti("RC",1);
	aws_id = mv41p->aws_id;

	/* 
	 * this is a very ad hoc way of trying to minimize the workspace
	 * used, and at the same time keeping workspace induced resizes
	 * from taking place, because they are very costly. Presumably
	 * datasets added by the user are not likely to be bigger than
	 * the default data set, which usually requires no more than
	 * mpWORKSPACE_SIZE_REQ bytes in the workspace.
	 */
	req_size = MAX(mpWORKSPACE_SIZE_REQ,
		       (int) 4 * 16 * mv41p->data_set_point_count);
#if 0
	printf("point count %d req size %d\n",
	       mv41p->data_set_point_count,req_size);
#endif
	if (aws_id < 1) {
		aws_id = _NhlNewWorkspace(NhlwsAREAMAP,NhlwsNONE,req_size);
		if (aws_id < 1) 
			return MIN(ret,(NhlErrorTypes)aws_id);
	}
	if ((*aws = _NhlUseWorkspace(aws_id)) == NULL) {
		e_text = "%s: error reserving area map workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}
	
	if (! _NhlWorkspaceDataIntact(aws_id) || mv41p->new_amap_req ) {
                
		c_mpseti("VS",1);
		c_mpseti("G2",2);
		c_mpseti("G1",1);
		_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
		(*aws)->req_size = req_size;
		subret = _NhlArinam(*aws,entry_name);
		if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
                subret = _NhlMplnam
                        (*aws,mv41p->data_set_name,
			 mv41p->min_fill_level,entry_name);
                if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
                if (mpp->dump_area_map)
                        _NhlDumpAreaMap(*aws,entry_name);
        }
        
	mv41p->aws_id = aws_id;
        mv41p->new_amap_req = False;
        
	return ret;
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
        NhlMapV41DataHandlerLayer 	mv41l,
	NhlMapPlotLayer			mpl,
	NhlString			entry_name
)
#else
(mv41l,mpl,entry_name)
        NhlMapV41DataHandlerLayer 	mv41l;
        NhlMapPlotLayer 		mpl;
	NhlString			entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
        NhlWorkspace		*aws = NULL;

        subret = mpSetUpAreamap(mv41l,mpl,&aws,entry_name);
	if ((ret = MIN(subret,ret)) < NhlWARNING) goto error_ret;

        subret = _NhlArscam(aws,(_NHLCALLF(hlumapfill,HLUMAPFILL)),
                            entry_name);
	if ((ret = MIN(subret,ret)) < NhlWARNING) goto error_ret;

 error_ret:

        subret = _NhlIdleWorkspace(aws);
        ret = MIN(subret,ret);

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
	int i;
        int geo_ix = -1 ,vs_ix = -1;
        
	if (Mpp == NULL) return 0;

        for (i = 0; i < *nai; i++) {
                if (iag[i] == 1) geo_ix = iai[i];
                if (iag[i] == 2) vs_ix = iai[i];
        }
        if (geo_ix < 1 || vs_ix < 0)
                return 0;
#if 0
        printf("grid mask %d\n", geo_ix);
#endif        
        switch (Mpp->grid_mask_mode) {
            case NhlMASKNONE:
            default:
                    draw_line = True;
                    break;
            case NhlMASKOCEAN:
                    if (geo_ix != OceanId)
                            draw_line = True;
                    break;
            case NhlMASKNOTOCEAN:
                    if (geo_ix == OceanId)
                            draw_line = True;
                    break;
            case NhlMASKLAND:
                    if (! NGCALLF(mpipai,MPIPAI)(&geo_ix,&LandId))
                            draw_line = True;
                    break;
            case NhlMASKNOTLAND:
                    if (NGCALLF(mpipai,MPIPAI)(&geo_ix,&LandId))
                            draw_line = True;
                    break;
            case NhlMASKFILLAREA:
                    if (Mpp->fill_boundaries == NhlNOBOUNDARIES) {
                            if (! DrawIds[geo_ix-1].spec_rec)
                                    draw_line = True;
                    }
                    else if (Mpp->fill_boundaries == NhlUSSTATES) {
                            NhlBoolean found = False;
                            for (i = UsIdCount-1;i>-1;i--) {
                                    if (NGCALLF(mpipai,MPIPAI)
                                        (&geo_ix,&UsIds[i])) {
                                            found = True;
                                            break;
                                    }
                            }
                            if (!(found || DrawIds[geo_ix-1].spec_rec))
                                    draw_line = True;
                    }
                    break;
            case NhlMASKMASKAREA:
                    if (! DrawIds[geo_ix-1].spec_rec) {
                            draw_line = True;
                    }
                    else {
                            v41SpecFillRec *frec = (v41SpecFillRec *)
                                    DrawIds[geo_ix-1].spec_rec;
                            if (frec->draw_mode != mpMASK)
                                    draw_line = True;
                    }
                    break;
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
        NhlMapV41DataHandlerLayer 	mv41l,
	NhlMapPlotLayer			mpl,
	NhlString			entry_name
)
#else
(mv41l,mpl,entry_name)
        NhlMapV41DataHandlerLayer 	mv41l;
        NhlMapPlotLayer 		mpl;
	NhlString			entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlMapPlotLayerPart	*mpp = &(mpl->mapplot);
	NhlWorkspace		*aws;
        float pole_param;

	Grid_Setup = False;
	c_mpseti("C2",mpp->grid.gks_color < 0 ? 0 : mpp->grid.gks_color);
	_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	c_mpseti("C4", mpp->limb.gks_color < 0 ? 0 :  mpp->limb.gks_color);
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
        subret = mpSetUpAreamap(mv41l,mpl,&aws,entry_name);
        if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

        subret = _NhlMapgrm(aws,(_NHLCALLF(hlumaskgrid,HLUMASKGRID)),
                            entry_name);
        if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
	_NHLCALLF(mdplmb,MDPLMB)();
	_NhlLLErrCheckPrnt(NhlWARNING,entry_name);

        subret = _NhlIdleWorkspace(aws);
        if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;
        
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
        NhlMapV41DataHandlerLayer 	mv41l,
	NhlMapPlotLayer			mpl,
	NhlString			entry_name
)
#else
(mv41l,mp,entry_name)
        NhlMapV41DataHandlerLayer 	mv41l;
        NhlMapPlotLayer 		mpl;
	NhlString			entry_name;
#endif
{
        NhlMapV41DataHandlerLayerPart *mv41p = &mv41l->mapv41dh;
	NhlErrorTypes		ret = NhlNOERROR;
	NhlMapPlotLayerPart	*mpp = &(mpl->mapplot);
	int			i;

	c_mpseti("C5",mpp->geophysical.gks_color < 0 ? 0 : mpp->geophysical.gks_color );
	_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	c_mpseti("C6",mpp->us_state.gks_color < 0 ? 0 : mpp->us_state.gks_color);
	_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	c_mpseti("C7",mpp->national.gks_color < 0 ? 0 : mpp->national.gks_color);
	_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	c_mpseti("C8",mpp->county.gks_color < 0 ? 0 : mpp->county.gks_color);
	_NhlLLErrCheckPrnt(NhlWARNING,entry_name);

        Color = -2;
        Dash_Pattern = -2;
        Dash_SegLen = -2.0;
        Thickness = -2.0;
        
        switch (mpp->outline_boundaries) {
            default:
	    case NhlNOBOUNDARIES:
	    	    Level = 0;
	    	    break;
            case NhlGEOPHYSICAL:
                    Level = 2;
                    break;
            case NhlNATIONAL:
                    Level = 3;
                    break;
            case NhlUSSTATES:
            case NhlGEOPHYSICALANDUSSTATES:
                    Level = 4;
                    break;
            case NhlALLBOUNDARIES:
                    Level = 5;
                    break;
        }
        mv41p->min_outline_level = MAX(Level,mv41p->min_outline_level);

        memset(DrawIds,0,mv41p->entity_rec_count * sizeof(mpDrawIdRec));

        for (i = 0; i < mv41p->outline_rec_count; i++) {
                int eid = mv41p->outline_recs[i].eid;
		char eidname[128];
                int j;

		strcpy(eidname,c_mdname(eid));
                for (j = 1; j <= mv41p->entity_rec_count; j++) {
#if 0
                        if (c_mpiosa(j,spec_level) == eid) {
			        printf("%s contains %s at level %d\n",
				       eidname,c_mdname(j),spec_level);
                                DrawIds[j-1].spec_rec =   
                                        (void *) &mv41p->outline_recs[i];
			}
#endif
                        if (c_mpipai(j,eid)) {
#if 0
			        printf("%s: type %d contains %s type %d\n",
				       eidname,spec_level, c_mdname(j),c_mpiaty(j));
#endif
                                DrawIds[j-1].spec_rec =   
                                        (void *) &mv41p->outline_recs[i];
                        }
                }
        }

        c_mplndr(mv41p->data_set_name,mv41p->min_outline_level);
        
	return ret;
}

static NhlErrorTypes MapV41DHDrawMapList
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
        NhlMapV41DataHandlerLayer mv41l = 
		(NhlMapV41DataHandlerLayer) instance;
	NhlMapPlotLayerPart	  *mpp = &mpl->mapplot;
	NhlString entry_name = "MapV41DHDrawMapList";
        NhlErrorTypes ret = NhlNOERROR;
	int i;
        
	Mpp = mpp;
	Mpl = mpl;
	Mv41p = &mv41l->mapv41dh;
	Mdhp = &mv41l->mapdh;

	/*
	 * this is necessary to ensure that Ezmap is using the
	 * correct data set.
	 */
	c_mplnri(Mv41p->data_set_name);

        if (DrawIdCount < Mv41p->entity_rec_count) {
                DrawIds = NhlRealloc
		  (DrawIds,Mv41p->entity_rec_count * sizeof(mpDrawIdRec));
                if (! DrawIds) {
                        NHLPERROR((NhlFATAL,ENOMEM,NULL));
                        return NhlFATAL;
                }
        }
	UsIdCount = Mv41p->basic_ids.us_id_count;
	for (i = 0; i < UsIdCount; i++) {
		UsIds[i] = Mv41p->basic_ids.us_ids[i];
	}
	LandId = Mv41p->basic_ids.land_id;
	WaterId = Mv41p->basic_ids.water_id;
	OceanId = Mv41p->basic_ids.ocean_id;
        
        switch (draw_op) {
	case mpDRAWFILL:
		ret = mpFill(mv41l,mpl,entry_name);
		break;
	case mpDRAWOUTLINE:
		ret = mpOutline(mv41l,mpl,entry_name);
		break;
	case mpDRAWGRID:
		ret =  mpGrid(mv41l,mpl,entry_name);
		break;
	default:
		break;
        }

	Mpp = NULL;
	Mpl = NULL;
	Mv41p = NULL;

        return ret;
}

/*
 * Function:  ModifyFill
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
static void   ModifyFill
#if	NhlNeedProto
(
        int ilty,
        int ioal,
        int ioar,
        int *npts,
	float *pnts
)

#else
(ilty,ioal,ioar,npts,pnts)
        int ilty;
        int ioal;
        int ioar;
        int *npts;
	float *pnts;
#endif
{
	NhlMapBoundarySets boundaries = Mpp->fill_boundaries;
	NhlBoolean found = False;
	int i;
	int do_boundary_check = 0;

        if (DrawIds[ioal-1].spec_rec && DrawIds[ioar-1].spec_rec) {
	  	v41SpecFillRec *lrec = 
		  	(v41SpecFillRec *)DrawIds[ioal-1].spec_rec;
	  	v41SpecFillRec *rrec = 
		  	(v41SpecFillRec *)DrawIds[ioar-1].spec_rec;
		int rtype, ltype;
		rtype = MAX(c_mpiaty(rrec->eid),rrec->level);
		ltype = MAX(c_mpiaty(lrec->eid),lrec->level);

		if (ilty > ltype && ilty > rtype) {
			*npts = 0;
			return;
		}
		return;
        }
        else if (DrawIds[ioal-1].spec_rec) {
		v41SpecFillRec *llspec = 
			(v41SpecFillRec *) DrawIds[ioal-1].spec_rec;
		if (ilty > MAX(c_mpiaty(llspec->eid),llspec->level)) {
			*npts = 0;
			return;
		}
	}
	else if  (DrawIds[ioar-1].spec_rec) {
		v41SpecFillRec *rlspec = (v41SpecFillRec *) DrawIds[ioar-1].spec_rec;
		if (ilty > MAX(c_mpiaty(rlspec->eid),rlspec->level)) {
			*npts = 0;
			return;
		}
	}
	else if (ilty > Level) {
		*npts = 0;
		return;
	}
	else {
		do_boundary_check = 1;
	}

	if (do_boundary_check) {
		switch (boundaries) {
		default:
		case NhlNOBOUNDARIES:
			*npts = 0;
			break;
		case NhlGEOPHYSICAL:
		case NhlNATIONAL:
		case NhlALLBOUNDARIES:
			break;
		case NhlUSSTATES:
			for (i = UsIdCount-1;i>-1;i--) {
				if (NGCALLF(mpipai,MPIPAI)(&ioal,&UsIds[i]) ||
				    NGCALLF(mpipai,MPIPAI)(&ioar,&UsIds[i])) {
					found = True;
					break;
				}
			}
			if (! found) {
				*npts = 0;
			}
			break;
		case NhlGEOPHYSICALANDUSSTATES:
			if (ilty > 1) {
				for (i = UsIdCount-1;i>-1;i--) {
					if (NGCALLF(mpipai,MPIPAI)(&ioal,&UsIds[i]) ||
					    NGCALLF(mpipai,MPIPAI)(&ioar,&UsIds[i])) {
						found = True;
						break;
					}
				}
				if (! found) {
					*npts = 0;
				}
			}
			break;
		}
	}


	return;
}


static void SetLineAttrs
(
        int type,
	int *npts
        )
{
        int color, dash_pattern;
        float thickness,dash_seglen;
        char *entry_name = "MapV41DHDrawMapList";
	
        switch (type) {
            case 1:
            case 2:
	    default:
                    color = Mpp->geophysical.gks_color;
                    dash_pattern = Mpp->geophysical.dash_pat;
                    dash_seglen = Mpp->geophysical.dash_seglen;
                    thickness = Mpp->geophysical.thickness;
                    break;
            case 3:
                    color = Mpp->national.gks_color;
                    dash_pattern = Mpp->national.dash_pat;
                    dash_seglen = Mpp->national.dash_seglen;
                    thickness = Mpp->national.thickness;
                    break;
            case 4:
                    color = Mpp->us_state.gks_color;
                    dash_pattern = Mpp->us_state.dash_pat;
                    dash_seglen = Mpp->us_state.dash_seglen;
                    thickness = Mpp->us_state.thickness;
                    break;
	    case 5:
                    color = Mpp->county.gks_color;
                    dash_pattern = Mpp->county.dash_pat;
                    dash_seglen = Mpp->county.dash_seglen;
                    thickness = Mpp->county.thickness;
                    break;
        }
	if (color == -1) {
		*npts = 0;
		Color = color;
		return;
	}
        if (color != Color) {
                gset_line_colr_ind(color);
                Color = color;
        }
        if (dash_pattern != Dash_Pattern ||
            dash_seglen != Dash_SegLen) {
                int	dpat;
                NhlString *sp;
                float	p0,p1,jcrt;
                int	slen;
                char	buffer[128];
                
                dpat = dash_pattern % Mpp->dash_table->num_elements;
                sp = (NhlString *) Mpp->dash_table->data;
                slen = strlen(sp[dpat]);
                p0 =  (float) c_kfpy(0.0);
                _NhlLLErrCheckPrnt(NhlWARNING,entry_name);
                p1 = dash_seglen;
                p1 = (float) c_kfpy(p1);
                _NhlLLErrCheckPrnt(NhlWARNING,entry_name);
                jcrt = (int) ((p1 - p0) / slen + 0.5);
                jcrt = jcrt > 1 ? jcrt : 1;
                strcpy(buffer,sp[dpat]);
	
                c_dashdc(buffer,jcrt,4);
                _NhlLLErrCheckPrnt(NhlWARNING,entry_name);
                Dash_Pattern = dash_pattern;
                Dash_SegLen = dash_seglen;
        }
        if (thickness != Thickness) {
              	gset_linewidth(thickness);
                _NhlLLErrCheckPrnt(NhlWARNING,entry_name);
                Thickness = thickness;
        }
        return;
}

/*
 * Function:  ModifyLines
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
static void   ModifyLines
#if	NhlNeedProto
(
        int ilty,
        int ioal,
        int ioar,
        int *npts,
	float *pnts
)

#else
(ilty,ioal,ioar,npts,pnts)
        int ilty;
        int ioal;
        int ioar;
        int *npts;
	float *pnts;
#endif
{
	NhlMapBoundarySets boundaries = Mpp->outline_boundaries;
	NhlBoolean found = False;
	int i;
	int do_boundary_check = 0;

        if (DrawIds[ioal-1].spec_rec && DrawIds[ioar-1].spec_rec) {
	  	v41SpecLineRec *lrec = 
		  	(v41SpecLineRec *)DrawIds[ioal-1].spec_rec;
	  	v41SpecLineRec *rrec = 
		  	(v41SpecLineRec *)DrawIds[ioar-1].spec_rec;
		int rtype, ltype;
		if (lrec->draw_mode == mpMASK && rrec->draw_mode == mpMASK) {
			/* definitely masked */
			*npts = 0;
			return;
		}
		
		rtype = MAX(c_mpiaty(rrec->eid),rrec->level);
		ltype = MAX(c_mpiaty(lrec->eid),lrec->level);
		if (lrec->draw_mode == mpMASK) {
			/* 
			 * if the left id specifies an area that is part of the 
			 * right id area, or if this line is at a "lower" level (where
			 * lower means a larger type number) than the specified entity
			 * (such as a county where the entity is a state), then don't
			 * draw.
			 */ 
			if ((rtype <= ltype) || (ilty > rtype)) {
				*npts = 0;
				return;
			}
			/*
			 * If water 
			 */
			if (lrec->eid  == Mv41p->basic_ids.water_id) {
				int count;
				if (rtype > 4) 
					count = CountyBorderWaterCount;
				else if (rtype > 3)
					count = StateBorderWaterCount;
				else if (rtype > 2)
					count = NatBorderWaterCount;
				else
					count = GeoBorderWaterCount;

				for (i = 0; i < count; i++) {
					if (ioal == BorderWaterEids[i])
						break;
				}
				if (i == count) {
					/* not in list, so it can be masked */
					*npts = 0; 
					return;
				}
			}
			else if (c_mpiola(lrec->eid,1) == Mv41p->basic_ids.water_id) {
				/* if a water body is specified explicitly then
				 * always mask
				 */
				*npts = 0; 
				return;
			}
		}
		else if (rrec->draw_mode == mpMASK) {
			if ((ltype <= rtype) || (ilty > ltype)) {
				*npts = 0;
				return;
			}
			/*
			if (c_mpiola(rrec->eid,1) == Mv41p->basic_ids.water_id) {
			*/
			if (rrec->eid == Mv41p->basic_ids.water_id) {
				int count;
				if (ltype > 4) 
					count = CountyBorderWaterCount;
			        else if (ltype > 3)
					count = StateBorderWaterCount;
				else if (ltype > 2)
					count = NatBorderWaterCount;
				else
					count = GeoBorderWaterCount;

				for (i = 0; i < count; i++) {
					if (ioar == BorderWaterEids[i])
						break;
				}
				if (i == count) {
					/* not in list, so it can be masked */
					*npts = 0; 
					return;
				}
			}
			else if (c_mpiola(rrec->eid,1) == Mv41p->basic_ids.water_id) {
				/* if a water body is specified explicitly then
				 * always mask
				 */
				*npts = 0; 
				return;
			}
		}
		else if (ilty > ltype && ilty > rtype) {
			*npts = 0;
			return;
		}
		SetLineAttrs(ilty,npts);
		return;
         }
        else if (DrawIds[ioal-1].spec_rec) {
		v41SpecLineRec *llspec = (v41SpecLineRec *) DrawIds[ioal-1].spec_rec;
		if (llspec->draw_mode == mpMASK) {
			if (ioal == Mv41p->basic_ids.ocean_id) {
				do_boundary_check = 1;
			}
			else if (llspec->eid == Mv41p->basic_ids.water_id) {
				int count;
				int start_ix = boundaries == 
					NhlGEOPHYSICALANDUSSTATES ?  USStartIndex :0;
				if (Level > 4) 
					count = CountyBorderWaterCount;
				else if (Level > 3)
					count = StateBorderWaterCount;
				else if (Level > 2)
					count = NatBorderWaterCount;
				else
					count = GeoBorderWaterCount;

				for (i = start_ix; i < count; i++) {
					if (ioal == BorderWaterEids[i])
						break;
				}
				if (i == count) {
					/* not in list, so it can be masked */
					*npts = 0; 
					return;
				}
				do_boundary_check = 1;
			}
			else {
				*npts = 0;
				return;
			}
		}
		else if (ilty > MAX(c_mpiaty(llspec->eid),llspec->level)) {
			*npts = 0;
			return;
		}
	}
	else if  (DrawIds[ioar-1].spec_rec) {
		v41SpecLineRec *rlspec = (v41SpecLineRec *) DrawIds[ioar-1].spec_rec;
		if (rlspec->draw_mode == mpMASK) {
			if (ioar == Mv41p->basic_ids.ocean_id) {
				do_boundary_check = 1;
			}
			else if (rlspec->eid == Mv41p->basic_ids.water_id) {
				int count;
				int start_ix = boundaries == 
					NhlGEOPHYSICALANDUSSTATES ?  USStartIndex :0;
				if (Level > 4) 
					count = CountyBorderWaterCount;
				else if (Level > 3)
					count = StateBorderWaterCount;
				else if (Level > 2)
					count = NatBorderWaterCount;
				else
					count = GeoBorderWaterCount;

				for (i = start_ix; i < count; i++) {
					if (ioar == BorderWaterEids[i])
						break;
				}
				if (i == count) {
					/* not in list, so it can be masked */
					*npts = 0; 
					return;
				}
				do_boundary_check = 1;
			}
			else {
				*npts = 0;
				return;
			}
		}
		else if (ilty > MAX(c_mpiaty(rlspec->eid),rlspec->level)) {
			*npts = 0;
			return;
		}
	}
	else if (ilty > Level) {
		*npts = 0;
		return;
	}
	else {
		do_boundary_check = 1;
	}

	if (do_boundary_check) {
		switch (boundaries) {
		default:
		case NhlNOBOUNDARIES:
			*npts = 0;
			break;
		case NhlGEOPHYSICAL:
		case NhlNATIONAL:
		case NhlALLBOUNDARIES:
			break;
		case NhlUSSTATES:
			for (i = UsIdCount-1;i>-1;i--) {
				if (NGCALLF(mpipai,MPIPAI)(&ioal,&UsIds[i]) ||
				    NGCALLF(mpipai,MPIPAI)(&ioar,&UsIds[i])) {
					found = True;
					break;
				}
			}
			if (! found) {
				*npts = 0;
			}
			break;
		case NhlGEOPHYSICALANDUSSTATES:
			if (ilty > 1) {
				for (i = UsIdCount-1;i>-1;i--) {
					if (NGCALLF(mpipai,MPIPAI)(&ioal,&UsIds[i]) ||
					    NGCALLF(mpipai,MPIPAI)(&ioar,&UsIds[i])) {
						found = True;
						break;
					}
				}
				if (! found) {
					*npts = 0;
				}
			}
			break;
		}
	}

	if (*npts) {
		SetLineAttrs(ilty,npts);
	}

	return;
}

/*
 * Function:  hlumpchln
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
void   (_NHLCALLF(hlumpchln,HLUMPCHLN))
#if	NhlNeedProto
(
        int *iflg,
        int *ilty,
        int *ioal,
        int *ioar,
        int *npts,
	float *pnts
)

#else
(iflg,ilty,ioal,ioar,npts,pnts)
        int *iflg;
        int *ilty;
        int *ioal;
        int *ioar;
        int *npts;
	float *pnts;
#endif
        
{

	if (Mpp == NULL) {
		_NHLCALLF(mpchln,MPCHLN)(iflg,ilty,ioal,ioar,npts,pnts);
		return;
	}
	/*
	 * The code following is used to count the dataset points in order to 
	 * calculate a reasonable size for the area map.
	 */

	if (Count_Points_Only) {
		if (Mv41p->min_fill_level >= *ilty)
			Mv41p->data_set_point_count += *npts;
		*npts = 0;
		return;
	}

        if (*ilty == 0) {
                *npts = 0;
                return;
        }

	switch (*iflg) {
	case 1:
	case 2:
	  ModifyFill(*ilty,*ioal,*ioar,npts,pnts);
	  break;
	case 3:
	default:
	  ModifyLines(*ilty,*ioal,*ioar,npts,pnts);
	  break;
	}

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
