/*
 *      $Id: MapV41DataHandlerP.h,v 1.6 2006-08-15 18:24:02 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		MapV41DataHandlerP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Apr 29 12:08:44 MDT 1998
 *
 *	Description:	
 */
#ifndef _NMapV41DataHandlerP_h
#define  _NMapV41DataHandlerP_h

#include <ncarg/hlu/MapDataHandlerP.h>
#include <ncarg/hlu/MapV41DataHandler.h>

#define Nhl_mpMAPDATAFILE	"NhlMapData"
#define v41ALLOC_UNIT		64
#define mpWORKSPACE_SIZE_REQ	5242880
#define NhlmpOUTLINE_TYPE_COUNT 8

typedef enum _mpOutlineType { 
	mpOcean = 0,
	mpContinent,
	mpLargeIsland,
	mpSmallIsland,
	mpInlandWater,
	mpNational,
	mpUSStateLand,
	mpUSStateWater 
} mpOutlineType;

typedef enum _mpBGroups {
	mpNULLAREA,
	mpALLNATIONAL,
	mpALLGEOPHYSICAL,
	mpLAND,
	mpWATER,
	mpINLANDWATER,
	mpOCEANS,
	mpCONTINENTS,
	mpISLANDS,
	mpLARGEISLANDS,
	mpSMALLISLANDS,
	mpALLUSSTATES,
	mpUSSTATESLAND,
	mpUSSTATESWATER 
} mpBGroups;

typedef struct _v41EntityRec {
        short level;             /* 1: land-water, 2: continental, 3: national,
                                    4: state or province, 5: county, etc. */
        short dynamic_gid;
        short fixed_gid;	 /* 1: ocean, 2: land, 3: inland-water */
        short eid;
  	short canonical_ix;      /* index into the canonical order */
        char  unique;
        char  *name;		 /* lower-case name for caseless comparisons */
} v41EntityRec;

/* draw Modes */

#define mpBACKGROUND	0
#define mpDRAW		1
#define mpMASK		2
#define mpNOINDEX	-1

typedef struct _v41SpecFillRec {
	short		eid;
  	short           spec_ix;      /* index into specifier array */
  	unsigned char	draw_mode;    /* draw or mask */
  	unsigned char	spec_col;     /* color specified ? */
        unsigned char   spec_pat;     /* fill pattern specified ? */
        unsigned char   spec_fscale;  /* fill scale specified ? */
	char   	        level;
} v41SpecFillRec;

typedef struct _v41SpecLineRec {
	short		eid;
  	short           spec_ix;      /* index into specifier array */
  	unsigned char	draw_mode;    /* draw or mask */
  	unsigned char	spec_col;     /* color specified ? */
        unsigned char   spec_dpat;     /* dash pattern specified ? */
        unsigned char   spec_thickness;  /* thickness specified ? */
	char            level;
} v41SpecLineRec;

typedef struct _mpDrawIdRec {
        void	*spec_rec;
} mpDrawIdRec;

typedef struct _mpBasicIds {
	int  us_ids[3];
	int  us_id_count;
	int  land_id;
	int  water_id;
	int  ocean_id;
} mpBasicIds;

typedef struct _NhlMapV41DataHandlerLayerPart {
	NhlString	data_set_name;

	v41SpecFillRec	*fill_recs;
	int		fill_rec_alloc;
	int		fill_rec_count;
        int		min_fill_level;
        
	v41SpecLineRec	*outline_recs;
	int		outline_rec_alloc;
	int		outline_rec_count;
        int		min_outline_level;
        
	int		aws_id;
        NhlBoolean	new_amap_req;
	mpBasicIds	basic_ids;
	int		data_set_point_count;

/* 
 * these fields mirror the class fields of the same name; when using the 
 * default data set they are set equal to the class fields; 
 * otherwise they are particular to the instance.
 */
        int		entity_rec_count;
        v41EntityRec 	*entity_recs;         /* eid order */
        v41EntityRec 	**alpha_recs;         /* alphabetical by base name */
        v41EntityRec    **long_alpha_recs;    /* alphabetical by long name */
}NhlMapV41DataHandlerLayerPart;

typedef struct _NhlMapV41DataHandlerLayerRec {
	NhlObjLayerPart			base;
	NhlMapDataHandlerLayerPart	mapdh;
	NhlMapV41DataHandlerLayerPart	mapv41dh;
}NhlMapV41DataHandlerLayerRec;

typedef struct _NhlMapV41DataHandlerClassPart {
        int		entity_rec_count;
        v41EntityRec 	*entity_recs;         /* eid order */
        v41EntityRec 	**alpha_recs;         /* alphabetical by base name */
        v41EntityRec    **long_alpha_recs;    /* alphabetical by long name */
} NhlMapV41DataHandlerClassPart;

typedef struct _NhlMapV41DataHandlerClassRec {
	NhlObjClassPart			base_class;
	NhlMapDataHandlerClassPart	mapdh_class;
	NhlMapV41DataHandlerClassPart	mapv41dh_class;
} NhlMapV41DataHandlerClassRec;

typedef struct _NhlMapV41DataHandlerClassRec *NhlMapV41DataHandlerClass;
typedef struct _NhlMapV41DataHandlerLayerRec *NhlMapV41DataHandlerLayer;

extern NhlMapV41DataHandlerClassRec NhlmapV41DataHandlerClassRec;

#endif  /*_NMapV41DataHandlerP_h*/
