/*
 *      $Id: MapV40DataHandlerP.h,v 1.2 1998-05-29 22:52:25 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		MapV40DataHandlerP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Apr 23 12:01:04 MDT 1998
 *
 *	Description:	
 */
#ifndef _NMapV40DataHandlerP_h
#define  _NMapV40DataHandlerP_h

#include <ncarg/hlu/MapDataHandlerP.h>
#include <ncarg/hlu/MapV40DataHandler.h>

#define Nhl_mpMAPDATAFILE	"NhlMapData"
#define mpALLOC_UNIT		128
#define mpWORKSPACE_SIZE_REQ	2097152

#define mpGLOBAL_AMAP	0
#define mpUSSTATES_AMAP	1

typedef enum _mpOutlineSet { 
	mpEMPTY = -1,
	mpCO,
	mpPO, 
	mpUS,
	mpPS
} mpOutlineSet;

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

typedef struct _mpOutlineRec {
	mpOutlineType type;
	short id[3];
	short cix[2];
	char *name;
} mpOutlineRec;

typedef enum _mpGlobalSetMode {
	mpNONE = 0,
	mpGEO,
	mpIMPLIED_NAT,
	mpNAT
} mpGlobalSetMode;

typedef enum _mpStateSetMode {
	mpNOSET,
	mpIMPLIED_SET,
	mpSET
} mpStateSetMode;


/* draw Modes */

#define mpBACKGROUND	0
#define mpDRAW		1
#define mpMASK		2
#define mpSYSEXCLUDE	3
#define mpDRAWSPECIAL	4

#define mpNOINDEX	-1

typedef union _mpFlags {
	unsigned long flags;
	struct {
		unsigned char	s_col;
		unsigned char	s_pat;
		unsigned char	s_scl;
		unsigned char	draw_mode;
	} f;
} mpFlags;

typedef struct _mpNameRec {
	short	name_ix;
	mpFlags u;
	unsigned char s_ix;
	short	ix;
} mpNameRec;

typedef struct _mpDrawIdRec {
	mpFlags 	u;
	unsigned char	s_ix;
	short		ix;
} mpDrawIdRec;

typedef struct _NhlMapV40DataHandlerLayerPart {
	mpNameRec	*fill_recs;
	int		fill_rec_alloc;
	int		fill_rec_count;
	mpNameRec	fill_groups[NhlmpOUTLINE_TYPE_COUNT];
        
	mpNameRec	*outline_recs;
	int		outline_rec_alloc;
	int		outline_rec_count;
	mpNameRec	outline_groups[NhlmpOUTLINE_TYPE_COUNT];
        
	mpGlobalSetMode	global_fill_mode;
	mpStateSetMode	usstates_fill_mode;
	mpGlobalSetMode	global_outline_mode;
	mpStateSetMode	usstates_outline_mode;
        
	int		co_aws_id;
        NhlBoolean      new_co_amap_req;
        int		us_aws_id;
        NhlBoolean      new_us_amap_req;
}NhlMapV40DataHandlerLayerPart;

typedef struct _NhlMapV40DataHandlerLayerRec {
	NhlObjLayerPart			base;
	NhlMapDataHandlerLayerPart	mapdh;
	NhlMapV40DataHandlerLayerPart	mapv40dh;
}NhlMapV40DataHandlerLayerRec;

typedef struct _NhlMapV40DataHandlerClassPart {
        mpOutlineRec 	*outline_recs;
        int		outline_rec_count;
        int		outline_type_start_ix[NhlmpOUTLINE_TYPE_COUNT+1];
} NhlMapV40DataHandlerClassPart;

typedef struct _NhlMapV40DataHandlerClassRec {
	NhlObjClassPart			base_class;
	NhlMapDataHandlerClassPart	mapdh_class;
	NhlMapV40DataHandlerClassPart	mapv40dh_class;
} NhlMapV40DataHandlerClassRec;

typedef struct _NhlMapV40DataHandlerClassRec *NhlMapV40DataHandlerClass;
typedef struct _NhlMapV40DataHandlerLayerRec *NhlMapV40DataHandlerLayer;

extern NhlMapV40DataHandlerClassRec NhlmapV40DataHandlerClassRec;

#endif  /*_NMapV40DataHandlerP_h*/
