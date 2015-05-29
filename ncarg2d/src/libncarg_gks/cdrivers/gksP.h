/*
 *      $Id: gksP.h,v 1.8.8.1 2010-03-17 20:53:30 brownrig Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		gksP.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Mar 8 19:18:41 MST 1996
 *
 *	Description:	Private GKS library interface.  This interface
 *			allows "C" native data types to be passed into
 *			the c drivers using the gescape function.  This
 *			interface should not be used directly, but only
 *			by the gescape function in libncarg_gksC.
 *
 *			There are no "public" interfaces defined in this file.
 */
#ifndef	_NGKSP_
#define	_NGKSP_

#define	NGESC_CNATIVE	-1450

#define	NGC_XGETXPIX	1
#define	NGC_XFREECI	2
#define NGC_XALLOCCOLOR	3
#define NGC_XSIZECHG	4
#define NGC_XWINCONFIG	5
#define NGC_PIXCONFIG	6
#define NGC_SETALPHA    7
#define NGC_GETALPHA    8
#define NGC_ANTIALIAS   9
#define NGC_CAIROFILLHACK 10   /* see Jira ticket ncl-1913 */
#define NGC_GEOREFERENCE  11

/* opcodes for setting opacity attributes */
#define NGC_LINEALPHA   0
#define NGC_FILLALPHA   1
#define NGC_MARKERALPHA 2
#define NGC_TEXTALPHA   3
#define NGC_BACKGROUNDALPHA 4

typedef struct {
	int		type;
	int		work_id;
	unsigned long	gksci;
	unsigned long	xpixnum;	/* return */
} _NGCXGetXPix;

typedef struct {
	int		type;
	int		work_id;
	unsigned long	gksci;
} _NGCXFreeCi;

typedef void (*_NGCXAllocColorProc)(
	void	*cref,
	void	*color_def	/* really (XColor*) */
);

typedef void (*_NGCXFreeColorsProc)(
	void		*cref,
	unsigned long	*pixels,
	int		npixels
);

typedef struct {
	int			type;
	int			work_id;
	_NGCXAllocColorProc	xalloc_color;
	_NGCXFreeColorsProc	xfree_colors;
	void			*cref;
} _NGCXAllocColor;

typedef void (*_NGCXGetSizeProc)(
	void		*closure,
	unsigned long	size
);

typedef struct {
	int			type;
	int			work_id;
	_NGCXGetSizeProc	xget_size;
	void			*closure;
} _NGCXGetSizeChg;

typedef struct {
	int			type;
	int			work_id;
	int			x;
	int			y;
	int			width;
	int			height;
	char			*title;
	char			*icon_title;
} _NGCXWinConfig;

typedef struct {
	int			type;
	int			work_id;
	int			width;
	int			height;
	int                     format;
	char                    *filename;
} _NGCPixConfig;

typedef struct {
    int     type;
    int     work_id;
    int     graphicAttrib;
    float   alpha;
} _NGCAlpha;

typedef struct {
    int     type;
    int     work_id;
    int     antialias_boolean;
} _NGCAntiAlias;

typedef struct {        /* Jira ncl-1913 */
    int     type;
    int     work_id;
    int     fill_mode_boolean;
} _NGCCairoFillHack;

typedef struct {
    int     type;
    int     work_id;
    int     projCode;
    float   worldX[4];
    float   worldY[4];
    float   ndcX[4];
    float   ndcY[4];
    float   parOrAngle1;
    float   parOrAngle2;
    float   meridianOrDist;    
} _NGCGeoReference;

typedef struct {
	int		type;
	int		work_id;
} _NGCAny;

typedef union _NGCescapeRec_ {
	int		  type;
	_NGCAny		  any;
	_NGCXGetXPix	  xgetxpix;
	_NGCXFreeCi	  xfreeci;
	_NGCXAllocColor	  xalloccolor;
	_NGCXGetSizeChg	  xgetsizechg;
	_NGCXWinConfig	  xwinconfig;
	_NGCPixConfig     pixconfig;
	_NGCAlpha         alphaconfig;
        _NGCAntiAlias     antialias;
        _NGCCairoFillHack fillhack;
        _NGCGeoReference  georeference;
} _NGCesc;

/*
 * This function should only be called from libncarg_gksC(s_gesc.c).
 */
int _NGCescape(
#ifdef NeedFuncProto
	int	func_id,
	_NGCesc	*cesc
#endif
);

/*
 * This function should only be called from individual output drivers.
 */
_NGCesc *
_NGGetCEscInit(
#ifdef	NeedFuncProto
	void
#endif
);

#endif	/* _NGKSP_ */
