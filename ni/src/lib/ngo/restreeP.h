/*
 *      $Id: restreeP.h,v 1.8 1999-06-02 03:40:08 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
**************************************************************f**********/
/*
 *	File:		restreeP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Jul 28 13:18:34 MDT 1997
 *
 *	Description:	
 */
#ifndef	_NG_RESTREEP_H_
#define	_NG_RESTREEP_H_

#include <ncarg/ngo/goP.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/BaseP.h>
#include <ncarg/hlu/ResourcesP.h>

#include <ncarg/ngo/browse.h>
#include <ncarg/ngo/restree.h>
#include <ncarg/ngo/htmlview.h>

 
#define DEBUG_ENTRY 1
#define DEBUG_TREE 1 << 1
#define DEBUG_RESTREE 0

/* defines for each node of the file tree
 * -- not including the leaf nodes.
 * 10's digit represents level in the tree hierarchy
 */
#define _rtTop		0

#define _rtLevel1	10
#define _rtLevel2	20
#define _rtLevel3	30
#define _rtLevel4	40
#define _rtLevel5	40

#define _rtNoGroup	00
#define _rtClassGroup   01
#define _rtClass	02
#define _rtResGroup	03
#define _rtRes		04
#define _rtResArray	05
#define _rtResDoc	06

#define _rtSuperClassGroup 0
#define _rtChildClassGroup 1

typedef char _rtNodeType;

typedef char _rtBool;

        
typedef struct _rtNodeData 
{
        struct _rtNodeData	*parent;
        void			*info;
        unsigned short		row;
        _rtNodeType		type;
        _rtBool			expanded;
        unsigned short		subcount;
	struct _rtNodeData  	*subdata;
} rtNodeData;

typedef struct _rtResData
{
        NrmResource	*res;
        NhlClass	real_class;
        _rtBool		vis;
        rtNodeData	*ndata;
        NhlString	value;
} rtResData;

typedef struct _rtSetValNode 
{
        struct _rtSetValNode	*next;
        rtResData		*res_data;
} rtSetValNode;

typedef struct _rtEnumInfoRec {
	NhlString		*strings;
        int			str_assigned_count;
        Pixmap			*pixmaps;
        float			*cmap;
	int			count;
	int			selected;
        Widget 			popup;
        Widget			menu;
        Widget			mega;
	NhlBoolean		up;
        Dimension		width,height;
        Position		x,y;
        Time			time;
} rtEnumInfoRec;

typedef enum _rtRowDisplayMode
{
        _rtHIDDEN,_rtUNEXPANDABLE,_rtEXPANDABLE 
} rtRowDisplayMode;

typedef struct _rtCntrlRes 
{
        NhlString res_name;
        NhlString cntrl_class;
        NhlPointer hide_val;
        NhlPointer expand_val;
	NhlString hide_str;
        NhlString fake_val;
} rtCntrlRes;

typedef struct _rtCntrlnfo
{
        rtResData	*res_data;
        rtCntrlRes	*cntrl_res;
        NhlPointer	cur_value;
        NhlBoolean	faked;
} rtCntrlInfo;
        
typedef struct _rtClassInfo
{
        NhlClass		class;
        int			res_count;
        NhlLayer		layer;
        rtRowDisplayMode	display_mode;
        rtCntrlInfo		*cntrl_info;
        rtNodeData		*ndata;
} rtClassInfo;

typedef struct _rtHtmlViewInfo
{
        int		id;
        Boolean		open;
        Position	y;
        Dimension	height;
        rtNodeData	*ndata;
} rtHtmlViewInfo;
        
typedef struct _NgResTreeRec 
{
            /* public data */
        NgResTree		restree;
        
            /* private fields */
        NgGO			go;
        NgPageId		page_id;
        int			nclstate;
        NrmQuark		qhlu;
        NhlClass		class;
        int			super_class_count;
        int			class_count;
        rtClassInfo		*class_info;
        int			qnames_count;
        NrmNameList		qnames;
        int			res_data_count;
        int			res_data_alloc_count;
        rtResData		*res_data;
        int			hlu_id;
        Dimension		c2_width;
        NhlBoolean		created;
        NhlBoolean		expand_called;
        rtNodeData		top;
        XmString		selected_row_xmstr;
        int			edit_row;
        int			focus_row;
	Boolean			manual_edit_started;
	Boolean			text_dropped;
        rtSetValNode		*set_val_list;
        Boolean			scroll_cbs_installed;
        Widget			text;
  	rtEnumInfoRec		enum_info;
        Boolean			size_update_req;
        int			htmlview_count;
        XmLArray		htmlview_list;
        Boolean			duping_data_list;
        _NhlCB			setval_cb;
} NgResTreeRec;

#endif	/* _NG_RESTREEP_H_ */


