/*
 *      $Id: browse.h,v 1.16 2000-05-16 01:59:14 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		browse.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Mar  4 12:38:45 MST 1997
 *
 *	Description:	
 */
#ifndef	_NG_BROWSE_H
#define	_NG_BROWSE_H

#include <ncarg/ngo/go.h>
#include <ncarg/ngo/ncl.h>
#include <ncarg/ngo/dataprofile.h>

extern NhlClass NgbrowseClass;

/*
 * Public api
 */

typedef enum _brPageType 
{
        _brNULL, _brREGVAR, _brFILEREF, _brFILEVAR, _brHLUVAR, _brPLOTVAR,
	_brFUNCTION, _brHTML
} brPageType;

#define NgNoPage 0
typedef int NgPageId;

/*
 * Currently defined page message types
 */

typedef enum _NgPageMessageType
{
	_NgNOMESSAGE,	   /* the null message */
	_NgHLUOBJCREATE,   /* msg type: brHluObjCreate - hlupage.h */
	_NgPLOTCREATE,   /* msg type: brPlotCreate - plotpage.h */
	_NgDATAPROFILE,    /* msg type: NgDataProfile - dataprofile.h */
	_NgDATAPROFILELINK_REQ,
	_NgVARDATA,  	   /* msg type: NgVarData     - dataprofile.h */
	_NgVARDATALINK_REQ,
	_NgDOSETVALCB,	   /* msg type: NhlBoolean */
	_NgDATAVARUPDATE   /* msg type: NhlBoolean */
} NgPageMessageType;

/*
 * Page messages are stored using this struct. The BrowseClass part keeps
 * these in a XmLArray until they are retrieved. 
 */

typedef struct _NgPageMessageRec {
	NgPageMessageType	mtype;
        NgPageId		from_id;
	NgPageMessageType	reply_req;
	brPageType		to_type;
        NrmQuark		to_qvar;
        NrmQuark		to_qfile;
	NhlPointer		message;
	NhlFreeFunc		message_free;
} NgPageMessageRec, *NgPageMessage;

/*
 * Pages use this struct as a uniform way to hold information about
 * who and what responses are needed to messages
 */

typedef struct _NgPageReplyRec {
	NgPageMessageType	req;
	NgPageId		id;
	NrmQuark		qfile;
	NrmQuark		qvar;
} NgPageReplyRec, *NgPageReply;

/*
 * This structure stores the state of pages when they are 'hidden' by the
 * user. A list of these is kept in the BrowseClass part struct.
 */
typedef struct _NgPageSaveStateRec {
	NgPageId	page_id;
	NrmQuark	qvar;
	NrmQuark	qfile;
	NhlPointer	page_state;
	NhlFreeFunc	page_state_free;
} NgPageSaveStateRec, *NgPageSaveState;

typedef void (*AdjustPageGeoFunc) (
 	NhlPointer data
);

extern NgPageId NgOpenPage(
        int		goid,
        brPageType	type,
        NrmQuark	*qname,
        int		qcount,
	NhlPointer	init_data
        );

extern void NgPageOutputNotify(
        int		goid,
        NgPageId	page_id,
        brPageType	output_page_type,
        NhlPointer	output_data
        );

extern NhlPointer NgPageData(
        int		goid,
        NgPageId	page_id
        );

extern NhlErrorTypes NgUpdatePage(
        int		goid,
        NgPageId	page_id
        );

extern NhlErrorTypes NgUpdatePages(
        int		goid,
	NhlBoolean	force_draw,
	int		wk_id
        );

extern NhlErrorTypes NgResetPage(
        int		goid,
        NgPageId	page_id
        );

extern NhlErrorTypes NgDeletePage(
        int		goid,
        NgPageId	page_id
        );

extern int NgGetPageId(
        int		goid,
        NrmQuark	qsym1,
        NrmQuark	qsym2
        );

extern NhlErrorTypes NgPageSetVisible(
        int		goid,
        NgPageId	page_id,
        Widget		requestor,
        XRectangle	*rect
        );

extern void NgPageMessageNotify(
        int		goid,
        NgPageId	page_id
        );

extern NhlErrorTypes NgSavePageState(
	int			goid,
	int			page_id,
        NrmQuark		qfile,
        NrmQuark		qvar,
	NhlPointer		page_state,
	NhlFreeFunc		page_state_free
	);

extern NhlErrorTypes NgPostPageMessage(
	int			goid,
	int			from_id,
	NgPageMessageType	reply_req,
	brPageType		to_type,
        NrmQuark		to_qfile,
        NrmQuark		to_qvar,
	NgPageMessageType	mtype,
	NhlPointer		message,
	NhlBoolean		overwrite,
	NhlFreeFunc		message_free,
	NhlBoolean		notify
	);

extern int NgRetrievePageMessages(            /* returns message count */
	int			goid,
	brPageType		to_type,
        NrmQuark		to_qfile,
        NrmQuark		to_qvar,
	NgPageMessage		**messages
	);

extern void NgDeletePageMessages(
	int		goid,
	int		count,
	NgPageMessage	*messages,
	NhlBoolean	delete_message_data
	);

#endif	/* _NG_BROWSE_H */
