/*
 *      $Id: mwin.h,v 1.3 1999-05-22 00:36:21 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		mwin.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Dec 10 11:38:06 MST 1996
 *
 *	Description:	
 */
#ifndef	_NG_MWIN_H
#define	_NG_MWIN_H

#include <ncarg/ngo/go.h>
#include <ncarg/ngo/xinteract.h>

#define NgNpmObjectTree         "pmObjectTree"
#define NgCpmObjectTree         "PmObjectTree"


extern NhlClass NgmWinClass;

/*
 * Public api
 */

extern NhlErrorTypes NgUpdateViewBB(
	NgWksState	wks_state,
	int		vw_id
	);

extern NhlErrorTypes NgGetViewsInRegion(
	NgWksState	wks_state,
	int		wks_id,
	NhlBoolean	limit_to_vp,
	NgXBBox		*xbbox,
	int		**views,
	int		*view_count
);
	
extern int NgTopLevelViews(
	NgWksState	wks_state,
	int		wks_id,
	int		**views
);

extern NgViewObj NgGetView(
	NgWksState	wks_state,
	int		view_id
);

extern void NgDrawUpdatedViews(
	NgWksState	wks_state
);

extern void NgUpdateTransformation(
	NgWksState	wks_state
);

#endif	/* _NG_MWIN_H */



