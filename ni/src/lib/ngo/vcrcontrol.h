/*
 *      $Id: vcrcontrol.h,v 1.4 2000-03-21 02:35:56 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		vcrcontrol.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Feb 10 13:59:32 MST 1997
 *
 *	Description:	
 */
#ifndef	_NG_VCRCONTROL_H
#define	_NG_VCRCONTROL_H

#include <ncarg/ngo/go.h>
#include <ncarg/ngo/ncl.h>

/*
 * Public api
 */

typedef struct _NgVcrControl 
{
        Widget		form;
        Widget		begin;
        Widget		fast_reverse;
        Widget		reverse;
        Widget		start_stop;
        Widget		forward;
        Widget		fast_forward;
        Widget		end;
} NgVcrControl;

NgVcrControl *NgCreateVcrControl
(
	int			go_id,
        NhlString		name,
        Widget			parent,
        Dimension		size,
        NhlBoolean		horizontal,
        NhlBoolean		begin,
        NhlBoolean		fast_reverse,
        NhlBoolean		reverse,
        NhlBoolean		start_stop,
        NhlBoolean		forward,
        NhlBoolean		fast_forward,
        NhlBoolean		end
        );

NhlErrorTypes NgUpdateVcrControl
(
        NgVcrControl		*vcr_control
        );
        
void NgDestroyVcrControl
(
        NgVcrControl		*vcr_control
        );

#endif	/* _NG_VCRCONTROL_H */
