/*
 *      $Id: ContourPlotI.h,v 1.2 2004-03-11 02:00:30 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1994			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		ContourPlotI.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Jul 10 19:43:11 MDT 1997
 *
 *	Description:	
 */
#ifndef	_NCONTOURPLOTI_H
#define	_NCONTOURPLOTI_H

extern NhlErrorTypes _NhlRasterFill(
#if	NhlNeedProto
	float 		*zdat,
	int		*cell,
	int		ica1,
	int		icam,
	int		ican,
	float		xcpf,
	float		ycpf,
	float		xcqf,
	float		ycqf,
	char		*entry_name
#endif        
        );

extern NhlErrorTypes _NhlTriMeshRasterFill(
#if	NhlNeedProto
	float		*rpnt,
	int             *iedg,
	int             *itri,
	int		*cell,
	int		ica1,
	int		icam,
	int		ican,
	float		xcpf,
	float		ycpf,
	float		xcqf,
	float		ycqf,
	char		*entry_name
#endif        
        );


extern NhlErrorTypes _NhlCellFill(
#if	NhlNeedProto
	NhlLayer                l,
	NhlString		entry_name
#endif        
        );


#endif	/* _NCONTOURPLOTI_H */
