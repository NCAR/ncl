/*
 *	$Id: ctxt_device.h,v 1.2 2000-07-12 16:51:56 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU Lesser General Public License as        *
* published by the Free Software Foundation; either version 2.1 of the  *
* License, or (at your option) any later version.                       *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* Lesser General Public License for more details.                       *
*                                                                       *
* You should have received a copy of the GNU Lesser General Public      *
* License along with this software; if not, write to the Free Software  *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

/*
 *      File:		ctxt_device.h
 *
 *      Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *      Date:		Mon May  6 15:22:20 MDT 1991
 *
 *      Description:	ctxt device functions declarations 
 */
#ifndef	_ctxt_device_
#define	_ctxt_device_

#include "gksc.h"

extern	void	ctxt_ConvPoints(
#ifdef	NeedFuncProto
	GKSC_Ptr        ddp,
	float   	*rawx, 
	float		*rawy,
	Points  	*points,
	int     	*n,
	int     	conv
#endif
);

extern	void	ctxt_ConvString(
#ifdef	NeedFuncProto
	GKSC_Ptr        ddp,
	int		*raw,
	String		*string,
	int     	*n,
	int     	conv
#endif
); 

extern	void	ctxt_ConvInts(
#ifdef	NeedFuncProto
	GKSC_Ptr        ddp,
	int		*raw,
	Ints		*ints,
	int     	*n,
	int     	conv
#endif
); 
		

extern	void	ctxt_ConvFloats(
#ifdef	NeedFuncProto
	GKSC_Ptr        ddp,
	float		*raw,
	Floats		*rloats,
	int		*n,
	int		conv
#endif
); 

extern	void	ctxt_ConvIndexes(
#ifdef	NeedFuncProto
	GKSC_Ptr        ddp,
	int		*raw,
	Indexes		*indexes,
	int		*n,
	int		conv
#endif
); 

extern	void	ctxt_ConvRGBs(
#ifdef	NeedFuncProto
	GKSC_Ptr        ddp,
	float		*raw,
	RGBs		*rgbs,
	int		*n,
	int		conv
#endif
); 


extern	int	ctxt_OpenWorkstation(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	ctxt_ActivateWorkstation(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	ctxt_CloseWorkstation(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	ctxt_ClearWorkstation(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	ctxt_Polyline(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	ctxt_Polymarker(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	ctxt_Text(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	ctxt_FillArea(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	ctxt_Cellarray(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	ctxt_SetLinetype(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	ctxt_SetLineWidthScaleFactor(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	ctxt_SetPolylineColorIndex(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	ctxt_SetMarkerType(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	ctxt_SetMarkerSizeScaleFactor(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	ctxt_SetPolymarkerColorIndex(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
);
		
extern	int	ctxt_SetTextFontAndPrecision(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	ctxt_SetCharacterExpansionFactor(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	ctxt_SetCharacterSpacing(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	ctxt_SetTextColorIndex(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	ctxt_SetCharacterHeightAndUpVector(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	ctxt_SetTextPath(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	ctxt_SetTextAlignment(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	ctxt_SetFillAreaInteriorStyle(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	ctxt_SetFillAreaStyleIndex(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	ctxt_SetFillAreaColorIndex(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	ctxt_SetColorRepresentation(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	ctxt_SetClipIndicator(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	ctxt_SetWindow(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	ctxt_SetViewport(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	ctxt_Esc(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
);
		
extern	int	ctxt_UpdateWorkstation(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	ctxt_GetColorRepresentation(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
);
		
extern	int	ctxt_DeactivateWorkstation(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
);

#endif	/*	_ctxt_device_	*/
