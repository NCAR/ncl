/*
 *	$Id: ps_device.h,v 1.2 2000-07-12 16:51:59 haley Exp $
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
 *      File:		ps_device.h
 *
 *      Author:         Fred Clare
 *                      National Center for Atmospheric Research
 *                      PO 3000, Boulder, Colorado
 *
 *      Date:           Tue Jul 27 11:40:28 MDT 1993
 *
 *      Description:	PostScript device functions declarations.
 */
#ifndef	_ps_device_
#define	_ps_device_

#include "gksc.h"

extern	void	ps_ConvPoints(
#ifdef	NeedFuncProto
	GKSC_Ptr        ddp,
	float   	*rawx, 
	float		*rawy,
	Points  	*points,
	int     	*n,
	int     	conv
#endif
);

extern	void	ps_ConvString(
#ifdef	NeedFuncProto
	GKSC_Ptr        ddp,
	int		*raw,
	String		*string,
	int     	*n,
	int     	conv
#endif
); 

extern	void	ps_ConvInts(
#ifdef	NeedFuncProto
	GKSC_Ptr        ddp,
	int		*raw,
	Ints		*ints,
	int     	*n,
	int     	conv
#endif
); 
		

extern	void	ps_ConvFloats(
#ifdef	NeedFuncProto
	GKSC_Ptr        ddp,
	float		*raw,
	Floats		*rloats,
	int		*n,
	int		conv
#endif
); 

extern	void	ps_ConvIndexes(
#ifdef	NeedFuncProto
	GKSC_Ptr        ddp,
	int		*raw,
	Indexes		*indexes,
	int		*n,
	int		conv
#endif
); 

extern	void	ps_ConvRGBs(
#ifdef	NeedFuncProto
	GKSC_Ptr        ddp,
	float		*raw,
	RGBs		*rgbs,
	int		*n,
	int		conv
#endif
); 


extern	int	ps_OpenWorkstation(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	ps_ActivateWorkstation(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	ps_CloseWorkstation(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	ps_ClearWorkstation(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	ps_Polyline(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	ps_Polymarker(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	ps_Text(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	ps_FillArea(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	ps_Cellarray(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	ps_SetLinetype(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	ps_SetLineWidthScaleFactor(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	ps_SetPolylineColorIndex(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	ps_SetMarkerType(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	ps_SetMarkerSizeScaleFactor(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	ps_SetPolymarkerColorIndex(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
);
		
extern	int	ps_SetTextFontAndPrecision(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	ps_SetCharacterExpansionFactor(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	ps_SetCharacterSpacing(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	ps_SetTextColorIndex(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	ps_SetCharacterHeightAndUpVector(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	ps_SetTextPath(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	ps_SetTextAlignment(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	ps_SetFillAreaInteriorStyle(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	ps_SetFillAreaStyleIndex(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	ps_SetFillAreaColorIndex(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	ps_SetColorRepresentation(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	ps_SetClipIndicator(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	ps_SetWindow(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	ps_Esc(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
);
		
extern	int	ps_UpdateWorkstation(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	ps_GetColorRepresentation(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
);
		
extern	int	ps_DeactivateWorkstation(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
);

extern	int	ps_SetViewport(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
);

extern	void 	ps_SoftFill (GKSC *, float, float);

#endif	/*	_ps_device_	*/
