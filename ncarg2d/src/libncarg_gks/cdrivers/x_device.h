/*
 *	$Id: x_device.h,v 1.4 2000-07-12 16:52:01 haley Exp $
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
 *      File:		x_device.h
 *
 *      Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *      Date:		Mon May  6 15:22:20 MDT 1991
 *
 *      Description:	X11 device functions declarations 
 */
#ifndef	_x_device_
#define	_x_device_

#include "gksc.h"

extern	void	X11_ConvPoints(
#ifdef	NeedFuncProto
	GKSC_Ptr        ddp,
	float   	*rawx, 
	float		*rawy,
	Points  	*points,
	int     	*n,
	int     	conv
#endif
);

extern	void	X11_ConvString(
#ifdef	NeedFuncProto
	GKSC_Ptr        ddp,
	int		*raw,
	String		*string,
	int     	*n,
	int     	conv
#endif
); 

extern	void	X11_ConvInts(
#ifdef	NeedFuncProto
	GKSC_Ptr        ddp,
	int		*raw,
	Ints		*ints,
	int     	*n,
	int     	conv
#endif
); 
		

extern	void	X11_ConvFloats(
#ifdef	NeedFuncProto
	GKSC_Ptr        ddp,
	float		*raw,
	Floats		*rloats,
	int		*n,
	int		conv
#endif
); 

extern	void	X11_ConvIndexes(
#ifdef	NeedFuncProto
	GKSC_Ptr        ddp,
	int		*raw,
	Indexes		*indexes,
	int		*n,
	int		conv
#endif
); 

extern	void	X11_ConvRGBs(
#ifdef	NeedFuncProto
	GKSC_Ptr        ddp,
	float		*raw,
	RGBs		*rgbs,
	int		*n,
	int		conv
#endif
); 

extern	int	X11_Exec(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
);

extern	int	X11_OpenWorkstation(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	X11_ActivateWorkstation(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	X11_CloseWorkstation(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	X11_ClearWorkstation(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	X11_Polyline(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	X11_Polymarker(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	X11_Text(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	X11_FillArea(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	X11_Cellarray(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	X11_SetLinetype(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	X11_SetLineWidthScaleFactor(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	X11_SetPolylineColorIndex(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	X11_SetMarkerType(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	X11_SetMarkerSizeScaleFactor(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	X11_SetPolymarkerColorIndex(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
);
		
extern	int	X11_SetTextFontAndPrecision(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	X11_SetCharacterExpansionFactor(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	X11_SetCharacterSpacing(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	X11_SetTextColorIndex(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	X11_SetCharacterHeightAndUpVector(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	X11_SetTextPath(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	X11_SetTextAlignment(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	X11_SetFillAreaInteriorStyle(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	X11_SetFillAreaStyleIndex(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	X11_SetFillAreaColorIndex(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	X11_SetColorRepresentation(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	X11_SetClipIndicator(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	X11_SetWindow(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	X11_SetViewport(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	X11_Esc(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
);

extern	int	X11_UpdateWorkstation(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	X11_GetColorRepresentation(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
);
		
extern	int	X11_DeactivateWorkstation(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
);

#endif	/*	_x_device_	*/
