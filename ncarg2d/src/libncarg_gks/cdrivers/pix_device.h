/*
 *	$Id: pix_device.h,v 1.1 2004-03-16 18:50:41 fred Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU General Public License as published     *
* by the Free Software Foundation; either version 2 of the License, or  *
* (at your option) any later version.                                   *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* General Public License for more details.                              *
*                                                                       *
* You should have received a copy of the GNU General Public License     *
* along with this software; if not, write to the Free Software          *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

/*
 *      File:		pix_device.h
 *
 *      Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *      Date:		Fri Mar 12 15:20:44 MST 2004
 *
 *      Description:	PIX device functions declarations 
 */
#ifndef	_pix_device_
#define	_pix_device_

#include "gksc.h"

extern	void	PIX_ConvPoints(
#ifdef	NeedFuncProto
	GKSC_Ptr        ddp,
	float   	*rawx, 
	float		*rawy,
	Points  	*points,
	int     	*n,
	int     	conv
#endif
);

extern	void	PIX_ConvString(
#ifdef	NeedFuncProto
	GKSC_Ptr        ddp,
	int		*raw,
	String		*string,
	int     	*n,
	int     	conv
#endif
); 

extern	void	PIX_ConvInts(
#ifdef	NeedFuncProto
	GKSC_Ptr        ddp,
	int		*raw,
	Ints		*ints,
	int     	*n,
	int     	conv
#endif
); 
		

extern	void	PIX_ConvFloats(
#ifdef	NeedFuncProto
	GKSC_Ptr        ddp,
	float		*raw,
	Floats		*rloats,
	int		*n,
	int		conv
#endif
); 

extern	void	PIX_ConvIndexes(
#ifdef	NeedFuncProto
	GKSC_Ptr        ddp,
	int		*raw,
	Indexes		*indexes,
	int		*n,
	int		conv
#endif
); 

extern	void	PIX_ConvRGBs(
#ifdef	NeedFuncProto
	GKSC_Ptr        ddp,
	float		*raw,
	RGBs		*rgbs,
	int		*n,
	int		conv
#endif
); 

extern	int	PIX_Exec(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
);

extern	int	PIX_OpenWorkstation(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	PIX_ActivateWorkstation(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	PIX_CloseWorkstation(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	PIX_ClearWorkstation(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	PIX_Polyline(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	PIX_Polymarker(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	PIX_Text(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	PIX_FillArea(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	PIX_Cellarray(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	PIX_SetLinetype(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	PIX_SetLineWidthScaleFactor(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	PIX_SetPolylineColorIndex(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	PIX_SetMarkerType(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	PIX_SetMarkerSizeScaleFactor(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	PIX_SetPolymarkerColorIndex(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
);
		
extern	int	PIX_SetTextFontAndPrecision(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	PIX_SetCharacterExpansionFactor(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	PIX_SetCharacterSpacing(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	PIX_SetTextColorIndex(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	PIX_SetCharacterHeightAndUpVector(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	PIX_SetTextPath(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	PIX_SetTextAlignment(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	PIX_SetFillAreaInteriorStyle(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	PIX_SetFillAreaStyleIndex(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	PIX_SetFillAreaColorIndex(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	PIX_SetColorRepresentation(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 
		
extern	int	PIX_SetClipIndicator(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	PIX_SetWindow(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	PIX_SetViewport(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	PIX_Esc(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
);

extern	int	PIX_UpdateWorkstation(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
); 

extern	int	PIX_GetColorRepresentation(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
);
		
extern	int	PIX_DeactivateWorkstation(
#ifdef	NeedFuncProto
	GKSC	*gksc
#endif
);

#endif	/*	_pix_device_	*/
