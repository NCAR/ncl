/*
 *	$Id: pix_device.h,v 1.2 2008-07-23 17:29:43 haley Exp $
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
