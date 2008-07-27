/*
**      $Id: yuv.h,v 1.4 2008-07-27 03:22:41 haley Exp $
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



#ifndef	_yuv_h_
#define	_yuv_h_

#include "ncarg_ras.h"

extern	Raster	*YUVOpen(
	char	*name
);

extern	int	YUVRead(
	Raster	*ras
);

extern	Raster	*YUVOpenWrite(
	char		*name,
	int		nx,
	int		ny,
	char		*comment,
	RasterEncoding	encoding
);

extern	int	YUVWrite(
	Raster	*ras
);

extern	int	YUVPrintInfo(
	Raster		*ras
);

extern	int	YUVClose(
	Raster	*ras
);

extern	int	YUVSetFunctions(
	Raster	*ras
);

#endif	/* _yuv_h_	*/
