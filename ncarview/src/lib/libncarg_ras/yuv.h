/*
**      $Id: yuv.h,v 1.1 1995-05-14 16:46:00 clyne Exp $
*/

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
