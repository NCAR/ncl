/*
 *	$Id: options.h,v 1.3 1992-03-20 18:43:47 don Exp $
 */
#ifndef __OPTIONS__
#define __OPTIONS__

#define RAS_LANDSCAPE		0
#define RAS_PORTRAIT		1

#define RAS_COMPRESS_OFF	0
#define RAS_COMPRESS_RLE	1

extern int	OptionOrientation;
extern int	OptionCompression;
extern int	OptionDotsPerInch;

#endif
