/*
 *	$Id: options.h,v 1.2 1991-08-16 11:11:43 clyne Exp $
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

#endif __OPTIONS__
