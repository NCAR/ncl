/*
 *	$Id: options.h,v 1.4 1995-01-10 17:26:40 clyne Exp $
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
extern int	OptionDitherPopular;
extern int	OptionDitherColors;
extern int	OptionDitherBits;
extern int	OptionInX;
extern int	OptionInY;
extern int	OptionInInvert;
extern int	OptionIndexed;
extern int	OptionInOffset;
extern int	OptionOutCMap;


#endif
