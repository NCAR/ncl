/*
 *      $Id: Converters.h,v 1.4 1994-04-19 00:04:37 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Converters.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jul 14 10:41:03 MDT 1993
 *
 *	Description:	This file contains the declarations of all the
 *			Publicly defined converter functions.
 */
#ifndef	_NCONVERTERS_H
#define	_NCONVERTERS_H

#include <ncarg/hlu/Convert.h>

extern NhlErrorTypes NhlCvtStringToEnum(
#ifdef	NhlNeedProto
	NrmValue		*from,	/* ptr to from data	*/
	NrmValue		*to,	/* ptr to to data	*/
	NhlConvertArgList	args,	/* add'n args for conv	*/
	int			nargs	/* number of args	*/
#endif
);

extern NhlErrorTypes NhlCvtIntToEnum(
#ifdef	NhlNeedProto
	NrmValue		*from,	/* ptr to from data	*/
	NrmValue		*to,	/* ptr to to data	*/
	NhlConvertArgList	args,	/* add'n args for conv	*/
	int			nargs	/* number of args	*/
#endif
);

extern NhlErrorTypes NhlCvtFloatToEnum(
#ifdef	NhlNeedProto
	NrmValue		*from,	/* ptr to from data	*/
	NrmValue		*to,	/* ptr to to data	*/
	NhlConvertArgList	args,	/* add'n args for conv	*/
	int			nargs	/* number of args	*/
#endif
);

extern NhlErrorTypes NhlCvtEnumToString(
#ifdef	NhlNeedProto
	NrmValue		*from,	/* ptr to from data	*/
	NrmValue		*to,	/* ptr to to data	*/
	NhlConvertArgList	args,	/* add'n args for conv	*/
	int			nargs	/* number of args	*/
#endif
);

extern NhlErrorTypes NhlCvtEnumToFStr(
#ifdef	NhlNeedProto
	NrmValue		*from,	/* ptr to from data	*/
	NrmValue		*to,	/* ptr to to data	*/
	NhlConvertArgList	args,	/* add'n args for conv	*/
	int			nargs	/* number of args	*/
#endif
);

extern NhlErrorTypes NhlCvtEnumToInt(
#ifdef	NhlNeedProto
	NrmValue		*from,	/* ptr to from data	*/
	NrmValue		*to,	/* ptr to to data	*/
	NhlConvertArgList	args,	/* add'n args for conv	*/
	int			nargs	/* number of args	*/
#endif
);

extern NhlErrorTypes NhlCvtEnumToFloat(
#ifdef	NhlNeedProto
	NrmValue		*from,	/* ptr to from data	*/
	NrmValue		*to,	/* ptr to to data	*/
	NhlConvertArgList	args,	/* add'n args for conv	*/
	int			nargs	/* number of args	*/
#endif
);

#endif	/* _NCONVERTERS_H */
