/*
 *      $Id: ResList.h,v 1.1 1994-02-18 02:54:46 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1994			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		ResList.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Feb 15 11:20:48 MST 1994
 *
 *	Description:	
 */
#ifndef	_NResList_H
#define	_NResList_H

typedef enum NhlRLType_ { NhlSETRL, NhlGETRL } NhlRLType;

extern int NhlRLCreate(
#if	NhlNeedProto
	NhlRLType	type	/* type of RL list to create */
#endif
);

extern void NhlRLDestroy(
#if	NhlNeedProto
	int	id	/* RL list to destroy	*/
#endif
);

extern void NhlRLClear(
#if	NhlNeedProto
	int	id	/* RL list to destroy	*/
#endif
);

extern void NhlRLUnSet(
#if	NhlNeedProto
	int		id,	/* RL list 		*/
	NhlString	name	/* resname to unset	*/
#endif
);

extern NhlBoolean NhlRLIsSet(
#if	NhlNeedProto
	int		id,	/* RL list		*/
	NhlString	name	/* resname to unset	*/
#endif
);

/*VARARGS3*/
extern NhlErrorTypes NhlRLSet(
#if	NeedVarArgProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	NhlString	type,		/* type of value		*/
	...				/* value to set resname to	*/
#endif
);

extern NhlErrorTypes NhlRLSetInt(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	int		value		/* value to set resname to	*/
#endif
);

extern NhlErrorTypes NhlRLSetFloat(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	float		value		/* value to set resname to	*/
#endif
);

extern NhlErrorTypes NhlRLSetString(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	NhlString	value		/* value to set resname to	*/
#endif
);

extern NhlErrorTypes NhlRLSetMDArray(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	NhlPointer	data,		/* array			*/
	NhlString	type,		/* type of elements of array	*/
	unsigned int	size,		/* size of elements of array	*/
	int		num_dimensions,	/* number dimensions in array	*/
	int		*len_dimensions	/* len each dimension in array	*/
#endif
);

extern NhlErrorTypes NhlRLSetMDIntArray(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	int		*data,		/* array			*/
	int		num_dimensions,	/* number dimensions in array	*/
	int		*len_dimensions	/* len each dimension in array	*/
#endif
);

extern NhlErrorTypes NhlRLSetMDFloatArray(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	float		*data,		/* array			*/
	int		num_dimensions,	/* number dimensions in array	*/
	int		*len_dimensions	/* len each dimension in array	*/
#endif
);

extern NhlErrorTypes NhlRLSetArray(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	NhlPointer	data,		/* array			*/
	NhlString	type,		/* type of elements of array	*/
	unsigned int	size,		/* size of elements of array	*/
	int		num_elements	/* number elements in array	*/
#endif
);

extern NhlErrorTypes NhlRLSetIntArray(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	int		*data,		/* array			*/
	int		num_elements	/* number elements in array	*/
#endif
);

extern NhlErrorTypes NhlRLSetFloatArray(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	float		*data,		/* array			*/
	int		num_elements	/* number elements in array	*/
#endif
);

extern NhlErrorTypes NhlRLSetStringArray(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	NhlString	*data,		/* array			*/
	int		num_elements	/* number elements in array	*/
#endif
);

/*VARARGS3*/
extern NhlErrorTypes NhlRLGet(
#if	NeedVarArgProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	NhlString	type,		/* type of value		*/
	...				/* addr to put value in		*/
#endif
);

extern NhlErrorTypes NhlRLGetInt(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	int		*value		/* addr to put value in		*/
#endif
);

extern NhlErrorTypes NhlRLGetFloat(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	float		*value		/* addr to put value in		*/
#endif
);

extern NhlErrorTypes NhlRLGetString(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	NhlString	*value		/* addr to put value in		*/
#endif
);

extern NhlErrorTypes NhlRLGetMDArray(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	NhlPointer	*data,		/* array			*/
	NhlString	*type,		/* type of elements of array	*/
	unsigned int	*size,		/* size of elements of array	*/
	int		*num_dimensions,/* number dimensions in array	*/
	int		**len_dimensions/* len each dimension in array	*/
#endif
);

extern NhlErrorTypes NhlRLGetMDIntArray(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	int		**data,		/* array			*/
	int		*num_dimensions,/* number dimensions in array	*/
	int		**len_dimensions/* len each dimension in array	*/
#endif
);

extern NhlErrorTypes NhlRLGetMDFloatArray(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	float		**data,		/* array			*/
	int		*num_dimensions,/* number dimensions in array	*/
	int		**len_dimensions/* len each dimension in array	*/
#endif
);

extern NhlErrorTypes NhlRLGetArray(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	NhlPointer	data,		/* array			*/
	NhlString	type,		/* type of elements of array	*/
	unsigned int	size,		/* size of elements of array	*/
	int		num_elements	/* number elements in array	*/
#endif
);

extern NhlErrorTypes NhlRLGetIntArray(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	int		*data,		/* array			*/
	int		num_elements	/* number elements in array	*/
#endif
);

extern NhlErrorTypes NhlRLGetFloatArray(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	float		*data,		/* array			*/
	int		num_elements	/* number elements in array	*/
#endif
);

extern NhlErrorTypes NhlRLGetStringArray(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	NhlString	*data,		/* array			*/
	int		num_elements	/* number elements in array	*/
#endif
);

#endif	/* _NResList_H */
