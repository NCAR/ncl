/*
 *      $Id: nclapi.c,v 1.1 2000-06-28 19:24:02 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		nclapi.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Jun  9 12:22:00 MDT 2000
 *
 *	Description:	
 */

#include <ncarg/ngo/nclapi.h>

/*
 * Reads att value for any kind of variable, depending on which
 * quarks are set to NrmNULLQUARK. The results are cached and 
 * therefore the caller should not free.
 */

NclExtValueRec *
NgNclReadAtt(
	NrmQuark	qfile,
	NrmQuark	qvar,
	NrmQuark	qcoord,
	NrmQuark	qatt
	)
{
	NclExtValueRec *val = NULL;
/*
 * caching will be set up later; for now, the caller is responsible for
 * freeing.
 */

	/* 
	 * coordinate vars in files are accessed as file vars; if the
	 * coordinate is specified use it; otherwise use the var
	 */
	if (qfile > NrmNULLQUARK && qcoord > NrmNULLQUARK)
		val = NclReadFileVarAtt(qfile,qcoord,qatt);
	else if (qfile > NrmNULLQUARK && qvar > NrmNULLQUARK)
		val = NclReadFileVarAtt(qfile,qvar,qatt);
	else if (qvar > NrmNULLQUARK && qcoord > NrmNULLQUARK)
		val = NclReadVarCoordAtt(qvar,qcoord,qatt);
	else if (qvar > NrmNULLQUARK)
		val = NclReadVarAtt(qvar,qatt);
	else if (qfile > NrmNULLQUARK)
		 val = NclReadFileAtt(qfile,qatt);

	return val;
}

extern NclExtValueRec *
NgNclReadVarValue(
	NrmQuark	qfile,
	NrmQuark	qvar,
	NrmQuark	qcoord,
	long		*start,
	long		*finish,
	long		*stride
	)
{
	NclExtValueRec *val = NULL;

	if (qfile > NrmNULLQUARK && qvar > NrmNULLQUARK &&
	    qcoord > NrmNULLQUARK)
		val = NclReadFileVarCoord
			(qfile,qvar,qcoord,start,finish,stride);
	else if (qfile > NrmNULLQUARK && qvar > NrmNULLQUARK)
		val = NclReadFileVar(qfile,qvar,start,finish,stride);
	else if (qvar > NrmNULLQUARK && qcoord > NrmNULLQUARK)
		val = NclReadVarCoord(qvar,qcoord,start,finish,stride);
	else if (qvar > NrmNULLQUARK)
		val = NclReadVar(qvar,start,finish,stride);

	return val;
}

extern NclExtValueRec *
NgNclReadCoordValue(
	NrmQuark	qfile,
	NrmQuark	qvar,
	int		dim_ix,
	long		*start,
	long		*finish,
	long		*stride
	)
{
	NclExtValueRec *val = NULL;
	NclApiDataList	*dl = NULL;
	NclApiVarInfoRec	*vinfo = NULL;
	NrmQuark	qcoord;

	if (qfile > NrmNULLQUARK && qvar > NrmNULLQUARK)
		dl = NclGetFileVarInfo(qfile,qvar);
	else if (qvar)
		dl = NclGetVarInfo(qvar);

	if (! dl)
		return NULL;

	vinfo = dl->u.var;

	if (dim_ix > vinfo->n_dims)
		return NULL;

	qcoord = vinfo->coordnames[dim_ix];

	if (qcoord <= NrmNULLQUARK)
		return NULL;

	if (qfile > NrmNULLQUARK)
		val = NclReadFileVarCoord
			(qfile,qvar,qcoord,start,finish,stride);
	else
		val = NclReadVarCoord(qvar,qcoord,start,finish,stride);
		
	return val;
}

double
NgNumericValToDouble(
        NclExtValueRec	*val,
        int		index
        )
{
        char *valp = ((char *) val->value) + index * val->elem_size;
        double dout;

        switch (val->type) {
            case NCLAPI_float:
                    dout = (double)*(float*)valp;
                    return dout;
            case NCLAPI_double:
                    dout = *(double*)valp;
                    return dout;
            case NCLAPI_byte:
                    dout = (double)*(unsigned char*)valp;
                    return dout;
            case NCLAPI_char:
                    dout = (double)*(char*)valp;
                    return dout;
            case NCLAPI_int:
                    dout = (double)*(int*)valp;
                    return dout;
            case NCLAPI_short:
                    dout = (double)*(short*)valp;
                    return dout;
            case NCLAPI_long:
                    dout = (double)*(long*)valp;
                    return dout;
            default:
                    NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			      "cannot convert non-numeric type to double\n"));
        }
        return 0.0;
}


