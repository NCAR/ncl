
/*
 *      $Id: TypeInitClassTemplate.c.sed,v 1.3 1995-12-19 20:42:41 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1995			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Jan 27 18:35:14 MST 1995
 *
 *	Description:	
 */
#include <ncarg/hlu/ConvertP.h>
/*ARGSUSED*/
static NhlErrorTypes CvtHLUGENTYPEREPToNclData
#if	NhlNeedProto
(NrmValue *from, NrmValue *to, NhlConvertArgList args, int nargs)
#else
(from, to, args, nargs)
NrmValue *from;
NrmValue *to;
NhlConvertArgList args;
int nargs;
#endif
{
	NhlGenArray gen;
	char func[] = "CvtHLUGENTYPEREPToNclData";
	void *val;
	NclMultiDValData tmp_md;
	

	if(nargs != 0) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: called with wrong number of args",func);
		to->size =0;
		return(NhlFATAL);
	}
	gen = (NhlGenArray)from->data.ptrval;
	if(!_NhlIsSubtypeQ(NrmStringToQuark(((NclTypeDATATYPEClass)nclTypeDATATYPEClass)->type_class.hlu_type_rep[1]),from->typeQ)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: called with wrong input type",func);
		to->size =0;
		return(NhlFATAL);
	}
	if(gen->my_data) {
		val = gen->data;
		gen->my_data = False;
	} else {
		val = NclMalloc((unsigned)gen->size * gen->num_elements);
		memcpy(val,gen->data,(unsigned)gen->size * gen->num_elements);
	}
	tmp_md = _NclCreateMultiDVal(
		NULL,NULL, Ncl_MultiDValData,
		0,val,NULL,gen->num_dimensions,
		gen->len_dimensions,TEMPORARY,NULL,(NclTypeClass)nclTypeDATATYPEClass);
	if(to->size < sizeof(NclMultiDValData)) {
		return(NhlFATAL);
	} else {
		*((NclMultiDValData*)(to->data.ptrval)) = (void*)tmp_md;
        	return(NhlNOERROR);
	}
}
/*ARGSUSED*/
static NhlErrorTypes CvtHLUTYPEREPToNclData
#if	NhlNeedProto
(NrmValue *from, NrmValue *to, NhlConvertArgList args, int nargs)
#else
(from, to, args, nargs)
NrmValue *from;
NrmValue *to;
NhlConvertArgList args;
int nargs;
#endif
{
	NhlArgVal * tmp;
	NclMultiDValData tmp_md;
	int n_dims = 1,len_dims = 1;

	tmp = NclMalloc((unsigned)sizeof(NhlArgVal));
	memcpy((void*)tmp,(void*)&from->data,sizeof(NhlArgVal));
	tmp_md = _NclCreateMultiDVal(
		NULL,NULL, Ncl_MultiDValData,
		0,(void*)tmp,NULL,n_dims,
		&len_dims,TEMPORARY,NULL,(NclTypeClass)nclTypeDATATYPEClass);
	if(to->size < sizeof(NclMultiDValData)) {
		return(NhlFATAL);
	} else {
		*((NclMultiDValData*)(to->data.ptrval)) = (void*)tmp_md;
        	return(NhlNOERROR);
	}
}

static NhlErrorTypes Ncl_Type_DATATYPE_InitClass
#if	NhlNeedProto
(void)
#else
()
#endif
{
        NhlRegisterConverter(NhlbaseClass,HLUGENTYPEREP,NhlTNclData,
		CvtHLUGENTYPEREPToNclData,NULL,0,False,NULL);
        NhlRegisterConverter(NhlbaseClass,HLUTYPEREP,NhlTNclData,
		CvtHLUTYPEREPToNclData,NULL,0,False,NULL);
	nclTypeDATATYPEClassRec.type_class.default_mis.DATATYPEval = DEFAULT_MISS;
	return(NhlNOERROR);
}

