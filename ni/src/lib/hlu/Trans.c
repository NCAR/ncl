/*
 *      $Id: Trans.c,v 1.13 1995-04-07 10:44:05 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Trans.c
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Oct 29 14:32:42 MST 1992
 *
 *	Description:	Contains public function calls for performing 
 *			transformations.
 *
 * -------------------> None of these functions perform an NhlSetTrans. It is up
 *			to the HLU programmer to determine when an NhlSetTrans
 *			call is needed. In general a set is always needed in
 *			the ndc_to_data and data_to_ndc methods of the
 *			NhlTransformClass. However it may not always be
 *			needed in the methods of the NhlTransObjClass.<-----
 *
 */

#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/TransObjP.h>
#include <ncarg/hlu/TransformP.h>

NhlErrorTypes 	_NhlWinToNDC
#if  NhlNeedProto
(NhlLayer instance, float *x,float *y,int n, float *xout,
	float *yout,int *istrans,float* xmissing, float* ymissing)
#else
(instance,x,y,n,xout,yout,istrans,xmissing,ymissing)
	NhlLayer	instance;
	float *x;
	float *y;
	int 	n;
	float *xout;
	float *yout;
	int	*istrans;
	float *xmissing;
	float *ymissing;
#endif
{	
	char			func[]="_NhlWinToNDC";
	NhlTransObjClass	i_class ;

	i_class = (NhlTransObjClass)instance->base.layer_class;


	if(_NhlIsTransObj(instance)) {	
		if(n>0) return (*i_class->trobj_class.win_to_ndc)(
					instance, x,y,n,xout,yout,
					xmissing,ymissing,istrans);
		return NhlNOERROR;
	}

	NhlPError(NhlFATAL,NhlEUNKNOWN,
		"%s: Transformation is wrong class",func);
	return(NhlFATAL);
}


NhlErrorTypes 	_NhlNDCToWin
#if  NhlNeedProto
(NhlLayer 	instance,float *x,float *y,int n, float *xout,
	float *yout,int *istrans,float *xmissing,float *ymissing)
#else
(instance,x,y,n,xout,yout,istrans,xmissing,ymissing)
	NhlLayer 	instance;
	float *x;
	float *y;
	int 	n;
	float *xout;
	float *yout;
	int *istrans;
	float *xmissing;
	float *ymissing;
#endif
{	
	char			func[]="_NhlNDCToWin";
	NhlTransObjClass	i_class ;

	i_class = (NhlTransObjClass)instance->base.layer_class;


	if(_NhlIsTransObj(instance)){
		if(n>0) return (*i_class->trobj_class.ndc_to_win)(
					instance, x,y,n,xout,yout,
					xmissing,ymissing,istrans);
		return NhlNOERROR;
	}

	NhlPError(NhlFATAL,NhlEUNKNOWN,
		"%s: Transformation is wrong class",func);
	return(NhlFATAL);
}

NhlErrorTypes 	_NhlDataToCompc
#if  NhlNeedProto
(NhlLayer 	instance,float *x,float *y,int n, float *xout,
	float *yout, int *istrans,float* xmissing,float* ymissing)
#else
(instance,x,y,n,xout,yout,istrans,xmissing,ymissing)
	NhlLayer 	instance;
	float *x;
	float *y;
	int 	n;
	float *xout;
	float *yout;
	int *istrans;
	float *xmissing;
	float *ymissing;
#endif
{	
	char			func[]="_NhlDataToCompc";
	NhlTransObjClass	i_class ;

	i_class = (NhlTransObjClass)instance->base.layer_class;


	if(_NhlIsTransObj(instance)){
		if(n>0) return (*i_class->trobj_class.data_to_compc)(
					instance, x,y,n,xout,yout,
					xmissing,ymissing,istrans);
		return NhlNOERROR;
	}

	NhlPError(NhlFATAL,NhlEUNKNOWN,
		"%s: Transformation is wrong class",func);
	return(NhlFATAL);
}


NhlErrorTypes 	_NhlCompcToData
#if  NhlNeedProto
(NhlLayer 	instance,float *x,float *y,int n, float *xout,
	float *yout,int *istrans,float* xmissing, float* ymissing)
#else
(instance,x,y,n,xout,yout,istrans,xmissing,ymissing)
	NhlLayer 	instance;
	float *x;
	float *y;
	int 	n;
	float *xout;
	float *yout;
	int *istrans;
	float *xmissing;
	float *ymissing;
#endif
{	
	char			func[]="_NhlCompcToData";
	NhlTransObjClass	i_class ;

	i_class = (NhlTransObjClass)instance->base.layer_class;


	if(_NhlIsTransObj(instance)){
		if(n>0) return (*i_class->trobj_class.compc_to_data)(
					instance, x,y,n,xout,yout,
					xmissing,ymissing,istrans);
		return NhlNOERROR;
	}

	NhlPError(NhlFATAL,NhlEUNKNOWN,
		"%s: Transformation is wrong class",func);
	return(NhlFATAL);
}


NhlErrorTypes 	_NhlDataToWin
#if  NhlNeedProto
(NhlLayer 	instance,float *x,float *y,int n, float *xout,
	float *yout,int *istrans,float *xmissing, float *ymissing)
#else
(instance,x,y,n,xout,yout,istrans,xmissing,ymissing)
	NhlLayer 	instance;
	float *x;
	float *y;
	int 	n;
	float *xout;
	float *yout;	
	int *istrans;
	float *xmissing;
	float *ymissing;
#endif
{	
	char			func[]="_NhlDataToWin";
	NhlTransObjClass	i_class ;

	i_class = (NhlTransObjClass)instance->base.layer_class;


	if(_NhlIsTransObj(instance)){
		if(n>0) return (*i_class->trobj_class.data_to_win)(
					instance, x,y,n,xout,yout,
					xmissing,ymissing,istrans);
		return NhlNOERROR;
	}

	NhlPError(NhlFATAL,NhlEUNKNOWN,
		"%s: Transformation is wrong class",func);
	return(NhlFATAL);
}

NhlErrorTypes 	_NhlWinToData
#if  NhlNeedProto
(NhlLayer 	instance,float *x,float *y,int n, float *xout,
	float *yout, int * istrans,float *xmissing, float *ymissing)
#else
(instance,x,y,n,xout,yout,istrans,xmissing,ymissing)
	NhlLayer 	instance;
	float *x;
	float *y;
	int 	n;
	float *xout;
	float *yout;
	int *istrans;
	float *xmissing;
	float *ymissing;
#endif
{	
	char			func[]="_NhlWinToData";
	NhlTransObjClass	i_class ;

	i_class = (NhlTransObjClass)instance->base.layer_class;


	if(_NhlIsTransObj(instance)){
		if(n>0) return (*i_class->trobj_class.win_to_data)(
					instance, x,y,n,xout,yout,
					xmissing,ymissing,istrans);
		return NhlNOERROR;
	}

	NhlPError(NhlFATAL,NhlEUNKNOWN,
		"%s: Transformation is wrong class",func);
	return(NhlFATAL);
}

NhlErrorTypes 	_NhlWinToCompc
#if  NhlNeedProto
(NhlLayer 	instance,float *x,float *y,int n, float *xout,
	float *yout,int *istrans,float *xmissing, float *ymissing)
#else
(instance,x,y,n,xout,yout,istrans,xmissing,ymissing)
	NhlLayer 	instance;
	float *x;
	float *y;
	int 	n;
	float *xout;
	float *yout;
	int *istrans;
	float *xmissing;
	float *ymissing;
#endif
{	
	char			func[]="_NhlWinToCompc";
	NhlTransObjClass	i_class ;

	i_class = (NhlTransObjClass)instance->base.layer_class;


	if(_NhlIsTransObj(instance)){
		if(n>0) return (*i_class->trobj_class.win_to_compc)(
					instance, x,y,n,xout,yout,
					xmissing,ymissing,istrans);
		return NhlNOERROR;
	}

	NhlPError(NhlFATAL,NhlEUNKNOWN,
		"%s: Transformation is wrong class",func);
	return(NhlFATAL);
}

NhlErrorTypes 	_NhlCompcToWin
#if  NhlNeedProto
(NhlLayer 	instance,float *x,float *y,int n, float *xout,
	float *yout,int *istrans,float *xmissing,float *ymissing)
#else
(instance,x,y,n,xout,yout,istrans,xmissing,ymissing)
	NhlLayer 	instance;
	float *x;
	float *y;
	int 	n;
	float *xout;
	float *yout;
	int *istrans;
	float *xmissing;
	float *ymissing;
#endif
{	
	char			func[]="_NhlCompcToWin";
	NhlTransObjClass	i_class ;

	i_class = (NhlTransObjClass)instance->base.layer_class;


	if(_NhlIsTransObj(instance)){
		if(n>0) return (*i_class->trobj_class.compc_to_win)(
					instance, x,y,n,xout,yout,
					xmissing,ymissing,istrans);
		return NhlNOERROR;
	}

	NhlPError(NhlFATAL,NhlEUNKNOWN,
		"%s: Transformation is wrong class",func);
	return(NhlFATAL);
}


NhlErrorTypes 	_NhlSetTrans
#if  NhlNeedProto
(NhlLayer instance,NhlLayer  parent)
#else
(instance,parent)
	NhlLayer instance;
	NhlLayer parent;
#endif
{
	char			func[] = "_NhlSetTrans";
	NhlTransObjClass	i_class ;

	if(!_NhlIsTransObj(instance) || !_NhlIsView(parent)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
					"%s:Called with invalid args",func);
		return NhlFATAL;
	}

	i_class = (NhlTransObjClass)instance->base.layer_class;
	if(i_class->trobj_class.set_trans != NULL)
		return (*i_class->trobj_class.set_trans)(instance,parent);
/*
* ERROR No SetTrans function to call
*/
	return NhlWARNING;
}

NhlErrorTypes NhlDataToNDC
#if NhlNeedProto
(
	int	pid,
	float	*x,
	float	*y,
	int	n,
	float	*xout,
	float	*yout,
	float	*xmissing,
	float	*ymissing,
	int	*status,
	float	*oor 
)
#else 
(pid,x,y,n,xout,yout,xmissing,ymissing,status,oor)
	int	pid;
	float	*x;
	float	*y;
	int	n;
	float	*xout;
	float	*yout;
	float	*xmissing;
	float	*ymissing;
	int	*status;
	float	*oor;
#endif
{
	NhlLayer		tl = _NhlGetLayer(pid);
	NhlTransformClass	tc;

	if(tl && _NhlIsTransform(tl)) {
		tc = (NhlTransformClass)tl->base.layer_class;

		return (*tc->trans_class.data_to_ndc)
			(tl,x,y,n,xout,yout,xmissing,ymissing,status,oor);
	}


	NhlPError(NhlFATAL,NhlEUNKNOWN,
		"NhlDataToNDC:called with invalid object");
	return NhlFATAL;
}

/*
 * Function:	nhl_fdatatondc
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
void _NHLCALLF(nhl_fdatatondc,NHL_FDATATONDC)
#if	NhlNeedProto
(
	int	*pid,
	float	*x,
	float	*y,
	int	*n,
	float	*xout,
	float	*yout,
	float	*xmiss,
	float	*ymiss,
	int	*usexmiss,
	int	*useymiss,
	int	*status,
	float	*out_of_range,
	int	*err
)
#else
(pid,x,y,n,xout,yout,xmiss,ymiss,usexmiss,useymiss,status,out_of_range,err)
	int	*pid;
	float	*x;
	float	*y;
	int	*n;
	float	*xout;
	float	*yout;
	float	*xmiss;
	float	*ymiss;
	int	*usexmiss;
	int	*useymiss;
	int	*status;
	float	*out_of_range;
	int	*err;
#endif
{
	*err = NhlDataToNDC(*pid,x,y,*n,xout,yout,((*usexmiss)?xmiss:NULL),
		((*useymiss)?ymiss:NULL),status,out_of_range);

	return;
}

NhlErrorTypes NhlNDCToData
#if NhlNeedProto
(
	int	pid,
	float	*x,
	float	*y,
	int	n,
	float	*xout,
	float	*yout,
	float	*xmissing,
	float	*ymissing,
	int	*status,
	float	*oor
)
#else 
(pid,x,y,n,xout,yout,xmissing,ymissing,status,oor)
	int pid;
	float *x;
	float *y;
	int n;
	float *xout;
	float *yout;
	float *xmissing;
	float *ymissing;
	int *status;
	float *oor;
#endif
{
	NhlLayer		tl = _NhlGetLayer(pid);
	NhlTransformClass	tc;

	if(tl && _NhlIsTransform(tl)) {
		tc = (NhlTransformClass)tl->base.layer_class;

		return (*tc->trans_class.ndc_to_data)
			(tl,x,y,n,xout,yout,xmissing,ymissing,status,oor);
	}


	NhlPError(NhlFATAL,NhlEUNKNOWN,
		"NhlNDCToData:called with invalid object");
	return NhlFATAL;
}

/*
 * Function:	nhl_fndctodata
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
void _NHLCALLF(nhl_fndctodata,NHL_FNDCTODATA)
#if	NhlNeedProto
(
	int	*pid,
	float	*x,
	float	*y,
	int	*n,
	float	*xout,
	float	*yout,
	float	*xmiss,
	float	*ymiss,
	int	*usexmiss,
	int	*useymiss,
	int	*status,
	float	*out_of_range,
	int	*err
)
#else
(pid,x,y,n,xout,yout,xmiss,ymiss,usexmiss,useymiss,status,out_of_range,err)
	int	*pid;
	float	*x;
	float	*y;
	int	*n;
	float	*xout;
	float	*yout;
	float	*xmiss;
	float	*ymiss;
	int	*usexmiss;
	int	*useymiss;
	int	*status;
	float	*out_of_range;
	int	*err;
#endif
{
	*err = NhlNDCToData(*pid,x,y,*n,xout,yout,((*usexmiss)?xmiss:NULL),
		((*useymiss)?ymiss:NULL),status,out_of_range);

	return;
}

NhlErrorTypes NhlDataPolyline
#if NhlNeedProto
(int pid, float *x,float *y, int n)
#else 
(pid,x,y,n)
	int pid;
	float *x;
	float *y;
	int n;
#endif
{
	NhlLayer		tl = _NhlGetLayer(pid);
	NhlTransformClass	tc;

	if(tl && _NhlIsTransform(tl)) {
		tc = (NhlTransformClass)tl->base.layer_class;

		NhlVASetValues(tl->base.wkptr->base.id,
			_NhlNwkSetPublic,	True,
			NULL);

		return (*tc->trans_class.data_polyline)(tl,x,y,n);
	}


	NhlPError(NhlFATAL,NhlEUNKNOWN,
		"NhlDataPolyline:called with invalid object");
	return NhlFATAL;
}

/*
 * Function:	nhl_fdatapolyline
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
void _NHLCALLF(nhl_fdatapolyline,NHL_FDATAPOLYLINE)
#if	NhlNeedProto
(
	int	*pid,
	float	*x,
	float	*y,
	int	*n,
	int	*err
)
#else
(pid,x,y,n,err)
	int	*pid;
	float	*x;
	float	*y;
	int	*n;
	int	*err;
#endif
{
	*err = NhlDataPolyline(*pid,x,y,*n);

	return;
}


NhlErrorTypes NhlNDCPolyline
#if NhlNeedProto
(
	int	pid,
	float	*x,
	float	*y,
	int	n
)
#else 
(pid,x,y,n)
	int pid;
	float *x;
	float *y;
	int n;
#endif
{
	NhlLayer		tl = _NhlGetLayer(pid);
	NhlTransformClass	tc;

	if(tl && _NhlIsTransform(tl)) {
		tc = (NhlTransformClass)tl->base.layer_class;

		NhlVASetValues(tl->base.wkptr->base.id,
			_NhlNwkSetPublic,	True,
			NULL);

		return (*tc->trans_class.ndc_polyline)(tl,x,y,n);
	}


	NhlPError(NhlFATAL,NhlEUNKNOWN,
		"NhlNDCPolyline:called with invalid object");
	return NhlFATAL;
}

/*
 * Function:	nhl_fndcpolyline
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
void _NHLCALLF(nhl_fndcpolyline,NHL_FNDCPOLYLINE)
#if	NhlNeedProto
(
	int	*pid,
	float	*x,
	float	*y,
	int	*n,
	int	*err
)
#else
(pid,x,y,n,err)
	int	*pid;
	float	*x;
	float	*y;
	int	*n;
	int	*err;
#endif
{
	*err = NhlNDCPolyline(*pid,x,y,*n);

	return;
}
