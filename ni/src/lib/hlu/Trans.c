/*
 *      $Id: Trans.c,v 1.9 1994-12-16 20:04:50 boote Exp $
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
 *			NhlTransformLayerClass. However it may not always be
 *			needed in the methods of the NhlTransObjLayerClass.<-----
 *
 */

#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/TransObjP.h>
#include <ncarg/hlu/TransformP.h>


static NhlErrorTypes CallDataPolyline(
#if	NhlNeedProto
	NhlLayer layer,
	NhlLayerClass class,
	float	*x,
	float 	*y,
	int	n
#endif
);

static NhlErrorTypes CallNDCPolyline(
#if	NhlNeedProto
	NhlLayer layer,
	NhlLayerClass class,
	float	*x,
	float 	*y,
	int	n
#endif
);

NhlErrorTypes 	_NhlWinToNDC
#if  NhlNeedProto
(NhlLayer instance, NhlLayer parent,float *x,float *y,int n, float *xout,
	float *yout,int *istrans,float* xmissing, float* ymissing)
#else
(instance,parent,x,y,n,xout,yout,istrans,xmissing,ymissing)
	NhlLayer	instance;
	NhlLayer	parent;
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
	NhlTransObjLayerClass i_class ;
	NhlErrorTypes ret = NhlNOERROR;

	i_class = (NhlTransObjLayerClass)instance->base.layer_class;


	if((_NhlIsTransObj(instance))&&(_NhlIsView(parent))) {	
		while(i_class != (NhlTransObjLayerClass)NhltransObjLayerClass) {
			if( i_class->trobj_class.win_to_ndc != NULL ) {	
				if(n>0)
				ret = (*i_class->trobj_class.win_to_ndc)(
					instance, parent, x,y,n,xout,yout,
					xmissing,ymissing,istrans);
				return(ret);
			} else {
				i_class = (NhlTransObjLayerClass)
						i_class->base_class.superclass;
			}
		}
		return(ret);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"_NhlWinToNDC: Either transformation or parent of wrong class");
		return(NhlFATAL);
	}
}


NhlErrorTypes 	_NhlNDCToWin
#if  NhlNeedProto
(NhlLayer 	instance,NhlLayer parent,float *x,float *y,int n, float *xout,
	float *yout,int *istrans,float *xmissing,float *ymissing)
#else
(instance,parent,x,y,n,xout,yout,istrans,xmissing,ymissing)
	NhlLayer 	instance;
	NhlLayer 	parent;
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
	NhlTransObjLayerClass i_class ;
	NhlErrorTypes ret = NhlNOERROR;

	i_class = (NhlTransObjLayerClass)instance->base.layer_class;


	if((_NhlIsTransObj(instance))&&(_NhlIsView(parent))) {	
		while(i_class != (NhlTransObjLayerClass)NhltransObjLayerClass) {
			if( i_class->trobj_class.ndc_to_win != NULL ) {	
				if(n>0)
				ret = (*i_class->trobj_class.ndc_to_win)(
					instance, parent, x,y,n,xout,yout,
					xmissing,ymissing,istrans);
				return(ret);
			} else {
				i_class = (NhlTransObjLayerClass)
						i_class->base_class.superclass;
			}
		}
		return(ret);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"_NhlNDCToWin: Either transformation or parent of wrong class");
		return(NhlFATAL);
	}
}

NhlErrorTypes 	_NhlDataToCompc
#if  NhlNeedProto
(NhlLayer 	instance,NhlLayer parent,float *x,float *y,int n, float *xout,
	float *yout, int *istrans,float* xmissing,float* ymissing)
#else
(instance,parent,x,y,n,xout,yout,istrans,xmissing,ymissing)
	NhlLayer 	instance;
	NhlLayer 	parent;
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
	NhlTransObjLayerClass i_class ;
	NhlErrorTypes ret = NhlNOERROR;

	i_class = (NhlTransObjLayerClass)instance->base.layer_class;


	if((_NhlIsTransObj(instance))&&(_NhlIsView(parent))) {	
		while(i_class != (NhlTransObjLayerClass)NhltransObjLayerClass) {
			if( i_class->trobj_class.data_to_compc != NULL ) {	
				if(n>0)
				ret = (*i_class->trobj_class.data_to_compc)(
					instance, parent, x,y,n,xout,yout,
					xmissing,ymissing,istrans);
				return(ret);
			} else {
				i_class = (NhlTransObjLayerClass)
						i_class->base_class.superclass;
			}
		}
		return(ret);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"_NhlNDCToWin: Either transformation or parent of wrong class");
		return(NhlFATAL);
	}
}



NhlErrorTypes 	_NhlCompcToData
#if  NhlNeedProto
(NhlLayer 	instance,NhlLayer parent,float *x,float *y,int n, float *xout,
	float *yout,int *istrans,float* xmissing, float* ymissing)
#else
(instance,parent,x,y,n,xout,yout,istrans,xmissing,ymissing)
	NhlLayer 	instance;
	NhlLayer 	parent;
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
	NhlTransObjLayerClass i_class ;
	NhlErrorTypes ret = NhlNOERROR;

	i_class = (NhlTransObjLayerClass)instance->base.layer_class;


	if((_NhlIsTransObj(instance))&&(_NhlIsView(parent))) {	
		while(i_class != (NhlTransObjLayerClass)NhltransObjLayerClass) {
			if( i_class->trobj_class.compc_to_data != NULL ) {	
				if(n>0)
				ret = (*i_class->trobj_class.compc_to_data)(
					instance, parent, x,y,n,xout,yout,
					xmissing,ymissing,istrans);
				return(ret);
			} else {
				i_class = (NhlTransObjLayerClass)
						i_class->base_class.superclass;
			}
		}
		return(ret);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"_NhlNDCToWin: Either transformation or parent of wrong class");
		return(NhlFATAL);
	}
}



NhlErrorTypes 	_NhlDataToWin
#if  NhlNeedProto
(NhlLayer 	instance,NhlLayer parent,float *x,float *y,int n, float *xout,
	float *yout,int *istrans,float *xmissing, float *ymissing)
#else
(instance,parent,x,y,n,xout,yout,istrans,xmissing,ymissing)
	NhlLayer 	instance;
	NhlLayer 	parent;
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
	NhlTransObjLayerClass i_class ;
	NhlErrorTypes ret = NhlNOERROR;

	i_class = (NhlTransObjLayerClass)instance->base.layer_class;


	if((_NhlIsTransObj(instance))&&(_NhlIsView(parent))) {	
		while(i_class != (NhlTransObjLayerClass)NhltransObjLayerClass) {
			if( i_class->trobj_class.data_to_win != NULL ) {	
				if(n>0)
				ret = (*i_class->trobj_class.data_to_win)(
					instance, parent, x,y,n,xout,yout,
					xmissing,ymissing,istrans);
				return(ret);
			} else {
				i_class = (NhlTransObjLayerClass)
						i_class->base_class.superclass;
			}
		}
		return(ret);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"_NhlNDCToWin: Either transformation or parent of wrong class");
		return(NhlFATAL);
	}
}




NhlErrorTypes 	_NhlWinToData
#if  NhlNeedProto
(NhlLayer 	instance,NhlLayer parent,float *x,float *y,int n, float *xout,
	float *yout, int * istrans,float *xmissing, float *ymissing)
#else
(instance,parent,x,y,n,xout,yout,istrans,xmissing,ymissing)
	NhlLayer 	instance;
	NhlLayer 	parent;
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
	NhlTransObjLayerClass i_class ;
	NhlErrorTypes ret = NhlNOERROR;

	i_class = (NhlTransObjLayerClass)instance->base.layer_class;


	if((_NhlIsTransObj(instance))&&(_NhlIsView(parent))) {	
		while(i_class != (NhlTransObjLayerClass)NhltransObjLayerClass) {
			if( i_class->trobj_class.win_to_data!= NULL ) {	
				if(n>0)
				ret = (*i_class->trobj_class.win_to_data)(
					instance, parent, x,y,n,xout,yout,
					xmissing,ymissing,istrans);
				return(ret);
			} else {
				i_class = (NhlTransObjLayerClass)
						i_class->base_class.superclass;
			}
		}
		return(ret);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"_NhlNDCToWin: Either transformation or parent of wrong class");
		return(NhlFATAL);
	}
}




NhlErrorTypes 	_NhlWinToCompc
#if  NhlNeedProto
(NhlLayer 	instance,NhlLayer parent,float *x,float *y,int n, float *xout,
	float *yout,int *istrans,float *xmissing, float *ymissing)
#else
(instance,parent,x,y,n,xout,yout,istrans,xmissing,ymissing)
	NhlLayer 	instance;
	NhlLayer 	parent;
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
	NhlTransObjLayerClass i_class ;
	NhlErrorTypes ret = NhlNOERROR;

	i_class = (NhlTransObjLayerClass)instance->base.layer_class;


	if((_NhlIsTransObj(instance))&&(_NhlIsView(parent))) {	
		while(i_class != (NhlTransObjLayerClass)NhltransObjLayerClass) {
			if( i_class->trobj_class.win_to_compc!= NULL ) {	
				if(n>0)
				ret = (*i_class->trobj_class.win_to_compc)(
					instance, parent, x,y,n,xout,yout,
					xmissing,ymissing,istrans);
				return(ret);
			} else {
				i_class = (NhlTransObjLayerClass)
						i_class->base_class.superclass;
			}
		}
		return(ret);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"_NhlNDCToWin: Either transformation or parent of wrong class");
		return(NhlFATAL);
	}
}



NhlErrorTypes 	_NhlCompcToWin
#if  NhlNeedProto
(NhlLayer 	instance,NhlLayer parent,float *x,float *y,int n, float *xout,
	float *yout,int *istrans,float *xmissing,float *ymissing)
#else
(instance,parent,x,y,n,xout,yout,istrans,xmissing,ymissing)
	NhlLayer 	instance;
	NhlLayer 	parent;
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
	NhlTransObjLayerClass i_class ;
	NhlErrorTypes ret = NhlNOERROR;

	i_class = (NhlTransObjLayerClass)instance->base.layer_class;


	if((_NhlIsTransObj(instance))&&(_NhlIsView(parent))) {	
		while(i_class != (NhlTransObjLayerClass)NhltransObjLayerClass) {
			if( i_class->trobj_class.compc_to_win!= NULL ) {	
				if(n>0)
				ret = (*i_class->trobj_class.compc_to_win)(
					instance, parent, x,y,n,xout,yout,
					xmissing,ymissing,istrans);
				return(ret);
			} else {
				i_class = (NhlTransObjLayerClass)
						i_class->base_class.superclass;
			}
		}
		return(ret);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"_NhlNDCToWin: Either transformation or parent of wrong class");
		return(NhlFATAL);
	}
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
	NhlTransObjLayerClass i_class ;
	NhlErrorTypes ret = NhlNOERROR;

	i_class = (NhlTransObjLayerClass)instance->base.layer_class;

	if(i_class->trobj_class.set_trans != NULL) {
		ret = (*i_class->trobj_class.set_trans)(instance,parent);
	} else {
/*
* ERROR No SetTrans function to call
*/
		ret = NhlWARNING;
	}
	return(ret);
	
}
static NhlErrorTypes CallDataToNDC
#if NhlNeedProto
(NhlLayer layer, NhlLayerClass class,float *x,float *y,int n, float *xout,float *yout,float *xmissing,float *ymissing,int *status,float* out_of_range)
#else 
(layer,class,x,y,n,xout,yout,xmissing,ymissing,status,out_of_range)
	NhlLayer layer;
	NhlLayerClass class;
	float	*x;
	float 	*y;
	int	n;
	float	*xout;
	float 	*yout;
	float   *xmissing;
	float 	*ymissing;
	int	*status;
	float	*out_of_range;
#endif
{
	NhlTransformLayerClass tclass = (NhlTransformLayerClass) class;
/*
* This is a call first method
*/
	if (class != NhlviewLayerClass) {
		if(tclass->trans_class.data_to_ndc != NULL) 
			return((*tclass->trans_class.data_to_ndc)(layer,x,y,n,xout,yout,xmissing,ymissing,status,out_of_range));
		else 
			return(CallDataToNDC(layer,tclass->base_class.superclass,
				x,y,n,xout,yout,xmissing,ymissing,status,out_of_range));
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"No Transformation function registered for plot class");
		
		return(NhlFATAL);
	}
}

NhlErrorTypes NhlDataToNDC
#if NhlNeedProto
(int pid, float *x,float *y, int n, float *xout, float *yout,float *xmissing,float *ymissing,int* status,float* out_of_range)
#else 
(pid,x,y,n,xout,yout,xmissing,ymissing,status,out_of_range)
	int pid;
	float *x;
	float *y;
	int n;
	float *xout;
	float *yout;
	float *xmissing;
	float *ymissing;
	int * status;
	float* out_of_range;
#endif
{
	NhlTransformLayer tlayer = (NhlTransformLayer)_NhlGetLayer(pid);
	NhlErrorTypes ret = NhlNOERROR;

	if(_NhlIsTransform(tlayer)) {
		ret = CallDataToNDC((NhlLayer)tlayer,tlayer->base.layer_class,
				x,y,n,xout,yout,xmissing,ymissing,status,out_of_range);
		
	} else {
/*
* ERROR : wrong plot class passed to  NhlDataToNDC
*/
		NhlPError(NhlFATAL,NhlEUNKNOWN,"NhlDataToNDC has been passed an object that can't perform the requested transformation");
		return(NhlFATAL);
	}
	return(ret);
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

static NhlErrorTypes CallNDCToData
#if NhlNeedProto
(NhlLayer layer, NhlLayerClass class,float *x,float *y,int n, float *xout,float *yout,float *xmissing,float *ymissing,int *status,float *out_of_range)
#else 
(layer,class,x,y,n,xout,yout,xmissing,ymissing,status,out_of_range)
	NhlLayer layer;
	NhlLayerClass class;
	float	*x;
	float 	*y;
	int	n;
	float	*xout;
	float 	*yout;
	float 	*xmissing;
	float 	*ymissing;
	int 	*status;
	float	*out_of_range;
#endif
{
	NhlTransformLayerClass tclass = (NhlTransformLayerClass) class;
/*
* This is a call first method
*/
	if (class != NhlviewLayerClass) {
		if(tclass->trans_class.ndc_to_data != NULL) 
			return((*tclass->trans_class.ndc_to_data)(layer,x,y,n,xout,yout,xmissing,ymissing,status,out_of_range));
		else 
			return(CallNDCToData(layer,tclass->base_class.superclass,
				x,y,n,xout,yout,xmissing,ymissing,status,out_of_range));
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"No Transformation function registered for plot class");
		
		return(NhlFATAL);
	}
}

NhlErrorTypes NhlNDCToData
#if NhlNeedProto
(int pid, float *x,float *y, int n, float *xout, float *yout,float *xmissing,float *ymissing,int* status,float* out_of_range)
#else 
(pid,x,y,n,xout,yout,xmissing,ymissing,status,out_of_range)
	int pid;
	float *x;
	float *y;
	int n;
	float *xout;
	float *yout;
	float *xmissing;
	float *ymissing;
	int *status;
	float *out_of_range;
#endif
{
	NhlTransformLayer tlayer = (NhlTransformLayer)_NhlGetLayer(pid);

	if(!_NhlIsTransform(tlayer)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
"NhlNDCToData has been passed an object that can't perform the requested transformation");
		return(NhlFATAL);
	}

	return CallNDCToData((NhlLayer)tlayer,tlayer->base.layer_class,
			x,y,n,xout,yout,xmissing,ymissing,status,out_of_range);
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
	NhlTransformLayer		tl = (NhlTransformLayer)_NhlGetLayer(pid);
	NhlErrorTypes		ret = NhlNOERROR;
	char			*e_text;
	char			*entry_name = "NhlDataPolyline";


	if (tl == NULL) {
		e_text = "%s: invalid object id"; 
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}
	else if (_NhlIsTransform(tl)) {

		ret = CallDataPolyline((NhlLayer)tl,tl->base.layer_class,x,y,n);

	} else {
		e_text = "%s: unsupported method for this object"; 
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}

	return(ret);
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

static NhlErrorTypes CallDataPolyline
#if NhlNeedProto
(NhlLayer layer, NhlLayerClass class,float *x,float *y,int n)
#else 
(layer,class,x,y,n)
	NhlLayer layer;
	NhlLayerClass class;
	float	*x;
	float 	*y;
	int	n;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;
	NhlTransformLayerClass 	tclass = (NhlTransformLayerClass) class;
	char			*e_text;
	char			*entry_name = "NhlDataPolyline";

	if (class != NhlviewLayerClass) {
		if (tclass->trans_class.data_polyline != NULL) 
			ret = (*tclass->trans_class.data_polyline)
				(layer,x,y,n);
		else 
			ret = CallDataPolyline(layer,
					       tclass->base_class.superclass,
					       x,y,n);
	} else {
		e_text = "%s: method not registered for this plot class";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}

	return ret;
}


NhlErrorTypes NhlNDCPolyline
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
	NhlTransformLayer		tl = (NhlTransformLayer)_NhlGetLayer(pid);
	NhlErrorTypes		ret = NhlNOERROR;
	char			*e_text;
	char			*entry_name = "NhlNDCPolyline";


	if (tl == NULL) {
		e_text = "%s: invalid object id"; 
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}
	else if (_NhlIsTransform(tl)) {

		ret = CallNDCPolyline((NhlLayer)tl,tl->base.layer_class,x,y,n);

	} else {
		e_text = "%s: unsupported method for this object"; 
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}

	return(ret);
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

static NhlErrorTypes CallNDCPolyline
#if NhlNeedProto
(NhlLayer layer, NhlLayerClass class,float *x,float *y,int n)
#else 
(layer,class,x,y,n)
	NhlLayer layer;
	NhlLayerClass class;
	float	*x;
	float 	*y;
	int	n;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;
	NhlTransformLayerClass 	tclass = (NhlTransformLayerClass) class;
	char			*e_text;
	char			*entry_name = "NhlNDCPolyline";

	if (class != NhlviewLayerClass) {
		if (tclass->trans_class.ndc_polyline != NULL) 
			ret = (*tclass->trans_class.ndc_polyline)
				(layer,x,y,n);
		else 
			ret = CallNDCPolyline(layer,
					      tclass->base_class.superclass,
					      x,y,n);
	} else {
		e_text = "%s: method not registered for this plot class";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}
	
	return ret;
}
