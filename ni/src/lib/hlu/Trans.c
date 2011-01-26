/*
 *      $Id: Trans.c,v 1.18 2000-06-29 01:45:11 dbrown Exp $
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
 * Function:	nhlpfdatatondc
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
void _NHLCALLF(nhlpfdatatondc,NHLPFDATATONDC)
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
 * Function:	nhlpfndctodata
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
void _NHLCALLF(nhlpfndctodata,NHLPFNDCTODATA)
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
(int pid,int gsid,float *x,float *y, int n)
#else 
(pid,gsid,x,y,n)
	int pid;
	int gsid;
	float *x;
	float *y;
	int n;
#endif
{
	NhlTransformClass	tc;
	NhlLayer		tl = _NhlGetLayer(pid);

        if (tl && _NhlIsWorkstation(tl)) {
                tl = _NhlDefaultPlot(tl);
        }
	if(tl && _NhlIsTransform(tl)) {
		NhlErrorTypes	ret;
		NhlTransformLayer tfl = (NhlTransformLayer)tl;
		tc = (NhlTransformClass)tl->base.layer_class;

		if (gsid == NhlNULLOBJID) {
			ret = NhlVAGetValues
				(tl->base.wkptr->base.id,
				 NhlNwkDefGraphicStyleId,&gsid,
				 NULL);
			if (ret < NhlWARNING)
				return ret;
		}
		ret = NhlVASetValues(tl->base.wkptr->base.id,
			_NhlNwkGraphicStyle,	gsid,
			NULL);
		if (ret < NhlWARNING)
			return ret;

		NhlVAGetValues(gsid,
			       NhlNgsClipOn,&tfl->trans.poly_clip_on,
			       NULL);
		return (*tc->trans_class.data_polyline)(tl,x,y,n);
	}


	NhlPError(NhlFATAL,NhlEUNKNOWN,
		"NhlDataPolyline:called with invalid object");
	return NhlFATAL;
}

/*
 * Function:	nhlpfdatapolyline
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
void _NHLCALLF(nhlpfdatapolyline,NHLPFDATAPOLYLINE)
#if	NhlNeedProto
(
	int	*pid,
	int     *gsid,
	float	*x,
	float	*y,
	int	*n,
	int	*err
)
#else
(pid,gsid,x,y,n,err)
	int	*pid;
	int	*gsid;
	float	*x;
	float	*y;
	int	*n;
	int	*err;
#endif
{
	*err = NhlDataPolyline(*pid,*gsid,x,y,*n);

	return;
}


NhlErrorTypes NhlNDCPolyline
#if NhlNeedProto
(
	int	pid,
	int	gsid,
	float	*x,
	float	*y,
	int	n
)
#else 
(pid,gsid,x,y,n)
	int pid;
	int gsid;
	float *x;
	float *y;
	int n;
#endif
{
	NhlLayer		tl = _NhlGetLayer(pid);
	NhlTransformClass	tc;

        if (tl && _NhlIsWorkstation(tl)) {
                tl = _NhlDefaultPlot(tl);
        }
	if(tl && _NhlIsTransform(tl)) {
		NhlErrorTypes	ret;
		NhlTransformLayer tfl = (NhlTransformLayer)tl;
		tc = (NhlTransformClass)tl->base.layer_class;

		if (gsid == NhlNULLOBJID) {
			ret = NhlVAGetValues
				(tl->base.wkptr->base.id,
				 NhlNwkDefGraphicStyleId,&gsid,
				 NULL);
			if (ret < NhlWARNING)
				return ret;
		}
		ret = NhlVASetValues(tl->base.wkptr->base.id,
			_NhlNwkGraphicStyle,	gsid,
			NULL);
		if (ret < NhlWARNING)
			return ret;

		NhlVAGetValues(gsid,
			       NhlNgsClipOn,&tfl->trans.poly_clip_on,
			       NULL);

		return (*tc->trans_class.ndc_polyline)(tl,x,y,n);
	}


	NhlPError(NhlFATAL,NhlEUNKNOWN,
		"NhlNDCPolyline:called with invalid object");
	return NhlFATAL;
}

/*
 * Function:	nhlpfndcpolyline
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
void _NHLCALLF(nhlpfndcpolyline,NHLPFNDCPOLYLINE)
#if	NhlNeedProto
(
	int	*pid,
	int	*gsid,
	float	*x,
	float	*y,
	int	*n,
	int	*err
)
#else
(pid,gsid,x,y,n,err)
	int	*pid;
	int	*gsid;
	float	*x;
	float	*y;
	int	*n;
	int	*err;
#endif
{
	*err = NhlNDCPolyline(*pid,*gsid,x,y,*n);

	return;
}


NhlErrorTypes NhlDataPolygon
#if NhlNeedProto
(int pid,int gsid,float *x,float *y, int n)
#else 
(pid,gsid,x,y,n)
	int pid;
	int gsid;
	float *x;
	float *y;
	int n;
#endif
{
	NhlLayer		tl = _NhlGetLayer(pid);
	NhlTransformClass	tc;

        if (tl && _NhlIsWorkstation(tl)) {
                tl = _NhlDefaultPlot(tl);
        }
	if(tl && _NhlIsTransform(tl)) {
		NhlErrorTypes	ret;
		NhlTransformLayer tfl = (NhlTransformLayer)tl;
		tc = (NhlTransformClass)tl->base.layer_class;

		if (gsid == NhlNULLOBJID) {
			ret = NhlVAGetValues
				(tl->base.wkptr->base.id,
				 NhlNwkDefGraphicStyleId,&gsid,
				 NULL);
			if (ret < NhlWARNING)
				return ret;
		}
		ret = NhlVASetValues(tl->base.wkptr->base.id,
				     _NhlNwkGraphicStyle,	gsid,
				     NULL);
		if (ret < NhlWARNING)
			return ret;

		NhlVAGetValues(gsid,
			       NhlNgsClipOn,&tfl->trans.poly_clip_on,
			       NULL);
		return (*tc->trans_class.data_polygon)(tl,x,y,n);
	}


	NhlPError(NhlFATAL,NhlEUNKNOWN,
		"NhlDataPolygon:called with invalid object");
	return NhlFATAL;
}

/*
 * Function:	nhlpfdatapolygon
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
void _NHLCALLF(nhlpfdatapolygon,NHLPFDATAPOLYGON)
#if	NhlNeedProto
(
	int	*pid,
	int	*gsid,
	float	*x,
	float	*y,
	int	*n,
	int	*err
)
#else
(pid,gsid,x,y,n,err)
	int	*pid;
	int	*gsid;
	float	*x;
	float	*y;
	int	*n;
	int	*err;
#endif
{
	*err = NhlDataPolygon(*pid,*gsid,x,y,*n);

	return;
}


NhlErrorTypes NhlNDCPolygon
#if NhlNeedProto
(
	int	pid,
	int	gsid,
	float	*x,
	float	*y,
	int	n
)
#else 
(pid,gsid,x,y,n)
	int pid;
	int gsid;
	float *x;
	float *y;
	int n;
#endif
{
	NhlLayer		tl = _NhlGetLayer(pid);
	NhlTransformClass	tc;

        if (tl && _NhlIsWorkstation(tl)) {
                tl = _NhlDefaultPlot(tl);
        }
	if(tl && _NhlIsTransform(tl)) {
		NhlErrorTypes	ret;
		NhlTransformLayer tfl = (NhlTransformLayer)tl;
		tc = (NhlTransformClass)tl->base.layer_class;

		if (gsid == NhlNULLOBJID) {
			ret = NhlVAGetValues
				(tl->base.wkptr->base.id,
				 NhlNwkDefGraphicStyleId,&gsid,
				 NULL);
			if (ret < NhlWARNING)
				return ret;
		}
		ret = NhlVASetValues(tl->base.wkptr->base.id,
				     _NhlNwkGraphicStyle,	gsid,
				     NULL);
		if (ret < NhlWARNING)
			return ret;

		NhlVAGetValues(gsid,
			       NhlNgsClipOn,&tfl->trans.poly_clip_on,
			       NULL);

		return (*tc->trans_class.ndc_polygon)(tl,x,y,n);
	}


	NhlPError(NhlFATAL,NhlEUNKNOWN,
		"NhlNDCPolygon:called with invalid object");
	return NhlFATAL;
}

/*
 * Function:	nhlpfndcpolygon
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
void _NHLCALLF(nhlpfndcpolygon,NHLPFNDCPOLYGON)
#if	NhlNeedProto
(
	int	*pid,
	int	*gsid,
	float	*x,
	float	*y,
	int	*n,
	int	*err
)
#else
(pid,gsid,x,y,n,err)
	int	*pid;
	int	*gsid;
	float	*x;
	float	*y;
	int	*n;
	int	*err;
#endif
{
	*err = NhlNDCPolygon(*pid,*gsid,x,y,*n);

	return;
}


NhlErrorTypes NhlDataPolymarker
#if NhlNeedProto
(int pid,int gsid,float *x,float *y, int n)
#else 
(pid,gsid,x,y,n)
	int pid;
	int gsid;
	float *x;
	float *y;
	int n;
#endif
{
	NhlLayer		tl = _NhlGetLayer(pid);
	NhlTransformClass	tc;

        if (tl && _NhlIsWorkstation(tl)) {
                tl = _NhlDefaultPlot(tl);
        }
	if(tl && _NhlIsTransform(tl)) {
		NhlErrorTypes	ret;
		NhlTransformLayer tfl = (NhlTransformLayer)tl;
		tc = (NhlTransformClass)tl->base.layer_class;

		if (gsid == NhlNULLOBJID) {
			ret = NhlVAGetValues
				(tl->base.wkptr->base.id,
				 NhlNwkDefGraphicStyleId,&gsid,
				 NULL);
			if (ret < NhlWARNING)
				return ret;
		}
		ret = NhlVASetValues(tl->base.wkptr->base.id,
				     _NhlNwkGraphicStyle,	gsid,
				     NULL);
		if (ret < NhlWARNING)
			return ret;

		NhlVAGetValues(gsid,
			       NhlNgsClipOn,&tfl->trans.poly_clip_on,
			       NULL);

		return (*tc->trans_class.data_polymarker)(tl,x,y,n);
	}


	NhlPError(NhlFATAL,NhlEUNKNOWN,
		"NhlDataPolymarker:called with invalid object");
	return NhlFATAL;
}

/*
 * Function:	nhlpfdatapolymarker
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
void _NHLCALLF(nhlpfdatapolymarker,NHLPFDATAPOLYMARKER)
#if	NhlNeedProto
(
	int	*pid,
	int	*gsid,
	float	*x,
	float	*y,
	int	*n,
	int	*err
)
#else
(pid,gsid,x,y,n,err)
	int	*pid;
	int	*gsid;
	float	*x;
	float	*y;
	int	*n;
	int	*err;
#endif
{
	*err = NhlDataPolymarker(*pid,*gsid,x,y,*n);

	return;
}


NhlErrorTypes NhlNDCPolymarker
#if NhlNeedProto
(
	int	pid,
	int	gsid,
	float	*x,
	float	*y,
	int	n
)
#else 
(pid,gsid,x,y,n)
	int pid;
	int gsid;
	float *x;
	float *y;
	int n;
#endif
{
	NhlLayer		tl = _NhlGetLayer(pid);
	NhlTransformClass	tc;

        if (tl && _NhlIsWorkstation(tl)) {
                tl = _NhlDefaultPlot(tl);
        }
	if(tl && _NhlIsTransform(tl)) {
		NhlErrorTypes	ret;
		NhlTransformLayer tfl = (NhlTransformLayer)tl;
		tc = (NhlTransformClass)tl->base.layer_class;

		if (gsid == NhlNULLOBJID) {
			ret = NhlVAGetValues
				(tl->base.wkptr->base.id,
				 NhlNwkDefGraphicStyleId,&gsid,
				 NULL);
			if (ret < NhlWARNING)
				return ret;
		}
		ret = NhlVASetValues(tl->base.wkptr->base.id,
				     _NhlNwkGraphicStyle,	gsid,
				     NULL);
		if (ret < NhlWARNING)
			return ret;

		NhlVAGetValues(gsid,
			       NhlNgsClipOn,&tfl->trans.poly_clip_on,
			       NULL);

		return (*tc->trans_class.ndc_polymarker)(tl,x,y,n);
	}


	NhlPError(NhlFATAL,NhlEUNKNOWN,
		"NhlNDCPolymarker:called with invalid object");
	return NhlFATAL;
}

/*
 * Function:	nhlpfndcpolymarker
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
void _NHLCALLF(nhlpfndcpolymarker,NHLPFNDCPOLYMARKER)
#if	NhlNeedProto
(
	int	*pid,
	int	*gsid,
	float	*x,
	float	*y,
	int	*n,
	int	*err
)
#else
(pid,gsid,x,y,n,err)
	int	*pid;
	int	*gsid;
	float	*x;
	float	*y;
	int	*n;
	int	*err;
#endif
{
	*err = NhlNDCPolymarker(*pid,*gsid,x,y,*n);

	return;
}
