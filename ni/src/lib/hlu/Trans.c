/*
 *      $Id: Trans.c,v 1.4 1993-12-14 23:31:54 ethan Exp $
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
 *			TransformLayerClass. However it may not always be
 *			needed in the methods of the TransObjLayerClass.<-----
 *
 */

#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/TransObjP.h>
#include <ncarg/hlu/TransformP.h>


NhlErrorTypes 	_NhlWinToNDC
#if  __STDC__
(Layer instance, Layer parent,float *x,float *y,int n, float *xout,
	float *yout,int *istrans,float* xmissing, float* ymissing)
#else
(instance,parent,x,y,n,xout,yout,istrans,xmissing,ymissing)
	Layer	instance;
	Layer	parent;
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
	TransObjLayerClass i_class ;
	NhlErrorTypes ret = NOERROR;

	i_class = (TransObjLayerClass)instance->base.layer_class;


	if((_NhlIsTransObj(instance))&&(_NhlIsView(parent))) {	
		while(i_class != (TransObjLayerClass)transObjLayerClass) {
			if( i_class->trobj_class.win_to_ndc != NULL ) {	
				if(n>0)
				ret = (*i_class->trobj_class.win_to_ndc)(
					instance, parent, x,y,n,xout,yout,
					xmissing,ymissing,istrans);
				return(ret);
			} else {
				i_class = (TransObjLayerClass)
						i_class->base_class.superclass;
			}
		}
		return(ret);
	} else {
		NhlPError(FATAL,E_UNKNOWN,"_NhlWinToNDC: Either transformation or parent of wrong class");
		return(FATAL);
	}
}


NhlErrorTypes 	_NhlNDCToWin
#if  __STDC__
(Layer 	instance,Layer parent,float *x,float *y,int n, float *xout,
	float *yout,int *istrans,float *xmissing,float *ymissing)
#else
(instance,parent,x,y,n,xout,yout,istrans,xmissing,ymissing)
	Layer 	instance;
	Layer 	parent;
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
	TransObjLayerClass i_class ;
	NhlErrorTypes ret = NOERROR;

	i_class = (TransObjLayerClass)instance->base.layer_class;


	if((_NhlIsTransObj(instance))&&(_NhlIsView(parent))) {	
		while(i_class != (TransObjLayerClass)transObjLayerClass) {
			if( i_class->trobj_class.ndc_to_win != NULL ) {	
				if(n>0)
				ret = (*i_class->trobj_class.ndc_to_win)(
					instance, parent, x,y,n,xout,yout,
					xmissing,ymissing,istrans);
				return(ret);
			} else {
				i_class = (TransObjLayerClass)
						i_class->base_class.superclass;
			}
		}
		return(ret);
	} else {
		NhlPError(FATAL,E_UNKNOWN,"_NhlNDCToWin: Either transformation or parent of wrong class");
		return(FATAL);
	}
}

NhlErrorTypes 	_NhlDataToCompc
#if  __STDC__
(Layer 	instance,Layer parent,float *x,float *y,int n, float *xout,
	float *yout, int *istrans,float* xmissing,float* ymissing)
#else
(instance,parent,x,y,n,xout,yout,istrans,xmissing,ymissing)
	Layer 	instance;
	Layer 	parent;
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
	TransObjLayerClass i_class ;
	NhlErrorTypes ret = NOERROR;

	i_class = (TransObjLayerClass)instance->base.layer_class;


	if((_NhlIsTransObj(instance))&&(_NhlIsView(parent))) {	
		while(i_class != (TransObjLayerClass)transObjLayerClass) {
			if( i_class->trobj_class.data_to_compc != NULL ) {	
				if(n>0)
				ret = (*i_class->trobj_class.data_to_compc)(
					instance, parent, x,y,n,xout,yout,
					xmissing,ymissing,istrans);
				return(ret);
			} else {
				i_class = (TransObjLayerClass)
						i_class->base_class.superclass;
			}
		}
		return(ret);
	} else {
		NhlPError(FATAL,E_UNKNOWN,"_NhlNDCToWin: Either transformation or parent of wrong class");
		return(FATAL);
	}
}



NhlErrorTypes 	_NhlCompcToData
#if  __STDC__
(Layer 	instance,Layer parent,float *x,float *y,int n, float *xout,
	float *yout,int *istrans,float* xmissing, float* ymissing)
#else
(instance,parent,x,y,n,xout,yout,istrans,xmissing,ymissing)
	Layer 	instance;
	Layer 	parent;
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
	TransObjLayerClass i_class ;
	NhlErrorTypes ret = NOERROR;

	i_class = (TransObjLayerClass)instance->base.layer_class;


	if((_NhlIsTransObj(instance))&&(_NhlIsView(parent))) {	
		while(i_class != (TransObjLayerClass)transObjLayerClass) {
			if( i_class->trobj_class.compc_to_data != NULL ) {	
				if(n>0)
				ret = (*i_class->trobj_class.compc_to_data)(
					instance, parent, x,y,n,xout,yout,
					xmissing,ymissing,istrans);
				return(ret);
			} else {
				i_class = (TransObjLayerClass)
						i_class->base_class.superclass;
			}
		}
		return(ret);
	} else {
		NhlPError(FATAL,E_UNKNOWN,"_NhlNDCToWin: Either transformation or parent of wrong class");
		return(FATAL);
	}
}



NhlErrorTypes 	_NhlDataToWin
#if  __STDC__
(Layer 	instance,Layer parent,float *x,float *y,int n, float *xout,
	float *yout,int *istrans,float *xmissing, float *ymissing)
#else
(instance,parent,x,y,n,xout,yout,istrans,xmissing,ymissing)
	Layer 	instance;
	Layer 	parent;
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
	TransObjLayerClass i_class ;
	NhlErrorTypes ret = NOERROR;

	i_class = (TransObjLayerClass)instance->base.layer_class;


	if((_NhlIsTransObj(instance))&&(_NhlIsView(parent))) {	
		while(i_class != (TransObjLayerClass)transObjLayerClass) {
			if( i_class->trobj_class.data_to_win != NULL ) {	
				if(n>0)
				ret = (*i_class->trobj_class.data_to_win)(
					instance, parent, x,y,n,xout,yout,
					xmissing,ymissing,istrans);
				return(ret);
			} else {
				i_class = (TransObjLayerClass)
						i_class->base_class.superclass;
			}
		}
		return(ret);
	} else {
		NhlPError(FATAL,E_UNKNOWN,"_NhlNDCToWin: Either transformation or parent of wrong class");
		return(FATAL);
	}
}




NhlErrorTypes 	_NhlWinToData
#if  __STDC__
(Layer 	instance,Layer parent,float *x,float *y,int n, float *xout,
	float *yout, int * istrans,float *xmissing, float *ymissing)
#else
(instance,parent,x,y,n,xout,yout,istrans,xmissing,ymissing)
	Layer 	instance;
	Layer 	parent;
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
	TransObjLayerClass i_class ;
	NhlErrorTypes ret = NOERROR;

	i_class = (TransObjLayerClass)instance->base.layer_class;


	if((_NhlIsTransObj(instance))&&(_NhlIsView(parent))) {	
		while(i_class != (TransObjLayerClass)transObjLayerClass) {
			if( i_class->trobj_class.win_to_data!= NULL ) {	
				if(n>0)
				ret = (*i_class->trobj_class.win_to_data)(
					instance, parent, x,y,n,xout,yout,
					xmissing,ymissing,istrans);
				return(ret);
			} else {
				i_class = (TransObjLayerClass)
						i_class->base_class.superclass;
			}
		}
		return(ret);
	} else {
		NhlPError(FATAL,E_UNKNOWN,"_NhlNDCToWin: Either transformation or parent of wrong class");
		return(FATAL);
	}
}




NhlErrorTypes 	_NhlWinToCompc
#if  __STDC__
(Layer 	instance,Layer parent,float *x,float *y,int n, float *xout,
	float *yout,int *istrans,float *xmissing, float *ymissing)
#else
(instance,parent,x,y,n,xout,yout,istrans,xmissing,ymissing)
	Layer 	instance;
	Layer 	parent;
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
	TransObjLayerClass i_class ;
	NhlErrorTypes ret = NOERROR;

	i_class = (TransObjLayerClass)instance->base.layer_class;


	if((_NhlIsTransObj(instance))&&(_NhlIsView(parent))) {	
		while(i_class != (TransObjLayerClass)transObjLayerClass) {
			if( i_class->trobj_class.win_to_compc!= NULL ) {	
				if(n>0)
				ret = (*i_class->trobj_class.win_to_compc)(
					instance, parent, x,y,n,xout,yout,
					xmissing,ymissing,istrans);
				return(ret);
			} else {
				i_class = (TransObjLayerClass)
						i_class->base_class.superclass;
			}
		}
		return(ret);
	} else {
		NhlPError(FATAL,E_UNKNOWN,"_NhlNDCToWin: Either transformation or parent of wrong class");
		return(FATAL);
	}
}



NhlErrorTypes 	_NhlCompcToWin
#if  __STDC__
(Layer 	instance,Layer parent,float *x,float *y,int n, float *xout,
	float *yout,int *istrans,float *xmissing,float *ymissing)
#else
(instance,parent,x,y,n,xout,yout,istrans,xmissing,ymissing)
	Layer 	instance;
	Layer 	parent;
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
	TransObjLayerClass i_class ;
	NhlErrorTypes ret = NOERROR;

	i_class = (TransObjLayerClass)instance->base.layer_class;


	if((_NhlIsTransObj(instance))&&(_NhlIsView(parent))) {	
		while(i_class != (TransObjLayerClass)transObjLayerClass) {
			if( i_class->trobj_class.compc_to_win!= NULL ) {	
				if(n>0)
				ret = (*i_class->trobj_class.compc_to_win)(
					instance, parent, x,y,n,xout,yout,
					xmissing,ymissing,istrans);
				return(ret);
			} else {
				i_class = (TransObjLayerClass)
						i_class->base_class.superclass;
			}
		}
		return(ret);
	} else {
		NhlPError(FATAL,E_UNKNOWN,"_NhlNDCToWin: Either transformation or parent of wrong class");
		return(FATAL);
	}
}




NhlErrorTypes 	_NhlSetTrans
#if  __STDC__
(Layer instance,Layer  parent)
#else
(instance,parent)
	Layer instance;
	Layer parent;
#endif
{
	TransObjLayerClass i_class ;
	NhlErrorTypes ret = NOERROR;

	i_class = (TransObjLayerClass)instance->base.layer_class;

	if(i_class->trobj_class.set_trans != NULL) {
		ret = (*i_class->trobj_class.set_trans)(instance,parent);
	} else {
/*
* ERROR No SetTrans function to call
*/
		ret = WARNING;
	}
	return(ret);
	
}
static NhlErrorTypes CallDataToNDC
#if __STDC__
(Layer layer, LayerClass class,float *x,float *y,int n, float *xout,float *yout,float *xmissing,float *ymissing,int *status,float* out_of_range)
#else 
(layer,class,x,y,n,xout,yout,xmissing,ymissing,status,out_of_range)
	Layer layer;
	LayerClass class;
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
	TransformLayerClass tclass = (TransformLayerClass) class;
/*
* This is a call first method
*/
	if(class != transformLayerClass) {
		if(tclass->trans_class.data_to_ndc != NULL) 
			return((*tclass->trans_class.data_to_ndc)(layer,x,y,n,xout,yout,xmissing,ymissing,status,out_of_range));
		else 
			return(CallDataToNDC(layer,tclass->base_class.superclass,
				x,y,n,xout,yout,xmissing,ymissing,status,out_of_range));
	} else {
		NhlPError(FATAL,E_UNKNOWN,"No Transformation function registered for plot class");
		
		return(FATAL);
	}
}

NhlErrorTypes NhlDataToNDC
#if __STDC__
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
	TransformLayer tlayer = (TransformLayer)_NhlGetLayer(pid);
	NhlErrorTypes ret = NOERROR;

	if(_NhlIsTransform(tlayer)) {
		ret = CallDataToNDC((Layer)tlayer,tlayer->base.layer_class,
				x,y,n,xout,yout,xmissing,ymissing,status,out_of_range);
		
	} else {
/*
* ERROR : wrong plot class passed to  NhlDataToNDC
*/
		NhlPError(FATAL,E_UNKNOWN,"NhlDataToNDC has been passed an object that can't perform the requested transformation");
		return(FATAL);
	}
	return(ret);
}

static NhlErrorTypes CallNDCToData
#if __STDC__
(Layer layer, LayerClass class,float *x,float *y,int n, float *xout,float *yout,float *xmissing,float *ymissing,int *status,float *out_of_range)
#else 
(layer,class,x,y,n,xout,yout,xmissing,ymissing,status,out_of_range)
	Layer layer;
	LayerClass class;
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
	TransformLayerClass tclass = (TransformLayerClass) class;
/*
* This is a call first method
*/
	if(class != transformLayerClass) {
		if(tclass->trans_class.ndc_to_data != NULL) 
			return((*tclass->trans_class.ndc_to_data)(layer,x,y,n,xout,yout,xmissing,ymissing,status,out_of_range));
		else 
			return(CallDataToNDC(layer,tclass->base_class.superclass,
				x,y,n,xout,yout,xmissing,ymissing,status,out_of_range));
	} else {
		NhlPError(FATAL,E_UNKNOWN,"No Transformation function registered for plot class");
		
		return(FATAL);
	}
}

NhlErrorTypes NhlNDCToData
#if __STDC__
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
	TransformLayer tlayer = (TransformLayer)_NhlGetLayer(pid);
	NhlErrorTypes ret = NOERROR;

	if(_NhlIsTransform(tlayer)) {

		ret = CallNDCToData((Layer)tlayer,tlayer->base.layer_class,
				x,y,n,xout,yout,xmissing,ymissing,status,out_of_range);
		
	} else {
		NhlPError(FATAL,E_UNKNOWN,"NhlNDCToData has been passed an object that can't perform the requested transformation");
		return(FATAL);
	}
	return(ret);
}
