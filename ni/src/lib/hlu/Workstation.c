
/*
 *      $Id: Workstation.c,v 1.1 1993-04-30 17:26:00 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Workstation.c
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Sep 9 11:30:43 MDT 1992
 *
 *	Description:	The workstation class can be thought of as a base
 *			for all gks workstation types. It contains methods
 *			for opening, closing, activating and deactivating
 *			the workstation. Since these functions function 
 *			differently than any of the Base class methods. For
 *			one thing all of these methods are "up-chained" meaning
 *			the subclass versions are called before the superclass
 *			versions. This enables subclasses to do something before
 *			the actual open or close occurs. For CGM workstations
 *			this means calling GESC and setting the name of the
 *			metafile. For X workstations it could mean calling 
 *			GESC and setting window ids, geometry and display
 *			information. 
 *			
 *			There are two globally callable private functions
 *				NhlErrorTypes	_NhlAddWorkChildLayer
 *				NhlErrorTypes	_NhlDeleteWorkChildLayer
 */

#include <stdio.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/WorkstationP.h>
#include <ncarg/hlu/hluutil.h>

/*
* ------------> NEED TO set up default colormap to place in default resource 
*		field <-------------------
*/
static NhlColor def_color[] = {
{1.000000, 1.000000, 1.000000},
{1.000000, 1.000000, 0.968627},
{1.000000, 1.000000, 0.905882},
{1.000000, 1.000000, 0.843137},
{1.000000, 1.000000, 0.780392},
{1.000000, 1.000000, 0.717647},
{1.000000, 1.000000, 0.654902},
{1.000000, 1.000000, 0.592157},
{1.000000, 1.000000, 0.529412},
{1.000000, 1.000000, 0.470588},
{1.000000, 1.000000, 0.407843},
{1.000000, 1.000000, 0.345098},
{1.000000, 1.000000, 0.282353},
{1.000000, 1.000000, 0.219608},
{1.000000, 1.000000, 0.156863},
{1.000000, 1.000000, 0.094118},
{1.000000, 1.000000, 0.031373},
{1.000000, 0.968627, 0.031373},
{1.000000, 0.905882, 0.094118},
{1.000000, 0.843137, 0.156863},
{1.000000, 0.780392, 0.219608},
{1.000000, 0.717647, 0.282353},
{1.000000, 0.654902, 0.345098},
{1.000000, 0.592157, 0.407843},
{1.000000, 0.529412, 0.470588},
{1.000000, 0.470588, 0.529412},
{1.000000, 0.407843, 0.592157},
{1.000000, 0.345098, 0.654902},
{1.000000, 0.282353, 0.717647},
{1.000000, 0.219608, 0.780392},
{1.000000, 0.156863, 0.843137},
{1.000000, 0.094118, 0.905882},
{1.000000, 0.031373, 0.968627},
{1.000000, 0.000000, 0.968627},
{1.000000, 0.000000, 0.905882},
{1.000000, 0.000000, 0.843137},
{1.000000, 0.000000, 0.780392},
{1.000000, 0.000000, 0.717647},
{1.000000, 0.000000, 0.654902},
{1.000000, 0.000000, 0.592157},
{1.000000, 0.000000, 0.529412},
{1.000000, 0.000000, 0.470588},
{1.000000, 0.000000, 0.407843},
{1.000000, 0.000000, 0.345098},
{1.000000, 0.000000, 0.282353},
{1.000000, 0.000000, 0.219608},
{1.000000, 0.000000, 0.156863},
{1.000000, 0.000000, 0.094118},
{1.000000, 0.000000, 0.031373},
{0.968627, 0.031373, 0.031373},
{0.905882, 0.094118, 0.094118},
{0.843137, 0.156863, 0.156863},
{0.780392, 0.219608, 0.219608},
{0.717647, 0.282353, 0.282353},
{0.654902, 0.345098, 0.345098},
{0.592157, 0.407843, 0.407843},
{0.529412, 0.470588, 0.470588},
{0.470588, 0.529412, 0.529412},
{0.407843, 0.592157, 0.592157},
{0.345098, 0.654902, 0.654902},
{0.282353, 0.717647, 0.717647},
{0.219608, 0.780392, 0.780392},
{0.156863, 0.843137, 0.843137},
{0.094118, 0.905882, 0.905882},
{0.031373, 0.968627, 0.968627},
{0.000000, 1.000000, 0.968627},
{0.000000, 1.000000, 0.937255},
{0.000000, 1.000000, 0.874510},
{0.000000, 1.000000, 0.811765},
{0.000000, 1.000000, 0.780392},
{0.000000, 1.000000, 0.717647},
{0.000000, 1.000000, 0.654902},
{0.000000, 1.000000, 0.592157},
{0.000000, 1.000000, 0.529412},
{0.000000, 1.000000, 0.470588},
{0.000000, 1.000000, 0.407843},
{0.000000, 1.000000, 0.345098},
{0.000000, 1.000000, 0.282353},
{0.000000, 1.000000, 0.219608},
{0.000000, 1.000000, 0.156863},
{0.000000, 1.000000, 0.094118},
{0.000000, 1.000000, 0.031373},
{0.000000, 0.968627, 0.031373},
{0.000000, 0.905882, 0.094118},
{0.000000, 0.843137, 0.156863},
{0.000000, 0.780392, 0.219608},
{0.000000, 0.717647, 0.282353},
{0.000000, 0.654902, 0.345098},
{0.000000, 0.592157, 0.407843},
{0.000000, 0.529412, 0.470588},
{0.000000, 0.470588, 0.529412},
{0.000000, 0.407843, 0.592157},
{0.000000, 0.345098, 0.654902},
{0.000000, 0.282353, 0.717647},
{0.000000, 0.219608, 0.780392},
{0.000000, 0.156863, 0.843137},
{0.000000, 0.094118, 0.905882},
{0.000000, 0.031373, 0.968627},
{0.000000, 0.000000, 0.968627},
{0.000000, 0.000000, 0.905882},
{0.000000, 0.000000, 0.843137},
{0.000000, 0.000000, 0.780392},
{0.000000, 0.000000, 0.717647},
{0.000000, 0.000000, 0.654902},
{0.000000, 0.000000, 0.592157},
{0.000000, 0.000000, 0.529412},
{0.000000, 0.000000, 0.470588},
{0.000000, 0.000000, 0.407843},
{0.000000, 0.000000, 0.345098},
{0.000000, 0.000000, 0.282353},
{0.000000, 0.000000, 0.219608},
{0.000000, 0.000000, 0.156863},
{0.000000, 0.000000, 0.094118},
{0.000000, 0.000000, 0.031373}
};

static NrmQuark colormap_name;
static NrmQuark colormaplen_name;
static NrmQuark bkgnd_name;

static NhlResource resources[] = {
	{ NhlNwkColorMap, NhlCwkColorMap, NhlTColorPtr , sizeof(NhlColor*),
	NhlOffset(WorkstationLayerRec,work.color_map), NhlTImmediate, (NhlPointer)def_color},
	{ NhlNwkColorMapLen, NhlCwkColorMapLen, NhlTInteger, sizeof(int),
	NhlOffset(WorkstationLayerRec,work.color_map_len), NhlTImmediate, (NhlPointer)(int)(sizeof(def_color)/(sizeof(NhlColor)))},
	{ NhlNwkBkgndColor, NhlCwkBkgndColor, NhlTColorPtr, sizeof(NhlColor*),
	NhlOffset(WorkstationLayerRec, work.bkgnd_color), NhlTImmediate, NULL}
};

/*
* Base class method declarations
*/

static NhlErrorTypes WorkstationClassPartInitialize(
#if	NhlNeedProto
	LayerClass	layerclass	/* layerclass to init	*/
#endif
);

static NhlErrorTypes WorkstationInitialize(
#ifdef NhlNeedProto
        LayerClass,	/* class */
        Layer,		/* req */
        Layer,		/* new */
        _NhlArgList,	/* args */
        int		/* num_args */
#endif
);

static NhlErrorTypes WorkstationClassInitialize();


static NhlErrorTypes WorkstationDestroy(
#ifdef NhlNeedProto
        Layer           /* inst */
#endif
);

static NhlErrorTypes    WorkstationSetValues(
#ifdef NhlNeedProto
        Layer,		/* old */
        Layer,		/* reference */
        Layer,		/* new */
        _NhlArgList,	/* args */
        int		/* num_args*/
#endif
);

static NhlErrorTypes 	WorkstationGetValues(
#ifdef NhlNeedProto
	Layer,		/* l */
	_NhlArgList, 	/* args */
	int		/* num_args */
#endif
);

/*
* WorkStation class method declarations
*/


static NhlErrorTypes WorkstationOpen(
#ifdef NhlNeedProto
	Layer	/* instance */
#endif
);

static NhlErrorTypes WorkstationClose(
#ifdef NhlNeedProto
	Layer	/* instance */
#endif
);

static NhlErrorTypes WorkstationActivate(
#ifdef NhlNeedProto
	Layer	/* instance */
#endif
);

static NhlErrorTypes WorkstationDeactivate(
#ifdef NhlNeedProto
	Layer	/* instance */
#endif
);

static NhlErrorTypes WorkstationUpdate(
#if	NhlNeedProto
	Layer	l	/* instance	*/
#endif
);

static NhlErrorTypes WorkstationClear(
#if	NhlNeedProto
	Layer	l	/* instance	*/
#endif
);


/*
* Private functions
*/
static NhlErrorTypes AllocateColors(
#ifdef NhlNeedProto
Layer	/* instance */
#endif
); 

static NhlErrorTypes DeallocateColors(
#ifdef NhlNeedProto
Layer	/* instance */
#endif
); 

/*
* Default color map
*/


WorkstationLayerClassRec workstationLayerClassRec = {
        {
/* superclass			*/	(LayerClass)&baseLayerClassRec,
/* class_name			*/	"Workstation",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(WorkstationLayerRec),
/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* child_resources		*/	NULL,
/* all_resources		*/	NULL,
/* class_part_initialize	*/	WorkstationClassPartInitialize,
/* class_inited			*/	False,
/* class_initialize		*/	WorkstationClassInitialize,
/* layer_initialize		*/	WorkstationInitialize,
/* layer_set_values		*/	WorkstationSetValues,
/* layer_set_values_not		*/	NULL,
/* layer_get_values		*/	WorkstationGetValues,
/* layer_pre_draw		*/	NULL,
/* layer_draw			*/	NULL,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/	NULL,
/* layer_clear			*/	NULL,
/* layer_destroy		*/	WorkstationDestroy
        },
	{
/* open_work		*/	WorkstationOpen,
/* close_work		*/	WorkstationClose,
/* activate_work	*/	WorkstationActivate,
/* deactivate_work	*/	WorkstationDeactivate,
/* update_work		*/	WorkstationUpdate,
/* clear_work		*/	WorkstationClear
	}
};

LayerClass workstationLayerClass = (LayerClass)&workstationLayerClassRec;

/*
 * Function:	WorkstationClassInitialize
 *
 * Description:	Just needed to call StringToQuark for the color types and
 *		then check to see if GKS is open. If its not then GKS is opened
 *		here.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
static NhlErrorTypes WorkstationClassInitialize()
{
	Gop_st status;
	int status1,dummy = 6;

	(void)NrmStringToQuark(NhlTColorPtr);
	(void)NrmStringToQuark(NhlTColor);
	colormap_name = NrmStringToQuark(NhlNwkColorMap);
	colormaplen_name = NrmStringToQuark(NhlNwkColorMapLen);
	bkgnd_name = NrmStringToQuark(NhlNwkBkgndColor);

	ginq_op_st(&status);

	if(status == GST_GKCL) {
/*
* Going to want to change what the logical unit errors will go out to
* which is the first parameter of the gopks call.
*/
		status1 = 0;
/* FORTRAN */ gopks_(&dummy,&status1);
	}
	
	
	
	return(NOERROR);
}

/*
 * Function:	WorkstationClassPartInitialize
 *
 * Description:	This function initializes the workstationclass part of
 *		this class and any subclasses of this class.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
WorkstationClassPartInitialize
#if	__STDC__
(
	LayerClass	layerclass	/* layerclass to init	*/
)
#else
(layerclass)
	LayerClass	layerclass;	/* layerclass to init	*/
#endif
{
	WorkstationLayerClass	lc = (WorkstationLayerClass)layerclass;
	WorkstationLayerClass	sc = (WorkstationLayerClass)
						lc->base_class.superclass;

	if(lc->work_class.update_work == NhlInheritUpdate){
		lc->work_class.update_work = sc->work_class.update_work;
	}

	if(lc->work_class.clear_work == NhlInheritClear){
		lc->work_class.clear_work = sc->work_class.clear_work;
	}

	return NOERROR;
}

/*
 * Function:	WorkstationInitialize
 *
 * Description:	 DOES NOT OPEN THE WORKSTATION! Simply converts any colormap
 *		info into an internal form. The workstation will be opened by
 *		Create function after all of the initializations have occured.
 *
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
/*ARGSUSED*/
static NhlErrorTypes WorkstationInitialize
#if  __STDC__
( LayerClass class,  Layer req, Layer new, _NhlArgList args , int num_args  )
#else
( class,  req, new, args , num_args  )
	LayerClass 	class;
	Layer		req;
	Layer		new;
	_NhlArgList		args;
	int		num_args;
#endif
{
	WorkstationLayer newl = (WorkstationLayer) new;
	NhlColor* tmp;
	int i;
	NhlErrorTypes retcode = NOERROR;
	
	newl->work.gkswksid = (int)FATAL;
	newl->work.gkswkstype = (int)FATAL;
	newl->work.gkswksconid = (int)FATAL;
	newl->work.children = NULL;
	newl->work.num_children = 0;

	for(i = 0; i < MAX_COLOR_MAP; i++) {
		newl->work.private_color_map[i].ci = UNSET;
		newl->work.private_color_map[i].red= 0.0;
		newl->work.private_color_map[i].green= 0.0;
		newl->work.private_color_map[i].blue= 0.0;
	}

/*
* If the background is not set at initialize it is set to black. The back
* ground color is a set once at create time resource
*/
	/* SUPPRESS 112 */
	if(newl->work.bkgnd_color != NULL) {
		/* SUPPRESS 112 */
		tmp = newl->work.bkgnd_color;
/*
* this may change when defaults work. Specifically a malloc may not be needed.
*/
		newl->work.bkgnd_color = (NhlColor*)NhlMalloc(sizeof(NhlColor));
		newl->work.bkgnd_color->red = tmp->red;
		newl->work.bkgnd_color->green = tmp->green;
		newl->work.bkgnd_color->blue = tmp->blue;
		newl->work.private_color_map[BACKGROUND].ci = 0;
		newl->work.private_color_map[BACKGROUND].red = tmp->red;
		newl->work.private_color_map[BACKGROUND].green = tmp->green;
		newl->work.private_color_map[BACKGROUND].blue = tmp->blue;
	} else {
		newl->work.bkgnd_color = (NhlColor*)NhlMalloc(sizeof(NhlColor));
		newl->work.bkgnd_color->red = 0.0;
		newl->work.bkgnd_color->green = 0.0;
		newl->work.bkgnd_color->blue = 0.0;
		newl->work.private_color_map[BACKGROUND].ci = 0;
	}
	

/*
* Need to process Color Map checking to see if it is null is just temporary
* until the defaults work
*/
	/* SUPPRESS 112 */
	if(newl->work.color_map != NULL) {
		if(newl->work.color_map_len >= MAX_COLOR_MAP) {
/*
* COLOR MAPS CAN ONLY HAVE MAX_COLOR_MAP colors including the background color. 
* Since the background color is a resource then the actual number of aceptable
* colors is MAX_COLOR_MAP - 1
*/
			retcode = WARNING;
		}
		newl->work.num_private_colors = ((newl->work.color_map_len < (MAX_COLOR_MAP ))? newl->work.color_map_len +1 : MAX_COLOR_MAP );
		for(i = 1; i < newl->work.num_private_colors; i++)  {
/*
* SETALMOST is changed to a GKS color index when the workstation is opened
* for now the ci will be the same as the array index but this may change
* and hence the need for the ci field.
*/
			newl->work.private_color_map[i].ci = SETALMOST;
			/* SUPPRESS 112 */
			newl->work.private_color_map[i].red = 
						newl->work.color_map[i-1].red;
			/* SUPPRESS 112 */
			newl->work.private_color_map[i].green = 
						newl->work.color_map[i-1].green;
			/* SUPPRESS 112 */
			newl->work.private_color_map[i].blue = 
						newl->work.color_map[i-1].blue;
		}
	} else {
/*
* ERROR IF NO COLOR MAP EXISTS AT ALL!!!
*/
		NhlPError(FATAL,E_UNKNOWN,"No color map provided to worksation and not able to default yet");
		retcode = FATAL;
	}
	return(retcode);
}


/*
 * Function:	WorkstationDestroy
 *
 * Description:	DOES NOT CLOSE WORKSTATION! Simply frees dynamically allocated
 *		storage. The workstation has been closed by the destroy 
 *		function closes the workstation before calling this destroy 
 *		method.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
static NhlErrorTypes WorkstationDestroy
#if	__STDC__
( Layer inst )
#else
(inst)
	Layer inst;
#endif
{
	WorkstationLayer	winst = (WorkstationLayer) inst;
	LayerList	step,tmp;
	NhlErrorTypes	retcode = NOERROR;
/*
	NhlFree(winst->work.bkgnd_color);
	NhlFree(winst->work.color_map);
*/
/*
* Questionable whether the destruction of a workstation should result in the
* destruction of all of the children. Here only the LayerList if freed
*/
	step = winst->work.children;
	while(step != NULL) {
		tmp = step->next;
		NhlFree(step);
		step = tmp;
	}
	return(retcode);
}

/*
 * Function:	WorkstationSetValues
 *
 * Description:
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
/*ARGSUSED*/
static NhlErrorTypes    WorkstationSetValues
#if  __STDC__
( Layer old, Layer reference, Layer new, _NhlArgList args, int num_args)
#else
(old,reference,new,args,num_args)
        Layer  old; 
        Layer  reference; 
        Layer  new;
        _NhlArgList args;
        int num_args;
#endif
{
	WorkstationLayer	newl = (WorkstationLayer) new;
	int i;
	WorkstationLayer	oldl = (WorkstationLayer) old;
	NhlErrorTypes	retcode = NOERROR,retcode1 = NOERROR;


/*
* -----------> Issue since the color_map field is not directly used by any
* methods other that setvalues and getvalues does the color_map resource 
* need to be copied in to static memory ????<--------
*/
	if(newl->work.color_map != oldl->work.color_map) {
		if(newl->work.color_map_len >= MAX_COLOR_MAP) {
/*
* COLOR MAPS CAN ONLY HAVE MAX_COLOR_MAP colors including the background color. 
* Since the background color is a resource then the actual number of aceptable
* colors is MAX_COLOR_MAP - 1
*/
			NhlPError(WARNING,E_UNKNOWN,"Maximum color map size exceeded");
			retcode = WARNING;
		}
		newl->work.num_private_colors = ((newl->work.color_map_len < (MAX_COLOR_MAP ))? newl->work.color_map_len +1 : MAX_COLOR_MAP );
		for(i = 1; i < newl->work.num_private_colors ; i++) {
/*
* SETALMOST is changed to a GKS color index when the workstation is opened
* for now the ci will be the same as the array index but this may change
* and hence the need for the ci field.
*/
			newl->work.private_color_map[i].ci = SETALMOST;
			newl->work.private_color_map[i].red = 
						newl->work.color_map[i-1].red;
			newl->work.private_color_map[i].green = 
						newl->work.color_map[i-1].green;
			newl->work.private_color_map[i].blue = 
						newl->work.color_map[i-1].blue;
		}
	}
	if(newl->work.bkgnd_color != oldl->work.bkgnd_color ) {
/*
* ERROR BACKGROUND COLOR CANNOT CHANGE ONCE THE WORKSTATION IS INITIALIZED
*/
	NhlPError(WARNING,E_UNKNOWN,"Background color can't change once the workstation is initialized");
		retcode = WARNING;
	}
	if(retcode != NOERROR)
		retcode1 = retcode;
	retcode = AllocateColors((Layer)newl);
	return((retcode < retcode1)? retcode : retcode1);
}

/*
 * Function:	WorkstationActivate
 *
 * Description:	WorkstationActivate activates the workdstation associated with
 *		this instance. If the workstation hasn't been initialize, which
 *		is next to impossible since NhlOpenWork is called from create,
 *		an error status is returned. Other wise the workstation is
 *		activated.
 *
 * In Args:	Takes just the instance
 *
 * Out Args:	Changes fields of the instance
 *
 * Return Values:
 *
 * Side Effects:
 */
static NhlErrorTypes WorkstationActivate
#if __STDC__
(Layer	instance )
#else
(instance)
	Layer	instance;
#endif
{
	WorkstationLayer  thework = (WorkstationLayer) instance;
	NhlErrorTypes retcode = NOERROR;	

	if(wksisopn(thework->work.gkswksid)) {
		if(!wksisact(thework->work.gkswksid)) {
			gactivate_ws(thework->work.gkswksid);
		} else {
/*
* WORKSTATION IS ALREADY ACTIVE
*/
			NhlPError(INFO,E_UNKNOWN,"WorkstationActivate called on already active workstation");
			retcode = INFO; 
		}
	} else {
/*
* ERROR WORKSTATION IS NOT OPEN INITILIZATION FAILED
*/
		NhlPError(FATAL,E_UNKNOWN, "WorkstationActivate can't activate an unopened workstation");
		retcode = FATAL;
	}
	return(retcode);
}

/*
 * Function:	WorkstationDeactivate
 *
 * Description:	Deactivates workstation. if not open FATAL error if not active
 *		informational message.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
static NhlErrorTypes WorkstationDeactivate
#if __STDC__
(Layer	instance )
#else
(instance)
	Layer	instance;
#endif
{
	WorkstationLayer  thework = (WorkstationLayer) instance;
	NhlErrorTypes	retcode = NOERROR;

	if(wksisopn(thework->work.gkswksid)&&wksisact(thework->work.gkswksid)){
		gdeactivate_ws(thework->work.gkswksid);
	} else {
/*
* ERROR WORKSTATION NOT ACTIVE OR NOT INITIALIZED
*/
		NhlPError(WARNING,E_UNKNOWN,"WorkstationDeactivate: workstation not active or not openned");
		retcode = WARNING;
	}

	return(retcode);
}

/*
 * Function:	WorkstationOpen
 *
 * Description: Checks and makes sure GKS is open and then procedes to try
 *		to find an available workstation id. Also WorkstationOpen checks
 *		the workstation type and conection id to see if they are valid
 *		if they are then it procedes to open the workstation. 
 *		Workstation open is part of an "up-chained" method so these
 *		values are set by the subclasses' WorkstationOpen before getting
 *		to this. This is done so escape elements that need to be called,
 *		by different workstation types, before the GOPWK call can be
 *		called.
 *
 * In Args:	Just take the workstation instance.
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
static NhlErrorTypes WorkstationOpen
#if __STDC__
(Layer	instance )
#else
(instance)
	Layer	instance;
#endif
{	
	WorkstationLayer thework = (WorkstationLayer) instance;
	NhlErrorTypes retcode = NOERROR;
	int i = 2;

	if(thework->work.gkswkstype == FATAL) {
		NhlPError(FATAL,E_UNKNOWN,"Unknown workstation type");
		return(FATAL);
		
	} 
	if(thework->work.gkswksconid == FATAL) {
		NhlPError(FATAL,E_UNKNOWN,"Unknown workstation connection id");
		return(FATAL);
	}
	while(wksisopn(i)) {
		i++;
	}
	thework->work.gkswksid = i;

/* FORTRAN */ gopwk_(&(thework->work.gkswksid),&(thework->work.gkswksconid),&(thework->work.gkswkstype));
	gset_clip_ind(GIND_NO_CLIP);

	retcode = AllocateColors((Layer)thework);

	return(retcode);
	
		
}

/*
 * Function:	WorkstationClose
 *
 * Description:	Called before workstation destroy. This like Open is an "up-
 *		chained method it is intended to allow subclasses to do things
 *		before the actual close. If workstation is not open or is 
 *		currently active an error message is provided.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
static NhlErrorTypes WorkstationClose
#if __STDC__
(Layer	instance )
#else
(instance)
	Layer	instance;
#endif
{
	WorkstationLayer	thework = (WorkstationLayer) instance;
	NhlErrorTypes retcode = NOERROR;

	if(wksisact(thework->work.gkswksid)){
		NhlPError(INFO,E_UNKNOWN,"WorkstationClose: workstation must be deactivated before closed, deactivating workstation now");
		_NhlDeactivateWorkstation(instance);
	} 
	if(!wksisopn(thework->work.gkswksid)) {
		NhlPError(INFO,E_UNKNOWN,"WorkstationClose: workstation already closed");
		retcode = INFO;
	} else {
		gclose_ws(thework->work.gkswksid);

	}
	return(retcode);
}

/*
 * Function:	WorkstationUpdate
 *
 * Description:	This function is used to update the workstation
 *
 * In Args:	
 *		Layer	l	workstation layer to update
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
WorkstationUpdate
#if	__STDC__
(
	Layer	l	/* workstation layer to update	*/
)
#else
(l)
	Layer	l;	/* workstation layer to update	*/
#endif
{
	gupd_ws(_NhlWorkstationId(l),GFLAG_PERFORM);

	return NOERROR;
}

/*
 * Function:	WorkstationClear
 *
 * Description:	This function is used to clear the workstation
 *
 * In Args:	
 *		Layer	l	workstation layer to update
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
WorkstationClear
#if	__STDC__
(
	Layer	l	/* workstation layer to update	*/
)
#else
(l)
	Layer	l;	/* workstation layer to update	*/
#endif
{
	gclear_ws(_NhlWorkstationId(l),GFLAG_ALWAYS);

	return NOERROR;
}

/*
 * Function:	NhlSetColor
 *
 * Description:	Convienience function for setting one color at a time.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
NhlErrorTypes	NhlSetColor
#if	__STDC__
(int pid, int ci, float red, float green, float blue)
#else
(pid,ci,red,green,blue)
	int     pid;
	int	ci;
	float	red;
	float	green;
	float	blue;
#endif
{
	return(_NhlSetColor(_NhlGetLayer(pid),ci,red,green,blue));
}
NhlErrorTypes	_NhlSetColor
#if	__STDC__
(Layer inst, int ci, float red, float green, float blue)
#else
(inst,ci,red,green,blue)
	Layer	inst;
	int	ci;
	float	red;
	float	green;
	float	blue;
#endif
{
	WorkstationLayer	thework = (WorkstationLayer)inst;
	
	if(ci > MAX_COLOR_MAP) {
/*
* COLOR INDEX EXCEEDS MAX_COLOR_MAP
*/
		NhlPError(WARNING,E_UNKNOWN,"_NhlSetColor: color index exceeds MAX_COLOR_MAP");
		return(WARNING);
	}

	thework->work.private_color_map[ci - 1].ci = SETALMOST;
	thework->work.private_color_map[ci - 1].red = red;
	thework->work.private_color_map[ci - 1].green = green;
	thework->work.private_color_map[ci - 1].blue = blue;

	return(AllocateColors((Layer)thework));
}

/*
 * Function:	NhlFreeColor
 *
 * Description:	removes a color index from the workstation color map
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
NhlErrorTypes	NhlFreeColor
#if	__STDC__
(int pid, int ci)
#else
(pid ,ci)
	int 	pid;
	int	ci;
#endif
{
	return(_NhlFreeColor(_NhlGetLayer(pid),ci));
}
NhlErrorTypes	_NhlFreeColor
#if	__STDC__
(Layer inst, int ci)
#else
(inst,ci)
	Layer inst;
	int	ci;
#endif
{
	WorkstationLayer	thework = (WorkstationLayer)inst;

	if(ci > MAX_COLOR_MAP) {
		NhlPError(WARNING,E_UNKNOWN,"_NhlFreeColor: color index exceeds MAX_COLOR_MAP");
		return(WARNING);
	}

	thework->work.private_color_map[ci - 1].ci =REMOVE;

	return(DeallocateColors((Layer)thework));
}


/*
 * Function:	AllocateColors
 *
 * Description: Used to allocate colors.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */

static NhlErrorTypes AllocateColors
#if  __STDC__
(Layer inst )
#else
(inst)
	Layer inst;
#endif
{
	WorkstationLayer thework = (WorkstationLayer) inst;
	Gcolr_rep tmpcolrrep;
	int i;
/*
* Temporary allocation routine until some color management scheme is put in 
* place. In fact this may turn in to a method
*/
	for( i = 0; i < MAX_COLOR_MAP; i++) {
		if(thework->work.private_color_map[i].ci == SETALMOST) {
			tmpcolrrep.rgb.red = thework->work.private_color_map[i].red;
			tmpcolrrep.rgb.green = thework->work.private_color_map[i].green;
			tmpcolrrep.rgb.blue= thework->work.private_color_map[i].blue;
			gset_colr_rep(thework->work.gkswksid,i,&tmpcolrrep);
			thework->work.private_color_map[i].ci = i;
		}
	}
	return(NOERROR);
}
/*
 * Function:	DeallocateColors
 *
 * Description: Used to deallocate colors.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */

static NhlErrorTypes DeallocateColors
#if  __STDC__
(Layer inst )
#else
(inst)
	Layer inst;
#endif
{
	WorkstationLayer thework = (WorkstationLayer) inst;
	int i;

	for( i = 0; i < MAX_COLOR_MAP; i++) {
		if(thework->work.private_color_map[i].ci == REMOVE) {
			thework->work.private_color_map[i].ci = UNSET;
		}
	}
	return(NOERROR);
}

/*
 * Function:	NhlNewColor
 *
 * Description: Does not require the user to provide a color index and returns
 *		either an error status or a color index(not a GKS color index
 *		though, just an index into the workstatoins color map .
 * 
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
int NhlNewColor
#if  __STDC__
(int pid, float red, float green, float blue)
#else
(pid,red,green,blue)
	int pid;
	float red;
	float green;
	float blue;
#endif
{
	return(_NhlNewColor(_NhlGetLayer(pid),red,green,blue));
}

int _NhlNewColor
#if   __STDC__
(Layer inst,float red,float green,float blue)
#else
(inst,red,green,blue)
        Layer   inst;
        float   red;
        float   green;
        float   blue;
#endif
{
	WorkstationLayer  thework = (WorkstationLayer) inst;
	int i = 1;
	NhlErrorTypes retcode = NOERROR;

	while( thework->work.private_color_map[i].ci != UNSET ) {
		i++;
		if(i == MAX_COLOR_MAP) {
/*
* ERROR : no available colors
*/		
			NhlPError(FATAL,E_UNKNOWN,"_NhlNewColor: no available colors");
			return(FATAL);
		}
	}
	thework->work.private_color_map[i].ci = SETALMOST;
	thework->work.private_color_map[i].red = red;
	thework->work.private_color_map[i].green = green;
	thework->work.private_color_map[i].blue = blue;
	retcode = AllocateColors((Layer)thework);

	return((retcode < INFO)? (int)retcode : i+1);
	 
}

/*
 * Function:	WorkstationGetValues
 *
 * Description:	
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */

static NhlErrorTypes	WorkstationGetValues
#if __STDC__
(Layer l, _NhlArgList args, int num_args)
#else
(l,args,num_args)
	Layer	l;
	_NhlArgList	args;
	int	num_args;
#endif
{
	WorkstationLayer wl = (WorkstationLayer)l;
	int i,j;
	NhlColor* tmp;
	NhlPrivateColor *private;

	for( i = 0; i< num_args; i++ ) {
		if(args[i].quark == colormap_name) {
			private = wl->work.private_color_map;
			
			tmp = (NhlColor*)NhlMalloc(wl->work.num_private_colors
					*sizeof(NhlColor) - 1);
			*((NhlColor**)(args[i].value)) = tmp;
			for(j = 0; j< wl->work.num_private_colors -1; j++) {
				tmp[j].red = private[j + 1].red;
				tmp[j].green = private[j + 1].green;
				tmp[j].blue = private[j + 1].blue;
			}
		} else if (args[i].quark == colormaplen_name) {
			*((int*)args[i].value) = wl->work.num_private_colors -1;
		} else if (args[i].quark == bkgnd_name) {
			tmp = (NhlColor*) NhlMalloc(sizeof(NhlColor));
			tmp->red = wl->work.private_color_map[BACKGROUND].red;
			tmp->green=wl->work.private_color_map[BACKGROUND].green;
			tmp->blue= wl->work.private_color_map[BACKGROUND].blue;
		}
	}
	return(NOERROR);
}

/*
* EVERYTHING BELOW HERE IS AND SHOULD BE PRIVATE BUT GLOBALLY CALLABLE
* FUNCTIONS AND SHOULD BE DECLARED IN "hluP.h"
*/

/*
 * Function:	_NhlAddWorkChildLayer
 *
 * Description:
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
NhlErrorTypes   _NhlAddWorkChildLayer
#if __STDC__
(Layer parent, Layer child)
#else
(parent,child)
	Layer parent;
	Layer child;
#endif
{
	WorkstationLayer wparent = (WorkstationLayer) parent;
	LayerList	tmp;
	

	if(wparent->work.children == NULL){
		wparent->work.children = (LayerList)
			NhlMalloc(sizeof(LayerListNode));
		wparent->work.children->next = NULL;
		wparent->work.children->layer = child;
		wparent->work.num_children = 1;
	} else {
		tmp = (LayerList) NhlMalloc(sizeof(LayerListNode));
		tmp->next = wparent->work.children;
		tmp->layer = child;
		wparent->work.children = tmp;
		++wparent->work.num_children;
	}
	return(NOERROR);
}

/*
 * Function:	_NhlDeleteWorkChildLayer
 *
 * Description:
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
NhlErrorTypes   _NhlDeleteWorkChildLayer
#if __STDC__
(Layer parent, Layer child)
#else 
(parent,child)
	Layer	parent;
	Layer 	child;
#endif
{
	WorkstationLayer	wparent = (WorkstationLayer) parent;
	LayerList 	step = wparent->work.children;
	LayerList	tmp;

	if(step != NULL ) {
		if(step->layer->base.id == child->base.id) {
			tmp = step;
			wparent->work.children = step->next;
			--wparent->work.num_children;
			NhlFree(step);
		}
		while(step->next != NULL){
			if(step->next->layer->base.id == child->base.id) {
				tmp = step->next;
				step->next = step->next->next; 
				NhlFree(tmp);
				--wparent->work.num_children;
				break;
			}
			step = step->next;
		}
		return(NOERROR);
	} else {
/*
*ERROR: No child in list
*/
		NhlPError(INFO,E_UNKNOWN,"_NhlDeleteWorkChildLayer: Delete requested on empty list");
		return(INFO);
	}
}

/*
 * Function:	_NhlWorkstationId
 *
 * Description:
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
int	_NhlWorkstationId
#if __STDC__
(Layer instance)
#else
(instance)
	Layer instance;
#endif
{
	WorkstationLayer wl = (WorkstationLayer) instance;

	return(wl->work.gkswksid);
}

/*
 * Function:	_NhlActivateWorkstation and CallActivateWorkstation
 *
 * Description: _NhlActivateWorkstation checks to see if the layer wks is
 *		a workstation and then calls CallActivate which recursively
 *		calls the activate_work methods.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
static NhlErrorTypes CallActivateWorkstation
#if __STDC__
	(Layer layer, LayerClass lc)
#else
	(layer,lc)
		Layer layer;
		LayerClass lc;
#endif
{
	WorkstationLayerClass wc = (WorkstationLayerClass) lc;
	NhlErrorTypes ancestor = NOERROR, thistime = NOERROR;

	if( wc->work_class.activate_work != NULL ) {
		thistime = (*(wc->work_class.activate_work))(layer);
		if( thistime < WARNING)
			return(thistime);
	}

	if( wc->base_class.superclass != baseLayerClass ) { 
		ancestor = CallActivateWorkstation(layer, wc->base_class.superclass);
	}
	return(MIN(ancestor,thistime));
}
NhlErrorTypes _NhlActivateWorkstation
#if __STDC__
(Layer wks)
#else
(wks)
	Layer wks;
#endif
{

	if(_NhlIsWorkstation(wks)) {
		return(CallActivateWorkstation(wks,wks->base.layer_class));
	} else {
		NhlPError(WARNING,E_UNKNOWN,"_NhlActivateWorkstation: attempt to perform activate on nonworkstation");
		return(WARNING);
	}
}

/*
 * Function:	_NhlDeactivateWorkstation and CallDeativateWorkstation(
 *
 * Description:	performs same task as _NhlWorkstationActivate except
 *		the deactivate_work method is called
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
static NhlErrorTypes CallDeactivateWorkstation
#if __STDC__
	(Layer layer, LayerClass lc)
#else
	(layer,lc)
		Layer layer;
		LayerClass lc;
#endif
{
	WorkstationLayerClass wc = (WorkstationLayerClass) lc;
	NhlErrorTypes ancestor = NOERROR, thistime = NOERROR;

	if( wc->work_class.deactivate_work != NULL ) {
		thistime = (*(wc->work_class.deactivate_work))(layer);
		if( thistime < WARNING)
			return(thistime);
	}

	if( wc->base_class.superclass != baseLayerClass ) { 
		ancestor = CallDeactivateWorkstation(layer, wc->base_class.superclass);
	}
	return(MIN(ancestor,thistime));
}
NhlErrorTypes _NhlDeactivateWorkstation
#if __STDC__
(Layer wks)
#else
(wks)
	Layer wks;
#endif
{

	if(_NhlIsWorkstation(wks)) {
		return(CallDeactivateWorkstation(wks,wks->base.layer_class));
	} else {
		NhlPError(WARNING,E_UNKNOWN,"_NhlActivateWorkstation: attempt to perform deactivate on nonworkstation");
		return(WARNING);
	}
}


/*
 * Function:	_NhlCloseWorkstation and CallCloseWorkstation
 *
 * Description:
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
static NhlErrorTypes CallCloseWorkstation
#if __STDC__
(Layer instance,LayerClass lc)
#else
(instance,lc)
        Layer instance;
        LayerClass lc;
#endif
{
        WorkstationLayerClass   wc =(WorkstationLayerClass)lc ;
        NhlErrorTypes ancestorerr = NOERROR, thisclass = NOERROR;

        if(wc->work_class.close_work != NULL) {
                thisclass = (*wc->work_class.close_work)(instance);
                if(thisclass < WARNING)
                        return(thisclass);
        }

        if(lc->base_class.superclass != baseLayerClass)
                ancestorerr = CallCloseWorkstation(instance,lc->base_class.superclass);


        return(MIN(ancestorerr,thisclass));
}

NhlErrorTypes _NhlCloseWorkstation
#if __STDC__
(Layer layer)
#else
(layer)
	Layer	layer;
#endif
{
	if(_NhlIsWorkstation(layer)) {
		return(CallCloseWorkstation(layer,layer->base.layer_class));
	} else {
		NhlPError(WARNING,E_UNKNOWN,"_NhlActivateWorkstation: attempt to perform close on nonworkstation");
		return(WARNING);
	}
}

/*
 * Function:    _NhlOpenWorkstation and CallOpenWorkstatation
 *
 * Description: Recursively calls open_work methods from the lowest subclass
 *              up.
 *
 * In Args:     New instance: layer
 *
 * Out Args:    NONE
 *
 * Return Values:       NONE
 *
 * Side Effects:        Workstation opened up and a workstation id assigned
 *
 */
static NhlErrorTypes CallOpenWorkstation
#if __STDC__
(Layer instance,LayerClass lc)
#else
(instance,lc)
        Layer instance;
        LayerClass lc;
#endif
{
        WorkstationLayerClass   wc =(WorkstationLayerClass)lc ;
        NhlErrorTypes ancestorerr = NOERROR, thisclass = NOERROR;

        if(wc->work_class.open_work != NULL) {
                thisclass = (*wc->work_class.open_work)(instance);
                if(thisclass < WARNING)
                        return(thisclass);
        }

        if(lc->base_class.superclass != baseLayerClass)
                ancestorerr = CallOpenWorkstation(instance,lc->base_class.superclass);


        return(MIN(ancestorerr,thisclass));
}

NhlErrorTypes _NhlOpenWorkstation
#if __STDC__
(Layer layer)
#else
(layer)
	Layer	layer;
#endif
{
	if(_NhlIsWorkstation(layer)) {
		return(CallOpenWorkstation(layer,layer->base.layer_class));
	} else {
		NhlPError(WARNING,E_UNKNOWN,"_NhlActivateWorkstation: attempt to perform open on nonworkstation");
		return(WARNING);
	}
}

/*
 * Function:	NhlGetWorkId
 *
 * Description:	Returns the GKS workstation id used by the workstation object
 *
 * In Args:	HLU Workstation object id
 *
 * Out Args:	NONE
 *
 * Returns:	integer gks workstation id
 */
int NhlGetGksWorkId
#if  __STDC__
(int workid)
#else
(workid)
int	workid;
#endif
{
	Layer l = _NhlGetLayer(workid);

	if(_NhlIsWorkstation(l)) {
		return(((WorkstationLayer)l)->work.gkswksid);
	} else {
		NhlPError(WARNING,E_UNKNOWN,"NhlGetGksWorkId: An incorrect type of object was passed");
		return(-1);
	}
}

/*
 * Function:	NhlGetGksCi
 *
 * Description: Returns a color index for GKS given a workstation color index.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
int NhlGetGksCi
#if __STDC__
(int pid, int ci)
#else
(pid,ci)
	int pid;
	int ci;
#endif
{

 	return(_NhlGetGksCi(_NhlGetLayer(pid),ci));	
}
int _NhlGetGksCi
#if __STDC__
( Layer workstation, int  ci)
#else
(workstation, ci)
	Layer	workstation;
	int	ci;
#endif
{
	
	WorkstationLayer  wk = (WorkstationLayer) workstation;
	if(_NhlIsWorkstation(workstation)){
		if(wk->work.private_color_map[ci+1].ci >= 0) {
			return(wk->work.private_color_map[ci+1].ci);
		} else {
			NhlPError(WARNING,E_UNKNOWN,"_NhlGetGksCi: Color index requested is not allocated");
			return((int)WARNING);
		}
	} else {
		NhlPError(WARNING,E_UNKNOWN,"_NhlGetGksCi: attempt to return color from none workstation");
		return((int)WARNING);
	}
}

/*
 * Function:	NhlUpdateWorkstation
 *
 * Description:	This function is used to flush the workstation.
 *
 * In Args:	
 *		int	workid	id of workstation class object
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlUpdateWorkstation
#if	__STDC__
(
	int	workid	/* id of workstation class object	*/
)
#else
(workid)
	int	workid;	/* id of workstation class object	*/
#endif
{
	Layer			l = _NhlGetLayer(workid);
	WorkstationLayerClass	lc;

	if(l == (Layer)NULL){
		NhlPError(FATAL,E_UNKNOWN,
			"Unable to update Workstation with PID#%d",workid);
		return FATAL;
	}

	if(!_NhlIsWorkstation(l)){
		NhlPError(FATAL,E_UNKNOWN,
			"PID#%d is not a Workstation Class object",workid);
		return FATAL;
	}

	lc = (WorkstationLayerClass)l->base.layer_class;

	return (*(lc->work_class.update_work))(l);
}

/*
 * Function:	NhlClearWorkstation
 *
 * Description:	This function is used to clear the workstation.
 *
 * In Args:	
 *		int	workid	id of workstation class object
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlClearWorkstation
#if	__STDC__
(
	int	workid	/* id of workstation class object	*/
)
#else
(workid)
	int	workid;	/* id of workstation class object	*/
#endif
{
	Layer			l = _NhlGetLayer(workid);
	WorkstationLayerClass	lc;

	if(l == (Layer)NULL){
		NhlPError(FATAL,E_UNKNOWN,
			"Unable to clear Workstation with PID#%d",workid);
		return FATAL;
	}

	if(!_NhlIsWorkstation(l)){
		NhlPError(FATAL,E_UNKNOWN,
			"PID#%d is not a Workstation Class object",workid);
		return FATAL;
	}

	lc = (WorkstationLayerClass)l->base.layer_class;

	return (*(lc->work_class.clear_work))(l);
}

NhlErrorTypes	NhlFrame
#if	__STDC__
(int wid)
#else
(wid)
	int wid;
#endif
{
	NhlErrorTypes ret = NOERROR;
	NhlErrorTypes ret1 = NOERROR;

	ret = NhlUpdateWorkstation(wid);
	ret1 = NhlClearWorkstation(wid);
	return(MIN(ret,ret1));
}
