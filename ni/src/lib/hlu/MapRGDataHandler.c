/*
 *      $Id: MapRGDataHandler.c,v 1.5 2003-07-14 23:12:00 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Apr 23 12:01:04 MDT 1998
 *
 *	Description:	
 */

#include <ncarg/hlu/MapRGDataHandlerP.h>

#define Oset(field)	NhlOffset(NhlMapRGDataHandlerLayerRec,maprgdh.field)

static NhlResource resources[] = {
	{NhlNmpDataResolution,NhlCmpDataResolution,
		NhlTMapDataResolution,sizeof(NhlMapDataResolution),
	 	Oset(data_resolution),NhlTImmediate, 
	 	_NhlUSET((NhlPointer)NhlUNSPECIFIEDRESOLUTION),0,NULL}
};

static NhlErrorTypes MapRGDHClassPartInit(
#if	NhlNeedProto
	NhlClass	lc
#endif
);


static NhlErrorTypes MapRGDHInitialize(
#if	NhlNeedProto
        NhlClass	class,
        NhlLayer	req,
        NhlLayer	new,
        _NhlArgList	args,
        int             num_args
#endif
);

static NhlErrorTypes  MapRGDHSetValues(
#if	NhlNeedProto
        NhlLayer	old,
        NhlLayer	reference,
        NhlLayer	new,
        _NhlArgList	args,
        int             num_args
#endif
);

static NhlErrorTypes    MapRGDHGetValues(
#if	NhlNeedProto
	NhlLayer        l,
	_NhlArgList     args,
	int             num_args
#endif
);

static NhlErrorTypes    MapRGDHDestroy(
#if	NhlNeedProto
	NhlLayer        l
#endif
);

static NhlErrorTypes MapRGDHUpdateDrawList(
#if	NhlNeedProto
	NhlLayer		instance,
        NhlBoolean  		init,
        NhlMapPlotLayer 	newmp,
        NhlMapPlotLayer 	oldmp,
        _NhlArgList		args,
        int             	num_args
#endif
);

static NhlErrorTypes MapRGDHDrawMapList(
#if	NhlNeedProto
	NhlLayer		instance,
        NhlMapPlotLayer 	mp,
        mpDrawOp		draw_op,
	NhlBoolean		init_draw
#endif
);


static void   load_hlumap_routines(
#if	NhlNeedProto
	NhlBoolean	flag
#endif
);

void   (_NHLCALLF(hlumapeod,HLUMAPEOD))(
#if	NhlNeedProto
	int *nout,
	int *nseg,
	int *idls,
	int *idrs,
	int *npts,
	float *pnts
#endif
);

NhlMapRGDataHandlerClassRec NhlmapRGDataHandlerClassRec = {
	{
/* class_name 		*/      "mapRGDataHandlerClass",
/* nrm_class 		*/      NrmNULLQUARK,
/* layer_size 		*/      sizeof(NhlMapRGDataHandlerLayerRec),
/* class_inited 	*/	False,
/* superclass		*/      (NhlClass)&NhlmapDataHandlerClassRec,
/* cvt_table		*/	NULL,

/* layer_resources 	*/   	resources,
/* num_resources 	*/     	NhlNumber(resources),
/* all_resources 	*/	NULL,
/* callbacks		*/	NULL,
/* num_callbacks	*/	0,
/* class_callbacks	*/	NULL,
/* num_class_callbacks	*/	0,

/* class_part_initialize */     MapRGDHClassPartInit,
/* class_initialize 	*/  	NULL,
/* layer_initialize 	*/  	MapRGDHInitialize,
/* layer_set_values 	*/  	MapRGDHSetValues,
/* layer_set_values_hook */  	NULL,
/* layer_get_values 	*/  	MapRGDHGetValues,
/* layer_reparent 	*/  	NULL,
/* layer_destroy 	*/    	MapRGDHDestroy,
	},
	{
/* update_map_draw_list */	MapRGDHUpdateDrawList,
/* draw_map_list        */      MapRGDHDrawMapList
	}
};

NhlClass NhlmapRGDataHandlerClass = (NhlClass)&NhlmapRGDataHandlerClassRec;


static NrmQuark Qdata_resolution = NrmNULLQUARK;

static NhlMapPlotLayer Mpl;
static NhlMapPlotLayerPart *Mpp;
static NhlMapRGDataHandlerClassPart	*Mrgcp;
static NhlMapRGDataHandlerLayerPart	*Mrgp;
static NhlBoolean Grid_Setup;


static NhlErrorTypes
MapRGDHClassPartInit
#if	NhlNeedProto
(
	NhlClass	lc
)
#else
(lc)
	NhlClass	lc;
#endif
{
	NhlMapRGDataHandlerClass	mdhc = (NhlMapRGDataHandlerClass)lc;
        
	Qdata_resolution = NrmStringToQuark(NhlNmpDataResolution);
        
        Mrgcp = &mdhc->maprgdh_class;

	load_hlumap_routines(False);
        
	return NhlNOERROR;
}

/*
 * Function:  mdhManageDynamicArrays
 *
 * Description: Creates and manages internal copies of each of the 
 *	MapDataHandler GenArrays. 
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
static NhlErrorTypes    mdhManageDynamicArrays
#if	NhlNeedProto
(
	NhlMapRGDataHandlerLayer	mv4new, 
	NhlMapRGDataHandlerLayer	mv4old,
	NhlBoolean			init,
	_NhlArgList			args,
	int				num_args)
#else
(mdhnew,mdhold,init,args,num_args)
	NhlMapRGDataHandlerLayer	mv4new;
	NhlMapRGDataHandlerLayer	mv4old;
	NhlBoolean			init;
	_NhlArgList			args;
	int				num_args;
#endif

{
        
	NhlErrorTypes ret = NhlNOERROR;

	return ret;
}

/*
 * Function:	MapRGDHInitialize
 *
 * Description: 
 *
 * In Args: 	class	objects layer_class
 *		req	instance record of requested values
 *		new	instance record of new object
 *		args	list of resources and values for reference
 *		num_args 	number of elements in args.
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:
 */
/*ARGSUSED*/
static NhlErrorTypes
MapRGDHInitialize
#if	NhlNeedProto
(
	NhlClass	class,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		num_args
)
#else
(class,req,new,args,num_args)
        NhlClass   class;
        NhlLayer        req;
        NhlLayer        new;
        _NhlArgList     args;
        int             num_args;
#endif
{
        NhlMapRGDataHandlerLayer mrgl = (NhlMapRGDataHandlerLayer) new;
        NhlMapRGDataHandlerLayerPart *mrgp = &mrgl->maprgdh;
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*entry_name = "MapRGDHInitialize";
	char			*e_text;
        
	mrgp->aws_id = -1;
	mrgp->fws_id = -1;

	mrgp->real_data_resolution = mrgp->data_resolution;
        
/* Manage the dynamic arrays */

	subret = mdhManageDynamicArrays((NhlMapRGDataHandlerLayer)new,
                                        (NhlMapRGDataHandlerLayer)req,
                                        True,args,num_args);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error managing dynamic arrays";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

        return ret;
}

/*
 * Function:	MapRGDHSetValues
 *
 * Description: 
 *
 * In Args:	old	copy of old instance record
 *		reference	requested instance record
 *		new	new instance record	
 *		args 	list of resources and values for reference
 *		num_args	number of elements in args.
 *
 * Out Args:	NONE
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:
 */
/*ARGSUSED*/
static NhlErrorTypes MapRGDHSetValues
#if	NhlNeedProto
(
	NhlLayer	old,
	NhlLayer	reference,
	NhlLayer	new,
	_NhlArgList	args,
	int		num_args
)
#else
(old,reference,new,args,num_args)
	NhlLayer	old;
	NhlLayer	reference;
	NhlLayer	new;
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*entry_name = "MapRGDHSetValues";
	char			*e_text;


/* Manage the dynamic arrays */

	subret = mdhManageDynamicArrays((NhlMapRGDataHandlerLayer)new,
                                        (NhlMapRGDataHandlerLayer)old,
                                        False,args,num_args);
	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error managing dynamic arrays";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(ret);
	}

        return ret;
}

/*
 * Function:    MapRGDHGetValues
 *
 * Description: Retrieves the current setting of MapRGDataHandler resources.
 *      Actually the resources belong to the superclass MapDataHandler --
 *      but they get their contents from the subclass.
 *
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 *      Memory is allocated when any of the following resources are retrieved:
 *		NhlNmpAreaNames
 *		NhlNmpAreaTypes
 *		NhlNmpDynamicAreaGroups
 *		NhlNmpSpecifiedFillColors
 *
 *      The caller is responsible for freeing this memory.
 */

static NhlErrorTypes    MapRGDHGetValues
#if	NhlNeedProto
(NhlLayer l, _NhlArgList args, int num_args)
#else
(l,args,num_args)
        NhlLayer        l;
        _NhlArgList     args;
        int     	num_args;
#endif
{
	
        return(NhlNOERROR);

}


/*
 * Function:    MapRGDHDestroy
 *
 * Description: Retrieves the current setting of MapRGDataHandler resources.
 *      Actually the resources belong to the superclass MapDataHandler --
 *      but they get their contents from the subclass.
 *
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 *      Memory is allocated when any of the following resources are retrieved:
 *		NhlNmpAreaNames
 *		NhlNmpAreaTypes
 *		NhlNmpDynamicAreaGroups
 *		NhlNmpSpecifiedFillColors
 *
 *      The caller is responsible for freeing this memory.
 */

static NhlErrorTypes    MapRGDHDestroy
#if	NhlNeedProto
(
        NhlLayer l
        )
#else
(l)
        NhlLayer        l;
#endif
{
        NhlMapRGDataHandlerLayer mrgl = (NhlMapRGDataHandlerLayer) l;
        NhlMapRGDataHandlerLayerPart *mrgp = &mrgl->maprgdh;

	if (mrgp->aws_id > 0)
		_NhlFreeWorkspace(mrgp->aws_id);
	if (mrgp->fws_id > 0)
		_NhlFreeWorkspace(mrgp->fws_id);


        return NhlNOERROR;
}


static NhlErrorTypes MapRGDHUpdateDrawList
#if	NhlNeedProto
(
	NhlLayer	instance,
        NhlBoolean  	init,
        NhlMapPlotLayer newmp,
        NhlMapPlotLayer oldmp,
        _NhlArgList	args,
        int             num_args
        )
#else
(instance,newmp,oldmp,args,num_args)
	NhlLayer	instance;
        NhlBoolean  	init;
        NhlMapPlotLayer newmp;
        NhlMapPlotLayer oldmp;
        _NhlArgList	args;
        int             num_args;
#endif
{
        NhlErrorTypes ret = NhlNOERROR;
                
        return ret;
}


/*
 * Function:	mpFill
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	


static NhlErrorTypes mpFill
#if	NhlNeedProto
(
        NhlMapRGDataHandlerLayer 	mrgl,
	NhlMapPlotLayer			mpl,
	NhlString			entry_name
)
#else
(mrgl,mpl,entry_name)
        NhlMapRGDataHandlerLayer 	mrgl;
        NhlMapPlotLayer 		mpl;
	NhlString			entry_name;
#endif
{
        NhlMapRGDataHandlerLayerPart *mrgp = &mrgl->maprgdh;
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
        NhlWorkspace		*aws = NULL;
        NhlWorkspace		*fws = NULL;
	int 			irgl;

	if (mrgp->fws_id < 1) {
		mrgp->fws_id = 
			_NhlNewWorkspace(NhlwsOTHER,NhlwsNONE,
					 mpRG_FLT_REQ_SIZE);
		if (mrgp->fws_id < 1) 
			return MIN(ret,(NhlErrorTypes)mrgp->fws_id);
	}
	if ((fws = _NhlUseWorkspace(mrgp->fws_id)) == NULL) {
		e_text = "%s: error reserving area map workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}

	if (mrgp->aws_id < 1) {
		/*
		 * can't use NhlwsAREAMaP because the new map 
		 * database doesn't do error recovery; so can't
		 * afford to trim the workspace -- it will result
		 * in errors later on
		 */
		mrgp->aws_id = 
			_NhlNewWorkspace(NhlwsOTHER,NhlwsNONE,
					 mpRG_AREAMAP_REQ_SIZE);
		if (mrgp->aws_id < 1) {
			ret = MIN(ret,(NhlErrorTypes)mrgp->aws_id);
			goto error_ret;
		}
	}
	if ((aws = _NhlUseWorkspace(mrgp->aws_id)) == NULL) {
		e_text = "%s: error reserving area map workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		ret = NhlFATAL;
		goto error_ret;
	}

	if (mrgp->data_resolution == NhlUNSPECIFIEDRESOLUTION) 
		c_mdrgdl(&irgl);
	else
		irgl = (int)mrgp->data_resolution;

	mrgp->real_data_resolution = (NhlMapDataResolution) irgl;

	subret = _NhlMdrgsf(irgl,fws,aws,entry_name);
        ret = MIN(subret,ret);


        subret = _NhlIdleWorkspace(aws);
        ret = MIN(subret,ret);

 error_ret:

        subret = _NhlIdleWorkspace(fws);
        ret = MIN(subret,ret);

	return ret;
}


/*
 * Function:	mpGrid
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes mpGrid
#if	NhlNeedProto
(
        NhlMapRGDataHandlerLayer 	mrgl,
	NhlMapPlotLayer			mpl,
	NhlString			entry_name
)
#else
(mrgl,mpl,entry_name)
        NhlMapRGDataHandlerLayer 	mrgl;
        NhlMapPlotLayer 		mpl;
	NhlString			entry_name;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR;
	NhlMapPlotLayerPart	*mpp = &(mpl->mapplot);
        float pole_param;

	Grid_Setup = False;
	c_mpseti("C2",mpp->grid.gks_color);
	_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	c_mpseti("C4", mpp->limb.gks_color);
	_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
        

	c_mpsetr("GT",mpp->grid_lat_spacing);
	_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	c_mpsetr("GN",mpp->grid_lon_spacing);
	_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
        pole_param = 1000.0 * mpp->grid_max_lat + mpp->grid_polar_lon_spacing;
        c_mpsetr("GP",pole_param);
	_NhlLLErrCheckPrnt(NhlWARNING,entry_name);

	if (mpp->grid_mask_mode != NhlMASKNONE) {
		char *e_text = 
		    "%s: Grid masking not supported for rangs-gshhs database";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
		ret = MIN(ret,NhlWARNING);
	}
	c_mapgrd();
	_NhlLLErrCheckPrnt(NhlWARNING,entry_name);

	return ret;
}

static void SetLineAttrs
(
	NhlMapPlotLayerPart	  *mpp,
	NhlString		  entry_name
        )
{
        int color,dash_pattern;
        float thickness,dash_seglen;
	int type = 1;

        switch (type) {
            case 1:
            case 2:
	    default:
                    color = Mpp->geophysical.gks_color;
                    dash_pattern = Mpp->geophysical.dash_pat;
                    dash_seglen = Mpp->geophysical.dash_seglen;
                    thickness = Mpp->geophysical.thickness;
                    break;
            case 3:
                    color = Mpp->national.gks_color;
                    dash_pattern = Mpp->national.dash_pat;
                    dash_seglen = Mpp->national.dash_seglen;
                    thickness = Mpp->national.thickness;
                    break;
            case 4:
                    color = Mpp->us_state.gks_color;
                    dash_pattern = Mpp->us_state.dash_pat;
                    dash_seglen = Mpp->us_state.dash_seglen;
                    thickness = Mpp->us_state.thickness;
                    break;
        }

#if 0
	/* dash patterns don't work because lines are drawn using GPL at
	   the low level 
	*/

	{
                int	dpat;
                NhlString *sp;
                float	p0,p1,jcrt;
                int	slen;
                char	buffer[128];
		int     i;
                
                dpat = dash_pattern % Mpp->dash_table->num_elements;
                sp = (NhlString *) Mpp->dash_table->data;
                slen = strlen(sp[dpat]);
                p0 =  (float) c_kfpy(0.0);
                _NhlLLErrCheckPrnt(NhlWARNING,entry_name);
                p1 = dash_seglen;
                p1 = (float) c_kfpy(p1);
                _NhlLLErrCheckPrnt(NhlWARNING,entry_name);
                jcrt = (int) ((p1 - p0) / slen + 0.5);
                jcrt = jcrt > 1 ? jcrt : 1;
                strcpy(buffer,sp[dpat]);
	
                c_dashdc(buffer,jcrt,4);
                _NhlLLErrCheckPrnt(NhlWARNING,entry_name);
        }
#endif
	{
              	gset_linewidth(thickness);
                _NhlLLErrCheckPrnt(NhlWARNING,entry_name);
        }
        return;
}


/*
 * Function:	mpOutline
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes mpOutline
#if	NhlNeedProto
(
        NhlMapRGDataHandlerLayer 	mrgl,
	NhlMapPlotLayer			mpl,
	NhlString			entry_name
)
#else
(mrgl,mp,entry_name)
        NhlMapRGDataHandlerLayer 	mrgl;
        NhlMapPlotLayer 		mpl;
	NhlString			entry_name;
#endif
{
        NhlMapRGDataHandlerLayerPart *mrgp = &mrgl->maprgdh;
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	NhlMapPlotLayerPart	*mpp = &(mpl->mapplot);
	NhlWorkspace		*fws;
	int 			irgl;

	if (mrgp->fws_id < 1) {
		mrgp->fws_id = 
			_NhlNewWorkspace(NhlwsOTHER,NhlwsNONE,
					 mpRG_FLT_REQ_SIZE);
		if (mrgp->fws_id < 1) 
			return MIN(ret,(NhlErrorTypes)mrgp->fws_id);
	}
	if ((fws = _NhlUseWorkspace(mrgp->fws_id)) == NULL) {
		e_text = "%s: error reserving area map workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}

	if (mrgp->data_resolution == NhlUNSPECIFIEDRESOLUTION) 
		c_mdrgdl(&irgl);
	else
		irgl = (int)mrgp->data_resolution;

	mrgp->real_data_resolution = (NhlMapDataResolution) irgl;

	_NhlLLErrCheckPrnt(NhlWARNING,entry_name);

	SetLineAttrs(mpp,entry_name);

	subret = _NhlMdrgol(irgl,fws,entry_name);
	ret = MIN(subret,ret);

        subret = _NhlIdleWorkspace(fws);
        if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

	return ret;
}

static NhlErrorTypes MapRGDHDrawMapList
#if	NhlNeedProto
(
	NhlLayer		instance,
        NhlMapPlotLayer 	mpl,
        mpDrawOp		draw_op,
	NhlBoolean		init_draw
        )
#else
(instance,mpl,draw_op,init_draw)
	NhlLayer		instance;
        NhlMapPlotLayer 	mpl;
        mpDrawOp		draw_op;
	NhlDrawOrder		init_draw;
#endif
{
        NhlMapRGDataHandlerLayer mrgl = 
		(NhlMapRGDataHandlerLayer) instance;
	NhlMapPlotLayerPart	  *mpp = &mpl->mapplot;
	NhlString entry_name = "MapRGDHDrawMapList";
        NhlErrorTypes ret = NhlNOERROR;
        int lcol[5], lcsf[5];

	Mpp = mpp;
	Mpl = mpl;
	Mrgp = &mrgl->maprgdh;

	lcsf[0] = Mpp->ocean.color;
	lcsf[1] = Mpp->land.color;
	lcsf[2] = Mpp->inland_water.color;
	lcsf[3] = Mpp->land.color;
	lcsf[4] = Mpp->inland_water.color;
	
	lcol[0] = lcol[1] = lcol[2] = lcol[3] = lcol[4] 
		= Mpp->geophysical.color;

	c_mdrgsc(lcol,lcsf);
	
        switch (draw_op) {
	case mpDRAWFILL:
		ret =  mpFill(mrgl,mpl,entry_name);
		break;
	case mpDRAWOUTLINE:
		ret =  mpOutline(mrgl,mpl,entry_name);
		break;
	case mpDRAWGRID:
		ret =  mpGrid(mrgl,mpl,entry_name);
		break;
	default:
		break;
        }

	Mpp = NULL;
	Mpl = NULL;
	Mrgp = NULL;

        return ret;
}

#if 0

/*
 * Function:  mdrgdi
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
void   (_NHLCALLF(mdrgdi,MDRGDI))
#if	NhlNeedProto
(
        NGstring dinm,
	int len
)

#else
(dinm,len)
        NGstring dinm;
	int len;
#endif
        
{
	NhlString path = getenv("NCARG_RANGS");
	int length;

	if (! path) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "NCARG_RANGS environment variable not set\n\t\n");
		path = ".";
		strcpy(dinm,NGCstrToFstr(path,1));
		return;
        }

	length = NGSTRLEN(path);

	strcpy(dinm,NGCstrToFstr(path,length));

	return;
}

#endif
/*
 * Function:  load_hlumap_routines
 *
 * Description: Forces the hlumap... routines to load from the HLU library
 *
 * In Args:   NhlBoolean flag - should always be False - dont actually
 *			        want to call the routines.
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: 
 */

/*ARGSUSED*/
static void   load_hlumap_routines
#if	NhlNeedProto
(
	NhlBoolean	flag
)
#else
(flag)
	NhlBoolean	flag;
#endif
{
	int idum;
	float fdum;


	if (flag) {
		_NHLCALLF(hlumapeod,HLUMAPEOD)
			(&idum,&idum,&idum,&idum,&idum,&fdum);
	}
	return;
}
