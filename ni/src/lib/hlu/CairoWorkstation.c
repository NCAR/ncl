/*
 *      $Id: CairoWorkstation.c,v 1.2 2009-12-18 23:15:50 brownrig Exp $
 */

# include   <stdio.h>
# include   <string.h>
# include   <ncarg/hlu/CairoWorkstationP.h>
# include   <ncarg/hlu/ConvertersP.h>


# define    Oset(field)     NhlOffset(NhlCairoWorkstationLayerRec, cairo.field)

static NhlResource resources[] = {
		/* Begin-documented-resources */

    {NhlNwkCairoFormat,NhlCwkCairoFormat,NhlTCairoFormat,sizeof(NhlCairoFormat),
        Oset(format),NhlTImmediate,(NhlPointer)NhlCPS,
        _NhlRES_NOSACCESS,NULL},
    {NhlNwkCairoFileName,NhlCwkCairoFileName,NhlTString,
        sizeof(NhlString),Oset(filename),NhlTImmediate,
        (NhlPointer)NULL,_NhlRES_NOSACCESS,(NhlFreeFunc)NhlFree},
    {NhlNwkOrientation,NhlCwkOrientation,NhlTWorkOrientation,
        sizeof(NhlWorkOrientation),Oset(orientation),NhlTImmediate,
        (NhlPointer)NhlPORTRAIT,_NhlRES_DEFAULT,NULL},
    {NhlNwkPDFResolution,NhlCwkPDFResolution,NhlTInteger,
        sizeof(int),Oset(dpi),NhlTImmediate,
        (NhlPointer)1800,_NhlRES_NOSACCESS,NULL},
    {NhlNwkDeviceLowerX,NhlCwkDeviceLowerX,NhlTInteger,
        sizeof(int),Oset(lower_x),NhlTImmediate,
        (NhlPointer)36,_NhlRES_DEFAULT,NULL},
    {NhlNwkDeviceLowerY,NhlCwkDeviceLowerY,NhlTInteger,
        sizeof(int),Oset(lower_y),NhlTImmediate,
        (NhlPointer)126,_NhlRES_DEFAULT,NULL},
    {NhlNwkDeviceUpperX,NhlCwkDeviceUpperX,NhlTInteger,
        sizeof(int),Oset(upper_x),NhlTImmediate,
        (NhlPointer)576,_NhlRES_DEFAULT,NULL},
    {NhlNwkDeviceUpperY,NhlCwkDeviceUpperY,NhlTInteger,
        sizeof(int),Oset(upper_y),NhlTImmediate,
        (NhlPointer)666,_NhlRES_DEFAULT,NULL},
    {NhlNwkWidth,NhlCwkWidth,NhlTInteger,sizeof(int),
        Oset(xres),NhlTImmediate,
        (NhlPointer)512,_NhlRES_DEFAULT,NULL},
    {NhlNwkHeight,NhlCwkHeight,NhlTInteger,sizeof(int),
        Oset(yres),NhlTImmediate,
        (NhlPointer)512,_NhlRES_DEFAULT,NULL},

#if 0
    {NhlNwkVisualType,NhlCwkVisualType,NhlTVisualType,sizeof(NhlVisualType),
        Oset(visual),NhlTImmediate,(NhlPointer)NhlCOLOR,
        _NhlRES_NOSACCESS,NULL},
    {NhlNwkOrientation,NhlCwkOrientation,NhlTWorkOrientation,
        sizeof(NhlWorkOrientation),Oset(orientation),NhlTImmediate,
        (NhlPointer)NhlPORTRAIT,_NhlRES_DEFAULT,NULL},
    {NhlNwkPDFResolution,NhlCwkPDFResolution,NhlTInteger,
        sizeof(int),Oset(resolution),NhlTImmediate,
        (NhlPointer)1800,_NhlRES_NOSACCESS,NULL},
    {NhlNwkDeviceLowerX,NhlCwkDeviceLowerX,NhlTInteger,
        sizeof(int),Oset(lower_x),NhlTImmediate,
        (NhlPointer)36,_NhlRES_DEFAULT,NULL},
    {NhlNwkDeviceLowerY,NhlCwkDeviceLowerY,NhlTInteger,
        sizeof(int),Oset(lower_y),NhlTImmediate,
        (NhlPointer)126,_NhlRES_DEFAULT,NULL},
    {NhlNwkDeviceUpperX,NhlCwkDeviceUpperX,NhlTInteger,
        sizeof(int),Oset(upper_x),NhlTImmediate,
        (NhlPointer)576,_NhlRES_DEFAULT,NULL},
    {NhlNwkDeviceUpperY,NhlCwkDeviceUpperY,NhlTInteger,
        sizeof(int),Oset(upper_y),NhlTImmediate,
        (NhlPointer)666,_NhlRES_DEFAULT,NULL},
    {NhlNwkFullBackground,NhlCwkFullBackground,NhlTBoolean,
        sizeof(NhlBoolean),Oset(full_background),NhlTImmediate,
        (NhlPointer)False,_NhlRES_DEFAULT,NULL},
    {NhlNwkColorModel,NhlCwkColorModel,NhlTColorModel,
        sizeof(NhlColorModel),
        Oset(color_model),NhlTImmediate,(NhlPointer)NhlRGB,
        _NhlRES_NOSACCESS,NULL},
    {NhlNwkSuppressBackground,NhlCwkSuppressBackground,NhlTBoolean,
        sizeof(NhlBoolean),Oset(suppress_background),NhlTImmediate,
        (NhlPointer)False,_NhlRES_NOSACCESS,NULL},
    {NhlNwkSuppressBBInfo,NhlCwkSuppressBBInfo,NhlTBoolean,
        sizeof(NhlBoolean),Oset(suppress_bbinfo),NhlTImmediate,
        (NhlPointer)False,_NhlRES_NOSACCESS,NULL},

/* End-documented-resources */
#endif
};


/*
 * CairoWorkstation base_class method declarations
 */

static NhlErrorTypes CairoWorkstationClassInitialize(
		void
);

static NhlErrorTypes CairoWorkstationInitialize(
        NhlClass,       /* class */
        NhlLayer,       /* req */
        NhlLayer,       /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
);

static NhlErrorTypes CairoWorkstationClassPartInitialize(
        NhlClass        /* lc */
);

static NhlErrorTypes CairoWorkstationDestroy(
        NhlLayer        /* inst */
);

static NhlErrorTypes CairoWorkstationSetValues(
        NhlLayer,       /* old */
        NhlLayer,       /* reference */
        NhlLayer,       /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
);

static NhlErrorTypes CairoWorkstationGetValues(
    NhlLayer,       /* l */
    _NhlArgList,    /* args */
    int             /* num_args */
);

/*
 * CairoWorkstation work_class method declarations
 */

static NhlErrorTypes CairoWorkstationOpen(
    NhlLayer    /* instance */
);

static NhlErrorTypes CairoWorkstationActivate(
    NhlLayer    l   /* instance */
);

NhlCairoWorkstationClassRec NhlcairoWorkstationClassRec = {
    {
        /* class_name           */  "cairoWorkstationClass",
        /* nrm_class            */  NrmNULLQUARK,
        /* layer_size           */  sizeof(NhlCairoWorkstationLayerRec),
        /* class_inited         */  False,
        /* superclass           */  (NhlClass)&NhlworkstationClassRec,
        /* cvt_table            */  NULL,

        /* layer_resources      */  resources,
        /* num_resources        */  NhlNumber(resources),
        /* all_resources        */  NULL,
        /* callbacks            */  NULL,
        /* num_callbacks        */  0,
        /* class_callbacks      */  NULL,
        /* num_class_callbacks  */  0,

        /* class_part_initialize */     CairoWorkstationClassPartInitialize,
        /* class_initialize      */     CairoWorkstationClassInitialize,
        /* layer_initialize      */     CairoWorkstationInitialize,
        /* layer_set_values      */     CairoWorkstationSetValues,
        /* layer_set_values_hook */     NULL,
        /* layer_get_values      */     CairoWorkstationGetValues,
        /* layer_reparent        */     NULL,
        /* layer_destroy         */     CairoWorkstationDestroy,

        /* child_resources       */     NULL,

        /* layer_draw            */     NULL,

        /* layer_pre_draw        */     NULL,
        /* layer_draw_segonly    */     NULL,
        /* layer_post_draw       */     NULL,
        /* layer_clear           */     NULL
    },

    {
        /* current_wks_count */     NhlInheritCurrentWksCount,
        /* gks_wks_recs      */     NhlInheritGksWksRecs,
        /* hlu_wks_flag      */     NhlInheritHluWksFlag,
        /* def_background    */     {1.0,1.0,1.0},
        /* rgb_dbm           */     NULL,
        /* pal               */     NhlInheritPalette,
        /* open_work         */     CairoWorkstationOpen,
        /* close_work        */     NhlInheritClose,
        /* activate_work     */     CairoWorkstationActivate,
        /* deactivate_work   */     NhlInheritDeactivate,
        /* alloc_colors      */     NhlInheritAllocateColors,
        /* update_work       */     NhlInheritUpdate,
        /* clear_work        */     NhlInheritClear,
        /* lineto_work       */     NhlInheritLineTo,
        /* fill_work         */     NhlInheritFill,
        /* marker_work       */     NhlInheritMarker,
        /* notify_work       */     NULL,
	/* update_drawbb     */      NULL
    },

    {
        /* foo  */          0
    }

};

NhlClass NhlcairoWorkstationClass = (NhlClass) &NhlcairoWorkstationClassRec;

/*
 * Function:    nhlfcairoworkstationclass
 *
 * Description: fortran ref to this class
 *
 * In Args:
 *
 * Out Args:
 *
 * Scope:   global Fortran
 * Returns: NhlClass
 * Side Effect:
 */

NhlClass
_NHLCALLF(nhlfcairoworkstationclass, NHLFCAIROWORKSTATIONCLASS)
(
    void
)
{
    return NhlcairoWorkstationClass;
}

/*
 * Function:    CairoWorkstationClassPartInitialize
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

static NhlErrorTypes
CairoWorkstationClassPartInitialize(NhlClass layerClass)
{
    return NhlNOERROR;
}


static NrmQuark fnameQ = NrmNULLQUARK;

/*
 * Function:    CairoWorkstationClassInitialize
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

static NhlErrorTypes
CairoWorkstationClassInitialize(void)
{
    _NhlEnumVals    fmtvals[] = {
        {NhlCPS,     "CPS"},
        {NhlCPNG,    "CPNG"},
        {NhlCPDF,    "CPDF"}
    };

    _NhlEnumVals    orientvals[] = {
        {NhlPORTRAIT,   "Portrait"},
        {NhlLANDSCAPE,  "Landscape"}
    };


    (void) _NhlRegisterEnumType(NhlcairoWorkstationClass,NhlTCairoFormat,
        fmtvals,NhlNumber(fmtvals));

    (void) _NhlRegisterEnumType(NhlcairoWorkstationClass,NhlTWorkOrientation,
        orientvals,NhlNumber(orientvals));

    fnameQ = NrmStringToQuark(NhlNwkCairoFileName);

    return NhlNOERROR;
}

/*
 * Function:    CairoWorkstationInitialize
 *
 * Description: Set Cairo Workstation type, filename, device coordinates
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
static NhlErrorTypes CairoWorkstationInitialize(NhlClass lclass, NhlLayer req, NhlLayer new, _NhlArgList args, int num_args)
{
    char    func[]= "CairoWorkstationInitialize";
    NhlCairoWorkstationLayer  newCairo = (NhlCairoWorkstationLayer) new;
    NhlCairoWorkstationLayerPart  *cairo = &newCairo->cairo;
    char    *tfname = NULL;
    char    buff[_NhlMAXFNAMELEN];
    NhlErrorTypes   ret = NhlNOERROR;

#ifndef BuildCAIRO

    NhlPError(NhlFATAL,NhlEUNKNOWN,
        "%s:CairoWorkstation support was not built into NCL", func);
    return NhlFATAL;

#else
    /*
     * Set gkswkstype
     */
    switch (cairo->format) {
    case NhlCPS:
    	newCairo->work.gkswkstype = CPS;
    	break;
    case NhlCPNG:
    	newCairo->work.gkswkstype = CPNG;
    	break;
    case NhlCPDF:
        newCairo->work.gkswkstype = CPDF;
        break;
    default:
        NhlPError(NhlWARNING, NhlEUNKNOWN,
            "%s: Invalid format \"%s\" defaulting to postscript",
            func, cairo->format);
        newCairo->work.gkswkstype = CPS;
        break;
    }

    newCairo->work.gkswksconid = 0;

    if (cairo->filename) {
        tfname = (char*) _NGResolvePath(cairo->filename);
        if (!tfname) {
            NhlPError(NhlWARNING,NhlEUNKNOWN,
                "%s:Unable to resolve path name for \"%s\", defaulting %s",
                func, cairo->filename, NhlNwkCairoFileName);
            ret = NhlWARNING;
        }
    }

    if (!tfname) {
        strcpy(buff, new->base.name);
        strcat(buff, ".");
        switch(cairo->format){
            case NhlCPS:
                strcat(buff, "ps");
                break;

            case NhlCPNG:
				strcat(buff, "png");
				break;

            case NhlCPDF:
                strcat(buff, "pdf");
                break;

            default:
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                    "%s:Unsupported Cairo format %d?", func,
                    cairo->format);
                return NhlFATAL;
        }
        tfname = buff;
    }

    /* This looks like a mem-leak at first glance, but upstream in _NhlCreate, a "context" has been keeping
     * track of memory allocated temporarily for the Layer, and cleans that up.
     *
     */
    cairo->filename = NhlMalloc(strlen(tfname) + 1);
    if (!cairo->filename) {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }
    strcpy(cairo->filename,tfname);

    if (cairo->lower_x >= cairo->upper_x) {
        NhlPError(NhlWARNING,NhlEUNKNOWN,
            "%s:Device X Coordinates invalid, defaulting",func);
        ret = NhlWARNING;
        cairo->lower_x = 36;
        cairo->upper_x = 576;
    }

    if (cairo->lower_y >= cairo->upper_y) {
        NhlPError(NhlWARNING,NhlEUNKNOWN,
            "%s:Device Y Coordinates invalid, defaulting",func);
        ret = NhlWARNING;
        cairo->lower_y = 126;
        cairo->upper_y = 666;
    }
    cairo->dev_bounds_updated = False;

    return ret;
#endif
}

/*
 * Function:    CairoWorkstationSetValues
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
static NhlErrorTypes CairoWorkstationSetValues
(
    NhlLayer    old,
    NhlLayer    ref,
    NhlLayer    new,
    _NhlArgList args,
    int     nargs
)
{
    char    func[]= "CairoWorkstationInitialize";
    NhlCairoWorkstationLayerPart *np = &((NhlCairoWorkstationLayer)new)->cairo;
    NhlCairoWorkstationLayerPart *op = &((NhlCairoWorkstationLayer)old)->cairo;
    NhlErrorTypes ret = NhlNOERROR;

#if 0
    if (np->full_background != op->full_background) {
        c_ngseti("wo", _NhlWorkstationId(new));
        c_ngseti("fu", np->full_background);
    }
#endif

    if (np->lower_x != op->lower_x ||
        np->upper_x != op->upper_x ||
        np->lower_y != op->lower_y ||
        np->upper_y != op->upper_y ||
        np->orientation != op->orientation)
        np->dev_bounds_updated = True;

    if (np->lower_x >= np->upper_x) {
        NhlPError(NhlWARNING,NhlEUNKNOWN,
            "%s:Device X Coordinates invalid, defaulting",func);
        ret = NhlWARNING;
        np->lower_x = 36;
        np->upper_x = 576;
    }

    if (np->lower_y >= np->upper_y) {
        NhlPError(NhlWARNING,NhlEUNKNOWN,
            "%s:Device Y Coordinates invalid, defaulting",func);
        ret = NhlWARNING;
        np->lower_y = 126;
        np->upper_y = 666;
    }

    return ret;
}

/*
 * Function:    CairoWorkstationGetValues
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

static NhlErrorTypes
CairoWorkstationGetValues(NhlLayer l, _NhlArgList args, int nargs)
{
    char    func[]= "CairoWorkStationGetValues";
    register int    i;
    NhlCairoWorkstationLayerPart   *pp = &((NhlCairoWorkstationLayer)l)->cairo;
    NhlString   str;
    NhlErrorTypes   ret = NhlNOERROR;

    for (i = 0; i < nargs; i++) {
        str = NULL;

        if(args[i].quark == fnameQ) {
            str = pp->filename;
        }

        if (str != NULL) {
            *(NhlString *)args[i].value.ptrval = NhlMalloc(strlen(str)+1);
            if (! *(NhlString *)args[i].value.ptrval) {
                NhlPError(NhlWARNING,ENOMEM,
                    "%s:Unable to retrieve %s",func,
                    NrmQuarkToString(args[i].quark));
                ret = NhlWARNING;
            }
            else
                strcpy(*(NhlString *) args[i].value.ptrval, str);
        }
    }

    return ret;
}

/*
 * Function:    CairoWorkstationDestroy
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

static NhlErrorTypes
CairoWorkstationDestroy(NhlLayer l)
{
    NhlCairoWorkstationLayerPart   *cairo = &((NhlCairoWorkstationLayer)l)->cairo;

    NhlFree(cairo->filename);

    return NhlNOERROR;
}

/*
 * Function:    CairoWorkstationOpen
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

static NhlErrorTypes
CairoWorkstationOpen(NhlLayer l)
{
    NhlWorkstationLayer work = (NhlWorkstationLayer)l;
    NhlCairoWorkstationLayerPart  *pp = &((NhlCairoWorkstationLayer)l)->cairo;
    NhlErrorTypes   ret;
    int d, w, h;
    int su = 0;

    c_ngsetc("me", pp->filename);
    c_ngseti("co", (pp->dpi/72 + 1));

    ret = (*NhlworkstationClassRec.work_class.open_work)(l);

    c_ngseti("wo", _NhlWorkstationId(l));
    c_ngseti("lx", pp->lower_x);
    c_ngseti("ux", pp->upper_x);
    c_ngseti("ly", pp->lower_y);
    c_ngseti("uy", pp->upper_y);
    c_ngseti("pl", pp->orientation);

    w = pp->upper_x - pp->lower_x;
    h = pp->upper_y - pp->lower_y;
    d = MAX(w, h);
    work->work.vswidth_dev_units = d/72*pp->dpi;
    return ret;
}

/*
 * Function:    CairoWorkstationActivate
 *
 * Description:
 *
 * In Args:
 *      NhlLayer    l
 *
 * Out Args:
 *
 * Scope:   static
 * Returns: NhlErrorTypes
 * Side Effect:
 */

static NhlErrorTypes
CairoWorkstationActivate(NhlLayer l)
{
    char    func[] = "CairoWorkstationClear";
    NhlWorkstationClass lc = (NhlWorkstationClass) NhlworkstationClass;
    NhlWorkstationLayerPart *wp = &((NhlWorkstationLayer)l)->work;
    NhlCairoWorkstationLayerPart *pp = &((NhlCairoWorkstationLayer)l)->cairo;
    int w, h, d;

    if (wp->cleared && pp->dev_bounds_updated) {
        c_ngseti("wo", _NhlWorkstationId(l));
        c_ngseti("lx", pp->lower_x);
        c_ngseti("ux", pp->upper_x);
        c_ngseti("ly", pp->lower_y);
        c_ngseti("uy", pp->upper_y);
        c_ngseti("pl", pp->orientation);
        pp->dev_bounds_updated = False;
    }

    w = pp->upper_x - pp->lower_x;
    h = pp->upper_y - pp->lower_y;
    d = MAX(w, h);
    wp->vswidth_dev_units = d/72 * pp->dpi;

    return (*(lc->work_class.activate_work))(l);
}
