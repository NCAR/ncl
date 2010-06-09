/*
 *      $Id: CairoWorkstation.c,v 1.8 2010-03-29 16:30:03 brownrig Exp $
 */

# include   <stdio.h>
# include   <string.h>
# include   <ncarg/hlu/CairoWorkstationP.h>
# include   <ncarg/hlu/ConvertersP.h>
# include   <ncarg/hlu/pageutil.h>

# define    Oset(field)     NhlOffset(NhlCairoWorkstationLayerRec, cairo.field)

/* resources for the PS-PDF Workstation */
static NhlResource resourcesPSPDFWS[] = {
		/* Begin-documented-resources */

    {NhlNwkFormat,NhlCwkFormat,NhlTCairoFormat,sizeof(NhlCairoFormat),
        Oset(format),NhlTImmediate,(NhlPointer)NhlCPS,
        _NhlRES_NOSACCESS,NULL},
    {NhlNwkFileName,NhlCwkFileName,NhlTString,
        sizeof(NhlString),Oset(filename),NhlTImmediate,
        (NhlPointer)NULL,_NhlRES_NOSACCESS,(NhlFreeFunc)NhlFree},
    {NhlNwkOrientation,NhlCwkOrientation,NhlTWorkOrientation,
        sizeof(NhlWorkOrientation),Oset(orientation),NhlTImmediate,
        (NhlPointer)NhlPORTRAIT,_NhlRES_DEFAULT,NULL},
    {NhlNwkPDFResolution,NhlCwkPDFResolution,NhlTInteger,
        sizeof(int),Oset(dpi),NhlTImmediate,
        (NhlPointer)1800,_NhlRES_NOSACCESS,NULL},

    /* these page size and margins are initialized as "-1" here, and are given appropriate
     * values when the workstation is opened, depending upon which resources are actually
     * available at that time.
     */
    {NhlNwkPaperSize,NhlCwkPaperSize,NhlTString,
        sizeof(NhlString),Oset(paper_size),NhlTImmediate,
        _NhlUSET(PAGEUTIL_DEFAULT_PAPERSIZE),_NhlRES_DEFAULT,(NhlFreeFunc)NhlFree},
    {NhlNwkPaperWidthF, NhlCwkPaperWidthF, NhlTFloat,
        sizeof(float), Oset(page_width), NhlTString,
        _NhlUSET("-1."), _NhlRES_DEFAULT, NULL},
    {NhlNwkPaperHeightF, NhlCwkPaperHeightF, NhlTFloat,
        sizeof(float), Oset(page_height), NhlTString,
        _NhlUSET("-1."), _NhlRES_DEFAULT, NULL},
    {NhlNwkDeviceLowerX,NhlCwkDeviceLowerX,NhlTInteger,
        sizeof(int),Oset(lower_x),NhlTImmediate,
        (NhlPointer)-1,_NhlRES_DEFAULT,NULL},
    {NhlNwkDeviceLowerY,NhlCwkDeviceLowerY,NhlTInteger,
        sizeof(int),Oset(lower_y),NhlTImmediate,
        (NhlPointer)-1,_NhlRES_DEFAULT,NULL},
    {NhlNwkDeviceUpperX,NhlCwkDeviceUpperX,NhlTInteger,
        sizeof(int),Oset(upper_x),NhlTImmediate,
        (NhlPointer)-1,_NhlRES_DEFAULT,NULL},
    {NhlNwkDeviceUpperY,NhlCwkDeviceUpperY,NhlTInteger,
        sizeof(int),Oset(upper_y),NhlTImmediate,
        (NhlPointer)-1,_NhlRES_DEFAULT,NULL},

/* End-documented-resources */
};

/* Resources for the Image Workstation */
static NhlResource resourcesImageWS[] = {
        /* Begin-documented-resources */

    {NhlNwkFormat,NhlCwkFormat,NhlTCairoFormat,sizeof(NhlCairoFormat),
        Oset(format),NhlTImmediate,(NhlPointer)NhlCPNG,
        _NhlRES_NOSACCESS,NULL},
    {NhlNwkFileName,NhlCwkFileName,NhlTString,
        sizeof(NhlString),Oset(filename),NhlTImmediate,
        (NhlPointer)NULL,_NhlRES_NOSACCESS,(NhlFreeFunc)NhlFree},
    {NhlNwkOrientation,NhlCwkOrientation,NhlTWorkOrientation,
        sizeof(NhlWorkOrientation),Oset(orientation),NhlTImmediate,
        (NhlPointer)NhlPORTRAIT,_NhlRES_DEFAULT,NULL},

    {NhlNwkDeviceLowerX,NhlCwkDeviceLowerX,NhlTInteger,
        sizeof(int),Oset(lower_x),NhlTImmediate,
        (NhlPointer)-1,_NhlRES_DEFAULT,NULL},
    {NhlNwkDeviceLowerY,NhlCwkDeviceLowerY,NhlTInteger,
        sizeof(int),Oset(lower_y),NhlTImmediate,
        (NhlPointer)-1,_NhlRES_DEFAULT,NULL},
    {NhlNwkDeviceUpperX,NhlCwkDeviceUpperX,NhlTInteger,
        sizeof(int),Oset(upper_x),NhlTImmediate,
        (NhlPointer)-1,_NhlRES_DEFAULT,NULL},
    {NhlNwkDeviceUpperY,NhlCwkDeviceUpperY,NhlTInteger,
        sizeof(int),Oset(upper_y),NhlTImmediate,
        (NhlPointer)-1,_NhlRES_DEFAULT,NULL},

    /* Resources for image-based output formats. We'll use the existing NGC_PIXCONFIG struct
     * for this purpose, but not all of its fields will be utilized.
     */
    {_NhlNwkPixConf,_NhlCwkPixConf,NhlTInteger,sizeof(int),Oset(pixconfig.type),
        NhlTImmediate,(NhlPointer)NGC_PIXCONFIG,
        _NhlRES_NOACCESS|_NhlRES_PRIVATE,NULL},
    {NhlNwkWidth,NhlCwkWidth,NhlTInteger,sizeof(int),
        Oset(pixconfig.width),NhlTImmediate,
        (NhlPointer)1024,_NhlRES_DEFAULT,NULL},
    {NhlNwkHeight,NhlCwkHeight,NhlTInteger,sizeof(int),
        Oset(pixconfig.height),NhlTImmediate,
        (NhlPointer)1024,_NhlRES_DEFAULT,NULL},
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

static NhlErrorTypes CairoPSPDFWorkstationOpen(
    NhlLayer    /* instance */
);

static NhlErrorTypes CairoImageWorkstationOpen(
    NhlLayer    /* instance */
);

static NhlErrorTypes CairoPSPDFWorkstationActivate(
    NhlLayer    l   /* instance */
);

static NhlErrorTypes CairoImageWorkstationActivate(
    NhlLayer    l   /* instance */
);

/* class-record for PS/PDF output formats */
NhlCairoWorkstationClassRec NhlcairoPSPDFWorkstationClassRec = {
    {
        /* class_name           */  "documentWorkstationClass",
        /* nrm_class            */  NrmNULLQUARK,
        /* layer_size           */  sizeof(NhlCairoWorkstationLayerRec),
        /* class_inited         */  False,
        /* superclass           */  (NhlClass)&NhlworkstationClassRec,
        /* cvt_table            */  NULL,

        /* layer_resources      */  resourcesPSPDFWS,
        /* num_resources        */  NhlNumber(resourcesPSPDFWS),
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
        /* open_work         */     CairoPSPDFWorkstationOpen,
        /* close_work        */     NhlInheritClose,
        /* activate_work     */     CairoPSPDFWorkstationActivate,
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

/* class-record for image-based output formats */
NhlCairoWorkstationClassRec NhlcairoImageWorkstationClassRec = {
    {
        /* class_name           */  "imageWorkstationClass",
        /* nrm_class            */  NrmNULLQUARK,
        /* layer_size           */  sizeof(NhlCairoWorkstationLayerRec),
        /* class_inited         */  False,
        /* superclass           */  (NhlClass)&NhlworkstationClassRec,
        /* cvt_table            */  NULL,

        /* layer_resources      */  resourcesImageWS,
        /* num_resources        */  NhlNumber(resourcesImageWS),
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
        /* open_work         */     CairoImageWorkstationOpen,
        /* close_work        */     NhlInheritClose,
        /* activate_work     */     CairoImageWorkstationActivate,
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

NhlClass NhlcairoPSPDFWorkstationClass = (NhlClass) &NhlcairoPSPDFWorkstationClassRec;
NhlClass NhlcairoImageWorkstationClass = (NhlClass) &NhlcairoImageWorkstationClassRec;

/*
 * Function:    nhlfcairopspdfworkstationclass
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
_NHLCALLF(nhlfcairopspdfworkstationclass, NHLFCAIROPSPDFWORKSTATIONCLASS)
(
    void
)
{
    return NhlcairoPSPDFWorkstationClass;
}

NhlClass
_NHLCALLF(nhlfcairoimageworkstationclass, NHLFCAIROIMAGEWORKSTATIONCLASS)
(
    void
)
{
    return NhlcairoImageWorkstationClass;
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
    _NhlEnumVals    psPdfFormats[] = {
        {NhlCPS,     "NEWPS"},
        {NhlCPDF,    "NEWPDF"}
    };

    _NhlEnumVals    imageFormats[] = {
        {NhlCPNG,    "NEWPNG"},
        {NhlCPNG,    "PNG"},
        {NhlCTIFF,   "TIFF"}
    };

    _NhlEnumVals    orientvals[] = {
        {NhlPORTRAIT,   "Portrait"},
        {NhlLANDSCAPE,  "Landscape"}
    };


    (void) _NhlRegisterEnumType(NhlcairoPSPDFWorkstationClass,NhlTCairoFormat,
        psPdfFormats,NhlNumber(psPdfFormats));

    (void) _NhlRegisterEnumType(NhlcairoImageWorkstationClass,NhlTCairoFormat,
        imageFormats,NhlNumber(imageFormats));

    (void) _NhlRegisterEnumType(NhlcairoPSPDFWorkstationClass,NhlTWorkOrientation,
        orientvals,NhlNumber(orientvals));

    fnameQ = NrmStringToQuark(NhlNwkFileName);

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
    case NhlCTIFF:
        newCairo->work.gkswkstype = CTIFF;
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
                func, cairo->filename, NhlNwkFileName);
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
    if (strlen(tfname) > _NhlMAXLLUPATHLEN) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,
		  "%s: Filepath %s exceeds maximum length of %d", func,
		  tfname,_NhlMAXLLUPATHLEN);
	return NhlFATAL;
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

    if ((newCairo->work.gkswkstype == CPS || newCairo->work.gkswkstype == CPDF) && cairo->paper_size) {
        char* tmpStr = cairo->paper_size;
        cairo->paper_size = NhlMalloc(strlen(tmpStr) + 1);
        if (!cairo->paper_size) {
            NHLPERROR((NhlFATAL,ENOMEM,NULL));
            return NhlFATAL;
        }
        strcpy(cairo->paper_size, tmpStr);
    }

    if (cairo->lower_x > 0 && cairo->upper_x > 0 && cairo->lower_x >= cairo->upper_x) {
        NhlPError(NhlWARNING,NhlEUNKNOWN,
            "%s:Device X Coordinates invalid, defaulting",func);
        ret = NhlWARNING;
        cairo->lower_x = 36;
        cairo->upper_x = 576;
    }

    if (cairo->lower_y > 0 && cairo->upper_y > 0 && cairo->lower_y >= cairo->upper_y) {
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
 * Function:    CairoXXXXXWorkstationOpen
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
CairoPSPDFWorkstationOpen(NhlLayer l)
{
    char func[]              = "CairoPSPDFWorkstationOpen";
    NhlWorkstationLayer work = (NhlWorkstationLayer)l;
    NhlCairoWorkstationLayerPart  *pp = &((NhlCairoWorkstationLayer)l)->cairo;
    Gescape_in_data gesc_in_pixconf;
    NhlErrorTypes   ret;
    int d, w, h;
    int su = 0;

    /* make use of a shared utility method that contains all the page-sizing logic common to cairo-document,
     * postscript, and PDF workstations. See pageutil.c
     */
    NhlPageInfo pageInfo;
    pageInfo.paperSize = pp->paper_size;
    pageInfo.paperSizeResName = NhlNwkPaperSize;
    pageInfo.paperWidthIn = pp->page_width;
    pageInfo.paperWidthResName = NhlNwkPaperWidthF;
    pageInfo.paperHeightIn = pp->page_height;
    pageInfo.paperHeightResName = NhlNwkPaperHeightF;

    ret = nhlGetPaperSize(&pageInfo);

    /* unbundle returned values */
    pp->page_width = pageInfo.paperWidthIn;
    pp->page_height = pageInfo.paperHeightIn;
    pp->lower_x = (pp->lower_x < 0) ? pageInfo.leftMargin : pp->lower_x;
    pp->upper_x = (pp->upper_x < 0) ? pageInfo.rightMargin: pp->upper_x;
    pp->lower_y = (pp->lower_y < 0) ? pageInfo.bottomMargin : pp->lower_y;
    pp->upper_y = (pp->upper_y < 0) ? pageInfo.topMargin : pp->upper_y;

    /* Note that these can be set for the "next" workstation */
    c_ngsetc("me", pp->filename);
    c_ngseti("pw", pageInfo.pageWidthPts);
    c_ngseti("ph", pageInfo.pageHeightPts);
    c_ngseti("co", (pp->dpi/72 + 1));
    c_ngseti("lx", pp->lower_x);
    c_ngseti("ux", pp->upper_x);
    c_ngseti("ly", pp->lower_y);
    c_ngseti("uy", pp->upper_y);

    ret = (*NhlworkstationClassRec.work_class.open_work)(l);

    /* these have to be set after the workstation is opened... */
    c_ngseti("wo", _NhlWorkstationId(l));
    c_ngseti("pl", pp->orientation);

    w = pp->upper_x - pp->lower_x;
    h = pp->upper_y - pp->lower_y;
    d = MAX(w, h);
    work->work.vswidth_dev_units = d/72*pp->dpi;

    return ret;
}

static NhlErrorTypes
CairoImageWorkstationOpen(NhlLayer l)
{
    char func[]              = "CairoImageWorkstationOpen";
    NhlWorkstationLayer work = (NhlWorkstationLayer)l;
    NhlCairoWorkstationLayerPart  *pp = &((NhlCairoWorkstationLayer)l)->cairo;
    Gescape_in_data gesc_in_pixconf;
    NhlErrorTypes   ret;

    /* we need to calculate the NDC frame within the image... */
    int minRange = (pp->pixconfig.width < pp->pixconfig.height) ? pp->pixconfig.width : pp->pixconfig.height;
    int adjust = (pp->pixconfig.width - minRange) / 2;
    pp->lower_x = adjust;
    pp->upper_x = pp->pixconfig.width - adjust;
    adjust = (pp->pixconfig.height - minRange) / 2;
    pp->lower_y = adjust;
    pp->upper_y = pp->pixconfig.height - adjust;

    /* Note that these can be set for the "next" workstation */
    c_ngsetc("me", pp->filename);
    c_ngseti("lx", pp->lower_x);
    c_ngseti("ux", pp->upper_x);
    c_ngseti("ly", pp->lower_y);
    c_ngseti("uy", pp->upper_y);

    /* image width/height must be set before opening workstation */
    pp->pixconfig.work_id = -1;  /* part of the escape mechanism; -1 means "apply to *next* workstation */
    gesc_in_pixconf.escape_r1.data = &pp->pixconfig;
    gesc_in_pixconf.escape_r1.size = sizeof(pp->pixconfig);
    gescape(NGESC_CNATIVE,&gesc_in_pixconf,NULL,NULL);

    ret = (*NhlworkstationClassRec.work_class.open_work)(l);

    return ret;
}

/*
 * Function:    CairoXXXXXWorkstationActivate
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
CairoPSPDFWorkstationActivate(NhlLayer l)
{
    char    func[] = "CairoPSPDFWorkstationActivate";
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

static NhlErrorTypes
CairoImageWorkstationActivate(NhlLayer l)
{
    char    func[] = "CairoImageWorkstationActivate";
    NhlWorkstationClass lc = (NhlWorkstationClass) NhlworkstationClass;
    return (*(lc->work_class.activate_work))(l);
}
