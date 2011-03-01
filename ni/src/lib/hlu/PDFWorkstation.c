/*
 *      $Id: PDFWorkstation.c,v 1.5 2010-03-29 16:30:03 brownrig Exp $
 */

# include   <stdio.h>
# include   <string.h>
# include   <ncarg/hlu/PDFWorkstationP.h>
# include   <ncarg/hlu/ConvertersP.h>
# include   <ncarg/hlu/pageutil.h>

# define    Oset(field)     NhlOffset(NhlPDFWorkstationLayerRec, pdf.field)

static NhlResource resources[] = {
/* Begin-documented-resources */

    {NhlNwkPDFFormat,NhlCwkPDFFormat,NhlTPDFFormat,sizeof(NhlPDFFormat),
        Oset(format),NhlTImmediate,_NhlUSET((NhlPointer)NhlPDF),
        _NhlRES_NOSACCESS,NULL},
    {NhlNwkVisualType,NhlCwkVisualType,NhlTVisualType,sizeof(NhlVisualType),
        Oset(visual),NhlTImmediate,_NhlUSET((NhlPointer)NhlCOLOR),
        _NhlRES_NOSACCESS,NULL},
    {NhlNwkOrientation,NhlCwkOrientation,NhlTWorkOrientation,
        sizeof(NhlWorkOrientation),Oset(orientation),NhlTImmediate,
        _NhlUSET((NhlPointer)NhlPORTRAIT),_NhlRES_DEFAULT,NULL},
    {NhlNwkPDFFileName,NhlCwkPDFFileName,NhlTString,
        sizeof(NhlString),Oset(filename),NhlTImmediate,
        _NhlUSET((NhlPointer)NULL),_NhlRES_NOSACCESS,(NhlFreeFunc)NhlFree},
    {NhlNwkPDFResolution,NhlCwkPDFResolution,NhlTInteger,
        sizeof(int),Oset(resolution),NhlTImmediate,
        _NhlUSET((NhlPointer)1800),_NhlRES_NOSACCESS,NULL},
    {NhlNwkFullBackground,NhlCwkFullBackground,NhlTBoolean,
        sizeof(NhlBoolean),Oset(full_background),NhlTImmediate,
        _NhlUSET((NhlPointer)False),_NhlRES_DEFAULT,NULL},
    {NhlNwkColorModel,NhlCwkColorModel,NhlTColorModel,
        sizeof(NhlColorModel),
        Oset(color_model),NhlTImmediate,_NhlUSET((NhlPointer)NhlRGB),
        _NhlRES_NOSACCESS,NULL},
    {NhlNwkSuppressBackground,NhlCwkSuppressBackground,NhlTBoolean,
        sizeof(NhlBoolean),Oset(suppress_background),NhlTImmediate,
        _NhlUSET((NhlPointer)False),_NhlRES_NOSACCESS,NULL},
    {NhlNwkSuppressBBInfo,NhlCwkSuppressBBInfo,NhlTBoolean,
        sizeof(NhlBoolean),Oset(suppress_bbinfo),NhlTImmediate,
        _NhlUSET((NhlPointer)False),_NhlRES_NOSACCESS,NULL},

    /* these page size and margins are initialized as "-1" here, and are given appropriate
     * values when the workstation is opened, depending upon which resources are actually
     * available at that time.
     */
    {NhlNwkPaperSize,NhlCwkPaperSize,NhlTString,
        sizeof(NhlString),Oset(paper_size),NhlTImmediate,
        _NhlUSET(PAGEUTIL_DEFAULT_PAPERSIZE),_NhlRES_NOSACCESS,NULL},
    {NhlNwkPaperWidthF, NhlCwkPaperWidthF, NhlTFloat,
        sizeof(float), Oset(page_width), NhlTString,
        _NhlUSET("-1."), _NhlRES_DEFAULT, NULL},
    {NhlNwkPaperHeightF, NhlCwkPaperHeightF, NhlTFloat,
        sizeof(float), Oset(page_height), NhlTString,
        _NhlUSET("-1."), _NhlRES_DEFAULT, NULL},
    {NhlNwkDeviceLowerX,NhlCwkDeviceLowerX,NhlTInteger,
        sizeof(int),Oset(lower_x),NhlTImmediate,
        _NhlUSET((NhlPointer)-1),_NhlRES_DEFAULT,NULL},
    {NhlNwkDeviceLowerY,NhlCwkDeviceLowerY,NhlTInteger,
        sizeof(int),Oset(lower_y),NhlTImmediate,
        _NhlUSET((NhlPointer)-1),_NhlRES_DEFAULT,NULL},
    {NhlNwkDeviceUpperX,NhlCwkDeviceUpperX,NhlTInteger,
        sizeof(int),Oset(upper_x),NhlTImmediate,
        _NhlUSET((NhlPointer)-1),_NhlRES_DEFAULT,NULL},
    {NhlNwkDeviceUpperY,NhlCwkDeviceUpperY,NhlTInteger,
        sizeof(int),Oset(upper_y),NhlTImmediate,
        _NhlUSET((NhlPointer)-1),_NhlRES_DEFAULT,NULL},

/* End-documented-resources */
};

/*
 * PDFWorkstation base_class method declarations
 */

static NhlErrorTypes PDFWorkstationClassInitialize(
# if    NhlNeedProto
    void
# endif
);

static NhlErrorTypes PDFWorkstationInitialize(
# if    NhlNeedProto
        NhlClass,       /* class */
        NhlLayer,       /* req */
        NhlLayer,       /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
# endif
);

static NhlErrorTypes PDFWorkstationClassPartInitialize(
# if    NhlNeedProto
        NhlClass        /* lc */
# endif
);

static NhlErrorTypes PDFWorkstationDestroy(
# if    NhlNeedProto
        NhlLayer        /* inst */
# endif
);

static NhlErrorTypes PDFWorkstationSetValues(
# if    NhlNeedProto
        NhlLayer,       /* old */
        NhlLayer,       /* reference */
        NhlLayer,       /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
# endif
);

static NhlErrorTypes PDFWorkstationGetValues(
# if    NhlNeedProto
    NhlLayer,       /* l */
    _NhlArgList,    /* args */
    int             /* num_args */
# endif
);

/*
 * PDFWorkstation work_class method declarations
 */

static NhlErrorTypes PDFWorkstationOpen(
# if    NhlNeedProto
    NhlLayer    /* instance */
# endif
);

static NhlErrorTypes PDFWorkstationActivate(
# if    NhlNeedProto
    NhlLayer    l   /* instance */
# endif
);

NhlPDFWorkstationClassRec NhlpdfWorkstationClassRec = {
    {
        /* class_name           */  "pdfWorkstationClass",
        /* nrm_class            */  NrmNULLQUARK,
        /* layer_size           */  sizeof(NhlPDFWorkstationLayerRec),
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

        /* class_part_initialize */     PDFWorkstationClassPartInitialize,
        /* class_initialize      */     PDFWorkstationClassInitialize,
        /* layer_initialize      */     PDFWorkstationInitialize,
        /* layer_set_values      */     PDFWorkstationSetValues,
        /* layer_set_values_hook */     NULL,
        /* layer_get_values      */     PDFWorkstationGetValues,
        /* layer_reparent        */     NULL,
        /* layer_destroy         */     PDFWorkstationDestroy,

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
        /* open_work         */     PDFWorkstationOpen,
        /* close_work        */     NhlInheritClose,
        /* activate_work     */     PDFWorkstationActivate,
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

NhlClass NhlpdfWorkstationClass = (NhlClass) &NhlpdfWorkstationClassRec;

/*
 * Function:    nhlfpdfworkstationclass
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
_NHLCALLF(nhlfpdfworkstationclass, NHLFPDFWORKSTATIONCLASS)
# if    NhlNeedProto
(
    void
)
# else
()
# endif
{
    return NhlpdfWorkstationClass;
}

/*
 * Function:    PDFWorkstationClassPartInitialize
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
PDFWorkstationClassPartInitialize
# if    NhlNeedProto
(
    NhlClass    lc
)
# else
(lc)
    NhlClass    lc;
# endif
{

    return NhlNOERROR;
}

static NrmQuark fnameQ = NrmNULLQUARK;

/*
 * Function:    PDFWorkstationClassInitialize
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
PDFWorkstationClassInitialize
# if    NhlNeedProto
(
    void
)
# else
()
# endif
{
    _NhlEnumVals    visvals[] = {
        {NhlCOLOR,  "Color"},
        {NhlMONOCHROME, "Monochrome"}
    };

    _NhlEnumVals    fmtvals[] = {
        {NhlPDF,     "PDF"}
    };

    _NhlEnumVals    orientvals[] = {
        {NhlPORTRAIT,   "Portrait"},
        {NhlLANDSCAPE,  "Landscape"}
    };

    _NhlEnumVals    colmodelvals[] = {
        {NhlCMYK,   "CMYK"},
        {NhlRGB,    "RGB"}
    };


    (void) _NhlRegisterEnumType(NhlpdfWorkstationClass,NhlTVisualType,
        visvals,NhlNumber(visvals));
    (void) _NhlRegisterEnumType(NhlpdfWorkstationClass,NhlTPDFFormat,
        fmtvals,NhlNumber(fmtvals));
    (void) _NhlRegisterEnumType(NhlpdfWorkstationClass,NhlTWorkOrientation,
        orientvals,NhlNumber(orientvals));
    (void) _NhlRegisterEnumType(NhlpdfWorkstationClass,NhlTColorModel,
        colmodelvals,NhlNumber(colmodelvals));

    fnameQ = NrmStringToQuark(NhlNwkPDFFileName);

    return NhlNOERROR;
}

/*
 * Function:    PDFWorkstationInitialize
 *
 * Description: Set PDF Workstation type, filename, device coordinates
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
static NhlErrorTypes PDFWorkstationInitialize
# if    NhlNeedProto
    (NhlClass lclass, NhlLayer req, NhlLayer new, _NhlArgList args, int num_args)
# else
    (lclass,req,new,args,num_args)
        NhlClass lclass;
        NhlLayer req;
        NhlLayer new;
        _NhlArgList args;
        int num_args;
# endif
{
    char    func[]= "PDFWorkstationInitialize";
    NhlPDFWorkstationLayer  wnew = (NhlPDFWorkstationLayer) new;
    NhlPDFWorkstationLayerPart  *np = &wnew->pdf;
    char    *tfname = NULL;
    char    buff[_NhlMAXFNAMELEN];
    NhlErrorTypes   ret = NhlNOERROR;

    /*
     * Set gkswkstype
     * For PDF, only PORTRAIT and LANDSCAPE make sense
     */
    switch (np->orientation) {
        case NhlPORTRAIT:
            wnew->work.gkswkstype = PDFPORTRAIT;
            break;

        case NhlLANDSCAPE:
            wnew->work.gkswkstype = PDFLANDSCAPE;
            break;

        default:
            NhlPError(NhlWARNING, NhlEUNKNOWN,
                "%s: Invalid orientation \"%s\" defaulting to PORTRAIT",
                func, np->orientation);
            wnew->work.gkswkstype = PDFPORTRAIT;
            break;
    }

    wnew->work.gkswksconid = 0;

    if (np->filename) {
        tfname = (char*) _NGResolvePath(np->filename);
        if (!tfname) {
            NhlPError(NhlWARNING,NhlEUNKNOWN,
                "%s:Unable to resolve path name for \"%s\", defaulting %s",
                func, np->filename, NhlNwkPDFFileName);
            ret = NhlWARNING;
        }
    }

    if (!tfname) {
        strcpy(buff, new->base.name);
        strcat(buff, ".");
        switch(np->format){
            case NhlPDF:
                strcat(buff, "pdf");
                break;

            default:
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                    "%s:Unsupported PDF format %d?", func,
                    np->format);
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

    np->filename = NhlMalloc(strlen(tfname) + 1);
    if (!np->filename) {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }
    strcpy(np->filename,tfname);

    if (np->paper_size) {
        char* tmpStr = np->paper_size;
        np->paper_size = NhlMalloc(strlen(tmpStr) + 1);
        if (!np->paper_size) {
            NHLPERROR((NhlFATAL,ENOMEM,NULL));
            return NhlFATAL;
        }
        strcpy(np->paper_size, tmpStr);
    }

    if (np->lower_x > 0 && np->upper_x > 0 && np->lower_x >= np->upper_x ) {
        NhlPError(NhlWARNING,NhlEUNKNOWN,
            "%s:Device X Coordinates invalid, defaulting",func);
        ret = NhlWARNING;
        np->lower_x = 36;
        np->upper_x = 576;
    }

    if (np->lower_y > 0 && np->upper_y > 0 && np->lower_y >= np->upper_y) {
        NhlPError(NhlWARNING,NhlEUNKNOWN,
            "%s:Device Y Coordinates invalid, defaulting",func);
        ret = NhlWARNING;
        np->lower_y = 126;
        np->upper_y = 666;
    }
    np->dev_bounds_updated = False;

    return ret;
}

/*
 * Function:    PDFWorkstationSetValues
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
static NhlErrorTypes PDFWorkstationSetValues
# if    NhlNeedProto
(
    NhlLayer    old,
    NhlLayer    ref,
    NhlLayer    new,
    _NhlArgList args,
    int     nargs
)
# else
(old,ref,new,args,nargs)
    NhlLayer    old;
    NhlLayer    ref;
    NhlLayer    new;
    _NhlArgList args;
    int     nargs;
# endif
{
    char    func[]= "PDFWorkstationInitialize";
    NhlPDFWorkstationLayerPart *np = &((NhlPDFWorkstationLayer)new)->pdf;
    NhlPDFWorkstationLayerPart *op = &((NhlPDFWorkstationLayer)old)->pdf;
    NhlErrorTypes ret = NhlNOERROR;

    if (np->full_background != op->full_background) {
        c_ngseti("wo", _NhlWorkstationId(new));
        c_ngseti("fu", np->full_background);
    }

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
 * Function:    PDFWorkstationGetValues
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
PDFWorkstationGetValues
# if    NhlNeedProto
(
    NhlLayer    l,
    _NhlArgList args,
    int     nargs
)
# else
(l,args,nargs)
    NhlLayer    l;
    _NhlArgList args;
    int     nargs;
# endif
{
    char    func[]= "PDFWorkStationGetValues";
    register int    i;
    NhlPDFWorkstationLayerPart   *pp = &((NhlPDFWorkstationLayer)l)->pdf;
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
 * Function:    PDFWorkstationDestroy
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
PDFWorkstationDestroy
# if    NhlNeedProto
(
    NhlLayer    l
)
# else
(l)
    NhlLayer    l;
# endif
{
    NhlPDFWorkstationLayerPart   *pdfp = &((NhlPDFWorkstationLayer)l)->pdf;

    NhlFree(pdfp->filename);
    NhlFree(pdfp->paper_size);

    return NhlNOERROR;
}

/*
 * Function:    PDFWorkstationOpen
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
PDFWorkstationOpen
# if    NhlNeedProto
(
    NhlLayer    l
)
# else
(l)
    NhlLayer    l;
# endif
{
    NhlWorkstationLayer work = (NhlWorkstationLayer)l;
    NhlPDFWorkstationLayerPart  *pp = &((NhlPDFWorkstationLayer)l)->pdf;
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


    c_ngsetc("me", pp->filename);
    c_ngseti("pw", pageInfo.pageWidthPts);
    c_ngseti("ph", pageInfo.pageHeightPts);
    c_ngseti("co", (pp->resolution/72 + 1));
    c_ngseti("cm", pp->color_model);

    if (pp->suppress_background && pp->suppress_bbinfo)
        su = 1;
    else if (pp->suppress_background)
        su = 2;
    else if (pp->suppress_bbinfo)
        su = 3;

    c_ngseti("su", su);

    ret = (*NhlworkstationClassRec.work_class.open_work)(l);

    c_ngseti("wo", _NhlWorkstationId(l));
    c_ngseti("lx", pp->lower_x);
    c_ngseti("ux", pp->upper_x);
    c_ngseti("ly", pp->lower_y);
    c_ngseti("uy", pp->upper_y);
    c_ngseti("pl", pp->orientation);
    c_ngseti("fu", pp->full_background);

    w = pp->upper_x - pp->lower_x;
    h = pp->upper_y - pp->lower_y;
    d = MAX(w, h);
    work->work.vswidth_dev_units = d/72*pp->resolution;

    return ret;
}

/*
 * Function:    PDFWorkstationActivate
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
PDFWorkstationActivate
# if    NhlNeedProto
(
    NhlLayer    l
)
# else
(l)
    NhlLayer    l;
# endif
{
    NhlWorkstationClass lc = (NhlWorkstationClass) NhlworkstationClass;
    NhlWorkstationLayerPart *wp = &((NhlWorkstationLayer)l)->work;
    NhlPDFWorkstationLayerPart *pp = &((NhlPDFWorkstationLayer)l)->pdf;
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
    wp->vswidth_dev_units = d/72 * pp->resolution;

    return (*(lc->work_class.activate_work))(l);
}
