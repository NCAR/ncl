/*
 *      $Id: datavargrid.c,v 1.14 2000-03-10 01:12:53 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		datavargrid.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Jul 27 14:04:18 MDT 1999
 *
 *	Description:	
 */

#include <ncarg/ngo/datavargridP.h>
#include <ncarg/ngo/xutil.h>
#include <ncarg/ngo/stringutil.h>
#include <ncarg/ngo/shell.h>

#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/Text.h>
#include <Xm/PushBG.h>
#include <Xm/MessageB.h>
#include  <ncarg/ngo/Grid.h>
#include <float.h>

#define SYSTEM_ERROR "System error"
#define INVALID_INPUT "Invalid input"
#define INVALID_SHAPE "Dimension size or count error"
#define INSUFFICIENT_DIMS \
	"Data var has insufficient dimensionality: %d dims requred"
#define INSUFFICIENT_DIMS_AS_SHAPED \
"Data var has insufficient dimensionality as currently shaped: %d dims required"
#define NONCONFORMING_VAR \
	"Data var dimensions do not match those of currently selected vars"

static void PopupShaperAction
(
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
);

static NhlBoolean Colors_Set = False;
static Pixel Foreground,Background;
static char *Buffer;
static int  Max_Width;
static int  CWidth;

static XtActionsRec datavargridactions[] = {
	{ "PopupShaperAction", PopupShaperAction }
};


static void
ErrorMessage(
	NgDataVarGridRec *dvp,
	NhlString	    message
)
{
	XmLMessageBox(dvp->public.grid,message,True);

	return;
}
	    
static NclApiDataList *GetInfo
(
	NrmQuark	qfile,
	NrmQuark	qvar,
	NrmQuark	qcoord
	)
{
	NclApiDataList *dl = NULL;

	if (qfile && qvar && qcoord)
		dl = NclGetFileVarCoordInfo(qfile,qvar,qcoord);
	else if (qfile && qvar)
		dl = NclGetFileVarInfo(qfile,qvar);
	else if (qvar && qcoord)
		dl = NclGetVarCoordInfo(qvar,qcoord);
	else if (qvar)
		dl = NclGetVarInfo(qvar);

	return dl;
}

static int
EffectiveDims(
	int	ndims,
	long	*start,
	long	*finish,
	long	*stride
	)
{
	int i;
	int dim_count = 0;

	for (i = 0; i < ndims; i++) {
		int el_count = abs((finish[i] - start[i])/stride[i]);
		if (el_count > 0)
			dim_count++;
	}
	return dim_count;
}

/*
*************************************************************************
* shape tool functions
*/

static void AdjustShapeToolGeometry
(
	NhlPointer pdata
)
{
        NgDataVarGridRec	*dvp = (NgDataVarGridRec *)pdata;
	NgShaper		*si = dvp->shaper;
	Position		x;
	Dimension		w,twidth;

	if (! si)
		return;
	XtVaGetValues(si->shapeinfogrid->grid,
		      XmNx,&x,
		      XmNwidth,&w,
		      NULL);
        twidth = x + w;
	XtVaGetValues(si->reverse,
		      XmNx,&x,
		      XmNwidth,&w,
		      NULL);
	twidth = MAX(twidth,x+w);

	if (si->datagrid && XtIsManaged(si->datagrid->grid)) {
		XtVaGetValues(si->datagrid->grid,
			      XmNx,&x,
			      XmNwidth,&w,
			      NULL);
		twidth = MAX(x+w,twidth);
		XtVaSetValues(si->datagrid->grid,
                              XmNwidth,twidth -
                              si->datagrid->sub_width - si->sub_width,
                              NULL);
	}
}

/*
 * This should only be used when the NclApiVarInfoRec information is used
 * immediately. It should not be saved away, because it may get freed at
 * any time.
 */

NclApiVarInfoRec 
*GetDataVarInfo
(
	NgVarData	vdata
)
{
	NclApiVarInfoRec *vinfo;

	if (vdata->dl) {
		vinfo = vdata->dl->u.var;
	}
	else {
		if (vdata->qfile && vdata->qvar && vdata->qcoord)
			vdata->dl = NclGetFileVarCoordInfo
				(vdata->qfile,vdata->qvar,vdata->qcoord);
		else if (vdata->qfile && vdata->qvar)
			vdata->dl = NclGetFileVarInfo
				(vdata->qfile,vdata->qvar);
		else if (vdata->qvar && vdata->qcoord)
			vdata->dl = NclGetVarCoordInfo
				(vdata->qvar,vdata->qcoord);
		else if (vdata->qvar)
			vdata->dl = NclGetVarInfo(vdata->qvar);
		if (vdata->dl)
			vinfo = vdata->dl->u.var;
	}
	if (! vinfo) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NULL;
	}
	return vinfo;
}

static NhlBoolean
SetShape
(
	NclApiVarInfoRec *vinfo,
	NgVarData	model_vdata,
	long		*start_in,
	long		*finish_in,
	long		*stride_in,
	int		ndims,
	long		*start,
	long		*finish,
	long		*stride
)
{
	NclApiVarInfoRec *mvinfo;
	int i,j;

	mvinfo = GetDataVarInfo(model_vdata);
	if (! (vinfo && mvinfo))
		return False;

	for (i = 0; i < vinfo->n_dims; i++) {
		NhlBoolean matched = False;
		for (j = 0; j < mvinfo->n_dims; j++) {
			if (vinfo->dim_info[i].dim_quark == 
			    mvinfo->dim_info[j].dim_quark) {
				start[i] = model_vdata->start[j];
				finish[i] = model_vdata->finish[j];
				stride[i] = model_vdata->stride[j];
				matched = True;
				break;
			}
		}
		if (! matched) {
			start[i] = start_in ? start_in[i] : 0;
			finish[i] = finish_in ? finish_in[i] : 0;
			stride[i] = stride_in ? stride_in[i] : 1;
		}
	}
	return True;
}

static void UpdateShaper
(
        NgDataVarGridRec *dvp
)
{
	NgVarData vdata = dvp->public.plotdata[dvp->data_ix].vdata;

	if (! dvp->shaper) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Invalid shaper reference"));
		return;
	}
	if (! dvp->shaper->vinfo ||
	    dvp->shaper->vinfo->n_dims < vdata->ndims) {
		dvp->start = NhlRealloc
			(dvp->start,sizeof(long)* vdata->ndims);
		dvp->finish = NhlRealloc
			(dvp->finish,sizeof(long)* vdata->ndims);
		dvp->stride = NhlRealloc
			(dvp->stride,sizeof(long)* vdata->ndims);
		if (! (dvp->start &&dvp->finish && dvp->stride)) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return;
		}
	}

	if (! dvp->shaper->vinfo ||
	    dvp->shaper->vinfo->n_dims != vdata->ndims ||
	    memcmp(dvp->start,vdata->start,sizeof(long)* vdata->ndims) ||
	    memcmp(dvp->finish,vdata->finish,sizeof(long)* vdata->ndims) ||
	    memcmp(dvp->stride,vdata->stride,sizeof(long)* vdata->ndims)) {
		dvp->shaper->vinfo = NULL;
		dvp->shaper->start = NULL;
		dvp->shaper->finish = NULL;
		dvp->shaper->stride = NULL;
		memcpy(dvp->start,vdata->start,sizeof(long)* vdata->ndims);
		memcpy(dvp->finish,vdata->finish,sizeof(long)* vdata->ndims);
		memcpy(dvp->stride,vdata->stride,sizeof(long)* vdata->ndims);
	}
	NgUpdateShaper(dvp->shaper,vdata->qfile,vdata->qvar,
		       dvp->start,dvp->finish,dvp->stride);
	dvp->shaper->pdata = dvp;
	return;
}

static NhlBoolean CheckAndApplyChanges
(
	 NgDataVarGridRec *dvp
)
{
	NgPlotData pd = &dvp->public.plotdata[dvp->data_ix];
	NgVarData vdata = pd->vdata;

	if (pd->ndims > EffectiveDims(pd->vdata->ndims,
				      dvp->start,dvp->finish,dvp->stride)) {
		char buf[256];
		sprintf(buf,INSUFFICIENT_DIMS_AS_SHAPED,pd->ndims);
		ErrorMessage(dvp,buf);
		return False;
	}
	memcpy(vdata->start,dvp->start,sizeof(long)* vdata->ndims);
	memcpy(vdata->finish,dvp->finish,sizeof(long)* vdata->ndims);
	memcpy(vdata->stride,dvp->stride,sizeof(long)* vdata->ndims);
	return True;
}


static void UpdateDataVarShape
(
	NhlPointer pdata
)
{
        NgDataVarGridRec *dvp = (NgDataVarGridRec *)pdata;
	NgPlotData pd = &dvp->public.plotdata[dvp->data_ix];
	NgVarData vdata = pd->vdata;
	NclApiVarInfoRec *vinfo = NULL;
	int		page_id;
	long nstart[32], nfinish[32], nstride[32];


	vinfo = GetDataVarInfo(vdata);
	if (! vinfo)
		return;

	if (! CheckAndApplyChanges(dvp)) {
		NgUpdateShaperCoords(dvp->shaper);
		return;
	}

	vdata->set_state = _NgSHAPED_VAR;
	vdata->cflags = _NgSHAPE_CHANGE;

	if (pd->conform_group > -1) {
		int i;
		for (i = 0; i < dvp->public.plotdata_count; i++) {
			NclApiVarInfoRec *vinfo;
			NgPlotData opdata = &dvp->public.plotdata[i];
			NgVarData ovdata = opdata->vdata;
			if (opdata->conform_group != pd->conform_group)
				continue;
			if (! ovdata->qvar)
				continue;
			vinfo = GetDataVarInfo(ovdata);
			SetShape(vinfo,vdata,ovdata->start,ovdata->finish,
				 ovdata->stride,
				 vinfo->n_dims,nstart,nfinish,nstride);
			NgSetVarData(NULL,ovdata,
				     ovdata->qfile,ovdata->qvar,ovdata->qcoord,
				     vinfo->n_dims,nstart,nfinish,nstride,
				     _NgSHAPED_VAR);
		}
	}

	NgUpdateDataVarGrid((NgDataVarGrid *)dvp,dvp->qname,
			    dvp->public.plotdata_count,
			    dvp->public.plotdata);

	page_id = NgGetPageId
		(dvp->go->base.id,dvp->qname,NrmNULLQUARK);
	if (page_id == NgNoPage)
		return;

	NgPostPageMessage(dvp->go->base.id,page_id,_NgNOMESSAGE,
			  _brPLOTVAR,NrmNULLQUARK,dvp->qname,
			  _NgDATAVARUPDATE,(NhlPointer)True,True,NULL,True);

	NgUpdateShaperCoords(dvp->shaper);

	return;
}

static void ShapeToolOKCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
        NgDataVarGridRec *dvp = (NgDataVarGridRec *)udata;

	UpdateDataVarShape((NhlPointer)dvp);
	NgGOPopdown(dvp->shape_tool_id);

}

static void ShapeToolApplyCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
        NgDataVarGridRec *dvp = (NgDataVarGridRec *)udata;

	
	UpdateDataVarShape((NhlPointer)dvp);

}

static void CreateShapeTool
(
	NgGO		go,
	NhlPointer	data
)
{
        NgDataVarGridRec *dvp = (NgDataVarGridRec *)data;
	NgPlotData pd = &dvp->public.plotdata[dvp->data_ix];
	NgVarData vdata = pd->vdata;
	Widget apply;
	char 		buf[256];

#if DEBUG_DATA_VAR_GRID      
	fprintf(stderr,"in create shape tool\n");
#endif        
	
	dvp->shaper = NgCreateShaper(go,go->go.manager);
	dvp->shaper->shape_notify = UpdateDataVarShape;
	dvp->shaper->geo_notify = AdjustShapeToolGeometry;
	dvp->shaper->pdata = NULL;

	XtAddCallback(go->go.manager,
                      XmNokCallback,ShapeToolOKCB,dvp);

	apply = XtVaCreateManagedWidget
		("Apply",xmPushButtonGadgetClass,go->go.manager,NULL);
	XtAddCallback(apply,XmNactivateCallback,ShapeToolApplyCB,dvp);

	if (vdata->qfile)
		sprintf(buf,"%s->%s Shape",
			NrmQuarkToString(vdata->qfile),
			NrmQuarkToString(vdata->qvar));
	else
		sprintf(buf,"%s Shape",
			NrmQuarkToString(vdata->qvar));

	XtVaSetValues(go->go.shell,
		      XmNtitle,buf,
#if 0
		      XmNtransient,False,
#endif
		      NULL);

	UpdateShaper(dvp);

	_NgGOWidgetTranslations(go,dvp->shaper->frame);
	XmProcessTraversal(dvp->shaper->shapeinfogrid->grid,
			   XmTRAVERSE_CURRENT);
}
static void PopupShaperAction
(
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
)
{
        NgDataVarGridRec *dvp;
	XButtonEvent	*xb = (XButtonEvent *) xev;
	unsigned char	row_type,col_type;
	int		row,col;
	char 		buf[256];
	NgVarData	vdata;
	NgGO		shape_go;

#if DEBUG_DATA_VAR_GRID      
	fprintf(stderr,"in popup shaper action\n");
#endif
	XmLGridXYToRowColumn(w,xb->x,xb->y,&row_type,&row,&col_type,&col);

	if (row_type != XmCONTENT)
		return;


	XtVaGetValues(w,
                      XmNuserData,(void*)&dvp,
                      NULL);

	dvp->data_ix = row;
	vdata = dvp->public.plotdata[dvp->data_ix].vdata;
	if (! (vdata && vdata->qvar))
		return;

	if (vdata->qfile)
		sprintf(buf,"%s->%s Shape",
			NrmQuarkToString(vdata->qfile),
			NrmQuarkToString(vdata->qvar));
	else
		sprintf(buf,"%s Shape",
			NrmQuarkToString(vdata->qvar));

	if (dvp->shape_tool_id <= NhlNULLOBJID) {
		NhlVACreate(&dvp->shape_tool_id,"ShapeTool",
			    NgshellClass,dvp->go->base.id,
			    NgNshContentFunc,CreateShapeTool,
			    NgNshContentFuncData,dvp,
			    NULL);
	}
	else {
		shape_go = (NgGO)_NhlGetLayer(dvp->shape_tool_id);
		if (! shape_go) {
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				   "error creating shapetool dialog"));
			return;
		}
		XtVaSetValues(shape_go->go.shell,
			      XmNtitle,buf,
			      NULL);

		UpdateShaper(dvp);
	}
	NgGOPopup(dvp->shape_tool_id);
		
	return;
}
/*
*************************************************************************
* end of shape tool functions
*/

static char *
ColumnWidths
(
	NgDataVarGridRec *dvp
)
{
	int	i;
        char	sizestr[10];
        int	twidth = 0;
	Dimension	fwidth;
	Position	x;

	XtVaGetValues(XtParent(dvp->parent),
		      XmNwidth,&fwidth,
		      NULL);
	XtVaGetValues(dvp->public.grid,
		      XmNx,&x,
		      NULL);
        
        Buffer[0] = '\0';
	for (i=0; i < 2; i++) {
                int width = dvp->cwidths[i];
                if (width + twidth > Max_Width)
                        width = Max_Width - twidth;
		if (i == 1) 
			width = MAX(width,fwidth/CWidth - twidth - 3*CWidth);
                twidth += width;
                sprintf(sizestr,"%dc ",width);
		strcat(Buffer,sizestr);
	}
        Buffer[strlen(Buffer)-1] = '\0';
#if DEBUG_DATA_VAR_GRID      
        fprintf(stderr,"%s\n",Buffer);
#endif        
        return Buffer;
}

static char *
TitleText
(
	NgDataVarGridRec	*dvp
)
{
        int len;
        
        
        sprintf(Buffer,"%s|",NrmQuarkToString(dvp->qname));
        len = dvp->cwidths[0] = strlen(Buffer);
        
        sprintf(&Buffer[len],"%s","Data Variables");
        dvp->cwidths[1] = strlen(Buffer) - len;
        
#if DEBUG_DATA_VAR_GRID      
        fprintf(stderr,"%s\n",Buffer);
#endif        

        return Buffer;
}

static XmString
Column0String
(
	NgDataVarGridRec	*dvp,
        int			dataix
)
{
	XmString xmstring;
	NgPlotData pd = &dvp->public.plotdata[dataix];

	if (! (pd->description))
		sprintf(Buffer,"Data Variable %d",dataix+1);
	else {
		sprintf(Buffer,"%s",pd->description);
	}

	dvp->cwidths[0] = MAX(dvp->cwidths[0],strlen(Buffer)+2);

	xmstring = NgXAppCreateXmString(dvp->go->go.appmgr,Buffer);

	return xmstring;
}

static XmString
Column1String
(
	NgDataVarGridRec	*dvp,
        int			dataix
)
{
	NgVarData vdata;
	XmString xmstring;
	int i;
	NgPlotData pd = &dvp->public.plotdata[dataix];
	vdata = pd->vdata;

	if (! vdata) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
		   "Internal error: NgVarData missing in datavargrid"));
		return NULL;
	}
		
		
	if (! vdata->qvar) {
		if (pd->required)
			sprintf(Buffer,"<undefined var: required>");
		else 
			sprintf(Buffer,"<undefined var: optional>");
	}
	else {
		if (vdata->qfile && vdata->qvar && vdata->qcoord)
			sprintf(Buffer,"%s->%s&%s(",
				NrmQuarkToString(vdata->qfile),
				NrmQuarkToString(vdata->qvar),
				NrmQuarkToString(vdata->qcoord));
		else if (vdata->qfile && vdata->qvar)
			sprintf(Buffer,"%s->%s(",
				NrmQuarkToString(vdata->qfile),
				NrmQuarkToString(vdata->qvar));
		else if (vdata->qvar && vdata->qcoord)
			sprintf(Buffer,"%s&%s(",
				NrmQuarkToString(vdata->qvar),
				NrmQuarkToString(vdata->qcoord));
		else
			sprintf(Buffer,"%s(",
				NrmQuarkToString(vdata->qvar));

		for (i=0; i< vdata->ndims; i++) {
			if ((vdata->finish[i] - vdata->start[i])
			    /vdata->stride[i] == 0)
				sprintf(&Buffer[strlen(Buffer)],
					"%ld,",vdata->start[i]);
			else if (vdata->finish[i] == vdata->size[i] - 1 &&
				 vdata->start[i] == 0 &&
				 vdata->stride[i] == 1)
				sprintf(&Buffer[strlen(Buffer)],":,");
			else if (vdata->stride[i] == 1)
				sprintf(&Buffer[strlen(Buffer)],"%ld:%ld,",
					vdata->start[i],vdata->finish[i]);
			else
				sprintf(&Buffer[strlen(Buffer)],"%ld:%ld:%ld,",
					vdata->start[i],
					vdata->finish[i],vdata->stride[i]);
		}
		/* back up 1 to remove final comma */
		Buffer[strlen(Buffer)-1] = ')';
	}
	dvp->cwidths[1] = MAX(dvp->cwidths[1],strlen(Buffer)+2);

	xmstring = NgXAppCreateXmString(dvp->go->go.appmgr,Buffer);

	return xmstring;
}

/*
 * Conformance requires that dimensions with more than one element required
 * by the plot data have the same name and the same number of elements. The
 * coordinate values (if used) should also match.
 */

static NhlBoolean ConformingVar
(
	NgDataVarGridRec	*dvp,
	NgPlotData		pdata,
	NclApiVarInfoRec	*vinfo	
)
{
	NclApiVarInfoRec *tvinfo;
	NgPlotData opdata;
	NgVarData vdata = NULL;
	int i;

	if (pdata->conform_group < 0)
		return True;

	for (i = 0; i < dvp->public.plotdata_count; i++) {
		opdata = &dvp->public.plotdata[i];
		if (opdata->conform_group != pdata->conform_group)
			continue;
		if (! (opdata->vdata && opdata->vdata->qvar))
			continue;
		vdata = opdata->vdata;
		break;
	}
	if (! vdata)
		return True;

	tvinfo = GetDataVarInfo(vdata);
	if (! tvinfo)
		return False;

	if (vinfo->n_dims < vdata->rank) {
		return False;
	}
	for (i = 0; i < vdata->ndims; i++) {
		if ((vdata->finish[i]-vdata->start[i]) / vdata->stride[i] >0) {
			int j;
			NhlBoolean matched = False;
			for (j = 0; j <vinfo->n_dims; j++) {
				if (vinfo->dim_info[j].dim_quark == 
				    tvinfo->dim_info[i].dim_quark) {
					if (vinfo->dim_info[j].dim_size == 
						tvinfo->dim_info[i].dim_size) {
						matched = True;
						break;
					}
				}
			}
			if (! matched)
				return False;
		}
	}
	return True;
}


/*
 * The explicit parameter distinguishes between an entry of all white space
 * and an entry of the string "null". All white space restores default
 * assignment of the resource value, whereas "null" means that the user intends
 * that the resource value be explicitly "null".
 */
static NhlBoolean 
EmptySymbol
(
	char            *var_string,
	NhlBoolean	*explicit
)
{
	char *sp;
	
	*explicit = False;
	sp = var_string;
	
	while (*sp != '\0' && isspace(*sp))
		sp++;
	if (*sp == '\0')
		return True;
	else if (! strncmp(sp,"null",4)) {
		sp += 4;
		while (*sp != '\0' && isspace(*sp))
			sp++;
		if (*sp == '\0') {
			*explicit = True;
			return True;
		}
	}

	return False;
}

#define EATWHITESPACE(cp) while (isspace(*cp)) cp++

static NhlBoolean
PossibleNclSymbol
(
	char *symtext
)
{
	char *cp = symtext;

	if (! (isalpha(*cp) || *cp == '_'))
		return False;

	while (*cp) {
		if (! (isalnum(*cp) || *cp == '_'))
			return False;
		cp++;
	}
#if 0
	if ! (NclSymbolDefined(symtext))
		return False;
#endif
	return True;
}

static char *
GetShape(char *shape,
	 NclApiDataList *dl,
	 int *ndims,
	 long **start,
	 long **finish,
	 long **stride)
{
	char *cp = shape;
	char *tcp,*end;
	int commas = 0;
	int i;
/*
 * doesn't handle dim reordering syntax yet 
 */
	*ndims = 0;
	*start = *finish = *stride = NULL;
	if (*cp != '(') {
		return cp;
	}
	if (! (end = strchr(cp,')'))) /* invalid syntax */
		return NULL;

	tcp = cp;
	while (tcp = strchr(tcp + 1,','))
	       commas++;
	*ndims = commas + 1;
	if (*ndims != dl->u.var->n_dims)
		return NULL;
	
	*start = NhlMalloc(*ndims * sizeof(int));
	*finish = NhlMalloc(*ndims * sizeof(int));
	*stride = NhlMalloc(*ndims * sizeof(int));
	    
	if (! (*start && *finish && *stride)) {
		return NULL;
	}

	cp++;
	for (i = 0; i < *ndims; i++) {
		int j;
		(*start)[i] = 0;
		(*finish)[i] = dl->u.var->dim_info[i].dim_size - 1;
		(*stride)[i] = 1;

		for (j = 0; j < 3; j++) {
			long tmp = strtol(cp,&tcp,10);
			if (tcp != cp) {
				switch (j) {
				case 0:
					(*start)[i] = tmp;
					break;
				case 1:
					(*finish)[i] = tmp;
					break;
				case 2:
					(*stride)[i] = tmp;
					break;
				}
				cp = tcp;
			}
			EATWHITESPACE(cp);
			switch (*cp) {
			case ':':
				cp++;
				continue;
			case ',':
			case ')':
				if (j == 0) {
					(*finish)[i] = (*start)[i];
				}
				j = 2;
				cp++;
				break;
			default:
				/* invalid syntax */
				goto err_ret;
			}
		}
	}
	return end + 1;
		
 err_ret:
	NhlFree(*start);
	NhlFree(*finish);
	NhlFree(*stride);
	*start = *finish = *stride = NULL;
	return NULL;
}

static NrmQuark
GetCoordSymbol
(
	NrmQuark	qfile,
	NrmQuark	qvar,
	char            *start,
	char		**cp
)
{
	NclApiDataList *dl;
	char buf[256];
	char *end,*tail;
	NrmQuark qsym;
	int i;

	*cp = NULL;

	if (*start != '&') {
		*cp = start;
		return NrmNULLQUARK;
	}
	if ((end = strchr(start,'('))) {	
		if (end == start) {
			return NrmNULLQUARK;
		}
	}
	else {
		end = start + strlen(start);
	}
	tail = end;

	end--;
	while (isspace(*end))
		end--;

	if (end < start) {
		return NrmNULLQUARK;
	}
	strncpy(buf,start,1+end-start);
	buf[1+end-start] = '\0';

	if (! PossibleNclSymbol(buf)) {
		return NrmNULLQUARK;
	}
	qsym = NrmStringToQuark(buf);

	if (qfile && qvar)
		dl = NclGetFileVarInfo(qfile,qvar);
	else 
		dl = NclGetVarInfo(qvar);

	if (! dl) {
		return NrmNULLQUARK;
	}
	for (i = 0; i < dl->u.var->n_dims; i++) {
		if (qsym == dl->u.var->coordnames[i])
			break;
	}
	if (i == dl->u.var->n_dims) {
		qsym = NrmNULLQUARK;
	}
	NclFreeDataList(dl);

	if (qsym > NrmNULLQUARK)
		*cp = tail;
	return qsym;
}

static NrmQuark
GetVarSymbol
(
	NrmQuark	qfile,
	char            *start,
	char		**cp
)
{
	NclApiDataList *dl,*vinfo;
	char buf[256];
	char *end,*tail;
	NrmQuark qsym;

	*cp = NULL;
	if (((end = strchr(start,'&')) || (end = strchr(start,'(')))) {	
		if (end == start) {
			return NrmNULLQUARK;
		}
	}
	else {
		end = start + strlen(start);
	}
	tail = end;

	end--;
	while (isspace(*end))
		end--;

	if (end < start) {
		return NrmNULLQUARK;
	}
	strncpy(buf,start,1+end-start);
	buf[1+end-start] = '\0';

	if (! PossibleNclSymbol(buf)) {
		return NrmNULLQUARK;
	}
	qsym = NrmStringToQuark(buf);

	if (qfile) {
		int i;
		dl = NclGetFileInfo(qfile);
		if (! dl) {
			return NrmNULLQUARK;
		}
		for (i = 0; i < dl->u.file->n_vars; i++) {
			if (qsym == dl->u.file->var_names[i])
				break;
		}
		if (i == dl->u.file->n_vars) {
			qsym = NrmNULLQUARK;
		}
	}
	else {
		dl = NclGetVarList();
		if (! dl) {
			return NrmNULLQUARK;
		}
		for (vinfo = dl; vinfo; vinfo = vinfo->next) {
			if (qsym == vinfo->u.var->name)
				break;
		}
		if (! vinfo) {
			qsym = NrmNULLQUARK;
		}
	}

	NclFreeDataList(dl);

	if (qsym > NrmNULLQUARK)
		*cp = tail;
	return qsym;
}	

static NrmQuark
GetFileSymbol
(
	char            *start,
	char		**cp
)
{
	NclApiDataList *dl,*finfo;
	char buf[256];
	char *end,*tail;
	NrmQuark qsym;


	/* 
	 * if there's a file symbol, cp will point to the character
	 * after the symbol, otherwise it will be unchanged
	 */

	*cp = NULL;
	if (!(end = strstr(start,"->"))) {
		*cp = start;
		return NrmNULLQUARK;
	}

	if (end == start) {
		return NrmNULLQUARK;
	}
	tail = end + 2;

	end--;
	while (isspace(*end))
		end--;

	if (end < start) {
		return NrmNULLQUARK;
	}

	strncpy(buf,start,1+end-start);
	buf[1+end-start] = '\0';

	if (! PossibleNclSymbol(buf))
		return NrmNULLQUARK;

	qsym = NrmStringToQuark(buf);
	dl = NclGetFileList();
	if (! dl) {
		return NrmNULLQUARK;
	}

	for (finfo = dl; finfo; finfo = finfo->next) {
		if (qsym == finfo->u.file->name)
			break;
	}

	if (! finfo)
		qsym = NrmNULLQUARK;

	NclFreeDataList(dl);

	if (qsym) {
		*cp = tail;
	}
	return qsym;
}

static NhlBoolean 
GetVar
(
	char            *var_string,
	NrmQuark	*qfile,
	NrmQuark	*qvar,
	NrmQuark	*qcoord,
	NclApiDataList	**dl,	
	int		*ndims,
	long		**start,
	long		**finish,
	long		**stride
	)
{
	char *cp = var_string;

	*ndims = 0;
	*start = *finish = *stride = NULL;
	*dl = NULL;
	*qvar = *qfile = *qcoord = NrmNULLQUARK;

	EATWHITESPACE(cp);

	if (*cp)
		*qfile = GetFileSymbol(cp,&cp);

	if (cp && *cp) {
		*qvar = GetVarSymbol(*qfile,cp,&cp);
		if (*qvar <= NrmNULLQUARK)
			return False;
	}
	if (cp && *cp)
		*qcoord = GetCoordSymbol(*qfile,*qvar,cp,&cp);

	*dl = GetInfo(*qfile,*qvar,*qcoord);

	if (cp && *cp)
		cp = GetShape(cp,*dl,ndims,start,finish,stride);
	
	if (cp)
		EATWHITESPACE(cp);

	/*
	 * if cp has been set to NULL it indicates an error.
	 * if it is not NULL, but not == '\0', then there was something
	 * besides space after the last valid token, and that would
	 * be an error also. Otherwise we're okay unless there's not
	 * a variable. (But that should have been caught earlier.)
	 */
	if (! cp || *cp || *qvar <= NrmNULLQUARK)
		return False;

	return True;
}

static void NonConformalOKCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	int *status = (int *)udata;
	*status = 1;
	return;
}

static void NonConformalCancelCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	int *status = (int *)udata;
	*status = -1;
	return;
}

static NhlBoolean
AcceptNonConformalData(
	NgDataVarGridRec	*dvp,
	NrmQuark		qfile,
	NrmQuark		qvar,
	NrmQuark		qcoord
)
{
	char    buf[512] = "";
	Arg	args[50];
	int	nargs;
        XmString xmname,xmtext;
        Widget  ok,help,cancel;
	int status = 0;
	XtAppContext context;
	Widget non_conform,shell;
	Atom WM_DELETE_WINDOW;

	if (qfile && qvar && qcoord)
		sprintf(buf,"%s->%s&%s is Non-Conforming",
			NrmQuarkToString(qfile),
			NrmQuarkToString(qvar),
			NrmQuarkToString(qcoord));
	else if (qfile && qvar)
		sprintf(buf,"%s->%s is Non-Conforming",
			NrmQuarkToString(qfile),
			NrmQuarkToString(qvar));
	else 
		sprintf(buf,"%s is Non-Conforming",
			NrmQuarkToString(qvar));

        xmname = NgXAppCreateXmString(dvp->go->go.appmgr,buf);
	xmtext = NgXAppCreateXmString
		(dvp->go->go.appmgr,
		 "Data does not conform in all required dimensions.\nRemaining data in the conformance group will be invalidated if you continue.");
	shell = XmLShellOfWidget(dvp->parent);

        nargs = 0;
	XtSetArg(args[nargs],XmNdialogTitle,xmname);nargs++;
	XtSetArg(args[nargs],XmNuserData,dvp);nargs++;
	XtSetArg(args[nargs],XmNmessageString,xmtext);nargs++;
	XtSetArg(args[nargs],
		 XmNdialogStyle,XmDIALOG_APPLICATION_MODAL);nargs++;

	non_conform = XmCreateMessageDialog
		(XtParent(dvp->parent),"NonConform",args,nargs);

	WM_DELETE_WINDOW = XmInternAtom(XtDisplay(dvp->parent),
					"WM_DELETE_WINDOW",False);
	XmAddWMProtocolCallback(shell, WM_DELETE_WINDOW, NonConformalCancelCB,
		(caddr_t)&status);

	NgXAppFreeXmString(dvp->go->go.appmgr,xmname);
	NgXAppFreeXmString(dvp->go->go.appmgr,xmtext);

	help = XmMessageBoxGetChild(non_conform,XmDIALOG_HELP_BUTTON);
	XtUnmanageChild(help);

	ok = XmMessageBoxGetChild(non_conform,XmDIALOG_OK_BUTTON);
	sprintf(buf,"Continue");
        xmname = NgXAppCreateXmString(dvp->go->go.appmgr,buf);
	XtVaSetValues(ok,
		      XmNlabelString,xmname,
		      NULL);
	NgXAppFreeXmString(dvp->go->go.appmgr,xmname);
		    

	XtAddCallback(non_conform,
		      XmNokCallback,NonConformalOKCB,&status);
	XtAddCallback(non_conform,
		      XmNcancelCallback,NonConformalCancelCB,&status);

        XtManageChild(non_conform);
        
	context = XtWidgetToApplicationContext(dvp->parent);
	while (! status || XtAppPending(context))
		XtAppProcessEvent(context, XtIMAll);
	XtDestroyWidget(non_conform);

	if (status > 0)
		return True;

	return False;
}

static NhlBoolean 
QualifyAndInsertVariable
(
	NgDataVarGridRec *dvp,
	int              index,
	char             *var_string
)
{
	NgPlotData pdata =  &dvp->public.plotdata[index];
	NgPlotData opdata;
	NrmQuark qfile = NrmNULLQUARK,
		qvar = NrmNULLQUARK,qcoord = NrmNULLQUARK;
	int ndims;
	long *start, *finish, *stride;
	long nstart[32], nfinish[32], nstride[32];
	NclApiDataList *dl = NULL;
	NgVarData vdata;
	NhlBoolean explicit;
	NgVarData last_vdata = NgNewVarData();
	NhlString message = SYSTEM_ERROR;
	NgVarDataSetState var_state = _NgVAR_UNSET;
	char buf[256];
	int i;

        if (! last_vdata)
                goto error_ret;

	vdata = pdata->vdata;
	NgCopyVarData(last_vdata,vdata);

	if (EmptySymbol(var_string,&explicit)) {
		if (! explicit) {
			if (! NgSetVarData
			    (NULL,vdata,NrmNULLQUARK,NrmNULLQUARK,NrmNULLQUARK,
			     0,NULL,NULL,NULL,_NgVAR_UNSET)) {
				goto error_ret;
			}
		}
		else { 
			if (! NgSetVarData
			    (NULL,vdata,NrmNULLQUARK,NrmNULLQUARK,NrmNULLQUARK,
			     0,NULL,NULL,NULL,_NgSHAPED_VAR)) {
				goto error_ret;
			}
		}
		NgFreeVarData(last_vdata);
		return True;

		/* Done with empty symbol processing */

	}

	if (! GetVar(var_string,&qfile,&qvar,&qcoord,
		     &dl,&ndims,&start,&finish,&stride)) {
		message = INVALID_INPUT;
		goto error_ret;
	}

	if (! dl) {
		goto error_ret;
	}
	var_state = (start || finish || stride) ? 
		_NgSHAPED_VAR : _NgDEFAULT_SHAPE;

	if (! ConformingVar(dvp,pdata,dl->u.var)) {
		if (! AcceptNonConformalData(dvp,qfile,qvar,qcoord))
			goto cancel_ret;
		else {
			for (i = 0; i < dvp->public.plotdata_count; i++) {
				opdata = &dvp->public.plotdata[i];
				if (opdata->conform_group != 
				    pdata->conform_group)
					continue;
				if (! opdata->vdata->qvar)
					continue;
				NgSetVarData(NULL,opdata->vdata,NrmNULLQUARK,
					     NrmNULLQUARK,NrmNULLQUARK,
					     0,NULL,NULL,NULL,_NgVAR_UNSET);
			}
		}
	}
	else if (pdata->ndims > dl->u.var->n_dims) {
		sprintf(buf,INSUFFICIENT_DIMS,pdata->ndims);
		message = buf;
		goto error_ret;
	} 

	switch (var_state) {
	case _NgSHAPED_VAR:
		/* 
		 * In this case, the shape of the var propagates to other
		 * vars in the conformance group. (at least if it's a valid
		 * shape)
		 */
		if (pdata->ndims > EffectiveDims(ndims,start,finish,stride)) {
			sprintf(buf,INSUFFICIENT_DIMS_AS_SHAPED,pdata->ndims);
			message = buf;
			goto error_ret;
		}
		ndims = dl->u.var->n_dims;
		if (! NgSetVarData(dl,vdata,qfile,qvar,qcoord,ndims,
				   start,finish,stride,var_state)) {
			goto error_ret;
		}
		if (pdata->conform_group > -1) {
			for (i = 0; i < dvp->public.plotdata_count; i++) {
				NclApiVarInfoRec *vinfo;
				opdata = &dvp->public.plotdata[i];
				if (opdata->conform_group != 
				    pdata->conform_group)
					continue;

				if (! opdata->vdata->qvar)
					continue;
				vinfo = GetDataVarInfo(opdata->vdata);
				SetShape(vinfo,vdata,start,finish,stride,
					 vinfo->n_dims,nstart,nfinish,nstride);
				NgSetVarData(NULL,opdata->vdata,
					     opdata->vdata->qfile,
					     opdata->vdata->qvar,
					     opdata->vdata->qcoord,
					     vinfo->n_dims,
					     nstart,nfinish,nstride,
					     var_state);
			}
		}
		break;
	case _NgDEFAULT_SHAPE:
		opdata = NULL;
		if (pdata->conform_group > -1) {
			for (i = 0; i < dvp->public.plotdata_count; i++) {
				opdata = &dvp->public.plotdata[i];
				if (opdata->conform_group != 
				    pdata->conform_group)
					continue;
				if (! opdata->vdata->qvar)
					continue;
				break;
			}
			if (i == dvp->public.plotdata_count)
				opdata = NULL;
		}
		if (! opdata) {
			ndims = pdata->ndims;
			if (! NgSetVarData(dl,vdata,qfile,qvar,qcoord,ndims,
					   NULL,NULL,NULL,var_state)) {
				goto error_ret;
			}
			break;
		}
		ndims = dl->u.var->n_dims;
		if (! SetShape(dl->u.var,opdata->vdata,
			       NULL,NULL,NULL,ndims,nstart,nfinish,nstride))
			goto error_ret;
		if (! NgSetVarData(dl,vdata,qfile,qvar,qcoord,ndims,
				   nstart,nfinish,nstride,var_state)) {
			goto error_ret;
		}
		break;
	}

	if (start)
		NhlFree(start);
	if (finish)
		NhlFree(finish);
	if (stride)
		NhlFree(stride);
	NgFreeVarData(last_vdata);
	return True;

 error_ret:
	ErrorMessage(dvp,message);

 cancel_ret:
        if (dl)
                NclFreeDataList(dl);
	if (start)
		NhlFree(start);
	if (finish)
		NhlFree(finish);
	if (stride)
		NhlFree(stride);
        if (last_vdata)
                NgFreeVarData(last_vdata);
	return False;
}

static void
EditCB
(
	Widget		w,
	XtPointer	data,
	XtPointer	cb_data
)
{
        NgDataVarGridRec *dvp = (NgDataVarGridRec *)data;
	NgDataVarGrid *pub = &dvp->public;
        XmLGridCallbackStruct *cb = (XmLGridCallbackStruct *) cb_data;
        XmLGridColumn colptr;
        XmLGridRow rowptr;
	char *new_string,*save_text = NULL;
	int data_ix;

#if DEBUG_DATA_VAR_GRID
	printf("entered DataVarGrid EditCB\n");
#endif

	colptr = XmLGridGetColumn(pub->grid,XmCONTENT,cb->column);
	rowptr = XmLGridGetRow(pub->grid,XmCONTENT,cb->row);

        switch (cb->reason) {
            case XmCR_EDIT_INSERT:
#if DEBUG_DATA_VAR_GRID      
                    fprintf(stderr,"edit insert\n");
#endif
                    XtVaSetValues(dvp->text,
                                  XmNcursorPosition,0,
                                  XmNborderWidth,2,
                                  XmNcursorPositionVisible,True,
				  XmNbackground,dvp->go->go.select_pixel,
                                  NULL);
		    if (! dvp->text_dropped) {
			    XmTextSetInsertionPosition(dvp->text,0);
			    if (dvp->edit_save_string)
				    XmStringFree(dvp->edit_save_string);
			    XtVaGetValues
				    (pub->grid,
				     XmNcolumnPtr,colptr,
				     XmNrowPtr,rowptr,
				     XmNcellString,&dvp->edit_save_string,
				     NULL);
		    }
		    else {
			    char *cur_string;
			    dvp->text_dropped = False;
			    XmStringGetLtoR(dvp->edit_save_string,
					    XmFONTLIST_DEFAULT_TAG,
					    &cur_string);
			    XmTextInsert(dvp->text,0,cur_string);
			    XmTextSetInsertionPosition(dvp->text,
						       strlen(cur_string));
			    XtFree(cur_string);
		    }
		    dvp->in_edit = True;
                    return;
            case XmCR_EDIT_BEGIN:
#if DEBUG_DATA_VAR_GRID
                    fprintf(stderr,"edit begin\n");
#endif

		    if (dvp->edit_save_string)
			    XmStringFree(dvp->edit_save_string);
                    XtVaGetValues
                            (pub->grid,
                             XmNcolumnPtr,colptr,
                             XmNrowPtr,rowptr,
                             XmNcellString,&dvp->edit_save_string,
                             NULL);
        
                    XtVaSetValues(dvp->text,
				  XmNbackground,dvp->go->go.select_pixel,
                                  NULL);
		    dvp->in_edit = True;
                    return;
            case XmCR_EDIT_CANCEL:
#if DEBUG_DATA_VAR_GRID      
                    fprintf(stderr,"edit cancel\n");
#endif
                    XtVaSetValues(dvp->text,
				  XmNbackground,dvp->go->go.edit_field_pixel,
                                  NULL);
		    dvp->in_edit = False;
                    return;
            case XmCR_EDIT_COMPLETE:
#if DEBUG_DATA_VAR_GRID      
                    fprintf(stderr,"edit complete\n");
#endif
		    if (! dvp->in_edit) 
			    return;

                    XtVaSetValues(dvp->text,
				  XmNbackground,dvp->go->go.edit_field_pixel,
                                  NULL);

		    dvp->in_edit = False;
                    break;
        }
/*
 * Only get here on edit complete
 */

	new_string = XmTextGetString(dvp->text);

	data_ix = cb->row;
	if (dvp->edit_save_string) {
		XmStringGetLtoR(dvp->edit_save_string,
				XmFONTLIST_DEFAULT_TAG,&save_text);
	}
	if (! new_string ||
	    (save_text && ! strcmp(new_string,save_text)) ||
	    ! QualifyAndInsertVariable(dvp,data_ix,new_string)) {
		if (dvp->edit_save_string)
			XtVaSetValues(pub->grid,
				      XmNcolumn,1,
				      XmNrow,cb->row,
				      XmNcellString,dvp->edit_save_string,
				      NULL);
	}
	else {
		XmString xmstr;
		int page_id;
		int conform_group = pub->plotdata[data_ix].conform_group;
		int i;

		for (i = 0; i <  pub->plotdata_count; i++) {
			if (pub->plotdata[i].conform_group != conform_group)
				continue;

			xmstr = Column1String(dvp,i);

			XtVaSetValues(pub->grid,
				      XmNrow,i,
				      XmNcolumn,1,
				      XmNcellString,xmstr,
				      NULL);
			NgXAppFreeXmString(dvp->go->go.appmgr,xmstr);
		}
		if (dvp->shaper && dvp->data_ix > -1) {
			UpdateShaper(dvp);
		}
	}

	if (save_text)
		XtFree(save_text);
	if (new_string)
		XtFree(new_string);

	return;
}

static void
SelectCB
(
	Widget		w,
	XtPointer	data,
	XtPointer	cb_data
)
{
        NgDataVarGridRec *dvp = (NgDataVarGridRec *)data;
	NgDataVarGrid *pub = &dvp->public;
        XmLGridCallbackStruct *cb = (XmLGridCallbackStruct *) cb_data;
        Boolean	editable;
	XmLGridColumn colptr;
	XmLGridRow rowptr;

#if DEBUG_DATA_VAR_GRID      
	fprintf(stderr,"entered DataVarGrid SelectCB\n");
#endif

        if (! Colors_Set) {

                Colors_Set = True;
        
                colptr = XmLGridGetColumn(pub->grid,XmCONTENT,0);
                rowptr = XmLGridGetRow(pub->grid,XmHEADING,0);
                XtVaGetValues(pub->grid,
                              XmNcolumnPtr,colptr,
                              XmNrowPtr,rowptr,
                              XmNcellForeground,&Foreground,
                              XmNcellBackground,&Background,
                              NULL);
        }


	if (dvp->selected_row > -1) {

                    /* restore last selected */
		XtVaSetValues(pub->grid,
			      XmNcolumn,0,
			      XmNrow,dvp->selected_row,
			      XmNcellForeground,Foreground,
			      XmNcellBackground,Background,
			      NULL);
		if (dvp->in_edit) {
			XmLGridEditComplete(pub->grid);
		}
	}
	colptr = XmLGridGetColumn(pub->grid,XmCONTENT,1);
	rowptr = XmLGridGetRow(pub->grid,XmCONTENT,cb->row);

	XtVaGetValues(pub->grid,
		      XmNcolumnPtr,colptr,
		      XmNrowPtr,rowptr,
		      XmNcellEditable,&editable,
		      NULL);
        XtVaSetValues(pub->grid,
                      XmNcolumn,0,
                      XmNrow,cb->row,
                      XmNcellForeground,Background,
                      XmNcellBackground,Foreground,
                      NULL);
	if (editable) {

		if (! dvp->text_dropped) {
			if (dvp->edit_save_string)
				XmStringFree(dvp->edit_save_string);
			XtVaGetValues
				(pub->grid,
				 XmNcolumnPtr,colptr,
				 XmNrowPtr,rowptr,
				 XmNcellString,&dvp->edit_save_string,
				 NULL);
		}
		XmLGridEditBegin(pub->grid,True,cb->row,True);
	}

	dvp->selected_row = cb->row;

	return;
}
static NhlErrorTypes
CopyPlotData(
	NgPlotData *to_pdata,
	int	   *to_pdata_count,
	NgPlotData from_pdata,
	int	   from_pdata_count
)
{
	int i;

	/*
	 * if the plotdata is actually the same don't do anything
	 */

	if (*to_pdata == from_pdata)
		return NhlNOERROR;

	if (*to_pdata_count) {
		for (i = 0; i < *to_pdata_count; i++) {
			NgFreeVarData((*to_pdata)[i].vdata);
		}
	}
	NhlFree(*to_pdata);
	*to_pdata_count = 0;
	*to_pdata = NULL;
	
	if (from_pdata_count) {
		*to_pdata = NhlMalloc
			(from_pdata_count * sizeof(NgPlotDataRec));
		if (! *to_pdata) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return NhlFATAL;
		}
		memcpy((*to_pdata),from_pdata,
		       from_pdata_count * sizeof(NgPlotDataRec));
		
		for (i = 0; i < from_pdata_count; i++) {
			(*to_pdata)[i].vdata = NgNewVarData();
			if (! (*to_pdata)[i].vdata) {
				NHLPERROR((NhlFATAL,ENOMEM,NULL));
				return NhlFATAL;
			}
			NgCopyVarData((*to_pdata)[i].vdata,
				      from_pdata[i].vdata);
		}
		*to_pdata_count = from_pdata_count;
	}
	return NhlNOERROR;
}

NhlErrorTypes NgUpdateDataVarGrid
(
        NgDataVarGrid		*data_var_grid,
        NrmQuark		qname,
	int			count,
        NgPlotData		plotdata
       )
{
        NgDataVarGridRec *dvp;
        int	i;
        static NhlBoolean first = True;
	int	row;
        
        dvp = (NgDataVarGridRec *) data_var_grid;
        if (!dvp) return NhlFATAL;
        if (first) {
                int		root_w;
                short		cw,ch;
                XmFontList      fontlist;
                XtVaGetValues(data_var_grid->grid,
                              XmNfontList,&fontlist,
                              NULL);
                XmLFontListGetDimensions(fontlist,&cw,&ch,True);
                root_w = WidthOfScreen(XtScreen(data_var_grid->grid));
                Max_Width = root_w / cw - cw;
		CWidth = cw;
                first = False;
        }
	CopyPlotData(&data_var_grid->plotdata,&data_var_grid->plotdata_count,
		     plotdata,count);
        dvp->qname = qname;
        XtVaSetValues(data_var_grid->grid,
		      XmNselectionPolicy,XmSELECT_NONE,
                      XmNrows,data_var_grid->plotdata_count,
                      NULL);

        for (i = 0; i < 2; i++)
                dvp->cwidths[i] = 0;
        
        XmLGridSetStringsPos(data_var_grid->grid,XmHEADING,0,XmCONTENT,0,
                             TitleText(dvp));
	XtVaSetValues(data_var_grid->grid,
		      XmNrowType,XmHEADING,
		      XmNrow,0,
		      XmNcolumn,0,
		      XmNcellAlignment, XmALIGNMENT_RIGHT,
		      XmNcellMarginRight,CWidth,
		      NULL);
	XtVaSetValues(data_var_grid->grid,
		      XmNrowType,XmHEADING,
		      XmNrow,0,
		      XmNcolumn,1,
		      XmNcellAlignment, XmALIGNMENT_LEFT,
		      XmNcellMarginLeft,CWidth,
		      NULL);
	
	row = 0;
        for (i = 0; i < data_var_grid->plotdata_count; i++) {
		XmString xmstr;

		xmstr = Column0String(dvp,i);

		XtVaSetValues(data_var_grid->grid,
			      XmNrow,row,
			      XmNcolumn,0,
			      XmNcellString,xmstr,
			      XmNcellAlignment, XmALIGNMENT_RIGHT,
			      XmNcellMarginRight,CWidth,
			      NULL);
		NgXAppFreeXmString(dvp->go->go.appmgr,xmstr);

		xmstr = Column1String(dvp,i);

		XtVaSetValues(data_var_grid->grid,
			      XmNrow,row,
			      XmNcolumn,1,
			      XmNcellMarginLeft,CWidth,
			      XmNcellString,xmstr,
			      XmNcellAlignment, XmALIGNMENT_LEFT,
			      XmNcellEditable,True,
			      XmNcellBackground,dvp->go->go.edit_field_pixel,
			      NULL);
		NgXAppFreeXmString(dvp->go->go.appmgr,xmstr);
		row++;
        }
        XtVaSetValues(data_var_grid->grid,
                      XmNsimpleWidths,ColumnWidths(dvp),
                      NULL);

	if (! dvp->created) {
		dvp->created = True;
		XtMapWidget(data_var_grid->grid);
		XtVaSetValues(data_var_grid->grid,
			      XmNimmediateDraw,False,
			      NULL);
	} 

        return NhlNOERROR;
}
static void
FocusEH
(
	Widget		w,
	XtPointer	udata,
	XEvent		*event,
	Boolean		*cont
)
{

	switch (event->type) {
	case FocusOut:
#if DEBUG_DATA_VAR_GRID      
                    fprintf(stderr,"focus out\n");
#endif
#if 0
		if (dvp->in_edit) {
			XmLGridEditComplete(dvp->public.grid);
		}
#endif
		return;
	case FocusIn:
#if DEBUG_DATA_VAR_GRID      
                    fprintf(stderr,"focus in\n");
#endif
		break;
	}
        
	return;
}

static void StartCellDropCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
        NgDataVarGridRec *dvp = (NgDataVarGridRec *)udata;
	NgDataVarGrid *pub = &dvp->public;
        XmLGridCallbackStruct *cb = (XmLGridCallbackStruct *)cb_data;
        XmLGridRow	rowptr;
        XmLGridColumn	colptr;

#if DEBUG_DATA_VAR_GRID      
	fprintf(stderr,"in datavargrid start cell drop cb\n");
#endif        
	rowptr = XmLGridGetRow(pub->grid,XmCONTENT,cb->row);
        colptr = XmLGridGetColumn(pub->grid,XmCONTENT,cb->column);

	if (cb->column != 1)
		return;

	if (dvp->edit_save_string)
		XmStringFree(dvp->edit_save_string);
		
	XtVaGetValues
		(pub->grid,
		 XmNcolumnPtr,colptr,
		 XmNrowPtr,rowptr,
		 XmNcellString,&dvp->edit_save_string,
		 NULL);
	return;
}

static void CellDropCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
        NgDataVarGridRec *dvp = (NgDataVarGridRec *)udata;
	NgDataVarGrid *pub = &dvp->public;
        XmLGridCallbackStruct *cb = (XmLGridCallbackStruct *)cb_data;
        Boolean	editable;
	XmLGridColumn colptr;
	XmLGridRow rowptr;

#if DEBUG_DATA_VAR_GRID      
	fprintf(stderr,"in datavargrid cell drop cb\n");
#endif        

	if (cb->column != 1)
		return;

        if (! Colors_Set) {

                Colors_Set = True;
        
                colptr = XmLGridGetColumn(pub->grid,XmCONTENT,0);
                rowptr = XmLGridGetRow(pub->grid,XmHEADING,0);
                XtVaGetValues(pub->grid,
                              XmNcolumnPtr,colptr,
                              XmNrowPtr,rowptr,
                              XmNcellForeground,&Foreground,
                              XmNcellBackground,&Background,
                              NULL);
        }

	if (dvp->selected_row > -1) {

                    /* restore last selected */
		XtVaSetValues(pub->grid,
			      XmNcolumn,0,
			      XmNrow,dvp->selected_row,
			      XmNcellForeground,Foreground,
			      XmNcellBackground,Background,
			      NULL);
		if (dvp->in_edit) {
			XmLGridEditComplete(pub->grid);
		}
	}

	colptr = XmLGridGetColumn(pub->grid,XmCONTENT,1);
	rowptr = XmLGridGetRow(pub->grid,XmCONTENT,cb->row);

	XtVaGetValues(pub->grid,
		      XmNcolumnPtr,colptr,
		      XmNrowPtr,rowptr,
		      XmNcellEditable,&editable,
		      NULL);
        XtVaSetValues(pub->grid,
                      XmNcolumn,0,
                      XmNrow,cb->row,
                      XmNcellForeground,Background,
                      XmNcellBackground,Foreground,
                      NULL);
	if (editable) {
		dvp->text_dropped = True;
		XmLGridEditBegin(pub->grid,True,cb->row,True);
	}

	dvp->selected_row = cb->row;
	
	return;
}

NgDataVarGrid *NgCreateDataVarGrid
(
	NgGO			go,
        Widget			parent,
        NrmQuark		qname,
	int			count,
        NgPlotData		plotdata
        )
{
        NgDataVarGridRec *dvp;
        NgDataVarGrid *data_var_grid;
        static NhlBoolean first = True;

        if (first) {
                Buffer = NhlMalloc(BUFINC);
                first = False;
        }
	XtAppAddActions(go->go.x->app,
                        datavargridactions,NhlNumber(datavargridactions));
        
        dvp = NhlMalloc(sizeof(NgDataVarGridRec));
        if (!dvp) return NULL;
        data_var_grid = &dvp->public;
	data_var_grid->plotdata = NULL;
	data_var_grid->plotdata_count = 0;
	CopyPlotData(&data_var_grid->plotdata,&data_var_grid->plotdata_count,
		     plotdata,count);
	dvp->go = go;
        dvp->qname = qname;
	dvp->created = False;
	dvp->selected_row = -1;
	dvp->parent = parent;
	dvp->in_edit = False;
	dvp->edit_save_string = NULL;
	dvp->text_dropped = False;
	dvp->shape_tool_id  = NhlNULLOBJID;
	dvp->shaper = NULL;
	dvp->data_ix = -1;
	dvp->start = NULL;
	dvp->finish = NULL;
	dvp->stride = NULL;
      
        data_var_grid->grid = XtVaCreateManagedWidget
                ("DataVarGrid",
                 xmlGridWidgetClass,parent,
                 XmNverticalSizePolicy,XmVARIABLE,
                 XmNhorizontalSizePolicy,XmVARIABLE,
                 XmNselectionPolicy,XmSELECT_NONE,
		 XmNautoSelect,False,
                 XmNcolumns,2,
                 XmNrows,0,
		 XmNimmediateDraw,True,
		 XmNmappedWhenManaged,False,
                 NULL);
        XmLGridAddRows(data_var_grid->grid,XmHEADING,0,1);
	XtVaSetValues(data_var_grid->grid,
		      XmNuserData,data_var_grid,
		      NULL);

        XtAddCallback
		(data_var_grid->grid,XmNeditCallback,EditCB,dvp);
        XtAddCallback
		(data_var_grid->grid,XmNselectCallback,SelectCB,dvp);
        XtAddCallback(data_var_grid->grid,
		      XmNcellDropCallback,CellDropCB,dvp);
        XtAddCallback(data_var_grid->grid,
		      XmNcellStartDropCallback,StartCellDropCB,dvp);
	XtVaGetValues(data_var_grid->grid,
		      XmNtextWidget,&dvp->text,
		      NULL);
        XtAddEventHandler(dvp->text,FocusChangeMask,
                          False,FocusEH,dvp);
        return data_var_grid;
}

void NgDeactivateDataVarGrid
(
        NgDataVarGrid		*data_var_grid
        )
{
	NgDataVarGrid	*pub = data_var_grid;
        NgDataVarGridRec *dvp;
        Boolean	editable;
	XmLGridColumn colptr;
	XmLGridRow rowptr;
        
        dvp = (NgDataVarGridRec *) pub;

	if (dvp->shape_tool_id > NhlNULLOBJID) {
		NgGOPopdown(dvp->shape_tool_id);
		/* 
		 * to ensure an update when the shaper is popped up again --
		 * and also to make sure the shaper doesn't try to reference
		 * freed memory (start finish and stride are just refs)
		 */
		dvp->shaper->vinfo = NULL;
		dvp->shaper->start = NULL;
		dvp->shaper->finish = NULL;
		dvp->shaper->stride = NULL;
	}
	if (dvp->start) 
		NhlFree(dvp->start);
	if (dvp->finish) 
		NhlFree(dvp->finish);
	if (dvp->stride) 
		NhlFree(dvp->stride);
	dvp->start = dvp->finish = dvp->stride;

	if (dvp->selected_row <= -1) 
		return;

	colptr = XmLGridGetColumn(pub->grid,XmCONTENT,1);
	rowptr = XmLGridGetRow(pub->grid,XmCONTENT,dvp->selected_row);

	XtVaGetValues(pub->grid,
		      XmNcolumnPtr,colptr,
		      XmNrowPtr,rowptr,
		      XmNcellEditable,&editable,
		      NULL);

	XtVaSetValues(pub->grid,
		      XmNcolumn,0,
		      XmNrow,dvp->selected_row,
		      XmNcellForeground,Foreground,
		      XmNcellBackground,Background,
		      NULL);
	if (editable) {
		XtVaSetValues(pub->grid,
			      XmNcolumn,1,
			      XmNrow,dvp->selected_row,
			      XmNcellBackground,dvp->go->go.edit_field_pixel,
			      NULL);
		XmLGridEditCancel(pub->grid);
	}
	dvp->in_edit = False;
	XmLGridDeselectAllRows(pub->grid,False);

	dvp->selected_row = -1;

	return;

}
		
        
void NgDestroyDataVarGrid
(
        NgDataVarGrid		*data_var_grid
        )
{
        NgDataVarGridRec *dvp;
        
        dvp = (NgDataVarGridRec *) data_var_grid;
        if (!dvp) return;

	if (dvp->edit_save_string)
		XmStringFree(dvp->edit_save_string);
	CopyPlotData(&data_var_grid->plotdata,&data_var_grid->plotdata_count,
		     NULL,0);
        NhlFree(dvp);
        
        return;
}

        
