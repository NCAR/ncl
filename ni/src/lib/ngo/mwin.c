/*
 *      $Id: mwin.c,v 1.23 1999-05-22 00:36:20 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		mwin.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Dec 10 10:39:54 MST 1996
 *
 *	Description:	
 */
#include <ncarg/ngo/mwinP.h>

#include <ncarg/ngo/nclstate.h>

#include <ncarg/hlu/Workstation.h>
#include <ncarg/hlu/ViewI.h>
#include <ncarg/hlu/TransformI.h>

#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <ncarg/ngo/CascadeBG.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/PanedW.h>
#include <Xm/Form.h>

#include <Xm/MenuShell.h>
#include <Xcb/xcbShells.h>

#include <ncarg/ngo/Tree.h>
#include <ncarg/ngo/xinteract.h>

#include "xterm.xpm"

#define	Oset(field)	NhlOffset(NgMWinRec,mwin.field)
static NhlResource resources[] = {
	{"no.res","no.res",NhlTInteger,sizeof(int),
		Oset(foo),NhlTImmediate,_NhlUSET(0),
		_NhlRES_NOACCESS,NULL},
};
#undef	Oset

static NhlErrorTypes MWInitialize(
	NhlClass	lc,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		nargs
);

static NhlErrorTypes MWDestroy(
	NhlLayer	l
);

static NhlBoolean MWCreateWin(
	NgGO	go
);

static NhlBoolean MWCreateWinHook(
	NgGO	go
);

static void RowMoveAction(
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
);

static void RowSetPosAction(
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
);

static XtActionsRec mwinactions[] = {
	{ "RowMoveAction", RowMoveAction },
	{ "RowSetPosAction", RowSetPosAction },
	
};

NgMWinClassRec NgmWinClassRec = {
	{
/* class_name		*/	"mWinClass",
/* nrm_class		*/	NrmNULLQUARK,
/* layer_size		*/	sizeof(NgMWinRec),
/* class_inited		*/	False,
/* superclass		*/	(NhlClass)&NggOClassRec,
/* cvt_table		*/	NULL,

/* layer_resources	*/	resources,
/* num_resources	*/	NhlNumber(resources),
/* all_resources	*/	NULL,
/* callbacks		*/	NULL,
/* num_callbacks	*/	0,
/* class_callbacks	*/	NULL,
/* num_class_callbacks	*/	0,

/* class_part_initialize*/	NULL,
/* class_initialize	*/	NULL,
/* layer_initialize	*/	MWInitialize,
/* layer_set_values	*/	NULL,
/* layer_set_values_hook*/	NULL,
/* layer_get_values	*/	NULL,
/* layer_reparent	*/	NULL,
/* layer_destroy	*/	MWDestroy,

	},
	{
/* dialog		*/	NULL,
/* toplevel		*/	NULL,
/* manager		*/	NULL,

/* top_win_chain	*/	False,
/* create_win		*/	MWCreateWin,
/* create_win_hook	*/	MWCreateWinHook,
/* close		*/	_NgGOInheritClose,
	},
	{
/* foo			*/	0,
	},
};

NhlClass NgmWinClass = (NhlClass)&NgmWinClassRec;

static NhlErrorTypes
MWInitialize
(
	NhlClass	lc,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		nargs
)
{
  char			func[] = "MWInitialize";
	NgMWin			mwin = (NgMWin)new;
	NgMWinPart		*np = &((NgMWin)new)->mwin;
	NgMWinPart		*rp = &((NgMWin)req)->mwin;

	fprintf(stderr,"NCARG Version: %s\n",GetNCARGVersion());
	return NhlNOERROR;
}

static NhlErrorTypes
MWDestroy
(
	NhlLayer	l
)
{
	NgMWin	mwin = (NgMWin)l;
	NgMWinPart	*mp = &((NgMWin)l)->mwin;

	return NhlNOERROR;
}

static int *Top_Level_Views = NULL;
static int Top_Level_View_Alloc_Count = 0;
static int Do_Draw = True;


typedef char NodeType;

#define _ngGenericNode  0
#define _ngWksNode	1
#define _ngViewNode	2

typedef struct 	_NgObjTreeNodeRec NgObjTreeNodeRec, *NgObjTreeNode;
typedef struct 	_NgObjTreeRec NgObjTreeRec, *NgObjTree;

typedef void *ObjNodeData;

struct _NgObjTreeNodeRec {
	NodeType		type;
	NgObjTree		tree;   /* root information */      
	NgObjTreeNode		pnode;	/* parent node */
	NgObjTreeNode		next;	/* next node at the same level */
	int			pos;	/* row number */
	int			level;	/* level in the tree */
	XmString		xmname; /* displayed name of node */
	NgObjTreeNode		cnodes; /* child nodes - one level down */
	int			n_cnodes;	/* number of child nodes */
	int			id;	/* node identifier */
	ObjNodeData		ndata;	/* node type specific information */
};

typedef struct 	_NgMissingBaseNodeRec {
	int	base_id;
	int	member_id;
	NgViewStatus member_type; /* overlay or annotation */
	struct _NgMissingBaseNodeRec *next;
} NgMissingBaseNodeRec, *NgMissingBaseNode;

struct _NgObjTreeRec{
	int			nsid;
	int			appmgr;
	NgGO			go;

	Widget			wtree;

	NgObjTreeNode		wklist;
	NgMissingBaseNode	missing_bn_list;

	_NhlCB			create;
	_NhlCB			destroy;
	_NhlCB			dncl;
};

static NgViewStatus GetViewStatus
(
	int pid
)
{
	if (! NhlIsTransform(pid)) {
		if (! _NhlIsAnnotation(pid))
			return _ngBASIC_VIEW;
		return _ngANNOTATION;
	}
	if (_NhlIsSimpleTransform(pid)) {
		if (_NhlIsOverlay(pid))
			return _ngOVERLAY;
		if (_NhlIsAnnotation(pid))
			return _ngANNOTATION;
		return _ngSIMPLE_TRANSFORM;
	}
	if (_NhlIsOverlay(pid))
		return _ngOVERLAY;
	if (_NhlIsAnnotation(pid))
		return _ngBASE_PLOT_ANNOTATION;
	return _ngBASE_PLOT;
}
		
			
static
NgObjTreeNode
AllocNode
(
	NgObjTree	tree,
	NgObjTreeNode	pnode,
	NodeType	type
)
{
	char		func[]="AllocNode";
	NgObjTreeNode	new = NhlMalloc(sizeof(NgObjTreeNodeRec));

	if(!new){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NULL;
	}
	new->type = type;
	new->tree = tree;
	new->pnode = pnode;
	new->next = NULL;
	new->pos = 0;
	new->level = 0;
	new->xmname = NULL;
	new->cnodes = NULL;
	new->n_cnodes = 0;
	new->ndata = NULL;
	new->id = NhlNULLOBJID;

	return new;
}

static
void
FreeNode
(
	NgObjTreeNode	node
)
{
	if (node->xmname)
		NgXAppFreeXmString(node->tree->appmgr,node->xmname);
	if (node->ndata)
		NhlFree(node->ndata);
	NhlFree(node);
}


static void
FreeNodeList
(
	NgObjTree	otree,
	NgObjTreeNode	node
)
{
	if(!node)
		return;

	FreeNodeList(otree,node->next);
	FreeNodeList(otree,node->cnodes);
	FreeNode(node);

	return;
}

static NgObjTreeNode
FindNode
(
	NgObjTreeNode	node,
	int		id
)
{
	NgObjTreeNode fnode;

	if (! node)
		return NULL;
	if (node->id == id)
		return node;

	fnode = FindNode(node->cnodes,id);
	if (fnode)
		return fnode;

	return FindNode(node->next,id);
}


static NgObjTreeNode
FindRowNode
(
	NgObjTreeNode	node,
	int		row
)
{
	NgObjTreeNode fnode;

	if (! node)
		return NULL;
	if (node->pos == row)
		return node;

	fnode = FindRowNode(node->cnodes,row);
	if (fnode)
		return fnode;

	return FindRowNode(node->next,row);
}

static void
GetWkObjs
(
	NgNclObj	obj,
	NhlPointer	udata
)
{
	char		func[]="GetWkObjs";
	NgNclHluObj	hlu = (NgNclHluObj)obj;
	NgObjTree	otree = (NgObjTree)udata;
	NgObjTreeNode	new;
	NgWksObj 	wko = NULL;
	NhlLayer	l;
	NgHluData 	hdata;

	if(!NhlClassIsSubclass(hlu->class_ptr,NhlworkstationClass))
		return;
	l = _NhlGetLayer(hlu->id);
	if (! l) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:Invalid obj id %d",
			   func,hlu->id));
		return;
	}	
	new = AllocNode(otree,NULL,_ngWksNode);
	if(!new){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return;
	}

	if (l->base.gui_data2) {
		hdata = (NgHluData) l->base.gui_data2;

		/* the NgWksObj data has been allocated already (by xwk) */
		wko = hdata ? (NgWksObj) hdata->gdata : NULL;
	}
	else {
		hdata = NgGetHluData();
		if (! hdata)
			return;
		l->base.gui_data2 = (NhlPointer) hdata;
	}
	if (! wko) {
		wko = NhlMalloc(sizeof(NgWksObjRec));
		if(!wko){
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			FreeNode(new);
			return;
		}
		hdata->gdata = wko;
		wko->wks_wrap_id = NhlNULLOBJID;
		wko->cccb = NULL;
		wko->auto_refresh = False;
	}

	wko->name = hlu->name;
	wko->parent_id = hlu->parent_id;

	new->id = hlu->id;
	new->ndata = (ObjNodeData) wko;
	new->next = otree->wklist;

	otree->wklist = new;

	return;
}

static void
GetVwObjs
(
	NgNclObj	obj,
	NhlPointer	udata
)
{
	char		func[] = "GetVwObjs";
	NgNclHluObj	hlu = (NgNclHluObj)obj;
	NgObjTree	otree = (NgObjTree)udata;
	NgObjTreeNode	work,new,*tmp;
	int		i;
	NhlLayer	l,wl;
	NgViewObj	vwo = NULL;
	NgHluData	hdata;

	/*
	 * Only interested in view class objects.
	 */
	if(NhlClassIsSubclass(hlu->class_ptr,NhlviewClass))
		return;
	if(!(l = _NhlGetLayer(hlu->id))){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:Invalid obj id %d",
			   func,hlu->id));
		return;
	}	
	if (l->base.gui_data2) {
		hdata = (NgHluData) l->base.gui_data2;
		vwo = hdata ? (NgViewObj) hdata->gdata : NULL;
	}
	else {
		hdata = NgGetHluData();
		if (! hdata)
			return;
		l->base.gui_data2 = (NhlPointer) hdata;
	}
	if (! vwo) {
		vwo = NhlMalloc(sizeof(NgViewObjRec));
		if(!vwo){
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return;
		}
		hdata->gdata = (NhlPointer) vwo;
		vwo->ovcb = NULL;
		vwo->ancb = NULL;
		vwo->svcb = NULL;
		vwo->vstatus = _ngBASIC_VIEW;
		vwo->xvp.p0.x = vwo->xvp.p0.y = 
			vwo->xvp.p1.x = vwo->xvp.p1.y = 15000;
		vwo->xbbox = vwo->xvp;
		vwo->visible = False;
	}
	vwo->name = hlu->name;
	vwo->parent_id = hlu->parent_id;

	/*
	 * In fact, only interested in view class objects that are
	 * immediate children of a workstation class object.
	 */
	wl = _NhlGetLayer(hlu->parent_id);
	if(!wl || !_NhlIsClass(wl,NhlworkstationClass))
		return;

	work = otree->wklist;
	while(work && work->id != hlu->parent_id){
		work = work->next;
	}
	if(!work){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			   "%s:View child can't find mommy",func));
		return;
	}

	new = AllocNode(otree,work,_ngViewNode);
	if(!new){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return;
	}
	new->id = hlu->id;
	new->ndata = (ObjNodeData) vwo;

	work->n_cnodes++;

	/*
	 * TODO - define the order of the children.
	 */
	tmp = &work->cnodes;
	i=1;
	while(*tmp){
		i++;
		tmp = &((*tmp)->next);
	}

	new->pos = i;

	return;
}

static void
IncrementLevels
(
	NgObjTreeNode		node,
	int			level_inc
)
{
	NgObjTreeNode np;
	if(!node)
		return;

	for (np = node; np; np = np->next) {
		np->level += level_inc;
		IncrementLevels(np->cnodes,level_inc);
	}
	return;
}

static void
SetLevels
(
	Widget			tree,
	NgObjTreeNode		node
)
{
	NgObjTreeNode np;
	if(!node)
		return;

	for (np = node; np; np = np->next) {
		XtVaSetValues(tree,
			      XmNrow,np->pos,
			      XmNrowLevel,np->level,
			      NULL);
		SetLevels(tree,np->cnodes);
	}
	return;
}

static int
NumberNodes
(
	NgObjTreeNode		node,
	int			pos
)
{
	NgObjTreeNode np;
	if(!node)
		return pos;

	for (np = node; np; np = np->next) {
		np->pos = ++pos;
		pos = NumberNodes(np->cnodes,pos);
	}
	return pos;
}

static int
LastNodePos
(
	NgObjTreeNode		node
)
{
	NgObjTreeNode lp, np;

	if (! node->cnodes)
		return node->pos;

	for (np = node->cnodes; np; np = np->next)
		lp = np;

	return LastNodePos(lp);
}

static void
AddVwNode
(
	NgObjTree	otree,
	Widget		tree,
	NgObjTreeNode	vwnode
)
{
	char *name;
	if(!vwnode)
		return;

	name = NgNclGetHLURef(otree->nsid,vwnode->id);
	vwnode->xmname = NgXAppCreateXmString(otree->appmgr,name);
	XmLTreeAddRow(tree,2,False,False,-1,
		XmUNSPECIFIED_PIXMAP,XmUNSPECIFIED_PIXMAP,vwnode->xmname);

	AddVwNode(otree,tree,vwnode->next);

	return;
}

static NhlBoolean
CopyCC
(
	NhlArgVal	cbdata,
	NhlArgVal	udata,
	NhlArgVal	*ret
)
{
	_NhlobjChangeChild	cc = (_NhlobjChangeChild)cbdata.ptrval;
	_NhlobjChangeChild	new;

	if(cc->reason != _NhlobjCCMove)
		return False;

	if (_NhlIsPlotMember(cc->child))
		return False;

	new = NhlMalloc(sizeof(_NhlobjChangeChildRec));
	if(!new){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return False;
	}

	*new = *cc;
	ret->ptrval = new;

	return True;
}

static void
FreeCC
(
	NhlArgVal	cbdata
)
{
	NhlFree(cbdata.ptrval);

	return;
}

static void
WkChildChangeCB
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	char			func[]="WkChildChangeCB";
	_NhlobjChangeChild	cc = (_NhlobjChangeChild)cbdata.ptrval;
	NgObjTreeNode		wknode = (NgObjTreeNode)udata.ptrval;
	NgObjTreeNode		*tmp,work,nwork,child;
	int			or,nr;
	int			nrows;
	NgWksObj		wkobj;

	/*
	 * Reason was checked in the copy func, so I know this is a "move".
	 *
	 * Only deal with the move in the "old" workstation.
	 */
	if(wknode->id != cc->old)
		return;

	/*
	 * Since this is called from a work proc, it is possible
	 * the plot was reparented again - need to check.
	 * (In fact, the child may not even exist anymore.... uggghhhh!
	 */
	cc->new = NhlGetParentWorkstation(cc->child);
	/*
	 * If the new is the same as the old, don't do anything, child
	 * was reparented back to old.  If new < 0, then child was destroyed,
	 * and we need to do is erase the plot on the old workstation (if X).
	 */
	if(cc->new == cc->old)
		return;

	if (_NhlIsXWorkstation(_NhlGetLayer(cc->old))) {
		wkobj = (NgWksObj) wknode->ndata;
		if (wkobj && wkobj->auto_refresh)
			NgClearXwkView(wkobj->wks_wrap_id,cc->child);
	}	
	if (cc->new <= 0)
		return;

	/*
	 * find node for new workstation
	 */
	work = wknode->tree->wklist;
	while(work && work->id != cc->new)
		work = work->next;
	if(!work){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:Unable to find new parent",
			   func));
		return;
	}

	/*
	 * Find child
	 */
	tmp = &wknode->cnodes;
	while(*tmp && (*tmp)->id != cc->child){
		or++;
		tmp = &(*tmp)->next;
	}

	if(!*tmp){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			   "%s:Unable to find child node",func));
		return;
	}
	child = *tmp;
	or=child->pos;

	/*
	 * Remove the child from the old work
	 */
	*tmp = child->next;
	child->next = NULL;
	nrows = NumberNodes(child,0);
	wknode->n_cnodes--;

	/*
	 * Add child to new work
	 * TODO: Add child to correct position - right now it is just to the
	 *	end.
	 */

	child->pnode = work;
	nr = LastNodePos(work)+1;

	tmp = &work->cnodes;
	while(*tmp)
		tmp = &(*tmp)->next;
	*tmp = child;
	work->n_cnodes++;

	/*
	 * Move the row in the widget.
	 * (if nr > or, then the old row will be removed before the new
	 * row inserted, so the index should be one less.
	 */
	if(nr>or)
		nr -= nrows;
	XmLGridMoveRows(work->tree->wtree,nr,or,nrows);

	/*
	 * Renumber the nodes starting from which-ever workstation is
	 * earlier in the tree.
	 */
	nwork = work;
	work = (work->pos < wknode->pos)? work: wknode;
	NumberNodes(work,work->pos - 1);

	if (_NhlIsXWorkstation(_NhlGetLayer(nwork->id))) {
		wkobj = (NgWksObj) nwork->ndata;
		if (wkobj && wkobj->auto_refresh)
			NgDrawXwkView(wkobj->wks_wrap_id,cc->child,False);
	}	
	
	return;
}

static void
AddWkNode
(
	NgObjTree	otree,
	Widget		tree,
	NgObjTreeNode	wknode
)
{
	char		func[]="AddWkNode";
	NhlArgVal	sel,udata;
	NgWksObj	wks;
	char		*name;

	if(!wknode)
		return;
	wks = (NgWksObj)wknode->ndata;
	NhlINITVAR(sel);
	NhlINITVAR(udata);
	sel.lngval = 0;	/* not used */
	udata.ptrval = wknode;
	wks->cccb = NgCBWPAdd
		(otree->appmgr,CopyCC,FreeCC,_NhlGetLayer(wknode->id),
		 _NhlCBobjChildChange,sel,WkChildChangeCB,udata);
	if(!wks->cccb){
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,
			   "%s:Unable to track ChangeWorkstation!",func));
	}

	name = NgNclGetHLURef(otree->nsid,wknode->id);
	wknode->xmname = NgXAppCreateXmString(otree->appmgr,name);

	XmLTreeAddRow(tree,1,True,False,-1,
		XmUNSPECIFIED_PIXMAP,XmUNSPECIFIED_PIXMAP,wknode->xmname);

	AddVwNode(otree,tree,wknode->cnodes);
	AddWkNode(otree,tree,wknode->next);

	return;
}

static void
InitWTree
(
	NgObjTree	otree,
	Widget		tree,
	NgObjTreeNode	wklist
)
{
	AddWkNode(otree,tree,wklist);

	return;
}

static NhlBoolean
AddWorkNode
(
	NgObjTree	otree,
	NgObjTreeNode	wknode,
	NgNclHluObj	hlu
)
{
	char		func[]="AddWorkNode";
	NhlArgVal	sel,udata;
	NgObjTreeNode	tmp,last;
	NgWksObj 	wko = NULL;
	NhlLayer	l;
	char		*name;
	NgHluData 	hdata;

	l = _NhlGetLayer(hlu->id);
	if (l->base.gui_data2) {
		hdata = (NgHluData) l->base.gui_data2;

		/* the NgWksObj data has been allocated already (by xwk) */
		wko = hdata ? (NgWksObj) hdata->gdata : NULL;
	}
	else {
		hdata = NgGetHluData();
		if (! hdata)
			return False;
		l->base.gui_data2 = (NhlPointer) hdata;
	}

	if(!wko){
		wko = NhlMalloc(sizeof(NgWksObjRec));
		if (! wko) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return False;
		}
		hdata->gdata = (NhlPointer) wko;
		wko->wks_wrap_id = NhlNULLOBJID;
		wko->cccb = NULL;
		wko->auto_refresh = False;
	}	
	
	wknode->id = hlu->id;

	wko->name = hlu->name;
	wko->parent_id = hlu->parent_id;

	wknode->ndata = (ObjNodeData) wko;

	if(! otree->wklist) {
		otree->wklist = wknode;
	}
	else {
		for (last=NULL,tmp=otree->wklist;tmp;tmp=tmp->next) {
			if(tmp->id == wknode->id)
				return False;
			last = tmp;
		}
		if (!last)
			return False;
		last->next = wknode;
		NumberNodes(wknode,LastNodePos(last));
	}

	NhlINITVAR(sel);
	NhlINITVAR(udata);
	sel.lngval = 0;	/* not used */
	udata.ptrval = wknode;
	wko->cccb = NgCBWPAdd(otree->appmgr,CopyCC,FreeCC,
			      _NhlGetLayer(wknode->id),
			      _NhlCBobjChildChange,sel,WkChildChangeCB,udata);
	if (!wko->cccb)	{
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,
			   "%s:Unable to track ChangeWorkstation!",func));
	}
	
	wknode->tree = otree;
	name = NgNclGetHLURef(otree->nsid,wknode->id);
	wknode->xmname = NgXAppCreateXmString(otree->appmgr,name);
	wknode->level = 1;
	XmLTreeAddRow
		(otree->wtree,wknode->level,True,False,-1,
		 XmUNSPECIFIED_PIXMAP,XmUNSPECIFIED_PIXMAP,wknode->xmname);

	return True;
}

static NgObjTreeNode
GetBasePlotNode
(
	NgObjTreeNode	node,
	int		base_id
)
{
	NgObjTreeNode tn,bnode;

	if (! node)
		return NULL;
	if (node->id == base_id)
		return node;

	/* assuming the node is a view node, if it has cnodes then they
	 *  should be folder nodes; meaning there must be two of them and
	 *  the second one is the annotation node, where any base plots would
	 *  have to be.
	 */	
	if (node->type == _ngViewNode && node->cnodes) {
		bnode = GetBasePlotNode(node->cnodes->next->cnodes,base_id);
		if (bnode)
			return bnode;
	}

	return GetBasePlotNode(node->next,base_id);
}

static int
BuildRowDefs
(
	NgObjTreeNode		node,
	XmLTreeRowDefinition	*rowdefs,
	int			ix
)
{
	NgObjTreeNode np;
	if(!node)
		return ix;

	for (np = node; np; np = np->next) {
                rowdefs[ix].level = np->level;
                rowdefs[ix].expands = np->cnodes ? True : False;
                rowdefs[ix].isExpanded = False;
                rowdefs[ix].pixmap = XmUNSPECIFIED_PIXMAP;
                rowdefs[ix].pixmask = XmUNSPECIFIED_PIXMAP;
                rowdefs[ix].string = np->xmname;
		ix++;
		ix = BuildRowDefs(np->cnodes,rowdefs,ix);
	}
	return ix;
}

#define HAS_OVERLAY_NODE_TOP(vwnode) (vwnode->cnodes != NULL)
#define HAS_ANNOTATION_NODE_TOP(vwnode) \
	(vwnode->cnodes != NULL && vwnode->cnodes->next != NULL)
#define OVERLAY_NODE_TOP(vwnode) (vwnode->cnodes)
#define ANNOTATION_NODE_TOP(vwnode) (vwnode->cnodes->next)

static void
AddBasePlotFolderNodes
(
	NgObjTree		otree,
	NgObjTreeNode		vwnode
	)
{
	NgObjTreeNode  node;
        XmLTreeRowDefinition *rowdefs;
        int i,rowcount = 2;

        rowdefs = NhlMalloc(rowcount * sizeof(XmLTreeRowDefinition));
	if(! rowdefs){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return;
	}

        for (i = 0; i < rowcount; i++) {
		node = AllocNode(otree,vwnode,_ngGenericNode);
		node->level = vwnode->level + 1;
		node->pos = vwnode->pos + 1 + i;
		if(! node ){
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return;
		}
                switch (i) {
		case 0:
			node->xmname = NgXAppCreateXmString
				(otree->appmgr,"Overlays");
			OVERLAY_NODE_TOP(vwnode) = node;
                            break;
		case 1:
			node->xmname = NgXAppCreateXmString
				(otree->appmgr,"Annotations");
			ANNOTATION_NODE_TOP(vwnode) = node;
			break;
                }
                rowdefs[i].level = node->level;
                rowdefs[i].expands = False;
                rowdefs[i].isExpanded = False;
                rowdefs[i].pixmap = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].pixmask = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].string = node->xmname;
        }
        XmLTreeAddRows(otree->wtree,rowdefs,rowcount,vwnode->pos+1);
	NhlFree(rowdefs);

	vwnode->n_cnodes = rowcount;

	return;
}
	

static NhlBoolean
AddAnnotationNode
(
	NgObjTree	otree,
	NgObjTreeNode	base_node,
	NgObjTreeNode	anno_node
)
{
	char		func[]="AddAnnotationNode";
	NgObjTreeNode	pnode,*ann;
	NgObjTreeNode	an_node_folder;
	int		i,level_inc,pos,rowcount;
	NgViewObj	vobj = (NgViewObj)anno_node->ndata;
        XmLTreeRowDefinition *rowdefs;

	/* 
	 * adding an annotation node is like adding an overlay node
	 * except that any overlays or annotations belonging to the
	 * annotation remain with it.
	 */
	if (! HAS_ANNOTATION_NODE_TOP(base_node)) {
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,
			   "%s:Internal error in base node!",func));
		return False;
	}
	an_node_folder = ANNOTATION_NODE_TOP(base_node);
	if (! an_node_folder->cnodes) {
		XtVaSetValues(otree->wtree,
			      XmNrow,an_node_folder->pos,
			      XmNrowExpands,True,
			      NULL);
	}

	anno_node->pnode = an_node_folder;
	level_inc = an_node_folder->level - anno_node->level + 1;
	IncrementLevels(anno_node,level_inc);
	pos = NumberNodes(anno_node,LastNodePos(an_node_folder));
	an_node_folder->n_cnodes++;
	ann = &an_node_folder->cnodes;
	while (*ann) {
		ann = &(*ann)->next;
	}
	*ann = anno_node;

	rowcount = pos - anno_node->pos + 1;
        rowdefs = NhlMalloc(rowcount * sizeof(XmLTreeRowDefinition));
	if(! rowdefs){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return False;
	}
	BuildRowDefs(anno_node,rowdefs,0);

        XmLTreeAddRows(otree->wtree,rowdefs,rowcount,anno_node->pos);
	NhlFree(rowdefs);

	vobj->vstatus = anno_node->id == _NhlOverlayBase(anno_node->id) ?
		_ngBASE_PLOT_ANNOTATION : _ngANNOTATION;

	if (vobj->vstatus == _ngBASE_PLOT_ANNOTATION) {
		if (! anno_node->cnodes) {
			/* add the base plot folder nodes */
			XtVaSetValues(otree->wtree,
				      XmNrow,anno_node->pos,
				      XmNrowExpands,True,
				      NULL);
			AddBasePlotFolderNodes(otree,anno_node);
			pos = NumberNodes(anno_node,anno_node->pos - 1);
		}
	}		

	/* now renumber following nodes */

	pos = NumberNodes(base_node->next,pos);
	for (pnode = base_node->pnode; pnode; pnode = pnode->pnode) {
		pos = NumberNodes(pnode->next,pos);
	}
	return True;
}

static NhlBoolean
RemoveAnnotationNode
(
	NgObjTree	otree,
	NgObjTreeNode	base_node,
	int		anno_id
)
{
	char		func[]="RemoveAnnotationNode";
	NgObjTreeNode	annode,*ann,an,pnode,wknode;
	NgObjTreeNode	an_node_folder;
	int		pos,orow,nrow,row_count,level_dec;
	NgViewObj	vobj;

	/* 
	 * Find the annotation node
	 */

	an_node_folder = ANNOTATION_NODE_TOP(base_node);
	for (ann = &an_node_folder->cnodes; *ann; ann = &(*ann)->next)
		if ((*ann)->id == anno_id)
			break;
	if (! *ann) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			   "%s:Unable to find overlay node",func));
		return False;
	}
	annode = *ann;
	orow = annode->pos;
	
	*ann = annode->next;
	annode->next = NULL;

	an_node_folder->n_cnodes--;


	/* find the workstation node -- base plot could be an annotation */

	for (wknode = base_node->pnode; wknode->pnode; wknode = wknode->pnode)
		;
	/* add to the end of the top level view list */

	nrow = LastNodePos(wknode) + 1;
	for (ann = &wknode->cnodes; *ann; ann = &(*ann)->next)
		;
	*ann = annode;
	annode->pnode = wknode;
	level_dec = wknode->level - an_node_folder->level;
	IncrementLevels(annode,level_dec);
	row_count = NumberNodes(annode,nrow-1) - nrow + 1;
	if (nrow > orow) nrow -= row_count;

	XmLGridMoveRows(otree->wtree,nrow,orow,row_count);
	NumberNodes(wknode,wknode->pos-1);
	SetLevels(otree->wtree,annode);

	vobj = (NgViewObj)annode->ndata;
	if (vobj->vstatus == _ngBASE_PLOT_ANNOTATION)
		vobj->vstatus = _ngBASE_PLOT;
	else if (NhlIsTransform(annode->id))
		vobj->vstatus = _ngSIMPLE_TRANSFORM;
	else
		vobj->vstatus = _ngBASIC_VIEW;

	if (! an_node_folder->cnodes) {
		XtVaSetValues(otree->wtree,
			      XmNrow,an_node_folder->pos,
			      XmNrowExpands,False,
			      NULL);
	}

	return True;
}
	

static NhlBoolean
AddOverlayNode
(
	NgObjTree	otree,
	NgObjTreeNode	base_node,
	NgObjTreeNode	overlay_node
)
{
	char		func[]="AddOverlayNode";
	NgObjTreeNode	on,pnode,ov_node_folder,an,an_node_folder;
	NgObjTreeNode	*ovn,*ann;
	int		i,pos,rowcount,level_inc;
	NgViewObj	vobj = (NgViewObj)overlay_node->ndata;
        XmLTreeRowDefinition *rowdefs;

	if (!HAS_OVERLAY_NODE_TOP(base_node) ||
		! HAS_ANNOTATION_NODE_TOP(base_node)) {
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,
			   "%s:Internal error in base node!",func));
		return False;
	}
	ov_node_folder = OVERLAY_NODE_TOP(base_node);
	an_node_folder = ANNOTATION_NODE_TOP(base_node);
	if (! ov_node_folder->cnodes) {
		XtVaSetValues(otree->wtree,
			      XmNrow,ov_node_folder->pos,
			      XmNrowExpands,True,
			      NULL);
	}

	/* 
	 * If the new overlay has overlays of its own they need to be
	 * removed and added to the new base plot. This is done by moving
	 * the overlay's own overlay nodes from the child nodes into subsequent
	 * "sibling" nodes, then freeing the overlays folder nodes. All 
	 * these nodes are then moved to the overlay folder of the base plot.
	 * Annotation nodes are moved directly to the annotation folder of
	 * the base plot.
	 */

	ann = NULL;
	if (HAS_OVERLAY_NODE_TOP(overlay_node)) {
		overlay_node->next = OVERLAY_NODE_TOP(overlay_node)->cnodes;
		OVERLAY_NODE_TOP(overlay_node)->cnodes = NULL;
		if (HAS_ANNOTATION_NODE_TOP(overlay_node)) {
			if (ANNOTATION_NODE_TOP(overlay_node)->cnodes &&
			    ! an_node_folder->cnodes) {
				XtVaSetValues(otree->wtree,
					      XmNrow,an_node_folder->pos,
					      XmNrowExpands,True,
					      NULL);
			}
			ann = &an_node_folder->cnodes;
			while (*ann)
				ann = &(*ann)->next;

			*ann = ANNOTATION_NODE_TOP(overlay_node)->cnodes;
			ANNOTATION_NODE_TOP(overlay_node)->cnodes = NULL;
		}
		FreeNodeList(otree,OVERLAY_NODE_TOP(overlay_node));
		OVERLAY_NODE_TOP(overlay_node) = NULL;
		overlay_node->n_cnodes = 0;
	}

	/* 
	 * For now the overlay node is added at the end of the base node's
	 * overlay folder: this will need to change.
	 * While we're at it update the pnode, the level, and the row pos
	 * for the overlay node. (Remember overlay nodes cannot, for now at
	 * least, have any cnodes of their own.)
	 */
	overlay_node->pnode = ov_node_folder;
	overlay_node->level = ov_node_folder->level + 1;
	overlay_node->pos = ov_node_folder->pos + 1;
	ov_node_folder->n_cnodes++;
	ovn = &ov_node_folder->cnodes;
	while (*ovn) {
		ovn = &(*ovn)->next;
		overlay_node->pos++;
	}
	*ovn = overlay_node;

	/*
	 * Now update pnode, level and pos for any new 'sibling' 
	 * overlay nodes. (that used to be overlays of the overlay).
	 */

	for (on = (*ovn)->next,pos = (*ovn)->pos; on; on = on->next) {
		on->pnode = overlay_node->pnode;
		on->pos = ++pos;
		on->level = overlay_node->level;
	}

	/* 
	 * Adding rows to the tree must be done in two steps, since
	 * the overlay and annotation rows are not contiguous
	 */

	rowcount = pos - overlay_node->pos + 1;
        rowdefs = NhlMalloc(rowcount * sizeof(XmLTreeRowDefinition));
	if(! rowdefs){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return False;
	}
	
	for (on = overlay_node,i = 0; on; on = on->next,i++) {
                rowdefs[i].level = on->level;
                rowdefs[i].expands = False;
                rowdefs[i].isExpanded = False;
                rowdefs[i].pixmap = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].pixmask = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].string = on->xmname;
	}
        XmLTreeAddRows(otree->wtree,rowdefs,rowcount,overlay_node->pos);
	NhlFree(rowdefs);

	/* update position for the anno node folder and any of its cnodes */

	pos = NumberNodes(an_node_folder,pos);
	
	/*
	 * Next update pnode, level, for any new annotations.
	 */
	if (ann && *ann) {
		for (an = *ann; an; an = an->next)
			an->pnode = an_node_folder;
		level_inc = an_node_folder->level - (*ann)->level + 1;
		IncrementLevels(*ann,level_inc);

		/*
		 * now add the annotation nodes to the tree
		 */
		rowcount = pos - (*ann)->pos + 1;
		rowdefs = NhlMalloc(rowcount * sizeof(XmLTreeRowDefinition));
		if(! rowdefs){
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return False;
		}
		BuildRowDefs(*ann,rowdefs,0);

		XmLTreeAddRows(otree->wtree,rowdefs,rowcount,(*ann)->pos);
		NhlFree(rowdefs);
	}

	/* now renumber everything after */

	pos = NumberNodes(base_node->next,pos);
	for (pnode = base_node->pnode; pnode; pnode = pnode->pnode) {
		pos = NumberNodes(pnode->next,pos);
	}
	vobj->vstatus = _ngOVERLAY;

	return True;
}

static NhlBoolean
RemoveOverlayNode
(
	NgObjTree	otree,
	NgObjTreeNode	base_node,
	int		id
)
{
	char		func[]="RemoveOverlayNode";
	NgObjTreeNode	ov_node_folder,an_node_folder;
	NgObjTreeNode	ovnode,ov,an,pnode,wknode;
	NgObjTreeNode	*ovn,*ann;
	int 		i,orow, nrow;
	int		*seq_ids,seq_id_count = 0;
	int		*anno_views,anno_view_count = 0;
	static		int getrl = -1;
	NhlBoolean	is_base_plot;
	NgViewObj	vobj;

	is_base_plot = id == _NhlBasePlot(id);

	if (! (HAS_OVERLAY_NODE_TOP(base_node) &&
	       HAS_ANNOTATION_NODE_TOP(base_node) ) ) {
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,
			   "%s:Internal error in base node!",func));
		return False;
	}
	ov_node_folder = OVERLAY_NODE_TOP(base_node);
	an_node_folder = ANNOTATION_NODE_TOP(base_node);
	/* 
	 * Find the overlay node only; we'll deal with the restore situation
	 * later on in the routine.
	 */

	for (ovn = &ov_node_folder->cnodes; *ovn; ovn = &(*ovn)->next)
		if ((*ovn)->id == id)
			break;
	if (! *ovn) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			   "%s:Unable to find overlay node",func));
		return False;
	}
	ovnode = *ovn;
	orow = ovnode->pos;
	
	*ovn = ovnode->next;
	ovnode->next = NULL;

	ov_node_folder->n_cnodes--;

	/* find the workstation node -- base plot could be an annotation */

	for (wknode = base_node->pnode; wknode->pnode; wknode = wknode->pnode)
		;
	/* add to the end of the top level view list */

	nrow = LastNodePos(wknode) + 1;

	for (ovn = &wknode->cnodes; *ovn; ovn = &(*ovn)->next)
		;
	*ovn = ovnode;
	ovnode->pnode = wknode;
	ovnode->level = ovnode->pnode->level + 1;

	if (nrow > orow) nrow--;

	ovnode->pos = nrow;
	XmLGridMoveRows(otree->wtree,nrow,orow,1);

	vobj = (NgViewObj)ovnode->ndata;
	NumberNodes(wknode,wknode->pos-1);
	if ( ! is_base_plot) {
		vobj->vstatus = _ngSIMPLE_TRANSFORM;
		if (! ov_node_folder->cnodes) {
			XtVaSetValues(otree->wtree,
				      XmNrow,ov_node_folder->pos,
				      XmNrowExpands,False,
				      NULL);
		}
		return True;
	}

	/* 
	 * The following code applies only for base plots (as opposed to 
	 * "simple transforms")
	 */

	vobj->vstatus = _ngBASE_PLOT;
	XtVaSetValues(otree->wtree,
		      XmNrow,nrow,
		      XmNrowLevel,ovnode->level,
		      XmNrowExpands,True,
		      NULL);
	AddBasePlotFolderNodes(otree,ovnode);
	
	if (getrl < 0)
		getrl = NhlRLCreate(NhlGETRL);
	else 
		NhlRLClear(getrl);
	NhlRLGetIntegerArray(getrl,"pmOverlaySequenceIds",
			     &seq_ids,&seq_id_count);
	NhlRLGetIntegerArray(getrl,"pmAnnoViews",
			     &anno_views,&anno_view_count);
	NhlGetValues(id,getrl);

	/* 
	 * Although this is not terribly efficient, for now we indivually 
	 * remove and then add each overlay that is being 'returned'.
	 * Start at one because the base_plot is the first item in the
	 * sequence.
	 */

	for (i = 1; i < seq_id_count; i++) {
		for (ovn = &ov_node_folder->cnodes; *ovn; ovn = &(*ovn)->next)
			if ((*ovn)->id == seq_ids[i])
				break;
		if (! *ovn) {
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				   "%s:Unable to find overlay node",func));
			return False;
		}
		ov = *ovn;
		orow = ov->pos;
	
		*ovn = ov->next;
		ov->next = NULL;

		ov_node_folder->n_cnodes--;
		XmLGridDeleteRows(otree->wtree,XmCONTENT,ov->pos,1);
		NumberNodes(wknode->cnodes,wknode->pos);
		AddOverlayNode(otree,ovnode,ov);
	}
	if (! ov_node_folder->cnodes) {
		XtVaSetValues(otree->wtree,
			      XmNrow,ov_node_folder->pos,
			      XmNrowExpands,False,
			      NULL);
	}
	/*
	 * same thing for annotations 
	 */
	for (i = 0; i < anno_view_count; i++) {
		for (ann = &an_node_folder->cnodes; *ann; ann = &(*ann)->next)
			if ((*ann)->id == anno_views[i])
				break;
		if (! *ann) {
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				   "%s:Unable to find annotation node",func));
			return False;
		}
		an = *ann;
		orow = an->pos;
	
		*ann = an->next;
		an->next = NULL;

		an_node_folder->n_cnodes--;
		XmLGridDeleteRows(otree->wtree,XmCONTENT,an->pos,1);
		NumberNodes(wknode->cnodes,wknode->pos);
		AddAnnotationNode(otree,ovnode,an);
	}
	if (! an_node_folder->cnodes) {
		XtVaSetValues(otree->wtree,
			      XmNrow,an_node_folder->pos,
			      XmNrowExpands,False,
			      NULL);
	}

	return True;
}

extern void SetViewBBOffscreen
(
	NgObjTree	otree,
	int		pid
	)
{
	NgObjTreeNode	vnode;
	NgViewObj	vobj;


	vnode = FindNode(otree->wklist,pid);
	if (vnode)
		vobj = (NgViewObj) vnode->ndata;

	if (vobj) {
		vobj->xbbox.p0.x = vobj->xbbox.p0.y = 10000;
		vobj->xbbox.p1.x = vobj->xbbox.p0.x + 1;
		vobj->xbbox.p1.y = vobj->xbbox.p0.y + 1;
		vobj->xvp = vobj->xbbox;
	}

	return;
}

static void
AnnoStatusCB
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	char		func[]="AnnoStatusCB";
	_NhlAnnoStatusCBData anstat = (_NhlAnnoStatusCBData)cbdata.ptrval;
	NgObjTree	otree = (NgObjTree)udata.ptrval;
	NhlLayer	vl;
	NgObjTreeNode   vwnode,basenode;
	NgObjTreeNode	bnode,annode,*vp,wp = NULL;
	NgViewObj	vwo;
	int		base_id;
	NgWksObj 	wks;
	NhlBoolean	is_xwork;

#if DEBUG_MWIN
	fprintf(stderr,"in anno status cb\n");
#endif
	vl = _NhlGetLayer(anstat->id);
	
	if (vl) {
		for (wp = otree->wklist; wp; wp = wp->next)
			if (wp->id == vl->base.wkptr->base.id)
				break;
	}
	if (! wp) {
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,
			   "%s:Invalid annotation id!",func));
		return;
	}
	is_xwork = _NhlIsXWorkstation(_NhlGetLayer(wp->id));

	base_id = _NhlIsOverlay(anstat->base_id) ?
		_NhlOverlayBase(anstat->base_id) : anstat->base_id;

	wks = (NgWksObj)wp->ndata;
	if (vl->base.wkptr->base.being_destroyed)
		wks->auto_refresh = False;
	if (anstat->isanno) {
		int pos,sel_id;

		/*
		 * it's a new annotation; currently it's a top-level child.
		 * if it has its own overlays or annotations, they remain
		 * with it.
		 */
		for (vp = &wp->cnodes; *vp; vp = &(*vp)->next) {
			if (anstat->id == (*vp)->id)
				break;
		}
		if (! (*vp)) {
			NHLPERROR((NhlWARNING,NhlEUNKNOWN,
				   "%s:Internal error finding overlay node!",
				   func));
			return;
		}
		/* 
		 * the base plot that it is being added to could be an
		 * annotation or an overlay. If it's an overlay, we really
		 * want to add the annotation to its base plot.
		 */
		bnode = GetBasePlotNode(wp->cnodes,base_id);
		if (! bnode) {
			NHLPERROR((NhlWARNING,NhlEUNKNOWN,
				   "%s:Internal error finding base plot node!",
				   func));
			return;
		}
		annode = *vp;
		*vp = annode->next;
		annode->next = NULL;
		pos = LastNodePos(annode);
		XmLGridDeleteRows(otree->wtree,XmCONTENT,annode->pos,
				  LastNodePos(annode) - annode->pos + 1);
		pos = NumberNodes((*vp),annode->pos-1);
		NumberNodes(annode->pnode->next,pos);

		/* now everything should be in sync again, so add the
		 * annotation node to the base plot.
		 * Note that we do the removal here in the
		 * callback so that AddAnnotationNode could work with a view
		 * that becomes an annotation at create time (not possible
		 * currently).
		 * But if it's an XWorkstation, clear view first
		 */

		if (Do_Draw && is_xwork && wks->auto_refresh) {
			int pbase_id;
			NgClearXwkView(wks->wks_wrap_id,annode->id);
			AddAnnotationNode(otree,bnode,annode);
			/* 
			 * Note that the annotation's base plot is not 
			 * necessarily the primary base plot. 
			 */
			pbase_id = _NhlBasePlot(base_id);
			/* 
			 * Redraw the base plot so the overlay will show up
			 */
			NgDrawXwkView(wks->wks_wrap_id,pbase_id,False);
			NhlVAGetValues(wks->wks_wrap_id,
				       NgNxwkSelectedView,&sel_id,
				       NULL);
			if (sel_id == annode->id) {
				NgSetSelectedXwkView
					(wks->wks_wrap_id,pbase_id);
			}
		}
		else {
			AddAnnotationNode(otree,bnode,annode);
		}
	}
	else {
		/* find the former base plot */

		bnode = GetBasePlotNode(wp->cnodes,base_id);
		if (! bnode) {
			NHLPERROR((NhlWARNING,NhlEUNKNOWN,
				   "%s:Internal error finding base plot node!",
				   func));
			return;
		}

		/* 
		 * If it's an XWorkstation, clear the base plot BB
		 */
		/*
		 * the remove function both removes and replaces the 
		 * tree rows.
		 */
		if (! (Do_Draw && is_xwork && wks->auto_refresh)) {
			RemoveAnnotationNode(otree,bnode,anstat->id);
		}
		else {
			NhlLayer bl = _NhlGetLayer(anstat->base_id);

			/*
			 * This draw must occur before removing the node so
			 * that the removed annotation won't yet be seen as
			 * an independent plot. If it's being destroyed it
			 * can't be drawn.
			 */

			if (bl && ! bl->base.being_destroyed)
				NgDrawXwkView(wks->wks_wrap_id,
					      anstat->base_id,True);
				

			RemoveAnnotationNode(otree,bnode,anstat->id);
			/* 
			 * Redraw the now top-level view
			 */
			if (vl->base.being_destroyed) {
				SetViewBBOffscreen(otree,anstat->id);
			}	
			else {
				NgUpdateViewBB((NgWksState)otree,anstat->id);
				NgDrawXwkView
					(wks->wks_wrap_id,anstat->id,False);
			}
		}

	}
	return;
}

static void
OverlayStatusCB
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	char		func[]="OverlayStatusCB";
	_NhlOverlayStatusCBData ovstat = 
		(_NhlOverlayStatusCBData)cbdata.ptrval;
	NhlLayer	tl;
	NgObjTree	otree = (NgObjTree)udata.ptrval;
	NgObjTreeNode   trnode,basenode;
	NgObjTreeNode	bvp,ovnode,*vp,wp = NULL;
	NgWksObj	wks;

#if DEBUG_MWIN
	fprintf(stderr,"in overlay status cb\n");
#endif
	tl = _NhlGetLayer(ovstat->id);
	
	if (tl) {
		for (wp = otree->wklist; wp; wp = wp->next)
			if (wp->id == tl->base.wkptr->base.id)
				break;
	}
	if (! wp) {
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,
			   "%s:Invalid overlay id!",func));
		return;
	}

	wks = (NgWksObj)wp->ndata;
	if (tl->base.wkptr->base.being_destroyed)
		wks->auto_refresh = False;
	if (ovstat->status == _tfCurrentOverlayMember) {
		int pos,sel_id;
		
		/*
		 * it's a new overlay; currently it's a top-level child.
		 * if it has its own overlays or annotations they will have
		 * to be removed.
		 */
		for (vp = &wp->cnodes; *vp; vp = &(*vp)->next) {
			if (ovstat->id == (*vp)->id)
				break;
		}
		if (! (*vp)) {
			NHLPERROR((NhlWARNING,NhlEUNKNOWN,
				   "%s:Internal error finding overlay node!",
				   func));
			return;
		}
		bvp = GetBasePlotNode(wp->cnodes,ovstat->base_id);
		if (! bvp) {
			NHLPERROR((NhlWARNING,NhlEUNKNOWN,
				   "%s:Internal error finding base plot node!",
				   func));
			return;
		}

		ovnode = *vp;
		*vp = ovnode->next;
		ovnode->next = NULL;
		pos = LastNodePos(ovnode);
		XmLGridDeleteRows(otree->wtree,XmCONTENT,ovnode->pos,
				  LastNodePos(ovnode) - ovnode->pos + 1);
		pos = NumberNodes((*vp),ovnode->pos-1);
		NumberNodes(ovnode->pnode->next,pos);

		/* now everything should be in sync again, so add the
		 * overlay node to the overlay base.
		 * Note that we do the removal here in the
		 * callback so that AddOverlayNode can work with a plot
		 * that is already an overlay when it is first added to the
		 * view tree.
		 * But if it's an XWorkstation, clear view first
		 */

		if (Do_Draw && _NhlIsXWorkstation(_NhlGetLayer(wp->id)) &&
		    wks->auto_refresh) {
			NgClearXwkView(wks->wks_wrap_id,ovnode->id);
			AddOverlayNode(otree,bvp,ovnode);
			NhlVAGetValues(wks->wks_wrap_id,
				       NgNxwkSelectedView,&sel_id,
				       NULL);
			if (sel_id == ovnode->id) {
				NgSetSelectedXwkView(wks->wks_wrap_id,bvp->id);
			}
			/* 
			 * Redraw the base plot so the overlay will show up
			 */
			NgDrawXwkView(wks->wks_wrap_id,bvp->id,False);
		}
		else {
			AddOverlayNode(otree,bvp,ovnode);
		}
	}
	else {
		/*
		 * it's been removed from an overlay. restore it to top-level
		 * status.
		 */
		bvp = GetBasePlotNode(wp->cnodes,ovstat->base_id);
		if (! bvp) {
			NHLPERROR((NhlWARNING,NhlEUNKNOWN,
				   "%s:Internal error finding overlay node!",
				   func));
			return;
		}

		/* 
		 * If it's an XWorkstation, clear the base plot BB
		 */
		/*
		 * the remove function both removes and replaces the 
		 * tree rows.
		 */

		if (! (Do_Draw && _NhlIsXWorkstation(_NhlGetLayer(wp->id)) &&
		    wks->auto_refresh)) {
			RemoveOverlayNode(otree,bvp,ovstat->id);
		}
		else {
			NhlLayer bl = _NhlGetLayer(ovstat->base_id);
			/*
			 * This draw must occur before removing the node so
			 * that the removed overlay won't yet be seen as
			 * an independent plot. If it's being destroyed it
			 * can't be drawn.
			 */
			if (bl && ! bl->base.being_destroyed)
				NgDrawXwkView(wks->wks_wrap_id,
					      ovstat->base_id,True);

			RemoveOverlayNode(otree,bvp,ovstat->id);
			/* 
			 * Redraw the now top-level view
			 */
			if (tl->base.being_destroyed) {
				SetViewBBOffscreen(otree,ovstat->id);
			}	
			else {
				NgUpdateViewBB((NgWksState)otree,ovstat->id);
				NgDrawXwkView
					(wks->wks_wrap_id,ovstat->id,False);
			}
		}

	}
	return;
}	

static void
CheckMissingBaseNodeList
(
	NgObjTree		otree,
	int			base_id
)
{
	char		func[]="CheckMissingBaseNodeList";
	NgMissingBaseNode	tmbn,*mbn = &otree->missing_bn_list;
	NhlArgVal 		cbdata,udata;
	_NhlAnnoStatusCBDataRec anstat;
	_NhlOverlayStatusCBDataRec ovstat;
/*
 * this routine fakes callback calls to notify annotations and overlays
 * whose nodes were added to the tree before their base plot nodes, that
 * their base plot nodes are now available.
 */
	while (*mbn) {
		if ((*mbn)->base_id != base_id) {
			mbn = &(*mbn)->next;
			continue;
		}
		tmbn = *mbn;
		*mbn = (*mbn)->next;
		switch (tmbn->member_type) {
		case _ngANNOTATION:	
		case _ngBASE_PLOT_ANNOTATION:
			anstat.id = tmbn->member_id;
			anstat.base_id = base_id;
			anstat.anno_manager_id = 0; /* don't care */
			anstat.isanno = True;
			NhlINITVAR(udata);
			NhlINITVAR(cbdata);
			cbdata.ptrval = (void *) &anstat;
			udata.ptrval = (void *) otree;
			AnnoStatusCB(cbdata,udata);
			break;
		case _ngOVERLAY:
			ovstat.id = tmbn->member_id;
			ovstat.base_id = base_id;
			ovstat.status = _tfCurrentOverlayMember;
			NhlINITVAR(udata);
			NhlINITVAR(cbdata);
			cbdata.ptrval = (void *) &ovstat;
			udata.ptrval = (void *) otree;
			OverlayStatusCB(cbdata,udata);
			break;
		default:
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				   "%s:Invalid plot member type!",func));
			break;
		}
		NhlFree(tmbn);
	}
}	
	
static void
SetValuesCB
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	char		func[]="OverlayStatusCB";
	_NhlValueSetCBData svstat = 
		(_NhlValueSetCBData)cbdata.ptrval;
	NhlLayer	vl;
	NgObjTree	otree = (NgObjTree)udata.ptrval;
	NgObjTreeNode	wp = NULL;
	NgViewObj	vobj;
	NgWksObj	wks;
	int		base_id;
	NgHluData	hdata;

#if DEBUG_MWIN
	fprintf(stderr,"in set values cb\n");
#endif
	vl = _NhlGetLayer(svstat->id);
	if (! vl) {
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,
			   "%s:Invalid view id!",func));
		return;
	}

	if (! _NhlIsClass(vl->base.wkptr,NhlxWorkstationClass))
		return;
	if (vl->base.being_destroyed || vl->base.wkptr->base.being_destroyed)
		return;


	hdata = (NgHluData) vl->base.gui_data2;
	vobj = hdata ? (NgViewObj) hdata->gdata : NULL;

	if (vobj) 
		NhlVAGetValues(svstat->id,
			       NhlNvpOn,&vobj->visible,
			       NULL);

	return;
}
	

static NhlBoolean
AddViewNode
(
	NgObjTree		otree,
	NgObjTreeNode		wknode,
	NgObjTreeNode		vwnode,
	NgNclHluObj		hlu
)
{
	char		func[]="AddViewNode";
	NhlArgVal	sel,udata;
	NgObjTreeNode	*vtp;
	NgObjTreeNode	bnode;
	int		i,base_id;
	Boolean		is_base_plot;
	int		last_node_pos;
	NhlBoolean	base_node_not_found = False;
	NhlLayer	l;
	char		*name;
	NgHluData	hdata;
	NgViewObj	vwo = NULL;

	l = _NhlGetLayer(hlu->id);
	if (l->base.gui_data2) {
		hdata = (NgHluData) l->base.gui_data2;
		vwo = hdata ? (NgViewObj) hdata->gdata : NULL;
	}
	else {
		hdata = NgGetHluData();
		if (! hdata)
			return False;
		l->base.gui_data2 = (NhlPointer) hdata;
	}
	if (! vwo) {
		vwo = NhlMalloc(sizeof(NgViewObjRec));
		if(!vwo){
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return False;
		}
		vwo->ovcb = NULL;
		vwo->ancb = NULL;
		vwo->svcb = NULL;
		vwo->vstatus = _ngBASIC_VIEW;
		vwo->xvp.p0.x = vwo->xvp.p0.y = 
			vwo->xvp.p1.x = vwo->xvp.p1.y = 15000;
		vwo->xbbox = vwo->xvp;
		vwo->visible = False;
		hdata->gdata = (NhlPointer) vwo;
	}
	vwo->name = hlu->name;
	vwo->parent_id = hlu->parent_id;

	vwnode->ndata = (ObjNodeData) vwo;
	vwnode->id = hlu->id;
	name = NgNclGetHLURef(otree->nsid,vwnode->id);
	if (name) {
		vwnode->xmname = NgXAppCreateXmString(otree->appmgr,name);
	}
	else if (hlu->name) {
		vwnode->xmname = 
			NgXAppCreateXmString(otree->appmgr,(char*)hlu->name);
	}
	else {
		vwnode->xmname = NgXAppCreateXmString(otree->appmgr,"no name");
	}
	vwo->name = hlu->name;
	vwo->parent_id = hlu->parent_id;

	vwo->ovcb = vwo->ancb = NULL;
	vwo->vstatus = GetViewStatus(hlu->id);
	
	NhlINITVAR(sel);
	NhlINITVAR(udata);
	sel.lngval = 0;	/* not used */
	udata.ptrval = otree;
	if (NhlIsTransform(vwnode->id)) {
		vwo->ovcb = _NhlAddObjCallback
			(_NhlGetLayer(vwnode->id),_NhlCBtfOverlayStatus,
			 sel,OverlayStatusCB,udata);
		if (!vwo->ovcb)	{
			NHLPERROR((NhlWARNING,NhlEUNKNOWN,
				   "%s:Unable to track overlay status!",func));
		}
	}
	vwo->ancb = _NhlAddObjCallback
		(_NhlGetLayer(vwnode->id),_NhlCBvpAnnoStatus,
		 sel,AnnoStatusCB,udata);
	if (!vwo->ancb)	{
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,
			   "%s:Unable to track annotation status!",func));
	}
	sel.lngval = NrmStringToQuark(NhlNvpOn);
	vwo->svcb = _NhlAddObjCallback
		(_NhlGetLayer(vwnode->id),_NhlCBobjValueSet,
		 sel,SetValuesCB,udata);
	if (!vwo->svcb)	{
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,
			   "%s:Unable to track set values!",func));
	}

	base_id = _NhlBasePlot(vwnode->id);
	is_base_plot = _NhlOverlayBase(vwnode->id) == vwnode->id;

	/*
	 * If a script loads several view objects some of which are
	 * annotations and/or overlays there is a problem: the base plots
	 * may not be in the object tree when the member plots are added.
	 * We will have to save them as primary plots initially, then
	 * somehow do the equivalent of executing the callback notification
	 * function after the base plot has been added to the tree.
	 */

	if (base_id != NhlNULLOBJID && base_id != vwnode->id) {
		NgWksObj wks = (NgWksObj) wknode->ndata;
		NhlBoolean save_auto_refresh = wks->auto_refresh;
		wks->auto_refresh = False;

		if (_NhlIsAnnotation(vwnode->id)) {
			base_id = _NhlAnnotationBase(vwnode->id);
		}
		bnode = GetBasePlotNode(wknode->cnodes,base_id);
		if(!bnode) {
			base_node_not_found = True;
			wks->auto_refresh = save_auto_refresh;
			if (NhlIsTransform(vwnode->id) &&
			    ! _NhlIsSimpleTransform(vwnode->id))
				is_base_plot = True;
		}
		else if (_NhlIsAnnotation(vwnode->id)) {
			if (!AddAnnotationNode(otree,bnode,vwnode)) {
				wks->auto_refresh = save_auto_refresh;
				return False;
			}
			if (is_base_plot) {
				CheckMissingBaseNodeList(otree,vwnode->id);
			}
			wks->auto_refresh = save_auto_refresh;
			return True;
		}
		else {
			NhlBoolean status;
			status = AddOverlayNode(otree,bnode,vwnode);
			wks->auto_refresh = save_auto_refresh;
			return status;
		}
	}
	/*
	 * TODO: Add view in correct Draw Order...
	 */

	vtp = &wknode->cnodes;
	last_node_pos = LastNodePos(wknode);
	while(*vtp){
		vtp = &((*vtp)->next);
	}
	*vtp = vwnode;
	wknode->n_cnodes++;
	vwnode->pos = last_node_pos + 1;
	
	vwnode->tree = otree;
	vwnode->level = vwnode->pnode->level + 1;
	XmLTreeAddRow
		(otree->wtree,vwnode->level,is_base_plot,False,vwnode->pos,
		 XmUNSPECIFIED_PIXMAP,XmUNSPECIFIED_PIXMAP,vwnode->xmname);
	if (is_base_plot) {
		if (! base_node_not_found)
			vwo->vstatus = _ngBASE_PLOT;
		AddBasePlotFolderNodes(otree,vwnode);
	}
	NumberNodes(wknode->next,LastNodePos(wknode));

	if (base_node_not_found) {
		/* save in the not yet found list */
		NgMissingBaseNode missing_bn = 
			NhlMalloc(sizeof(NgMissingBaseNodeRec));
		missing_bn->base_id = base_id;
		missing_bn->member_id = vwnode->id;
		missing_bn->member_type = vwo->vstatus;
		missing_bn->next = otree->missing_bn_list;
		otree->missing_bn_list = missing_bn;
#if DEBUG_MWIN
		fprintf(stderr,"base node not found\n");
#endif
	}
	if (is_base_plot) {
		Do_Draw = False;
		CheckMissingBaseNodeList(otree,vwnode->id);
		Do_Draw = True;
	}
	NgUpdateViewBB((NgWksState)otree,vwnode->id);
	if (! base_node_not_found) {
		if (_NhlIsClass(l->base.wkptr,NhlxWorkstationClass)) {
			NgHluData	hdata;
			NgWksObj	wks;

			hdata = (NgHluData) l->base.wkptr->base.gui_data2;
			wks = hdata ? (NgWksObj) hdata->gdata : NULL;
			if (wks && wks->auto_refresh)
				NgDrawXwkView
					(wks->wks_wrap_id,vwnode->id,False);
				NgSetSelectedXwkView
					(wks->wks_wrap_id,vwnode->id);
		}
	}

	return True;
}

static void
CreateCB
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	char		func[]="CreateCB";
	NgNclHluObj	hlu = (NgNclHluObj)cbdata.ptrval;
	NgObjTree	otree = (NgObjTree)udata.ptrval;
	NgObjTreeNode	new;
	NgObjTreeNode	tmp;
	NhlLayer	l;

	if (! strncmp(hlu->name,"_NgPreview_",11))
		return;

	l = _NhlGetLayer(hlu->id);;

	if (! l) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:Invalid obj id %d",
			   func,hlu->id));
		return;
	}
	else if (_NhlIsClass(l,NhlworkstationClass)){
		new = AllocNode(otree,NULL,_ngWksNode);
		if(!new){
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return;
		}
		if (! AddWorkNode(otree,new,hlu))
			FreeNode(new);
	}
	else if (_NhlIsClass(l,NhlviewClass) &&
		_NhlIsClass(_NhlGetLayer(hlu->parent_id),NhlworkstationClass)){

		for(tmp=otree->wklist;tmp;tmp=tmp->next)
			if(tmp->id == hlu->parent_id)
				break;
		if(!tmp) {
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				   "%s: Workstation not found for view id %d",
				   func,hlu->id));
			return;
		}
		new = AllocNode(otree,tmp,_ngViewNode);
		if(!new){
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return;
		}
		if (! AddViewNode(otree,tmp,new,hlu)) {
			FreeNode(new);
			return;
		}
	}

	return;
}

static void
DestroyCB
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	NgNclHluObj	hlu = (NgNclHluObj)cbdata.ptrval;
	NgObjTree	otree = (NgObjTree)udata.ptrval;
	NgObjTreeNode	*tptr,tmp,work;

	if (! strncmp(hlu->name,"_NgPreview_",11))
		return;

	if (NhlClassIsSubclass(hlu->class_ptr,NhlworkstationClass)){
		for(tptr=&otree->wklist;*tptr;tptr=&((*tptr)->next))
			if((*tptr)->id == hlu->id)
				break;
		if(!*tptr)
			return;
		work = *tptr;
		*tptr = (*tptr)->next;
		NumberNodes(*tptr,work->pos-1);
		work->next = NULL;
		
		XmLGridDeleteRows(otree->wtree,XmCONTENT,work->pos,
				  LastNodePos(work) - work->pos + 1);
		FreeNodeList(otree,work);
	}
        else if (NhlClassIsSubclass(hlu->class_ptr,NhlviewClass) &&
                _NhlIsClass(_NhlGetLayer(hlu->parent_id),NhlworkstationClass)){
		NgWksObj	wkobj;
		NgViewObj	vobj;
		NhlLayer	wl;

		for(work=otree->wklist;work;work=work->next)
			if(work->id == hlu->parent_id)
				break;
		if(!work)
			return;
		for(tptr=&work->cnodes;*tptr;tptr=&((*tptr)->next))
			if((*tptr)->id == hlu->id)
				break;
		if(!*tptr)
			return;
		tmp = *tptr;
		vobj = (NgViewObj)tmp->ndata;
		wkobj = (NgWksObj) work->ndata;
		wl = _NhlGetLayer(work->id);
		if (wl->base.being_destroyed)
			wkobj->auto_refresh = False;

		/*
		 * Now that the node has been found, erase it if it's in
		 * an XWorkstationit window. Then all overlays and 
		 * annotations must be removed before proceding. 
		 */
		if (_NhlIsXWorkstation(_NhlGetLayer(work->id)) && 
		    wkobj->auto_refresh)
			NgClearXwkView(wkobj->wks_wrap_id,hlu->id);

		if (vobj->vstatus == _ngBASE_PLOT) {
			NgObjTreeNode ov = tmp->cnodes->cnodes;
			while (ov) {
				if (! _NhlIsOverlay(ov->id)) {
					RemoveOverlayNode(otree,tmp,ov->id);
					ov = tmp->cnodes->cnodes;
				}
				else 
					ov = ov->next;
			}	
		}
		*tptr=tmp->next;
		tmp->next = NULL;
		XmLGridDeleteRows(otree->wtree,XmCONTENT,tmp->pos,
				  LastNodePos(tmp) - tmp->pos + 1);
		FreeNodeList(otree,tmp);
		work->n_cnodes--;
		NumberNodes(work,work->pos-1);
	}

	return;
}

static void
CleanUpXCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
);

static void
CleanUp
(
	NgObjTree	otree
)
{
	_NhlCBDelete(otree->create);
	_NhlCBDelete(otree->destroy);
	_NhlCBDelete(otree->dncl);
	XtRemoveCallback(otree->wtree,XmNdestroyCallback,CleanUpXCB,otree);
	FreeNodeList(otree,otree->wklist);
	NhlFree(otree);

	return;
}

static void
CleanUpCB
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
#if 0        
	CleanUp((NgObjTree)udata.ptrval);
#endif        
	return;
}

static void
CleanUpXCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
)
{
	CleanUp((NgObjTree)udata);
	return;
}

static NgWksState
ManageObjTree
(
	NgGO	go,
	Widget	tree,
	int	nsid
)
{
	char		func[] = "ManageObjTree";
	NhlLayer	ncl = _NhlGetLayer(nsid);
	NgObjTree	otree;
	NhlArgVal	sel,udata;
        NhlPointer      guiData;

	if(!ncl || !_NhlIsClass(ncl,NgnclStateClass)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,       
			   "%s:Invalid nclstate id",func));
		return NULL;
	}

	otree = NhlMalloc(sizeof(NgObjTreeRec));
	if(!otree){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NULL;
	}

	otree->go = go;
	otree->nsid = nsid;
	NhlVAGetValues(nsid,
		_NhlNguiData,	&guiData,
		NULL);
        otree->appmgr = (int)guiData;

	if(!NhlIsClass(otree->appmgr,NgappMgrClass)){
		NhlFree(otree);
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:Invalid appmgr",func));
		return NULL;
	}

	otree->wtree = tree;

	otree->wklist = NULL;
	otree->missing_bn_list = NULL;
	NgNclEnumerateObj(nsid,NgNclHLUOBJ,GetWkObjs,otree);
	NgNclEnumerateObj(nsid,NgNclHLUOBJ,GetVwObjs,otree);

	NumberNodes(otree->wklist,-1);
	InitWTree(otree,otree->wtree,otree->wklist);

	NhlINITVAR(sel);
	NhlINITVAR(udata);
	sel.lngval = NgNclCBCREATE_HLUOBJ;
	udata.ptrval = otree;
	otree->create = _NhlAddObjCallback
		(ncl,NgCBnsObject,sel,CreateCB,udata);

	sel.lngval = NgNclCBDELETE_HLUOBJ;
	otree->destroy = _NhlAddObjCallback
		(ncl,NgCBnsObject,sel,DestroyCB,udata);

	sel.lngval = 0;
	otree->dncl = _NhlAddObjCallback
		(ncl,_NhlCBobjDestroy,sel,CleanUpCB,udata);

	XtVaSetValues(otree->wtree,
		XmNuserData,	otree,
		NULL);

	XtAddCallback(otree->wtree,XmNdestroyCallback,CleanUpXCB,otree);

	return (NgWksState) otree;
}

static void
CreateXWorkCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
)
{
	char		func[]="CreateXWorkCB";
	int		goid;
	int		appmgr;
	int		nclstate = NhlDEFAULT_APP;
	char		*name;
	char		line[512];
        NhlPointer      guiData;

	goid = NgGOWidgetToGoId(w);
	if(goid == NhlDEFAULT_APP){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:invalid Widget",func));
		return;
	}
	NhlVAGetValues(goid,
		_NhlNguiData,	&guiData,
		NULL);
        appmgr = (int)guiData;

	NhlVAGetValues(appmgr,
		NgNappNclState,	&nclstate,
		NULL);
	if(!NhlIsClass(nclstate,NgnclStateClass)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
					"%s:invalid nclstate obj",func));
		return;
	}

	/*
	 * Submit to nclstate
	 */
	name = NgNclGetSymName(nclstate,"Xwk",True);
	sprintf(line,
	"%s = create \"%s\" xWorkstationClass defaultapp\nend create\n",
		name,name);
	(void)NgNclSubmitBlock(nclstate,line);

        if (! NclSymbolDefined(Ng_SELECTED_WORK)) {
                sprintf(line,"%s = %s\n",Ng_SELECTED_WORK,name);
                (void)NgNclSubmitBlock(nclstate,line);
        }
        
	return;
}

static NhlBoolean
MWCreateWin
(
	NgGO	go
)
{
	char		func[]="MWCreateWin";
	NgMWinPart	*mp = &((NgMWin)go)->mwin;
	Widget		pane;
	Widget		pform,ptbform,dtbform,dform;
	Widget		cwki,cvi;
	Widget		objtree;
	int		nsid;
	NgWksState	wks_state;

	XtAppAddActions(go->go.x->app,
                        mwinactions,NhlNumber(mwinactions));

	_NgGOCreateMenubar(go);
	_NgGOSetTitle(go,"Plot Manager",NULL);

	pane = XtVaCreateManagedWidget("pane",xmPanedWindowWidgetClass,
								go->go.manager,
		XmNtopAttachment,	XmATTACH_WIDGET,
		XmNtopWidget,		go->go.menubar,
		NULL);

	pform = XtVaCreateManagedWidget("pform",xmFormWidgetClass,pane,
		NULL);

	ptbform = XtVaCreateManagedWidget("ptbform",xmFormWidgetClass,pform,
		XmNbottomAttachment,	XmATTACH_NONE,
		NULL);

	objtree = XtVaCreateManagedWidget
		("otree",xmlTreeWidgetClass,pform,
		 XmNtopAttachment,	XmATTACH_WIDGET,
		 XmNtopWidget,		ptbform,
		 NULL);

	NhlVAGetValues(go->go.appmgr,
		NgNappNclState,	&nsid,
		NULL);

	wks_state = ManageObjTree(go,objtree,nsid);

	NhlVASetValues(go->go.appmgr,
		       NgNappWksState,wks_state,
		       NULL);
	XtVaSetValues(objtree,
		      XmNuserData,wks_state,
		      NULL);

	mp->cwki=cwki = XtVaCreateManagedWidget
		("cwki",xmPushButtonWidgetClass,ptbform,
		 XmNrightAttachment,	XmATTACH_NONE,
		 NULL);
	XtAddCallback(cwki,XmNactivateCallback,CreateXWorkCB,NULL);

	cvi = XtVaCreateManagedWidget("cvi",xmPushButtonWidgetClass,ptbform,
		XmNleftAttachment,	XmATTACH_WIDGET,
		XmNleftWidget,		cwki,
		NULL);

	dform = XtVaCreateManagedWidget("dform",xmFormWidgetClass,pane,
		NULL);

	dtbform = XtVaCreateManagedWidget("dtbform",xmFormWidgetClass,dform,
		NULL);

	return True;
}

static NhlBoolean
MWCreateWinHook
(
	NgGO	go
)
{
	char		func[]="MWCreateWinHook";
	NgMWinPart	*mp = &((NgMWin)go)->mwin;
	Pixmap		pmap = None;
	int		xpmstatus;
	XpmAttributes	xpmat;
	XpmColorSymbol	colsym;
	Pixel		back;
#if 0
	XtRealizeWidget(go->go.shell);

	/*
	 * Set the colorsymbol "none" to the background - poor man's
	 * transparency.
	 */
	XtVaGetValues(mp->cwki,
		XmNbackground,	&back,
		NULL);

	colsym.name = NULL;
	colsym.value = "none";
	colsym.pixel = back;
	xpmat.colorsymbols = &colsym;
	xpmat.numsymbols = 1;
	xpmat.valuemask = XpmColorSymbols;

	xpmstatus = XcbXpmCreatePixmapFromData(go->go.xcb,XtWindow(mp->cwki),
				xterm_xpm,&pmap,NULL,&xpmat);

	if(xpmstatus != XpmSuccess){
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,"Unable to create Pixmap"));
		return True;
	}

	XtVaSetValues(mp->cwki,
		XmNlabelType,	XmPIXMAP,
		XmNlabelPixmap,	pmap,
		NULL);

	XtAddCallback(mp->cwki,XmNdestroyCallback,XcbFreePixmapCB,
							(XtPointer)pmap);
#endif
	return True;
}
/*
 * recursively update the viewports of all overlays and annotations of view
 */
static void
UpdateViewBB
(
	NgObjTreeNode	wknode,
	NgObjTreeNode	vnode
)
{
	NgWksObj	wks_obj;
	NgViewObj	view_obj;
        NhlBoundingBox  thebox;
	float		x,y,width,height;

	if (! wknode || ! vnode)
		return;

	if (vnode->type != _ngViewNode)
		return;

	wks_obj = (NgWksObj) wknode->ndata;
	view_obj = (NgViewObj) vnode->ndata;

	if (! wks_obj || ! view_obj)
		return;

	/* update the bounding box */

	NhlGetBB(vnode->id,&thebox);
	NgNDCToXCoord(wks_obj->wks_wrap_id,&view_obj->xbbox,
		      thebox.l,thebox.t,
		      thebox.r - thebox.l,thebox.t - thebox.b);

	/* update the viewport */

	NhlVAGetValues(vnode->id,
		       NhlNvpXF,&x,
		       NhlNvpYF,&y,
		       NhlNvpWidthF,&width,
		       NhlNvpHeightF,&height,
		       NhlNvpOn,&view_obj->visible,
		       NULL);

	NgNDCToXCoord(wks_obj->wks_wrap_id,&view_obj->xvp,
		      x,y,width,height);
	return;
}
static void
UpdateMemberViewBB
(
	NgObjTreeNode	wknode,
	NgObjTreeNode	vnode
)
{
	if (! vnode)
		return;

	UpdateMemberViewBB(wknode,vnode->cnodes);

	UpdateViewBB(wknode,vnode);

	UpdateMemberViewBB(wknode,vnode->next);

	return;
}

extern NhlErrorTypes NgUpdateViewBB
(
	NgWksState	wks_state,
	int		pid
	)
{
	char		func[]="NgUpdateViewBB";
	NgObjTree	otree = (NgWksState)wks_state;
	NgObjTreeNode	vnode,wknode;
	NhlLayer	l = _NhlGetLayer(pid);

	if (! wks_state) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			   "%s:invalid Workstation state tree",func));
		return NhlFATAL;
	}

	if (! l) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			   "%s:invalid view object id",func));
		return NhlFATAL;
	}
	/*
	 * bounding boxes only of interest if an XWorkstation
	 */
	if (! (l->base.wkptr && _NhlIsXWorkstation(l->base.wkptr)))
		return NhlNOERROR;

	
	vnode = FindNode(otree->wklist,pid);
	wknode = FindNode(otree->wklist,l->base.wkptr->base.id);
	/* 
	 * the nodes may not have been entered yet
	 */
	if (! (vnode && wknode))
		return NhlNOERROR;

	UpdateViewBB(wknode,vnode);
	UpdateMemberViewBB(wknode,vnode->cnodes);

	return NhlNOERROR;
}

extern NhlErrorTypes NgGetViewsInRegion(
	NgWksState	wks_state,
	int		wks_id,
	NhlBoolean	limit_to_vp,
	NgXBBox		*xbbox,
	int		**views,
	int		*view_count
)
{
	char		func[]="NgGetViewsInRegion";
	NgObjTree	otree = (NgWksState)wks_state;
	NgObjTreeNode	vwnode,wknode;
	int		lcount = 0;

#if DEBUG_MWIN
	fprintf(stderr,"%s\n",func);
	fprintf(stderr,"view count %d\n",*view_count);
#endif

	/* 
	 * This routine considers only top-level views, so it does
	 * not have to be recursive.
	 * view_count is input/output. on input it contains the
	 * current view allocation. On output it contains the
	 * view count (and a new allocation if necessary).
	 */
	
	wknode = FindNode(otree->wklist,wks_id);
	if (!wknode) {
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,
			   "%s: X workstation node not found",func));
		return NhlWARNING;
	}

	for (vwnode = wknode->cnodes; vwnode; vwnode = vwnode->next) {
		NgViewObj vobj = (NgViewObj) vwnode->ndata;
		NgXBBox   *bbox;
		if (! vobj->visible)
			continue;
		bbox = limit_to_vp ? &vobj->xvp : &vobj->xbbox;
#if DEBUG_MWIN

		fprintf(stderr,"checking intersection for plot id %d\n",
			vwnode->id);
		fprintf(stderr,"rubber box TL %d %d BR %d %d\n",
		       xbbox->p0.x,xbbox->p0.y,xbbox->p1.x,xbbox->p1.y);
		fprintf(stderr,"view box TL %d %d BR %d %d\n",
		       bbox->p0.x,bbox->p0.y,
		       bbox->p1.x,bbox->p1.y);
#endif

		/* 
		 * we will assume that both boxes have been normalized 
		 * such that p0.x <= p1.x and p0.y <= p1.y
		 */
		if (xbbox->p1.x < bbox->p0.x ||
		    xbbox->p0.x > bbox->p1.x ||
		    xbbox->p1.y < bbox->p0.y ||
		    xbbox->p0.y > bbox->p1.y)
			continue;
		
		if (lcount >= *view_count) {
			*views = NhlRealloc(*views,(lcount+1) * sizeof(int));
#if DEBUG_MWIN
			fprintf(stderr,"views allocated %d\n",lcount+1);
#endif
		}
		(*views)[lcount] = vwnode->id;
		lcount++;

#if DEBUG_MWIN
		fprintf(stderr,"plot id %d intersect\n",vwnode->id);
#endif
	}
	*view_count = lcount;

#if DEBUG_MWIN
	fprintf(stderr,"view count %d\n",*view_count);
#endif
	
	return NhlNOERROR;
}

extern int NgTopLevelViews(
	NgWksState	wks_state,
	int		wks_id,
	int		**views
)
{
	char		func[]="NgTopLevelViews";
	NgObjTree	otree = (NgWksState)wks_state;
	NgObjTreeNode	vwnode,wknode;
	int		lcount = 0;

#if DEBUG_MWIN
	fprintf(stderr,"%s\n",func);
#endif

	/* 
	 * This routine considers only top-level views, so it does
	 * not have to be recursive.
	 * views is assigned to allocated static internal storage, so if the 
	 * information needs to be preserved, it must be copied by
	 * the caller. It may be passed in as NULL if the caller is 
	 * only interested in the count. The ids in views are in drawing
         * order.
	 */
	
	wknode = FindNode(otree->wklist,wks_id);
	if (!wknode) {
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,
			   "%s: X workstation node not found",func));
		return NhlWARNING;
	}

	if (views) {
		for (vwnode = wknode->cnodes; vwnode; vwnode = vwnode->next) {
			NgViewObj vobj = (NgViewObj) vwnode->ndata;
		
			if (lcount >= Top_Level_View_Alloc_Count) {
				Top_Level_View_Alloc_Count++;
				Top_Level_Views = NhlRealloc
					(Top_Level_Views,
					 Top_Level_View_Alloc_Count * 
					 sizeof(int));
#if DEBUG_MWIN
				fprintf(stderr,
					"Top_Level_View_Alloc_Count %d\n",
					Top_Level_View_Alloc_Count);
#endif
			}
			(Top_Level_Views)[lcount] = vwnode->id;
			lcount++;
		}
		*views = Top_Level_Views;
	}
	else {
		for (vwnode = wknode->cnodes; vwnode; vwnode = vwnode->next)
			lcount++;
	}
		
#if DEBUG_MWIN
	fprintf(stderr,"view count %d\n",lcount);
#endif

	return lcount;
}

extern NgViewObj NgGetView
(
	NgWksState	wks_state,
	int		view_id
)
{
	NgObjTree	otree = (NgObjTree) wks_state;
	NgObjTreeNode	vwnode;

	if (! otree)
		return NULL;

	vwnode = FindNode(otree->wklist,view_id);
	if (! vwnode)
		return NULL;

	return (NgViewObj) vwnode->ndata;
}

extern void NgUpdateTransformation(
	NgWksState	wks_state
)
{
	char		func[]="NgUpdateTransformation";
	NgObjTree	otree = (NgWksState)wks_state;
	NgObjTreeNode	vwnode,wknode;

	for (wknode = otree->wklist; wknode; wknode = wknode->next) {
		for (vwnode = wknode->cnodes; vwnode; vwnode = vwnode->next) {
			UpdateViewBB(wknode,vwnode);
			UpdateMemberViewBB(wknode,vwnode);
		}
	}
}


extern void NgDrawUpdatedViews(
	NgWksState	wks_state
)
{
	char		func[]="NgDrawUpdatedViews";
	NgObjTree	otree = (NgWksState)wks_state;
	NgObjTreeNode	vwnode,wknode;
	NhlBoolean	draw_single;
	int		selected_id;

	for (wknode = otree->wklist; wknode; wknode = wknode->next) {
		NgWksObj wkobj = (NgWksObj) wknode->ndata;
		if (! wkobj->auto_refresh)
			continue;
		NhlVAGetValues(wkobj->wks_wrap_id,
			       NgNxwkSelectedView,&selected_id,
			       NgNxwkDrawSelectedViewOnly,&draw_single,
			       NULL);
		for (vwnode = wknode->cnodes; vwnode; vwnode = vwnode->next) {
			NgHluData hdata;
			NhlLayer l = _NhlGetLayer(vwnode->id);
			if (! l)
				continue;
			if (draw_single && vwnode->id != selected_id)
				continue;
			hdata = (NgHluData) l->base.gui_data2;
			if (! hdata)
				continue;
			if (hdata->draw_req)
				NgDrawView
					(otree->go->base.id,vwnode->id,True);
		}
	}
}

static void RowMoveAction
(
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
)

{
        int		x,y;
        unsigned char	rowtype, coltype;
        int		row,col;
        XmLGridRow	rowptr;
        XmLGridColumn	colptr;
	NgObjTree	otree;
	NgObjTreeNode	node; 
	Pixmap		pixmap,pixmap_mask;
	NgGO		go;

#if DEBUG_MWIN
	fprintf(stderr,"in row move action\n");
#endif
        x = xev->xbutton.x;
        y = xev->xbutton.y;
        
        if (XmLGridXYToRowColumn(w,x,y,&rowtype,&row,&coltype,&col) < 0)
                return;

        rowptr = XmLGridGetRow(w,XmCONTENT,row);
        colptr = XmLGridGetColumn(w,XmCONTENT,col);

        XtVaGetValues(w,
                      XmNrowPtr,rowptr,
                      XmNuserData,&otree,
                      NULL);

#if DEBUG_MWIN
	fprintf(stderr,"top row is %d\n",otree->wklist->pos);
#endif
	node = FindRowNode(otree->wklist,row);

	if (! node)
		return;
#if DEBUG_MWIN
	fprintf(stderr,"found row %d with id %d\n",node->pos,node->id);
#endif
	
	if (node->type != _ngViewNode)
		return;

	XtVaGetValues(w,
		      XmNrowPtr,rowptr,
		      XmNcolumnPtr,colptr,
		      XmNcellPixmap,&pixmap,
		      XmNcellPixmapMask,&pixmap_mask,
		      NULL);
#if 0		
        XDefineCursor(otree->go->go.x->dpy,quantize_pixmaps
                      XtWindow(otree->go->go.manager),pixmap);
#endif
	return;
}	


static void RowSetPosAction
(
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
)
{
        int		x,y;
        unsigned char	rowtype, coltype;
        int		row,col;
        XmLGridRow	rowptr;
        XmLGridColumn	colptr;
	NgObjTree	otree;
	NgObjTreeNode	node; 
	Pixmap		pixmap,pixmap_mask;
	NgGO		go;

#if DEBUG_MWIN
	fprintf(stderr,"in row set pos action\n");
#endif
        x = xev->xbutton.x;
        y = xev->xbutton.y;
        
        if (XmLGridXYToRowColumn(w,x,y,&rowtype,&row,&coltype,&col) < 0)
                return;

        rowptr = XmLGridGetRow(w,XmCONTENT,row);
        colptr = XmLGridGetColumn(w,XmCONTENT,col);

        XtVaGetValues(w,
                      XmNrowPtr,rowptr,
                      XmNuserData,&otree,
                      NULL);

        XUndefineCursor(otree->go->go.x->dpy,XtWindow(otree->go->go.manager));

	return;
}
