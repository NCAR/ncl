/*
 *      $Id: mwin.c,v 1.10 1997-09-17 16:41:09 boote Exp $
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
#include <ncarg/hlu/View.h>

#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/CascadeBG.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/PanedW.h>
#include <Xm/Form.h>

#include <Xm/MenuShell.h>
#include <Xcb/xcbShells.h>

#include <XmL/Tree.h>

#define	Oset(field)	NhlOffset(NgMWinRec,mwin.field)
static NhlResource resources[] = {
	{"no.res","no.res",NhlTString,sizeof(NhlString),
		Oset(foo),NhlTImmediate,_NhlUSET((NhlPointer)NULL),
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
/* create_win_hook	*/	NULL,
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

typedef struct NgObjTreeNodeRec NgObjTreeNodeRec, *NgObjTreeNode;
typedef struct NgObjTreeRec NgObjTreeRec, *NgObjTree;

struct NgObjTreeNodeRec{
	NgObjTree		tree;
	NgObjTreeNode		wk_node;
	NgObjTreeNode		next;

	int			pos;

	NgObjTreeNode		children;
	int			nchildren;

	NgCBWP			cccb;

	NhlLayer		l;
	int			id;
	int			parent_id;
	Const char		*name;
	XmString		xmname;
	NhlClass		cp;

	Const char		*vname;
};

struct NgObjTreeRec{
	int		nsid;
	int		appmgr;

	Widget		wtree;

	NgObjTreeNode	wklist;

	_NhlCB		create;
	_NhlCB		destroy;
	_NhlCB		dncl;
};

static
NgObjTreeNode
AllocNode(
	NgNclHluObj	hlu
)
{
	char		func[]="AllocNode";
	NgObjTreeNode	new = NhlMalloc(sizeof(NgObjTreeNodeRec));

	if(!new){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NULL;
	}

	new->tree = NULL;
	new->wk_node = NULL;
	new->next = NULL;

	new->pos = 0;

	new->children = NULL;
	new->nchildren = 0;

	new->cccb = NULL;

	new->l = _NhlGetLayer(hlu->id);
	if(!new->l){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:Invalid obj id %d",
								func,hlu->id));
		NhlFree(new);
		return NULL;
	}
	new->id = hlu->id;
	new->name = hlu->name;
	new->parent_id = hlu->parent_id;
	new->cp = hlu->class_ptr;

	new->xmname = NULL;
	new->vname = NULL;

	return new;
}

static void
GetWkObjs
(
	NgNclObj	obj,
	NhlPointer	udata
)
{
	NgNclHluObj	hlu = (NgNclHluObj)obj;
	NgObjTree	otree = (NgObjTree)udata;
	NgObjTreeNode	new;

	if(!NhlClassIsSubclass(hlu->class_ptr,NhlworkstationClass))
		return;

	new = AllocNode(hlu);
	if(!new){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return;
	}

	new->tree = otree;

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
	NhlLayer	l;


	/*
	 * Only interested in view class objects.
	 */
	if(NhlClassIsSubclass(hlu->class_ptr,NhlviewClass))
		return;

	/*
	 * In fact, only interested in view class objects that are
	 * immediate children of a workstation class object.
	 */
	l = _NhlGetLayer(hlu->parent_id);
	if(!l || !_NhlIsClass(l,NhlworkstationClass))
		return;

	new = AllocNode(hlu);
	if(!new){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return;
	}

	new->tree = otree;

	work = otree->wklist;
	while(work && work->id != new->parent_id){
		work = work->next;
	}
	if(!work){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
					"%s:View child can't find mommy",func));
		return;
	}

	new->wk_node = work;
	work->children++;

	/*
	 * TODO - define the order of the children.
	 */
	new->next = NULL;
	tmp = &work->children;
	i=1;
	while(*tmp){
		i++;
		tmp = &((*tmp)->next);
	}

	new->pos = i;
	*tmp = new;

	return;
}

static void
NumberNodes
(
	NgObjTreeNode	wk,
	int		previous
)
{
	if(!wk)
		return;

	wk->pos = previous + 1;
	NumberNodes(wk->next,wk->pos + wk->nchildren);
	return;
}

static void
AddVwNode
(
	NgObjTree	otree,
	Widget		tree,
	NgObjTreeNode	vwnode
)
{
	if(!vwnode)
		return;

	vwnode->xmname = NgXAppCreateXmString(otree->appmgr,
							(char*)vwnode->name);
	XmLTreeAddRow(tree,1,False,False,-1,
		XmUNSPECIFIED_PIXMAP,XmUNSPECIFIED_PIXMAP,vwnode->xmname);

	AddVwNode(otree,tree,vwnode->next);

	return;
}

static NhlBoolean
CopyCC
(
	NhlArgVal	cbdata,
	NhlArgVal	*ret
)
{
	_NhlobjChangeChild	cc = (_NhlobjChangeChild)cbdata.ptrval;
	_NhlobjChangeChild	new;

	if(cc->reason != _NhlobjCCMove)
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
	NgObjTreeNode		*tmp,work,child;
	int			or,nr;

	/*
	 * Reason was checked in the copy func, so I know this is a "move".
	 *
	 * Only deal with the move in the "old" workstation.
	 */
	if(wknode->id != cc->old)
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
	tmp = &wknode->children;
	or=wknode->pos+1;
	while(*tmp && (*tmp)->id != cc->child){
		or++;
		tmp = &(*tmp)->next;
	}

	if(!*tmp){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:Unable to find child node",
								func));
		return;
	}
	child = *tmp;

	/*
	 * Remove the child from the old work
	 */
	*tmp = child->next;
	child->next = NULL;
	wknode->nchildren--;

	/*
	 * Add child to new work
	 * TODO: Add child to correct position - right now it is just to the
	 *	end.
	 */
	tmp = &work->children;
	while(*tmp)
		tmp = &(*tmp)->next;
	*tmp = child;
	work->nchildren++;
	nr = work->pos + work->nchildren;

	/*
	 * Move the row in the widget.
	 * (if nr > or, then the old row will be removed before the new
	 * row inserted, so the index should be one less.
	 */
	if(nr>or)
		nr--;
	XmLGridMoveRows(work->tree->wtree,nr,or,1);

	/*
	 * Renumber the nodes starting from which-ever workstation is
	 * earlier in the tree.
	 */
	child = (work->pos < wknode->pos)? work: wknode;
	NumberNodes(child->next,child->pos + child->nchildren);

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

	if(!wknode)
		return;

	NhlINITVAR(sel);
	NhlINITVAR(udata);
	sel.lngval = 0;	/* not used */
	udata.ptrval = wknode;
	wknode->cccb = NgCBWPAdd(otree->appmgr,CopyCC,FreeCC,wknode->l,
				_NhlCBobjChildChange,sel,WkChildChangeCB,udata);
	if(!wknode->cccb){
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,
				"%s:Unable to track ChangeWorkstation!",func));
	}

	wknode->xmname = NgXAppCreateXmString(otree->appmgr,
							(char*)wknode->name);
	XmLTreeAddRow(tree,0,True,False,-1,
		XmUNSPECIFIED_PIXMAP,XmUNSPECIFIED_PIXMAP,wknode->xmname);

	AddVwNode(otree,tree,wknode->children);
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
	NgObjTreeNode	new = AllocNode(hlu);
	NgObjTreeNode	tmp,last;
	NhlLayer	l=_NhlGetLayer(hlu->id);
	int		level=0,pos=-1,i;
	Boolean		expn = True;
	NhlArgVal	sel,ludata;

	if(!new){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return;
	}

	if(l && _NhlIsClass(l,NhlworkstationClass)){
		if(otree->wklist){
			for(last=NULL,tmp=otree->wklist;tmp;tmp=tmp->next){
				if(tmp->id == new->id)
					goto BADOBJ;
				last = tmp;
			}
			if(!last)
				goto BADOBJ;
			last->next = new;
			NumberNodes(new,last->pos + last->nchildren);
		}
		else{
			otree->wklist = new;
		}

		NhlINITVAR(sel);
		NhlINITVAR(ludata);
		sel.lngval = 0;	/* not used */
		ludata.ptrval = new;
		new->cccb = NgCBWPAdd(otree->appmgr,CopyCC,FreeCC,new->l,
			_NhlCBobjChildChange,sel,WkChildChangeCB,ludata);
		if(!new->cccb){
			NHLPERROR((NhlWARNING,NhlEUNKNOWN,
				"%s:Unable to track ChangeWorkstation!",func));
		}
	}
	else if(l && _NhlIsClass(l,NhlviewClass) &&
		_NhlIsClass(_NhlGetLayer(new->parent_id),NhlworkstationClass)){
		NgObjTreeNode	*tptr;

		for(tmp=otree->wklist;tmp;tmp=tmp->next)
			if(tmp->id == new->parent_id)
				break;
		if(!tmp)
			goto BADOBJ;
		/*
		 * TODO: Add view in correct Draw Order...
		 */
		tptr = &tmp->children;
		i=1;
		while(*tptr){
			i++;
			tptr = &((*tptr)->next);
		}
		new->pos = i;
		*tptr = new;
		tmp->nchildren++;
		NumberNodes(tmp->next,tmp->pos + tmp->nchildren);
		pos = i + tmp->pos;
		expn = False;
		level = 1;
	}
	else{
BADOBJ:
		NhlFree(new);
		return;
	}

	new->tree = otree;
	new->xmname = NgXAppCreateXmString(otree->appmgr,(char*)new->name);
	XmLTreeAddRow(otree->wtree,level,expn,False,pos,
		XmUNSPECIFIED_PIXMAP,XmUNSPECIFIED_PIXMAP,new->xmname);

	return;
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
	FreeNodeList(otree,node->children);
	NgCBWPDestroy(node->cccb);
	NgXAppFreeXmString(otree->appmgr,node->xmname);
	NhlFree(node);

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

	if(NhlClassIsSubclass(hlu->class_ptr,NhlworkstationClass)){
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
							work->nchildren+1);
		FreeNodeList(otree,work);
	}
	else if(NhlClassIsSubclass(hlu->class_ptr,NhlviewClass) &&
		_NhlIsClass(_NhlGetLayer(hlu->parent_id),NhlworkstationClass)){

		for(work=otree->wklist;work;work=work->next)
			if(work->id == hlu->parent_id)
				break;
		if(!work)
			return;
		for(tptr=&work->children;*tptr;tptr=&((*tptr)->next))
			if((*tptr)->id == hlu->id)
				break;
		if(!*tptr)
			return;
		tmp = *tptr;
		*tptr=tmp->next;
		work->nchildren--;
		NumberNodes(work->next,work->pos+work->nchildren);
		tmp->next = NULL;
		XmLGridDeleteRows(otree->wtree,XmCONTENT,work->pos+tmp->pos,1);
		FreeNodeList(otree,tmp);
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
	CleanUp((NgObjTree)udata.ptrval);
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

static void
ManageObjTree
(
	Widget	tree,
	int	nsid
)
{
	char		func[] = "ManageObjTree";
	NhlLayer	ncl = _NhlGetLayer(nsid);
	NgObjTree	otree;
	NhlArgVal	sel,udata;

	if(!ncl || !_NhlIsClass(ncl,NgnclStateClass)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:Invalid nclstate id",func));
		return;
	}

	otree = NhlMalloc(sizeof(NgObjTreeRec));
	if(!otree){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return;
	}

	otree->nsid = nsid;
	NhlVAGetValues(nsid,
		_NhlNguiData,	&otree->appmgr,
		NULL);
	if(!NhlIsClass(otree->appmgr,NgappMgrClass)){
		NhlFree(otree);
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:Invalid appmgr",func));
		return;
	}

	otree->wtree = tree;

	otree->wklist = NULL;
	NgNclEnumerateObj(nsid,NgNclHLUOBJ,GetWkObjs,otree);
	NgNclEnumerateObj(nsid,NgNclHLUOBJ,GetVwObjs,otree);

	NumberNodes(otree->wklist,-1);
	InitWTree(otree,otree->wtree,otree->wklist);

	NhlINITVAR(sel);
	NhlINITVAR(udata);
	sel.lngval = NgNclCBCREATE_HLUOBJ;
	udata.ptrval = otree;
	otree->create = _NhlAddObjCallback(ncl,NgCBnsObject,sel,CreateCB,udata);

	sel.lngval = NgNclCBDELETE_HLUOBJ;
	otree->destroy = _NhlAddObjCallback(ncl,NgCBnsObject,sel,DestroyCB,
									udata);

	sel.lngval = 0;
	otree->dncl = _NhlAddObjCallback(ncl,_NhlCBobjDestroy,sel,CleanUpCB,
									udata);

	XtVaSetValues(otree->wtree,
		XmNuserData,	otree,
		NULL);

	XtAddCallback(otree->wtree,XmNdestroyCallback,CleanUpXCB,otree);

	return;
}

static void
CreateXWorkCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
)
{
	char	func[]="CreateXWorkCB";
	int	goid;
	int	appmgr;
	int	nclstate = NhlDEFAULT_APP;
	char	*name;
	char	line[512];

	goid = NgGOWidgetToGoId(w);
	if(goid == NhlDEFAULT_APP){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:invalid Widget",func));
		return;
	}
	NhlVAGetValues(goid,
		_NhlNguiData,	&appmgr,
		NULL);
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

	objtree = XtVaCreateManagedWidget("otree",xmlTreeWidgetClass,pform,
		XmNtopAttachment,	XmATTACH_WIDGET,
		XmNtopWidget,		ptbform,
		NULL);

	NhlVAGetValues(go->go.appmgr,
		NgNappNclState,	&nsid,
		NULL);

	ManageObjTree(objtree,nsid);

	cwki = XtVaCreateManagedWidget("cwki",xmPushButtonWidgetClass,ptbform,
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
