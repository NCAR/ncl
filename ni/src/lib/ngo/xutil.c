/*
 *      $Id: xutil.c,v 1.4 1997-02-27 20:25:46 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		xutil.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Oct 8 16:43:00 MDT 1996
 *
 *	Description:	
 */
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/BaseI.h>
#include <ncarg/ngo/xutil.h>
#include <ncarg/ngo/xapp.h>
#include <ncarg/ngo/util.h>

#include <Xm/FileSB.h>
#include <Xm/List.h>

typedef struct NgXListRec NgXListRec, *NgXList;
struct NgXListRec{
	int		nsid;
	int		appmgr;

	Widget		xmlist;
	NgNclCBType	ltype;
	NgXListTest	inclusion;
	NhlPointer	closure;

	NgXListNode	list;
	_NhlCB		create;
	_NhlCB		destroy;
	_NhlCB		dncl;
};

static NgXListNode
AllocNode
(
	NgNclObj	node
)
{
	NgNclAny	any = (NgNclAny)node;
	NgXListNode	lnode = NhlMalloc(sizeof(NgXListNodeRec));

	if(!lnode){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return lnode;
	}

	lnode->id = any->id;
	lnode->name = any->name;
	/*
	 * Fill in IFF added to list.
	 */
	lnode->xmname = NULL;
	lnode->left = NULL;
	lnode->right = NULL;

	return lnode;
}

static NgXListNode
InsertNode
(
	NgXListNode	*tree,
	NgXListNode	node
)
{
	if(!node)
		return NULL;

	if(!*tree)
		return *tree = node;

	if((*tree)->id == node->id)
		return NULL;

	if(strcmp(node->name,(*tree)->name) < 0)
		return InsertNode(&(*tree)->left,node);
	else
		return InsertNode(&(*tree)->right,node);
}

static NgXListNode
RemoveNode
(
	NgXListNode	*tree,
	NgNclObj	node
)
{
	NgNclAny	any = (NgNclAny)node;
	NgXListNode	tmp;

	if(!*tree)
		return NULL;

	if((*tree)->id == any->id){
		tmp = *tree;
		*tree = tmp->left;
		(void)InsertNode(tree,tmp->right);
		tmp->left = NULL;
		tmp->right = NULL;
		return tmp;
	}

	if(strcmp(any->name,(*tree)->name) < 0)
		return RemoveNode(&(*tree)->left,node);
	else
		return RemoveNode(&(*tree)->right,node);
}

/*
 * The position of any given node in the list is the position of the
 * node above, plus the number of nodes to the left plus 1 for self.
 */
static int
NumberNodes
(
	NgXListNode	node,
	int		above
)
{
	if(!node)
		return above;

	node->pos = NumberNodes(node->left,above) + 1;

	return NumberNodes(node->right,node->pos);
}

static void
CreateCB
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	NgNclObj	node = (NgNclObj)cbdata.ptrval;
	NgNclAny	any = (NgNclAny)node;
	NgXList		xlist = (NgXList)udata.ptrval;
	NgXListNode	tmp,new;

	if(!(*xlist->inclusion)(node,xlist->closure))
		return;

	tmp = AllocNode(node);
	if(!tmp){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return;
	}

	new = InsertNode(&xlist->list,tmp);
	if(!new){
		NhlFree(tmp);
		return;
	}

	new->xmname = NgXAppCreateXmString(xlist->appmgr,(char*)any->name);
	(void)NumberNodes(xlist->list,0);
	XmListAddItemUnselected(xlist->xmlist,new->xmname,new->pos);

	return;
}

static void
DestroyCB
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	NgNclObj	node = (NgNclObj)cbdata.ptrval;
	NgXList		xlist = (NgXList)udata.ptrval;
	NgXListNode	tmp;

	tmp = RemoveNode(&xlist->list,node);
	if(!tmp)
		return;

	XmListDeletePos(xlist->xmlist,tmp->pos);
	(void)NumberNodes(xlist->list,0);

	NgXAppFreeXmString(xlist->appmgr,tmp->xmname);
	NhlFree(tmp);

	return;
}

static NhlBoolean
TrueFunc
(
	NgNclObj	node,
	NhlPointer	udata
)
{
	return True;
}

static void
GetObjs
(
	NgNclObj	obj,
	NhlPointer	udata
)
{
	NgNclAny	any = (NgNclAny)obj;
	NgXList		list = (NgXList)udata;
	NgXListNode	tmp,new;

	if(!(*list->inclusion)(obj,list->closure))
		return;

	tmp = AllocNode(obj);
	if(!tmp){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return;
	}

	new = InsertNode(&list->list,tmp);
	if(!new){
		NhlFree(tmp);
		return;
	}

	new->xmname = NgXAppCreateXmString(list->appmgr,(char*)any->name);

	return;
}

static void
InitXmList
(
	Widget		xmlist,
	NgXListNode	node
)
{
	if(!node)
		return;

	InitXmList(xmlist,node->left);
	XmListAddItemUnselected(xmlist,node->xmname,0);
	InitXmList(xmlist,node->right);

	return;
}

static void
FreeList
(
	NgXList		list,
	NgXListNode	node
)
{
	if(!node)
		return;

	FreeList(list,node->left);
	FreeList(list,node->right);
	NgXAppFreeXmString(list->appmgr,node->xmname);
	NhlFree(node);
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
	NgXList	list
)
{
	_NhlCBDelete(list->create);
	_NhlCBDelete(list->destroy);
	_NhlCBDelete(list->dncl);
	XtRemoveCallback(list->xmlist,XmNdestroyCallback,CleanUpXCB,list);
	FreeList(list,list->list);
	NhlFree(list);

	return;
}

static void
CleanUpCB
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	CleanUp((NgXList)udata.ptrval);
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
	CleanUp((NgXList)udata);
	return;
}

NhlBoolean
NgXListManage
(
	int		nsid,
	Widget		xmlist,
	NgNclCBType	ltype,
	NgXListTest	tfunc,
	NhlPointer	closure
)
{
	char		func[]="NgXListManage";
	NgXList		list;
	NgNclCBType	ccb,dcb;
	NhlLayer	ncl = _NhlGetLayer(nsid);
	NhlArgVal	sel,udata;

	if(!ncl || !_NhlIsClass(ncl,NgnclStateClass)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:Invalid nclstate id",func));
		return False;
	}

	switch(ltype){
		case	NgNclHLUOBJ:
			ccb = NgNclCBCREATE_HLUOBJ;
			dcb = NgNclCBDELETE_HLUOBJ;
			break;
		case	NgNclHLUVAR:
			ccb = NgNclCBCREATE_HLUVAR;
			dcb = NgNclCBDELETE_HLUVAR;
			break;
		case	NgNclVAR:
			ccb = NgNclCBCREATE_VAR;
			dcb = NgNclCBDELETE_VAR;
			break;
		case	NgNclFILEVAR:
			ccb = NgNclCBCREATE_FILEVAR;
			dcb = NgNclCBDELETE_FILEVAR;
			break;
		case	NgNclFUNC:
			ccb = NgNclCBCREATE_FUNC;
			dcb = NgNclCBUNKNOWN;
			break;
		default:
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:Unsupported ltype",
									func));
			return False;
	}

	list = NhlMalloc(sizeof(NgXListRec));
	if(!list){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return False;
	}

	list->nsid = nsid;
	NhlVAGetValues(nsid,
		_NhlNguiData,	&list->appmgr,
		NULL);

	if(!NhlIsClass(list->appmgr,NgappMgrClass)){
		NhlFree(list);
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:Invalid appmgr",func));
		return False;
	}

	list->xmlist = xmlist;
	list->ltype = ltype;
	list->inclusion = (tfunc)?tfunc:TrueFunc;
	list->closure = closure;
	list->list = NULL;

	NgNclEnumerateObj(nsid,ltype,GetObjs,list);
	NumberNodes(list->list,0);
	InitXmList(list->xmlist,list->list);

	NhlINITVAR(sel);
	NhlINITVAR(udata);
	sel.lngval = ccb;
	udata.ptrval = list;
	list->create = _NhlAddObjCallback(ncl,NgCBnsObject,sel,CreateCB,udata);
	if(dcb == NgNclCBUNKNOWN){
		list->destroy = NULL;
	}
	else{
		sel.lngval = dcb;
		list->destroy =_NhlAddObjCallback(ncl,NgCBnsObject,sel,
							DestroyCB,udata);
	}
	sel.lngval = 0;
	list->dncl = _NhlAddObjCallback(ncl,_NhlCBobjDestroy,sel,CleanUpCB,
									udata);

	XtVaSetValues(list->xmlist,
		XmNuserData,	list,
		NULL);
	XtAddCallback(list->xmlist,XmNdestroyCallback,CleanUpXCB,list);

	return;
}

void
NgXFileSearchProc
(
	Widget		w,
	XtPointer	cbdata
)
{
	XmFileSelectionBoxCallbackStruct	*cbs =
				(XmFileSelectionBoxCallbackStruct*)cbdata;
	char		*mask;
	nglob_t		nglob_buf;
	XmString	*flist = NULL;
	unsigned char	ftype;
	int		i,j=0;
	struct stat	stat_buff;
	
	if(!XmStringGetLtoR(cbs->mask,XmFONTLIST_DEFAULT_TAG,&mask)){
		XtVaSetValues(w,
			XmNlistUpdated,	True,
			NULL);
		return;
	}

	if(Ngglob(mask,
		NgGLOB_BRACE|NgGLOB_NOMAGIC|NgGLOB_QUOTE|NgGLOB_TILDE,
		NULL,&nglob_buf)){
		XtFree(mask);
		Ngglobfree(&nglob_buf);
		XtVaSetValues(w,
			XmNlistUpdated,	True,
			NULL);
		return;
	}

	if(nglob_buf.ngl_pathc > 0){
		flist = (XmString*)XtMalloc(sizeof(XmString) *
							nglob_buf.ngl_pathc);
		if(!flist){
			XtFree(mask);
			Ngglobfree(&nglob_buf);
			XtVaSetValues(w,
				XmNlistUpdated,	True,
				NULL);
			return;
		}
	
		XtVaGetValues(w,
			XmNfileTypeMask,	&ftype,
			NULL);
	
		switch(ftype){
			case XmFILE_ANY_TYPE:
				for(i=0;i<nglob_buf.ngl_pathc;i++)
					flist[i] =
					XmStringCreate(nglob_buf.ngl_pathv[i],
							XmFONTLIST_DEFAULT_TAG);
				break;
			case XmFILE_REGULAR:
				for(i=0;i<nglob_buf.ngl_pathc;i++){
					if(stat(nglob_buf.ngl_pathv[i],
								&stat_buff))
						continue;
					if(S_ISREG(stat_buff.st_mode))
						flist[j++] = XmStringCreate(nglob_buf.ngl_pathv[i],XmFONTLIST_DEFAULT_TAG);
				}
				break;
			case XmFILE_DIRECTORY:
				for(i=0;i<nglob_buf.ngl_pathc;i++){
					if(stat(nglob_buf.ngl_pathv[i],
								&stat_buff))
						continue;
					if(S_ISDIR(stat_buff.st_mode))
						flist[j++] = XmStringCreate(nglob_buf.ngl_pathv[i],XmFONTLIST_DEFAULT_TAG);
				}
				break;
		}
	}

	XtVaSetValues(w,
		XmNdirSpec,		cbs->dir,
		XmNlistUpdated,		True,
		XmNfileListItems,	flist,
		XmNfileListItemCount,	j,
		NULL);
	XtFree(mask);
	Ngglobfree(&nglob_buf);
	for(i=0;i<j;i++)
		XmStringFree(flist[i]);
	XtFree((XtPointer)flist);

	return;
}
