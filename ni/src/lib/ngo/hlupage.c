/*
 *      $Id: hlupage.c,v 1.24 1999-06-02 03:40:07 dbrown Exp $
 */
/*******************************************x*****************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		hlupage.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Jun  9 21:02:27 MDT 1997
 *
 *	Description:	
 */

#include <ncarg/ngo/hlupageP.h>
#include <ncarg/ngo/nclstate.h>
#include <ncarg/ngo/graphic.h>
#include <ncarg/ngo/xinteract.h>
#include <ncarg/ngo/stringutil.h>
#include <ncarg/ngo/Grid.h>

#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/PushBG.h>
#include <Xm/ToggleBG.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/XWorkstation.h>
#include <ncarg/hlu/DataComm.h>
#include <ncarg/hlu/CoordArrays.h>
#include <ncarg/hlu/ScalarField.h>
#include <ncarg/hlu/VectorField.h>


static NrmQuark QFillValue = NrmNULLQUARK;
static NrmQuark QString = NrmNULLQUARK;
static NrmQuark QGenArray = NrmNULLQUARK;
static NrmQuark QInteger = NrmNULLQUARK;

typedef struct _ResData {
	int  res_count;
	char *res[16];
	NhlPointer values[16];
	NrmQuark types[16];
	NgVarData vdata[16];
} ResData;

typedef struct _PlotStyleAppRec {
	struct _PlotStyleAppRec *next;
	NrmQuark  		qname;
	int			app_id;
	NhlBoolean		preview;
	int			ref_count;
} PlotStyleAppRec, *PlotStyleApp;

PlotStyleApp PlotStyleAppList = NULL;

static void FreeDataLinks(
	XmLArray datalinks
	)
{
	int i,count;
	NgPageReply reply;

	if (! datalinks)
		return;
	count = XmLArrayGetCount(datalinks);
	for (i = 0; i < count; i++) {
		reply = (NgPageReply) XmLArrayGet(datalinks,i);
		NhlFree(reply);
	}
	XmLArrayFree(datalinks);
}

static void HluPageFocusNotify (
        brPage *page,
        NhlBoolean in
        )
{
        brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec	*)pdp->type_rec;
        
        if (in && rec->res_tree)
                NgRestoreResTreeOverlays(rec->res_tree);
        return;
}
#if 0
static void
SetValCB
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	brSetValCBInfo   *info = (brSetValCBInfo *)udata.lngval;
	_NhlValueSetCBData vsdata = (_NhlValueSetCBData) cbdata.ptrval;
	brHluPageRec	*rec;

#if DEBUG_HLUPAGE
        fprintf(stderr,"in setval cb\n");
#endif
	rec = (brHluPageRec *)NgPageData(info->goid,info->pid);
        if (! rec || rec->hlu_id <= NhlNULLOBJID)
                return;
/*
 * The setvalue callback must be blocked for setvalues calls performed from
 * the hlupage itself.
 */
#if 0
        if (rec->do_setval_cb)
                NgResTreeResUpdateComplete(rec->res_tree,rec->hlu_id,False);
#endif

        return;
}
#endif

static NhlBoolean UpdateDataLink
(
	brHluPageRec		*rec,
	NhlBoolean		on,
	NgPageMessageType	message_type,
	NgPageId		page_id,
	NrmQuark		qfile,
	NrmQuark		qvar
)

{
	int		i,count,pos;
	NgPageReply 	link;
	NhlBoolean	new = False;

	if (! rec->datalinks) {
		rec->datalinks = XmLArrayNew(0,0);
		if (! rec->datalinks) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return False;
		}
	}

	pos = count = XmLArrayGetCount(rec->datalinks);

	for (i = 0; i < count; i++) {
		link = XmLArrayGet(rec->datalinks,i);
		if (link->id == page_id &&
		    link->req == message_type) {
			pos = i;
			break;
		}
	}
	if (pos == count) {
		if (! on) 
			return True;
		pos = 0;
		link = NhlMalloc(sizeof(NgPageReplyRec));
		if (! link) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return False;
		}
		new = True;
	}
	else if (! on) {
		XmLArrayDel(rec->datalinks,pos,1);
		NhlFree(link);
		return True;
	}

	link->req = message_type;
	link->id = page_id;
	link->qfile = qfile;
	link->qvar = qvar;

	if (new) {
		XmLArrayAdd(rec->datalinks,0,1);
		XmLArraySet(rec->datalinks,0,link);
	}

	return True;
}

static void PostDataProfileMessage
(
        brPage		*page,
	NrmQuark	qname
)
{
	brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec *) pdp->type_rec;

	NgPostPageMessage(page->go->base.id,page->id,_NgDATAPROFILE,
			  _brHLUVAR,NrmNULLQUARK,qname,
			  _NgDATAPROFILE,rec->data_profile,True,NULL,True);
	return;

}

static void TalkToDataLinks
(
	brPage			*from_page,
	NgPageMessageType	link_type,
	int			skip_id
)
{
        brPageData	*pdp = from_page->pdata;
	brHluPageRec	*rec = (brHluPageRec	*)pdp->type_rec;
	NgPageReply 	link;
	int		i, count;
	brPage 		*skip_page = NULL;

	if (! rec->datalinks)
		return;

	count = XmLArrayGetCount(rec->datalinks);


	for (i = 0; i < count; i++) {
		link = (NgPageReply) XmLArrayGet(rec->datalinks,i);
		if (link->id == 0 && skip_id > 0) {
			skip_page = _NgGetPageRef(rec->go->base.id,skip_id);
			if (skip_page && skip_page->qvar == link->qvar)
				link->id = skip_id;
		}
		if (link->id == skip_id)
			continue;
		switch (link->req) {
		case _NgDATAPROFILE:
			if (link_type == _NgDATAPROFILE) {
				PostDataProfileMessage(from_page,link->qvar);
			}
			else if (link_type == _NgDOSETVALCB) {
				NgPostPageMessage
					(rec->go->base.id,from_page->id,
				 _NgNOMESSAGE,
				 link->qfile ? _brFILEVAR : _brREGVAR,
				 link->qfile,link->qvar,_NgDOSETVALCB,
				 (NhlPointer)rec->do_setval_cb,
				 True,NULL,True);
			}
			break;
		case _NgVARDATALINK_REQ:
			if (link_type != _NgVARDATALINK_REQ)
				continue;
			NgPostPageMessage
				(rec->go->base.id,from_page->id,
				 _NgVARDATA,
				 link->qfile ? _brFILEVAR : _brREGVAR,
				 link->qfile,link->qvar,_NgVARDATALINK_REQ,
				 (NhlPointer)&rec->data_profile->linked,
				 True,NULL,True);
			break;
		default:
			break;
		}
	}

	return;
}

static NhlErrorTypes GetHluObjCreateMessage
(
        brPage		*page,
	NgPageMessage   message,
	NhlBoolean	reset
        )
{
        brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec	*)pdp->type_rec;
        NgHluPage 	*pub = &rec->public;
	brHluObjCreate	obj_create;
	NhlBoolean	do_link = False;
	NgPageMessageType reply_req = _NgNOMESSAGE;
	NhlErrorTypes 	ret = NhlNOERROR;

	obj_create = (brHluObjCreate) message->message;
	pub->class_name = obj_create->class_name;
	pub->plot_style = obj_create->plot_style;
	pub->plot_style_dir = obj_create->plot_style_dir;
	rec->hlu_id = obj_create->obj_id;
	rec->has_input_data = obj_create->has_input_data;
	rec->state = obj_create->state;
	if (pub->data_profile) {
		NgFreeDataProfile(pub->data_profile);
		pub->data_profile = NULL;
	}
	/* 
	 * The DataProfile will have a free func defined only if it is a
	 * copy that can be freed.
	 */
	if (obj_create->dprof) {
		if (obj_create->dprof->free) { /* dprof is a copy */
			pub->data_profile = obj_create->dprof;
			obj_create->dprof = NULL; 
		}
		else 
			pub->data_profile = 
				NgCopyDataProfile(obj_create->dprof);
	}	

	/* A free func is provided only if the message data is a copy and
	   therefore is owned by the message itself */

	if (message->message_free) {
		(*message->message_free)(obj_create);
	}

	/* send reply if appropriate */
	switch (message->reply_req) {
	case _NgNOMESSAGE:
		break;
	case _NgDATAPROFILELINK_REQ:
		reply_req = _NgDATAPROFILE;
		do_link = True;
		break;
	case _NgVARDATALINK_REQ:
		reply_req = _NgVARDATA;
		do_link = True;
		break;
	default:
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,
			   "invalid reply message request"));
		ret = NhlWARNING;
	}
	if (do_link) {
		brPage *frpage = 
			_NgGetPageRef(rec->go->base.id,message->from_id);

		if (frpage) {
			brDataLinkReqRec data_link_req;

			data_link_req.on = True;
			data_link_req.link_ditem = 
				pub->data_profile->master_data_ix;
			NgPostPageMessage
				(page->go->base.id,page->id,
				 _NgNOMESSAGE,_brHLUVAR,
				 frpage->qfile,frpage->qvar,
				 message->reply_req,
				 (NhlPointer)&data_link_req,True,
				 NULL,True);
			if (! UpdateDataLink
			    (rec,True,reply_req,
			     frpage->id,frpage->qfile,frpage->qvar)) {
				return NhlFATAL;
			}
		}
	}
	/* call reset on this page */

	if (reset)
		return NgResetPage(rec->go->base.id,page->id);
	else
		return NhlNOERROR;
}

static void SetInputDataFlag
(
	brHluPageRec	*rec
)
{
	int 		i,datavar_count = 0;

	if (! rec->data_profile) {
		rec->new_data = False;
		rec->has_input_data = False;
		return;
	}
/*
 * For has_input_data to be True all required, but at least one, data items
 * of type _NgDATAVAR must be set.
 */
	rec->has_input_data = True;
	for (i = 0; i < rec->data_profile->n_dataitems; i++) {
		NgDataItem ditem = rec->data_profile->ditems[i];
		if (ditem->item_type == _NgDATAVAR) {
			switch (ditem->vdata->set_state) {
			case _NgEXPRESSION:
				if (ditem->vdata->expr_val)
					datavar_count++;
				else if (ditem->required)
					rec->has_input_data = False;
				break;
			case _NgUNKNOWN_DATA:
				datavar_count++;
				break;
			default:
				if (ditem->vdata->qvar)
					datavar_count++;
				else if (ditem->required)
					rec->has_input_data = False;
				break;
			}
		}
		if (ditem->vdata->cflags)
			rec->new_data = True;
	}
	if (! datavar_count) 
		rec->has_input_data = False;
/*
 * Since updates occurring as a result of changes to the data object 
 * indirectly result in changes to the view object, it is necessary to 
 * set the draw_req flag here
 */
	if (rec->new_data) {
		NhlLayer l = _NhlGetLayer(rec->hlu_id);
		if (l && _NhlIsView(l)) {
			NhlLayer tl;
			int top_id = _NhlTopLevelView(rec->hlu_id);
			tl = _NhlGetLayer(top_id);
			if (tl) {
				NgHluData hdata = 
					(NgHluData) tl->base.gui_data2;
				if (hdata)
					hdata->draw_req = True;
			}
		}
	}
		
	return;
}

static NhlErrorTypes GetDataProfileMessage
(
        brPage *page,
	NgPageMessage   message
        )
{
        brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec	*)pdp->type_rec;
        NgHluPage 	*pub = &rec->public;
	NgDataProfile	dprof;

	dprof = (NgDataProfile) message->message;

	if (pub->data_profile) {
		NgTransferDataProfileInfo(pub->data_profile,dprof);
		if (message->message_free && dprof->free)
			(*dprof->free) (dprof);
	}
	/* this clause is redundant but hopefully safe */
	else if (message->message_free && dprof->free) {
		NhlString name;

		/* 
		 * We have a copy so if the class matches we can just
		 * assign; otherwise create.
		 */

		name = NgHasDataProfile(rec->go,pub->class_name) ?
			pub->class_name : NULL;
		if ( ! (name || dprof->class_name) ||
		    ! strcmp(name,dprof->class_name))
			pub->data_profile = dprof;
		else {
			pub->data_profile = NgNewDataProfile(rec->go,name);
			if (! pub->data_profile) {
				NHLPERROR((NhlFATAL,NhlEUNKNOWN,
					   "error getting data profile for %s",
					   pub->class_name));
				return NhlFATAL;
			}
			NgTransferDataProfileInfo(pub->data_profile,dprof);
		}
	}
	rec->data_profile = pub->data_profile;

	SetInputDataFlag(rec);

	if (rec->new_data && page->qvar && rec->data_profile && 
	    rec->data_source_grid) {
		NgUpdateDataSourceGrid
			(rec->data_source_grid,page->qvar,rec->data_profile);
	}
	TalkToDataLinks(page,_NgDATAPROFILE,message->from_id);
	
	return NhlNOERROR;
}
static NhlErrorTypes GetDoSetValCBMessage
(
        brPage *page,
	NgPageMessage   message
        )
{
        brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec	*)pdp->type_rec;
        NgHluPage 	*pub = &rec->public;

	rec->do_setval_cb = (NhlBoolean) message->message;
/*
 * If something in this hlu object was set by another object while the
 * setval_cb was disabled, then it will not have been updated correctly.
 * So do an update now, to ensure that it is.
 */
#if 0
	if (rec->do_setval_cb == True) {
		NgResTreeResUpdateComplete(rec->res_tree,rec->hlu_id,False);
	}
#endif

	return NhlNOERROR;
}

static NhlErrorTypes GetDataLinkReqMessage
(
        brPage *page,
	NgPageMessage   message
        )
{
        brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec	*)pdp->type_rec;
        NgHluPage 	*pub = &rec->public;
	brDataLinkReq	dlink;
	NgPageMessageType mesg_type = _NgNOMESSAGE;
	brPage		*frpage;

	dlink = (brDataLinkReq) message->message;
	switch (message->mtype) {
	case _NgDATAPROFILELINK_REQ:
		mesg_type = _NgDATAPROFILE;
		break;
	case _NgVARDATALINK_REQ:
		mesg_type = _NgVARDATA;
		break;
	default:
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			   "invalid message type for DataLine"));
		return NhlFATAL;
	}
	frpage = _NgGetPageRef(rec->go->base.id,message->from_id);

	if (frpage) {
		UpdateDataLink
			(rec,dlink->on,mesg_type,frpage->id,
			 frpage->qfile,frpage->qvar);
	}
	if (message->message_free)
		(*message->message_free)(message->message);

	return NhlNOERROR;
}
static NhlErrorTypes GetVarDataMessage
(
        brPage *page,
	NgPageMessage   message
        )
{
        brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec	*)pdp->type_rec;
        NgHluPage 	*pub = &rec->public;
	NgVarData	vdata;

	vdata = (NgVarData) message->message;

	NgSetDataProfileVar(rec->data_profile,vdata,False,True);

	SetInputDataFlag(rec);

	if (rec->new_data && page->qvar && rec->data_profile && 
	    rec->data_source_grid) {
		NgUpdateDataSourceGrid
			(rec->data_source_grid,
			 page->qvar,rec->data_profile);
	}
	
	TalkToDataLinks(page,_NgDATAPROFILE,message->from_id);

	if (message->message_free)
		(*message->message_free)(message->message);
	
	return NhlNOERROR;
}

static NhlErrorTypes GetPageMessages
(
        brPage 		*page,
	NhlBoolean	reset
        )
{
        brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec	*)pdp->type_rec;
        NgHluPage 	*pub = &rec->public;
	NgPageMessage   *messages;
	int 		i,count;
	NhlErrorTypes	ret = NhlNOERROR,subret = NhlNOERROR;

	count = NgRetrievePageMessages
		(page->go->base.id,_brHLUVAR,
		 NrmNULLQUARK,page->qvar,&messages);

	/*
	 * Reading in this order should ensure that Create messages are
	 * parsed and acted on before data update messages.
	 */
	for (i = count - 1; i >= 0; i--) {
		switch (messages[i]->mtype) {  
		case _NgHLUOBJCREATE:
			subret = GetHluObjCreateMessage
				(page,messages[i],reset);
			ret = MIN(ret,subret);
			break;
		case _NgDATAPROFILELINK_REQ:
		case _NgVARDATALINK_REQ:
			subret = GetDataLinkReqMessage(page,messages[i]);
			ret = MIN(ret,subret);
			break;
		case _NgDATAPROFILE:
			subret = GetDataProfileMessage(page,messages[i]);
			ret = MIN(ret,subret);
			break;
		case _NgVARDATA:
			subret = GetVarDataMessage(page,messages[i]);
			ret = MIN(ret,subret);
			break;
		case _NgDOSETVALCB:
			subret = GetDoSetValCBMessage(page,messages[i]);
			ret = MIN(ret,subret);
			break;
		}
	}
	if (count)
		NgDeletePageMessages(page->go->base.id,count,messages,False);
	
	return ret;
}

static void GetDataVal
(
	NgVarData 	vdata,
	NhlPointer 	*value,
	NrmQuark 	*type,
	NhlBoolean 	preview
)
{
	int i;
	NclApiVarInfoRec *vinfo;
	NclExtValueRec *val = NULL;
	NhlString type_str;


	*value = NULL;
	*type = NrmNULLQUARK;

	if (! vdata || vdata->set_state == _NgUNKNOWN_DATA)
		return;


	if (! preview) {
		char buf[1024];

		if (vdata->set_state == _NgEXPRESSION) {
			if (! vdata->qexpr_var) {
				if (! vdata->expr_val && vdata->go)
					return;
				if (! NgSetExpressionVarData
				    (vdata->go->base.id,vdata,vdata->expr_val))
					return;
			}
			sprintf(buf,"%s(",
				NrmQuarkToString(vdata->qexpr_var));
		}
		else {

			if (! vdata->qvar) {
				sprintf(buf,"null");
				*value = NhlMalloc(strlen(buf) + 1);
				strcpy((char *)*value,buf);
				*type = QString;
				return;
			}

			switch (vdata->type) {
			case FILEVAR:
				sprintf(buf,"%s->%s(",
					NrmQuarkToString(vdata->qfile),
					NrmQuarkToString(vdata->qvar));
				break;
			case COORD:
				if (vdata->qfile)
					sprintf(buf,"%s->%s&%s(",
						NrmQuarkToString(vdata->qfile),
						NrmQuarkToString(vdata->qvar),
						NrmQuarkToString
						(vdata->qcoord));
				else 
					sprintf(buf,"%s&%s(",
						NrmQuarkToString(vdata->qvar),
						NrmQuarkToString
						(vdata->qcoord));
				break;
			case NORMAL:
				sprintf(buf,"%s(",
					NrmQuarkToString(vdata->qvar));
				break;
			default:
				fprintf(stderr,"invalid var type\n");
				return;
			}
		}
		for (i = 0; i < vdata->ndims; i++) {
			if (abs((vdata->finish[i] - vdata->start[i]) /
				vdata->stride[i]) < 1) {
				sprintf(&buf[strlen(buf)],"%d,",
					vdata->start[i]);
				continue;
			}
	                sprintf(&buf[strlen(buf)],"%d:%d:%d,",
                        vdata->start[i],vdata->finish[i],vdata->stride[i]);
		}
		/* backing up 1 to get rid of last comma */
		sprintf(&buf[strlen(buf)-1],")");
		*value = NhlMalloc(strlen(buf) + 1);
		strcpy((char *)*value,buf);
		*type = QString;
		return;
	}

	if (vdata->set_state == _NgEXPRESSION) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"invalid var data set state"));
		return;
	}
	switch (vdata->type) {
	case FILEVAR:
		val = NclReadFileVar
			(vdata->qfile,vdata->qvar,
			 vdata->start,vdata->finish,vdata->stride);
		break;
	case COORD:
		if (vdata->qfile) 
			val = NclReadFileVarCoord
				(vdata->qfile,vdata->qfile,vdata->qvar,
				 vdata->start,vdata->finish,vdata->stride);
		else
			val = NclReadVarCoord
				(vdata->qvar,vdata->qcoord,
				 vdata->start,vdata->finish,vdata->stride);
		break;
	case NORMAL:
		val = NclReadVar
			(vdata->qvar,
			 vdata->start,vdata->finish,vdata->stride);
		break;
	default:
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"invalid var type"));
		return;
	}
	if (! val)
		return;

	type_str = NgHLUTypeString(val->type);
	*value = _NhlCreateGenArray(val->value,type_str,val->elem_size,
				   val->n_dims,val->dim_sizes,!val->constant);
	NclFreeExtValue(val);
	*type = QGenArray;

	return;
}


static void GetDataObjValue
(
	brHluPageRec	*rec,
	NgDataItem	ditem,
	NhlPointer 	*value,
	NrmQuark 	*type,
	NhlBoolean 	preview
)
{
	NrmQuark qname = ditem->vdata->qvar;
	int i;

	if (preview) {
		for (i = 0; i < rec->data_object_count; i++) {
			if (rec->data_objects[i]->qname == qname) {
				*value = (NhlPointer)rec->data_objects[i]->id;
				*type = QInteger;
				return;
			}
		}
	}
	*value = (NhlPointer) NrmQuarkToString(qname);
	*value = NhlMalloc(strlen(NrmQuarkToString(qname)) + 1);
	strcpy((char *)*value,NrmQuarkToString(qname));
	*type = QString;
	
	return;
		
}
static NhlBoolean GetConfigValue
(
	brHluPageRec	*rec,
	NgDataItem	ditem,
	NhlPointer 	*value,
	NrmQuark 	*type,
	NhlBoolean 	preview
)
{

	if (! ditem->get_val)
		return False;

	return (*ditem->get_val)(ditem,value,type,preview);
}

static NhlBoolean GetFillValue
(
	brHluPageRec	*rec,
	NgDataItem	ditem,
	NhlPointer 	*value,
	NrmQuark 	*type,
	NhlBoolean 	preview
)
{

	if (! ditem->get_val)
		return False;

	return (*ditem->get_val)(ditem,value,type,preview);
}
 
static void PreviewResList
(
        int		setrl_id,
        NhlPointer	data
        )
{
	ResData *res_data = (ResData *) data;
	int i;

	for (i = 0; i < res_data->res_count; i++) {
		if (! res_data->values[i])
			continue;
		NhlRLSet(setrl_id,res_data->res[i],
			 NrmQuarkToString(res_data->types[i]),
			 res_data->values[i]);
	}
	return;
}

static int AddResList
(
        int		nclstate,
        NhlPointer	data,
        int		block_id
        )
{
	ResData *res_data = (ResData *) data;
	NhlBoolean quote[16];
	int i,lcount = 0;
	NhlString res[16],values[16];

	if (! res_data->res_count)
		return 0;

	for (i = 0; i < res_data->res_count; i++) {
		if (res_data->values[i]) {
			res[lcount] = res_data->res[i];
			values[lcount] = (NhlString) res_data->values[i];
			quote[lcount] = False;
			lcount++;
		}
	}

        NgNclVisBlockAddResList(nclstate,block_id,lcount,res,values,quote);

	return res_data->res_count;
}


extern void NgFreeHluObjCreateRec
(
	brHluObjCreate obj_create
)

{
	NgFreeDataProfile(obj_create->dprof);
	NhlFree(obj_create);
	return;
}

extern brHluObjCreate NgNewHluObjCreateRec
(
        NhlString	class_name,
	NgDataProfile	data_profile,
	NhlString	plot_style,
	NhlString	plot_style_dir,
	int		obj_id,
	NhlBoolean	has_input_data,
	_hluState	state
)
{
	brHluObjCreate obj_create = 
		NhlMalloc(sizeof(brHluObjCreateRec));
	if (! obj_create) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NULL;
	}
 	obj_create->obj_id = obj_id;
	obj_create->class_name = class_name;
	obj_create->plot_style = plot_style;
	obj_create->plot_style_dir = plot_style_dir;
	obj_create->has_input_data = has_input_data;
	obj_create->state = state;
	obj_create->dprof = data_profile;

	return obj_create;

}
static void PostHluObjCreateMessage
(
        brPage		*page,
	NhlString       class_name,
	NrmQuark	qname,
	int		hlu_id,
	_hluState	state
)
{
	brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec *) pdp->type_rec;
	brHluObjCreate  obj_create;
	NgDataProfile 	dprof;
	NgPageMessageRec   message_rec;

	dprof = NgNewDataProfile(page->go,class_name);
	NgTransferDataProfileInfo(dprof,rec->data_profile);

	obj_create = NgNewHluObjCreateRec
		(class_name,dprof,rec->public.plot_style,
		 rec->public.plot_style_dir,hlu_id,rec->has_input_data,state);

	if (obj_create)
		NgPostPageMessage(page->go->base.id,page->id,
				  _NgDATAPROFILELINK_REQ,
				  _brHLUVAR,NrmNULLQUARK,qname,
				  _NgHLUOBJCREATE,obj_create,True,
				  (NhlFreeFunc)NgFreeHluObjCreateRec,False);
	return;

}

static int CreatePlotStyleApp
(
        brPage		*page,
	NhlBoolean	preview
)
{
	NhlErrorTypes	ret = NhlNOERROR;
	brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec *) pdp->type_rec;
	char		plotstyledir[512];
	ResData 	app_res_data;
        NgSetResProc	setresproc[2];
        NhlPointer	appresdata;
	int		app_id;

	if (! rec->public.plot_style) 
		return NhlNULLOBJID;

#if DEBUG_HLUPAGE
	fprintf(stderr,"plot style %s\n",rec->public.plot_style);
#endif

	app_res_data.res_count = 1;
	app_res_data.res[0] = "appUsrDir";
	app_res_data.types[0] = QString;
	if (rec->public.plot_style_dir) {
		if (preview)
			plotstyledir[0] = '\0';
		else 
			strcpy(plotstyledir,"\"");
		strcat(plotstyledir,rec->public.plot_style_dir);
		if (! preview)
			strcat(plotstyledir,"\"");
		app_res_data.values[0] = (NhlPointer) plotstyledir;
	}
	else if (preview) {
		app_res_data.values[0] = (NhlPointer) "./plot_styles";
	}
	else {
		app_res_data.values[0] = (NhlPointer) "\"./plot_styles\"";
	}

	if (preview) {
		NgPreviewResProc	resproc;

		resproc = PreviewResList;
		appresdata = &app_res_data;
		ret = NgCreatePreviewGraphic
			(rec->go->base.id,&app_id,
			 rec->public.plot_style,NULL,"appClass",
			 1,&resproc,&appresdata);
	}
	else {
		NgSetResProc	resproc;

		resproc = AddResList;
		appresdata = &app_res_data;
		ret = NgCreateGraphic(rec->go->base.id,&app_id,
				      rec->public.plot_style,NULL,"appClass",
				      1,&resproc,&appresdata);
	}
	if (! app_id || ret < NhlWARNING)
		return (int) ret;

	return app_id;
}

static void DestroyPlotStyleApp
(
        brPage		*page,
	NhlBoolean	preview
)
{
	brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec *) pdp->type_rec;
		
	if (! rec->public.plot_style)
		return;
	if (! rec->app_id) 
		return;

	if (preview)
		NgDestroyPreviewGraphic(rec->go->base.id,rec->app_id);
	else
		NgDestroyGraphic(rec->go->base.id,rec->public.plot_style);

	rec->app_id = NhlNULLOBJID;

	return;
}

static int GetPlotStyleAppId
(
	brPage		*page,
	NhlBoolean	*is_preview
)
{
	brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec *) pdp->type_rec;
	PlotStyleApp	ps_app = PlotStyleAppList;
	NrmQuark       	qname;
	int		app_id;

	*is_preview = False;

	if (! rec->public.plot_style)
		return NhlNULLOBJID;

	qname  = NrmStringToQuark(rec->public.plot_style);

	while (ps_app) {
		if (ps_app->qname == qname) {
			*is_preview = ps_app->preview;
			return ps_app->app_id;
		}
		ps_app = ps_app->next;
	}
	return NhlNULLOBJID;
}

static void DeletePlotStyleAppRef
(
	int		go_id,
	NrmQuark	qplotstyle
)
{
	PlotStyleApp	*ps_app = &PlotStyleAppList;
	PlotStyleApp	dps_app = NULL;
	NrmQuark	qname;

	while (*ps_app) {
		if ((*ps_app)->qname == qplotstyle) {
			(*ps_app)->ref_count--;
			if ((*ps_app)->ref_count > 0)
				return;
			dps_app = *ps_app;
			*ps_app = (*ps_app)->next;
			break;
		}
		ps_app = &((*ps_app)->next);
	}
	if (dps_app) {
		if (dps_app->preview)
			NgDestroyPreviewGraphic(go_id,dps_app->app_id);
		else
			NgDestroyGraphic(go_id,NrmQuarkToString(qplotstyle));
		NhlFree(dps_app);
	}
	return;
}

static int NewPlotStyleAppRef
(
	brPage		*page,
	NhlBoolean	preview
)
{
	brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec *) pdp->type_rec;
	PlotStyleApp ps_app = PlotStyleAppList;
	int app_id;
	NrmQuark qname;
	NhlBoolean found = False;

	if (! rec->public.plot_style)
		return NhlNULLOBJID;

	qname  = NrmStringToQuark(rec->public.plot_style);

	while (ps_app) {
		if (ps_app->qname == qname) {
			ps_app->ref_count++;

			if (ps_app->preview && ! preview) {
				DestroyPlotStyleApp(page,True);
				found = True;
				break;
			}
			else if (! _NhlGetLayer(ps_app->app_id)) {
				found = True;
				break;
			}
			return ps_app->app_id;
		}
		ps_app = ps_app->next;
	}

	app_id = CreatePlotStyleApp(page,preview);
	if (app_id <= NhlNULLOBJID)
		return NhlNULLOBJID;

	if (! found) {
		ps_app = NhlMalloc(sizeof(PlotStyleAppRec));
		if (! ps_app) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return (int) NhlFATAL;
		}
		ps_app->ref_count = 1;
		ps_app->next = PlotStyleAppList;
		PlotStyleAppList = ps_app;
	}

	ps_app->app_id = app_id;
	ps_app->qname = qname;
	ps_app->preview = preview;

	return app_id;
}

static void
DestroyCB
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	NgHluData 	hdata = (NgHluData) udata.ptrval; 


#if DEBUG_HLUPAGE
        fprintf(stderr,"in destroy cb\n");
#endif
	if (hdata->qplotstyle)
		DeletePlotStyleAppRef(hdata->go_id,hdata->qplotstyle);

	NgCBWPDestroy(hdata->destroy_cb);

	NgFreeHluData(hdata);

        return;
}

static void
RegisterHluDataDestroyCB
(
        brPage		*page,
	int		hlu_id,
	NhlBoolean	preview
)
{
	brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec *) pdp->type_rec;
	NhlLayer l = _NhlGetLayer(hlu_id);
	NgHluData 	hdata;
	NhlArgVal 	sel,user_data;

	if (! l)
		return;

	if (! l->base.gui_data2) {
		l->base.gui_data2  = (NhlPointer) NgGetHluData();
		if (! l->base.gui_data2) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return;
		}
	}
	
	hdata = (NgHluData) l->base.gui_data2;

	hdata->preview = preview;
	hdata->go_id = page->go->base.id;

	if (rec->public.plot_style) {
		hdata->qplotstyle = NrmStringToQuark(rec->public.plot_style);
	}
	NhlINITVAR(sel);
	NhlINITVAR(user_data);
	sel.lngval = 0;
	user_data.ptrval = hdata;
	hdata->destroy_cb = NgCBWPAdd
		(page->go->go.appmgr,NULL,NULL,_NhlGetLayer(hlu_id),
		 _NhlCBobjDestroy,sel,DestroyCB,user_data);
	return;
}

static Boolean ManageDataObj
(
        brPage		*page,
	NgClassType 	type,
	NgDataItem	obj_ditem,
	int		data_ix
)
{
	brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec *) pdp->type_rec;
	int i, res_count = 0;
	NhlBoolean preview = rec->state == _hluNOTCREATED;
	int hlu_id;
	char *class_name;
	NhlErrorTypes ret = NhlNOERROR;
	ResData res_data;
	NgSetResProc	resproc; 
	NhlPointer	resdata;
	char *suffix;
	char buf[256];
	char *name;
	int count;
	NgDataProfile	dprof = rec->data_profile;

	if (obj_ditem->item_type != _NgDATAOBJ)
		return False;

	memset(res_data.res,0,16 * sizeof(char *));
	memset(res_data.values,0,16 * sizeof(NhlPointer));
	memset(res_data.types,0,16 * sizeof(NrmQuark));

	switch (type) {
	case _NgSCALARFIELD:
		class_name = "scalarFieldClass";
		suffix = "sf";
		break;
	case _NgVECTORFIELD:
		class_name = "vectorFieldClass";
		suffix = "vf";
		break;
	case _NgCOORDARRAY:
		class_name = "coordArraysClass";
		suffix = "ca";
		break;
	}

	count = 0;
	for (i=0; i < dprof->n_dataitems; i++) {
		NgDataItem ditem = dprof->ditems[i];
		NgVarData vdata;
		if (ditem->class_type != type)
			continue;
		vdata = ditem->vdata;
		if (! vdata->cflags)
			continue;
		res_data.res[count] = ditem->resname;
		switch (ditem->item_type) {
		case _NgMISSINGVAL:
			if (! vdata->cflags & _NgSYMBOL_CHANGE)
				continue;
			if (!GetFillValue(rec,ditem,&res_data.values[count],
				&res_data.types[count],preview))
				continue;
			break;
		case _NgCONFIG:
			if (! GetConfigValue
			    (rec,ditem,&res_data.values[count],
			     &res_data.types[count],preview))
				continue;
			break;
		case _NgDATAVAR:
                case _NgCOORDVAR:
			if (preview && ! vdata->qvar)
				continue;
			GetDataVal(vdata,&res_data.values[count],
				   &res_data.types[count],preview);
			break;
		default:
			continue;
		}
			
		res_data.vdata[count] = vdata;
		count++;
	}
	res_data.res_count = count;

        if (rec->data_object_count < data_ix + 1) {
                rec->data_object_count = data_ix + 1;
                rec->data_objects = NhlRealloc
                        (rec->data_objects,rec->data_object_count * 
                         sizeof(brDataObjInfo));
                rec->data_objects[data_ix] = NhlMalloc
                        (sizeof(brDataObjInfoRec));
		rec->data_objects[data_ix]->qname = NrmNULLQUARK;
		rec->data_objects[data_ix]->id = NhlNULLOBJID;
        }
	if (preview) {
		NgVarData vdata = obj_ditem->vdata;
		NgPreviewResProc	resproc;

		rec->app_id = NewPlotStyleAppRef(page,True);

		if (rec->app_id) {
			res_data.res[res_data.res_count] = "objAppObj";
			res_data.values[res_data.res_count] =
				(NhlPointer)rec->app_id;
			res_data.types[res_data.res_count] = QInteger;
			res_data.res_count++;
		}

		sprintf(buf,"%s_%s",NrmQuarkToString(page->qvar),suffix);
                name = NgNclGetSymName(rec->nclstate,buf,True);
                
                    /* create the NCL graphic variable using this name now
                       in order that it won't be "stolen" before the hlu
                       object actually gets created */
                
                sprintf(buf,"%s = new(1,graphic)\n",name);
                (void)NgNclSubmitBlock(rec->nclstate,buf);
                
                    /* the name quark gets set even if the create fails */
		rec->data_objects[data_ix]->qname = NrmStringToQuark(name);
                
		resproc = PreviewResList;
		resdata = (NhlPointer) &res_data;
			
		ret = NgCreatePreviewGraphic
			(rec->go->base.id,&hlu_id,name,NULL,
			 class_name,1,&resproc,&resdata);
		if (! hlu_id || ret < NhlWARNING)
			return False;

                    /* hlu id gets set only if create succeeds */
		rec->data_objects[data_ix]->id = hlu_id;
		RegisterHluDataDestroyCB(page,hlu_id,True);

		vdata->qvar = rec->data_objects[data_ix]->qname;
		vdata->type = HLUOBJ;
		vdata->ndims = 0;
		vdata->cflags = _NgSYMBOL_CHANGE;
		
                for (i = 0; i< res_data.res_count; i++) {
                        if (res_data.values[i]) {
                                if (res_data.types[i] == QGenArray)
                                        NhlFreeGenArray(res_data.values[i]);
                                else if (res_data.types[i] == QString)
                                        NhlFree(res_data.values[i]);
                        }
                }
		PostHluObjCreateMessage
			(page,class_name,vdata->qvar,hlu_id,_hluPREVIEW);
		UpdateDataLink(rec,True,_NgDATAPROFILE,
			       NgNoPage,NrmNULLQUARK,vdata->qvar);
				   
		return True;
	}
	
	resproc = AddResList;
	resdata = (NhlPointer) &res_data;
        if (rec->data_objects[data_ix]->id <= NhlNULLOBJID ) {
		NgVarData vdata = obj_ditem->vdata;

		rec->app_id = NewPlotStyleAppRef(page,False);
		if (rec->app_id) {
			res_data.res[res_data.res_count] = "objAppObj";
			res_data.values[res_data.res_count] =
				NhlMalloc(strlen(rec->public.plot_style)+1);
			strcpy((char *)res_data.values[res_data.res_count],
			       rec->public.plot_style);
			res_data.types[res_data.res_count] = QString;
			res_data.res_count++;
		}

		if (rec->data_objects[data_ix]->qname) {
			name = NrmQuarkToString
				(rec->data_objects[data_ix]->qname);
		} 
		else {
			sprintf(buf,"%s_%s",
				NrmQuarkToString(page->qvar),suffix);
			name = NgNclGetSymName(rec->nclstate,buf,True);
                        sprintf(buf,"%s = new(1,graphic)\n",name);
                        (void)NgNclSubmitBlock(rec->nclstate,buf);
		}
		rec->data_objects[data_ix]->qname = NrmStringToQuark(name);
		ret = NgCreateGraphic
			(rec->go->base.id,&hlu_id,name,NULL,
			 class_name,1,&resproc,&resdata);
#if DEBUG_HLUPAGE
		fprintf(stderr,"created hlu obj with id %d\n", hlu_id);
#endif        
		if (! hlu_id || ret < NhlWARNING) {
			char buf[512];
			sprintf(buf,"%s = %s@_FillValue\n",
				NrmQuarkToString(page->qvar),
				NrmQuarkToString(page->qvar));
			(void)NgNclSubmitBlock(rec->nclstate,buf);
			return ret;
		}
                
		rec->data_objects[data_ix]->id = hlu_id;
		RegisterHluDataDestroyCB(page,hlu_id,False);

		vdata->qvar = rec->data_objects[data_ix]->qname;
		vdata->type = HLUOBJ;
		vdata->ndims = 0;
		vdata->cflags = _NgSYMBOL_CHANGE;

		for (i = 0; i < dprof->n_dataitems; i++) {
			NgDataItem ditem = dprof->ditems[i];
			if (ditem->class_type == type)
				ditem->hlu_id = hlu_id;
		}
		PostHluObjCreateMessage
			(page,class_name,vdata->qvar,hlu_id,_hluCREATED);
		UpdateDataLink(rec,True,_NgDATAPROFILE,
			       NgNoPage,NrmNULLQUARK,vdata->qvar);
        }
        else if (res_data.res_count) {
		ret = NgUpdateGraphic
			(rec->go->base.id,
			 NrmQuarkToString(rec->data_objects[data_ix]->qname),
			 1,&resproc,&resdata);
		if (ret < NhlWARNING)
			return False;
	}
        for (i = 0; i< res_data.res_count; i++) {
                if (res_data.values[i]) {
                        if (res_data.types[i] == QGenArray)
                                NhlFreeGenArray(res_data.values[i]);
                        else if (res_data.types[i] == QString)
                                NhlFree(res_data.values[i]);
                }
        }
        
	return True;
}

static NhlBoolean ManagePlotObj
(
        brPage	*page,
        int	wk_id
)
{
	brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec *) pdp->type_rec;
	ResData 	res_data,app_res_data;
	NhlBoolean	preview = rec->state == _hluNOTCREATED;
	NhlString	parent = NULL;
	int		hlu_id,i;
        NgSetResProc	setresproc[2];
        NhlPointer	setresdata[2],appresdata;
	NhlErrorTypes	ret;
	NgDataProfile	dprof = rec->data_profile;
	int		count;

	count = 0;
	for (i=0; i < dprof->n_dataitems; i++) {
		NgDataItem ditem = dprof->ditems[i];
		NgVarData vdata;
		if (ditem->class_type != dprof->type)
			continue;
		
		vdata = ditem->vdata;
		if (rec->state == _hluCREATED && ! vdata->cflags)
			continue;
		res_data.res[count] = ditem->resname;
		switch (ditem->item_type) {
		case _NgMISSINGVAL:
			if (!GetFillValue(rec,ditem,&res_data.values[count],
				&res_data.types[count],preview))
				continue;
			break;
		case _NgCONFIG:
			if (! GetConfigValue
			    (rec,ditem,&res_data.values[count],
			     &res_data.types[count],preview))
				continue;
			break;
		case _NgDATAOBJ:
			if (! vdata->qvar)
				continue;
			GetDataObjValue(rec,ditem,&res_data.values[count],
					&res_data.types[count],preview);

			break;
		case _NgDATAVAR:
                case _NgCOORDVAR:
			if (preview && ! vdata->qvar)
				continue;
			GetDataVal(vdata,&res_data.values[count],
				   &res_data.types[count],preview);
			break;
		}
			
		res_data.vdata[count] = vdata;
		count++;
	}
	res_data.res_count = count;

	if (preview) { 
		NgPreviewResProc	resproc[2];
		NhlPointer		resdata[2];

		if (rec->app_id) {
			res_data.res[res_data.res_count] = "objAppObj";
			res_data.values[res_data.res_count] =
				(NhlPointer)rec->app_id;
			res_data.types[res_data.res_count] = QInteger;
			res_data.res_count++;
		}
		resproc[0] = PreviewResList;
		resdata[0] = &res_data;
		resproc[1] = NgResTreePreviewResList;
		resdata[1] = (NhlPointer)rec->res_tree;

		if (wk_id != NhlNULLOBJID)
			parent = NgNclGetHLURef(rec->go->go.nclstate,wk_id);
		ret = NgCreatePreviewGraphic
			(rec->go->base.id,&hlu_id,
			 NrmQuarkToString(page->qvar),parent,
			 rec->public.class_name,2,resproc,resdata);
		if (! hlu_id || ret < NhlWARNING)
			return False;
		rec->hlu_id = hlu_id;
		RegisterHluDataDestroyCB(page,hlu_id,True);
                for (i = 0; i< res_data.res_count; i++) {
                        if (res_data.values[i]) {
				if (res_data.types[i] == QGenArray)
					NhlFreeGenArray(res_data.values[i]);
				else if (res_data.types[i] == QString)
					NhlFree(res_data.values[i]);
			}
                }
		return True;
	}

        setresproc[0] = AddResList;
	setresdata[0] = (NhlPointer)&res_data;
        setresproc[1] = NgResTreeAddResList;
        setresdata[1] = (NhlPointer)rec->res_tree;
	
        if (rec->state == _hluPREVIEW || rec->hlu_id <= NhlNULLOBJID) {
		if (rec->app_id) {
			res_data.res[res_data.res_count] = "objAppObj";
			res_data.values[res_data.res_count] =
				NhlMalloc(strlen(rec->public.plot_style)+1);
			strcpy((char *)res_data.values[res_data.res_count],
			       rec->public.plot_style);
			res_data.types[res_data.res_count] = QString;
			res_data.res_count++;
		}
		if (wk_id != NhlNULLOBJID)
			parent = NgNclGetHLURef(rec->go->go.nclstate,wk_id);

		ret = NgCreateGraphic
			(rec->go->base.id,&hlu_id,
			 NrmQuarkToString(page->qvar),parent,
			 rec->public.class_name,2,setresproc,setresdata);
		RegisterHluDataDestroyCB(page,hlu_id,False);
#if DEBUG_HLUPAGE
		fprintf(stderr,"created hlu obj with id %d\n", hlu_id);
#endif        
		if (! hlu_id || ret < NhlWARNING) {
			char buf[512];
			sprintf(buf,"%s = %s@_FillValue\n",
				NrmQuarkToString(page->qvar),
				NrmQuarkToString(page->qvar));
			(void)NgNclSubmitBlock(rec->nclstate,buf);
			return ret;
		}
		rec->hlu_id = hlu_id;

		for (i = 0; i < dprof->n_dataitems; i++) {
			NgDataItem ditem = dprof->ditems[i];
			if (ditem->class_type == dprof->type)
				ditem->hlu_id = hlu_id;
		}
        }
        else {
		int rix;
		int setresproc_count = res_data.res_count > 0 ? 2 : 1;
		rix = 2 - setresproc_count;
		ret = NgUpdateGraphic
			(rec->go->base.id,NrmQuarkToString(page->qvar),
			 setresproc_count,&setresproc[rix],&setresdata[rix]);
		if (ret < NhlWARNING)
			return False;
	}

        for (i = 0; i< res_data.res_count; i++) {
                if (res_data.values[i]) {
                        if (res_data.types[i] == QGenArray)
                                NhlFreeGenArray(res_data.values[i]);
                        else if (res_data.types[i] == QString)
                                NhlFree(res_data.values[i]);
                }
        }
	return True;
}

static int
CreatePreviewInstance
(
        brPage	*page,
        int	wk_id
)
{
        NhlErrorTypes	ret = NhlNOERROR;
	brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec *) pdp->type_rec;
        int		hlu_id;
        NgPreviewResProc	resproc[2];
        XtPointer	resdata[2];
        NhlString	parent = NULL;
	NgDataProfile	dprof = rec->data_profile;
	int		i,count = 0;

	rec->app_id = NewPlotStyleAppRef(page,True);

        if (dprof && rec->has_input_data) {
		for (i = 0; i < dprof->n_dataitems; i++) {
			NgDataItem ditem = dprof->ditems[i];
			if (ditem->item_type == _NgDATAOBJ &&
			    (ditem->val_defined)(dprof,i)) {
				if (! ManageDataObj
				    (page,ditem->ref_ditem->class_type,
				     ditem,count))
					return NhlFATAL;
				count++;
			}
		}
		if (! ManagePlotObj(page,wk_id))
			return NhlFATAL;
	}
	else {
		resproc[0] = NgResTreePreviewResList;
		resdata[0] = (NhlPointer)rec->res_tree;
		if (wk_id != NhlNULLOBJID)
			parent = NgNclGetHLURef(rec->go->go.nclstate,wk_id);
		ret = NgCreatePreviewGraphic
			(rec->go->base.id,&hlu_id,
			 NrmQuarkToString(page->qvar),parent,
			 rec->public.class_name,1,resproc,resdata);
		if (ret < NhlWARNING || hlu_id <= NhlNULLOBJID)
			return NhlFATAL;
		rec->hlu_id = hlu_id;
	}

	return rec->hlu_id;
}

static NhlBoolean
CopySVCB
(
        NhlArgVal       cbdata,
	NhlArgVal	udata,
        NhlArgVal       *ret
)
{
	brSetValCBInfo   *info = (brSetValCBInfo *)udata.lngval;
	_NhlValueSetCBData vsdata = (_NhlValueSetCBData) cbdata.ptrval;
	brHluPageRec	*rec;
	_NhlValueSetCBData new;

	rec = (brHluPageRec *)NgPageData(info->goid,info->pid);
	
        if (! rec || rec->hlu_id <= NhlNULLOBJID)
                return False;
#if DEBUG_HLUPAGE
        fprintf(stderr,"%s data profile in setval copy cb: %s\n",
		rec->class->base_class.class_name,
		NrmQuarkToString(vsdata->resq));
#endif

/*
 * The setvalue callback must be blocked for setvalues calls performed from
 * the hlupage itself.
 */
	if  (! rec->do_setval_cb)
		return False;

        new = NhlMalloc(sizeof(_NhlobjChangeChildRec));
        if(!new){
                NHLPERROR((NhlFATAL,ENOMEM,NULL));
                return False;
        }

        *new = *vsdata;
        ret->ptrval = new;

        return True;
}

static void
FreeSVCB
(
        NhlArgVal       cbdata
)
{
        NhlFree(cbdata.ptrval);

        return;
}

static void
DProfSetValCB
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	brSetValCBInfo   *info = (brSetValCBInfo *)udata.lngval;
	_NhlValueSetCBData vsdata = (_NhlValueSetCBData) cbdata.ptrval;
	brHluPageRec	*rec;
	int		i;
	NgDataProfile	dprof;
	NgDataItem	ditem;
	static		int grlist = -1;
	brPage		*page;

	rec = (brHluPageRec *)NgPageData(info->goid,info->pid);
	
        if (! rec || rec->hlu_id <= NhlNULLOBJID)
                return;
#if DEBUG_HLUPAGE
        fprintf(stderr,"%s data profile in setval cb: %s\n",
		rec->class->base_class.class_name,
		NrmQuarkToString(vsdata->resq));
#endif

	if  (! rec->do_setval_cb)
		return;
/*
 * The setvalue callback must be blocked for setvalues calls performed from
 * the hlupage itself.
 */
	dprof = rec->data_profile;
	for (i = 0; i < dprof->n_dataitems; i++) {
		NhlGenArray gen;

		ditem = dprof->ditems[i];
		if (ditem->resq != vsdata->resq)
			continue;
#if DEBUG_HLUPAGE
		fprintf(stderr,
			"%s has been set\n",NrmQuarkToString(ditem->resq));
#endif
		NgSetUnknownDataItem(ditem);
		NgSetDependentVarData(dprof,-1,False);
	}
	page = _NgGetPageRef(info->goid,info->pid);

	NgUpdateDataSourceGrid
		(rec->data_source_grid,page->qvar,rec->data_profile);
	
        return;
}

static int
CreateInstance
(
        brPage	*page,
        int	wk_id
)
{
        NhlErrorTypes	ret;
	brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec *) pdp->type_rec;
        int		i,hlu_id;
        NgSetResProc	setresproc[2];
        XtPointer	setresdata[2];
        NhlString	parent = NULL;
	NgWksObj	wks = NULL;
	NgDataProfile	dprof = rec->data_profile;
	int		count = 0;
        NhlArgVal       sel,udata;
	NhlBoolean	is_preview;

	rec->app_id = NewPlotStyleAppRef(page,False);

        if (dprof && rec->has_input_data) {
		NhlINITVAR(sel);
		NhlINITVAR(udata);

		for (i = 0; i < dprof->n_dataitems; i++) {
			NgDataItem ditem = dprof->ditems[i];
			if (ditem->item_type == _NgDATAOBJ &&
			    (ditem->val_defined)(dprof,i)) {
				if (! ManageDataObj
				    (page,ditem->ref_ditem->class_type,
				     ditem,count))
					return NhlFATAL;
				count++;
			}
			
		}
		if (! ManagePlotObj(page,wk_id))
			return NhlFATAL;

		udata.ptrval = &rec->setval_info;
		for (i = 0; i < dprof->n_dataitems; i++) {
			NgDataItem ditem = dprof->ditems[i];
			if (! ditem->hlu_id)
				continue;
			sel.lngval = ditem->resq;
			ditem->svcb = NgCBWPAdd
				(page->go->go.appmgr,CopySVCB,FreeSVCB,
				 _NhlGetLayer(ditem->hlu_id),_NhlCBobjValueSet,
				 sel,DProfSetValCB,udata);
		}
	}
	else if (NhlClassIsSubclass(rec->class,NhldataItemClass)) {
		return NhlFATAL;
	}
	else {

		setresproc[0] = NgResTreeAddResList;
		setresdata[0] = (NhlPointer)rec->res_tree;

		if (wk_id != NhlNULLOBJID)
			parent = NgNclGetHLURef(rec->go->go.nclstate,wk_id);

		ret = NgCreateGraphic
			(rec->go->base.id,&hlu_id,
			 NrmQuarkToString(page->qvar),parent,
			 rec->public.class_name,1,setresproc,setresdata);
#if DEBUG_HLUPAGE
		fprintf(stderr,"created hlu obj with id %d\n", hlu_id);
#endif        
		if (ret < NhlWARNING) {
			char buf[512];
			sprintf(buf,"%s = %s@_FillValue\n",
				NrmQuarkToString(page->qvar),
				NrmQuarkToString(page->qvar));
			(void)NgNclSubmitBlock(rec->nclstate,buf);
			return ret;
		}
		rec->hlu_id = hlu_id;
	}

	return rec->hlu_id;

/*
 * No explicit draw on create because it is handled automatically by
 * the ViewTree (mwin) create CB.
 */
}

static NhlErrorTypes
UpdateInstance
(
        brPage	*page,
        int	wk_id,
	NhlBoolean do_draw
)
{
        NhlErrorTypes	ret = NhlNOERROR;
	brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec *) pdp->type_rec;
        NgSetResProc	setresproc[2];
        XtPointer	setresdata[2];
	NhlLayer	l = _NhlGetLayer(rec->hlu_id);
	NhlLayer	wl = _NhlGetLayer(wk_id);
	NgDataProfile	dprof = rec->data_profile;
	int		i,count = 0;

	if (! l)	
		return NhlFATAL;

        if (dprof && rec->has_input_data) {
		for (i = 0; i < dprof->n_dataitems; i++) {
			NgDataItem ditem = dprof->ditems[i];
			if (rec->new_data &&
			    ditem->item_type == _NgDATAOBJ &&
			    (ditem->val_defined)(dprof,i)) {
				if (! ManageDataObj
				    (page,ditem->ref_ditem->class_type,
				     ditem,count))
					return NhlFATAL;
				count++;
			}
		}
		
		if (! ManagePlotObj(page,wk_id))
			return NhlFATAL;
		for (i = 0; i < rec->data_object_count; i++) {
			NhlLayer l = _NhlGetLayer(rec->data_objects[i]->id);
			NhlString class_name = 
				l->base.layer_class->base_class.class_name;
		}
	}
        else {
                setresproc[0] = NgResTreeAddResList;
                setresdata[0] = (XtPointer)rec->res_tree;
        
                ret = NgUpdateGraphic
                        (rec->go->base.id,NrmQuarkToString(page->qvar),
                         1,setresproc,setresdata);
        }
	/*
	 * This can happen if a DataObj is created after the main object
	 * is created 
	 */
/*
 * There is no auto callback for setvalues yet, so for updates the draw
 * must occur here
 */
	if (do_draw && NhlClassIsSubclass(rec->class,NhlviewClass) &&
	    _NhlIsClass(wl,NhlxWorkstationClass)) {
		NgWksObj	wks = NULL;
		int 		draw_id = _NhlTopLevelView(rec->hlu_id);
		
		if (draw_id && wl) {
			NgHluData hdata = (NgHluData) wl->base.gui_data2;
			wks = hdata ? (NgWksObj) hdata->gdata : NULL;
		}
		if (wks && wks->auto_refresh) {
			NgDrawXwkView(wks->wks_wrap_id,draw_id,True);
		}
	}
	else if (NhlClassIsSubclass(rec->class,NhlxWorkstationClass)) {
		NgWksObj	wks = NULL;
		wl = _NhlGetLayer(rec->hlu_id);
		if (wl) {
			NgHluData hdata = (NgHluData) wl->base.gui_data2;
			wks = hdata ? (NgWksObj) hdata->gdata : NULL;
		}
		if (wks) {
			NgGO xl = (NgGO)_NhlGetLayer(wks->wks_wrap_id);
			if (xl) 
				NgXWorkPopup(xl->go.appmgr,xl->base.id);
		}
	}

        return ret;
}
static void 
DestroyPreviewDataObjects
(
	brPage	*page
)
{
	brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec *) pdp->type_rec;
	NclExtValueRec 	*hval;
	NrmQuark	qname;
	NhlLayer	l;
	int i, new_id;
/*
 * it's possible that the user may have already destroyed the preview
 * object and created a 'real' object. In that case, don't do a destroy --
 * just update the info.
 */
	for (i = 0; i < rec->data_object_count; i++) {
		int id_count, *id_array;
		new_id = NhlNULLOBJID;

		l = _NhlGetLayer(rec->data_objects[i]->id);
		qname = rec->data_objects[i]->qname;

		if (qname != NrmNULLQUARK) {
			NhlString name = NrmQuarkToString(qname);
			if (! NclSymbolDefined(name))
				rec->data_objects[i]->qname = NrmNULLQUARK;
			else {
				new_id = NgNclGetHluObjId
					(rec->nclstate,name,
					 &id_count,&id_array);
				if (id_count > 1)
					NhlFree(id_array);
			}
		}
		if (new_id > NhlNULLOBJID) {
			if (l && rec->data_objects[i]->id != new_id) {
				NgDestroyPreviewGraphic
					(rec->go->base.id,l->base.id);
			}
			rec->data_objects[i]->id = new_id;
		}
		else {
			rec->data_objects[i]->id = NhlNULLOBJID;
			if (l) {
				NgDestroyPreviewGraphic
					(rec->go->base.id,l->base.id);
			}
		}
	}
	return;
}
static void 
UpdateDataProfileFromResTree
(
	brPage *page
	)
{
	brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec *) pdp->type_rec;
	int i;
	NhlBoolean new_data = False;

	if (! rec->data_profile) {
		rec->new_data = False;
		return;
	}
/*
 * If a data profile item is set in the restree, the restree value always
 * overrules
 */
	for (i = 0; i < rec->data_profile->n_dataitems; i++) {
		NgDataItem ditem = rec->data_profile->ditems[i];
		if (ditem->class_type == rec->data_profile->type) {
			NhlString setval = NgResTreeGetSetValue
				(rec->res_tree,ditem->resq);
			if (setval) {
				NgSetExpressionVarData
					(rec->go->base.id,
					 ditem->vdata,setval);
				new_data = True;
			}
		}
	}
	if (new_data) {
		NgUpdateDataSourceGrid
			(rec->data_source_grid,page->qvar,rec->data_profile);
		rec->new_data = True;
	}

}

static void
CreateUpdate
(
        brPage		*page,
        int		wk_id,
	NhlBoolean	do_draw,
	NhlBoolean	propagate_profile
)
{
	brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec *) pdp->type_rec;
        NhlBoolean 	already_created = False;
        int		i,hlu_id;
	NhlLayer	l,wl = _NhlGetLayer(wk_id);
	
#if DEBUG_HLUPAGE
        fprintf(stderr,"CreateUpdate -- class %s, hlu_id %d\n",
		rec->class->base_class.class_name,rec->hlu_id);
#endif

        /*
	 * First complete any edits in progress, either in the data profile 
	 * grid, or in the restree.
	 */
	XmLGridEditComplete(rec->data_source_grid->grid);
	XmLGridEditComplete(rec->res_tree->tree);
	UpdateDataProfileFromResTree(page);
	SetInputDataFlag(rec);
        rec->do_setval_cb = False;
	TalkToDataLinks(page,_NgDOSETVALCB,NgNoPage);

        if (rec->state == _hluNOTCREATED) {
		if (! rec->class) {
			NHLPERROR((NhlWARNING,NhlEUNKNOWN,
			   "No class specified for %s: cannot create graphic",
				   NrmQuarkToString(page->qvar)));
			return;
		}
                if (NhlClassIsSubclass(rec->class,NhldataItemClass)) {
                        hlu_id = NhlNULLOBJID;
                }
                else {
                        hlu_id = CreatePreviewInstance(page,wk_id);
                        if (hlu_id <= NhlNULLOBJID) {
                                NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                                     "Could not create preview for %s graphic",
                                           NrmQuarkToString(page->qvar)));
                                rec->hlu_id = NhlNULLOBJID;
                                return;
                        }
                }
                rec->hlu_id = hlu_id;
                rec->state = _hluPREVIEW;
                rec->res_tree->preview_instance = True;
		if (rec->new_data && propagate_profile)
			TalkToDataLinks(page,_NgDATAPROFILE,NgNoPage);
        }
        else  if (rec->state == _hluPREVIEW) {
                XmString xmstring;

                rec->preview_destroy = True;
                if (rec->hlu_id > NhlNULLOBJID) {
                        NgDestroyPreviewGraphic(rec->go->base.id,rec->hlu_id);
		}
		DestroyPreviewDataObjects(page);
		rec->preview_destroy = False;
                hlu_id = CreateInstance(page,wk_id);
                if (hlu_id <= NhlNULLOBJID) {
                        rec->hlu_id = NhlNULLOBJID;
                        rec->state = _hluPREVIEW;
                }
                else {
                        rec->hlu_id = hlu_id;
                        rec->state = _hluCREATED;
                
                        xmstring = NgXAppCreateXmString
                                (rec->go->go.appmgr,"Update");
                        XtVaSetValues(rec->create_update,
                                      XmNlabelString,xmstring,
                                      NULL);
                        NgXAppFreeXmString(rec->go->go.appmgr,xmstring);
                        rec->res_tree->preview_instance = False;
                }

		if (rec->new_data) {
			for (i = 0; i < rec->data_profile->n_dataitems; i++) {
				NgVarData vdata = 
					rec->data_profile->ditems[i]->vdata;
				if (vdata->set_state == _NgEXPRESSION &&
				    vdata->qexpr_var) {
					NgDeleteExpressionVarData
						(rec->go->base.id,vdata);
				}
			}
			if (propagate_profile)
				TalkToDataLinks(page,_NgDATAPROFILE,NgNoPage);
		}
		if (rec->data_profile) {
			for (i = 0; i < rec->data_profile->n_dataitems; i++)
			       rec->data_profile->ditems[i]->vdata->cflags =0; 
		}
		rec->new_data = False;
        }
        else {
                UpdateInstance(page,wk_id,do_draw);
                already_created = True;
		if (rec->new_data) {
			for (i = 0; i < rec->data_profile->n_dataitems; i++) {
				NgVarData vdata = 
					rec->data_profile->ditems[i]->vdata;
				if (vdata->set_state == _NgEXPRESSION &&
				    vdata->qexpr_var) {
					NgDeleteExpressionVarData
						(rec->go->base.id,vdata);
				}
			}
			if (propagate_profile)
				TalkToDataLinks(page,_NgDATAPROFILE,NgNoPage);
		}
		if (rec->data_profile) {
			for (i = 0; i < rec->data_profile->n_dataitems; i++)
			       rec->data_profile->ditems[i]->vdata->cflags = 0;
		}
		rec->new_data = False;
        }
        if (rec->hlu_id > NhlNULLOBJID && ! already_created) {
#if 0
                NhlArgVal sel,user_data;
                
                NhlINITVAR(sel);
                NhlINITVAR(user_data);
                sel.lngval = 0;
		rec->setval_info.pid = page->id;
		rec->setval_info.goid = page->go->base.id;
                user_data.ptrval = &rec->setval_info;
                rec->setval_cb = _NhlAddObjCallback
                        (_NhlGetLayer(rec->hlu_id),_NhlCBobjValueSet,
                         sel,SetValCB,user_data);
#endif
		NgResTreeResUpdateComplete(rec->res_tree,rec->hlu_id,False);
		NgResTreeInstallSetValCB(rec->res_tree,rec->hlu_id,True);
        }
#if 0
        NgResTreeResUpdateComplete(rec->res_tree,rec->hlu_id,False);
#endif
	rec->do_setval_cb = True;
	TalkToDataLinks(page,_NgDOSETVALCB,NgNoPage);

	l = _NhlGetLayer(rec->hlu_id);
	if (l && _NhlIsView(l)) {
		if (do_draw && rec->state == _hluCREATED) {
			NgWksObj	wks = NULL;
			int 	draw_id = _NhlTopLevelView(rec->hlu_id);
			NhlLayer 	drawl = _NhlGetLayer(draw_id);
		
			if (drawl && wl) {
				NgHluData hdata = 
					(NgHluData) wl->base.gui_data2;
				wks = hdata ? (NgWksObj) hdata->gdata : NULL;
			}
			if (wks) {
				NgSetSelectedXwkView(wks->wks_wrap_id,draw_id);
			}
		}
	}

	return;
}

int GetWorkstation
(
        brPage		*page,
        NhlBoolean	*work_created
        )
{
	brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec *) pdp->type_rec;
        int wk_id;
	NhlLayer l = _NhlGetLayer(rec->hlu_id);
        
        if (! (NhlClassIsSubclass(rec->class,NhlviewClass) ||
	       NhlClassIsSubclass(rec->class,NhlgraphicStyleClass) ) ) {
                wk_id = NhlNULLOBJID;
        }
        else if (l) {
		wk_id = l->base.wkptr->base.id;
	}
	else {
                wk_id = NgAppGetSelectedWork
                        (page->go->go.appmgr,True,work_created);
        }
        return wk_id;
}

static void CreateUpdateCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
        brPage		*page = (brPage *)udata;
        int		wk_id;
        NhlBoolean	work_created;

#if DEBUG_HLUPAGE
        fprintf(stderr,"in CreateUpdateCB\n");
#endif
        
        wk_id = GetWorkstation(page,&work_created);
	CreateUpdate(page,wk_id,True,True);

        return;
}

static void AutoUpdateCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
        brPage		*page = (brPage *)udata;
	brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec *) pdp->type_rec;
        Boolean		set;

#if DEBUG_HLUPAGE
        fprintf(stderr,"in AutoUpdateCB\n");
#endif

        XtVaGetValues(w,
                      XmNset,&set,
                      NULL);
        
        rec->do_auto_update = set;
        
        return;
}

static NhlErrorTypes HluPageMessageNotify (
        brPage *page
        )
{
        brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec	*)pdp->type_rec;
        NgHluPage 	*pub = &rec->public;
        
        
#if DEBUG_HLUPAGE
        fprintf(stderr,"in hlu page message notify\n");
#endif
	GetPageMessages(page,True);

        if (rec->new_data &&
	    (rec->state == _hluNOTCREATED || rec->do_auto_update)) {
		int		wk_id;
		NhlBoolean 	work_created;
                
                wk_id = GetWorkstation(page,&work_created);
                
		CreateUpdate(page,wk_id,True,True);

        }
        
        return NhlNOERROR;
}
static NhlPointer PublicHluPageData (
        brPage *page
        )
{
        brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec	*)pdp->type_rec;
        NgHluPage 	*pub = &rec->public;
        
#if DEBUG_HLUPAGE
        fprintf(stderr,"in public hlu page data\n");
#endif
        
        return (NhlPointer) pub;
}
static void SetValuesCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
        brPage		*page = (brPage *)udata;
	brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec *) pdp->type_rec;

#if	0
	NgResTreeSetValues(rec->res_tree);
#endif
	return;
}
static void
FreeHluSaveState
(
	brHluSaveState hs_state
)
{
	if (hs_state->data_object_count)
		NhlFree(hs_state->data_objects);
	FreeDataLinks(hs_state->datalinks);
	NgFreeDataProfile(hs_state->data_profile);
	NhlFree(hs_state);
	return;
}

static void
SaveHluState
(
	brPage	*page
)
{
	brHluPageRec *rec = (brHluPageRec *)page->pdata->type_rec;
	NhlLayer l = _NhlGetLayer(rec->hlu_id);
	int hlu_id,count,*hlu_array = NULL;
	int nclstate;
	brHluSaveState hs_state;
	if (! l)
		return;
	NhlVAGetValues(page->go->go.appmgr,
		NgNappNclState,	&nclstate,
		NULL);

	if (! NclSymbolDefined(NrmQuarkToString(page->qvar)))
		return;

	hlu_id = NgNclGetHluObjId
                (nclstate,NrmQuarkToString(page->qvar),&count,&hlu_array);
	if (hlu_id < NhlNOERROR)
		return;

	if (count > 1) {
		NhlFree(hlu_array);
	}
	if ((rec->state == _hluPREVIEW && hlu_id != NhlNULLOBJID) ||
	    (rec->state == _hluCREATED && hlu_id != rec->hlu_id))
		return;
/*
 * initial stab at saving state; this is just the most important stuff.
 */
	hs_state = NhlMalloc(sizeof(brHluSaveStateRec));
	hs_state->class_name = rec->public.class_name;
	hs_state->plot_style = rec->public.plot_style;
	hs_state->plot_style_dir = rec->public.plot_style_dir;
	hs_state->data_profile = rec->data_profile;
	hs_state->hlu_id = rec->hlu_id;
	hs_state->state = rec->state;
	hs_state->do_auto_update = rec->do_auto_update;
	hs_state->data_object_count = rec->data_object_count;
	hs_state->data_objects = rec->data_objects;
	hs_state->datalinks = rec->datalinks;
	hs_state->has_input_data = rec->has_input_data;
	rec->datalinks = NULL;
	rec->data_object_count = 0;
	rec->data_profile = rec->public.data_profile = NULL;
	rec->data_objects = NULL;

	NgSavePageState(rec->go->base.id,page->id,page->qfile,page->qvar,
			(NhlPointer)hs_state,(NhlFreeFunc) FreeHluSaveState);

	return;
}
static void
DeactivateHluPage
(
	brPage	*page
)
{
	brHluPageRec *rec = (brHluPageRec *)page->pdata->type_rec;
	NhlLayer l = _NhlGetLayer(rec->hlu_id);
        int i;

	GetPageMessages(page,False);

/*
 * if the object has already been destroyed its callbacks will have been
 * removed. 
 */
        if (l) {
		SaveHluState(page);
#if 0
		if (rec->setval_cb) {
			_NhlCBDelete(rec->setval_cb);
		}
#endif
		if (rec->destroy_cb) {
			NgCBWPDestroy(rec->destroy_cb);
		}
	}

	rec->destroy_cb = NULL;
#if 0
	rec->setval_cb = NULL;
#endif

	/* 
	 * setting the last parameter False causing this routine to uninstall
	 */
        NgResTreeInstallSetValCB(rec->res_tree,rec->hlu_id,False);

        if (rec->create_update)
                XtRemoveCallback(rec->create_update,
                                 XmNactivateCallback,CreateUpdateCB,page);
        if (rec->auto_update)
                XtRemoveCallback(rec->auto_update,
                                 XmNvalueChangedCallback,AutoUpdateCB,page);
        rec->state = _hluNOTCREATED;
	rec->class = NULL;
        rec->hlu_id = NhlNULLOBJID;
	rec->app_id = NhlNULLOBJID;
        rec->do_auto_update = False;
        rec->public.class_name = NULL;
        rec->public.plot_style = NULL;
	rec->public.plot_style_dir = NULL;
        rec->new_data = True;
        rec->has_input_data = False;

	if (rec->res_tree) {
		XtUnmanageChild(rec->res_tree->tree);
	}
        
        for (i=0; i <  rec->data_object_count; i++) {
		NhlFree(rec->data_objects[i]);
	}
	if (rec->data_object_count)
		NhlFree(rec->data_objects);
	rec->data_objects = NULL;
	rec->data_object_count = 0;
	FreeDataLinks(rec->datalinks);
	rec->datalinks = NULL;
	

	NgFreeDataProfile(rec->data_profile);
	rec->data_profile = rec->public.data_profile = NULL;
        rec->activated = False;
}

static void DestroyHluPage
(
	NhlPointer data
)
{
	brHluPageRec	*rec = (brHluPageRec *)data;
        int i;
	NhlLayer l = _NhlGetLayer(rec->hlu_id);
        
	
        NgDestroyDataSourceGrid(rec->data_source_grid);
        NgDestroyResTree(rec->res_tree);
        
        NhlFree(data);
	return;
}

static void
AdjustHluPageGeometry
(
        NhlPointer	data
)
{
	brPage	*page = (brPage *) data;
        brHluPageRec	*rec;
	Dimension		w,h,y,twidth,theight;
        Dimension		avail_width,avail_height;
        
	rec = (brHluPageRec *)page->pdata->type_rec;
        
	twidth = 0;
	theight = 0;
        w = 0;
        y = 0;
        h = 0;
        if (rec->data_source_grid && 
	    XtIsManaged(rec->data_source_grid->grid)) {
                XtVaGetValues(rec->data_source_grid->grid,
                              XmNwidth,&w,
                              XmNy,&y,
                              XmNheight,&h,
                              NULL);
        }
	twidth = w;
        
        if (rec->res_tree)
                XtVaGetValues(rec->res_tree->tree,
                              XmNwidth,&w,
                              XmNy,&y,
                              XmNheight,&h,
                              NULL);
        
	twidth = MAX(w,twidth);
        theight = y + h;
        
        NgSetFolderSize(page->pdata->pane,
                        twidth,theight,&avail_width,&avail_height);
	
	return;
}

static NhlErrorTypes UpdateHluPage
(
        brPage *page
)
{
        brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec	*)pdp->type_rec;
        NgHluPage 	*pub = &rec->public;
        int		wk_id;
        NhlBoolean	work_created;

#if DEBUG_HLUPAGE
        fprintf(stderr,"in UpdateHluPage\n");
#endif
        wk_id = GetWorkstation(page,&work_created);

	CreateUpdate(page,wk_id,False,False);

        return NhlNOERROR;

}
        
static NhlErrorTypes ResetHluPage
(
        brPage *page
        )
{
        brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec	*)pdp->type_rec;
        NgHluPage 	*pub = &rec->public;
	int		hlu_id,i;
	NhlBoolean	preview = rec->state < _hluCREATED;
	NhlBoolean	reset;
        
#if DEBUG_HLUPAGE
        fprintf(stderr,"in updata hlu page\n");
#endif
	GetPageMessages(page,False);

        if (! pub->class_name) {
               NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                          "no class specified for graphic variable %s",
                          NrmQuarkToString(page->qvar)));
                return NhlFATAL;
        } 
          

#if DEBUG_HLUPAGE
        fprintf(stderr,"%s: %d\n",pub->class_name,rec->hlu_id);
#endif

        if (rec->hlu_id > NhlNULLOBJID)
                rec->class = NhlClassOfObject(rec->hlu_id);
        else
                rec->class = NgNclHluClassPtrFromName(rec->nclstate,
                                                      pub->class_name);
        
        if (!rec->class) {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                           "%s is not a user instantiable class",
                           pub->class_name));
                return NhlFATAL;
        }

	if (! (NhlClassIsSubclass(rec->class,NhldataCommClass) ||
		NhlClassIsSubclass(rec->class,NhldataItemClass))) {
		if (XtIsManaged(rec->data_source_grid->grid))
			XtUnmanageChild(rec->data_source_grid->grid);
	}

	if (! pub->data_profile) {
		NhlString name;
		name = NgHasDataProfile(rec->go,pub->class_name) ?
			pub->class_name : NULL;
		pub->data_profile = NgNewDataProfile(rec->go,name);
		if (! pub->data_profile) {
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				   "error getting data profile for %s",
				   pub->class_name));
			return NhlFATAL;
		}
	}
		
	rec->data_profile = pub->data_profile;
	if (rec->data_profile->n_dataitems) {
		int mix = rec->data_profile->master_data_ix;
		int datavar_count = 0;
		NgUpdateDataSourceGrid
			(rec->data_source_grid,page->qvar,rec->data_profile);
		if (! XtIsManaged(rec->data_source_grid->grid))
			XtManageChild(rec->data_source_grid->grid);

		SetInputDataFlag(rec);

		if (rec->state == _hluCREATED) {
			NhlArgVal       sel,udata;

			NhlINITVAR(sel);
			NhlINITVAR(udata);
			udata.ptrval = &rec->setval_info;

			for (i = 0; i < rec->data_profile->n_dataitems; i++) {
				NgDataItem ditem = 
					rec->data_profile->ditems[i];
				if (! ditem->hlu_id)
					continue;
				sel.lngval = ditem->resq;
				ditem->svcb = NgCBWPAdd
				(page->go->go.appmgr,CopySVCB,FreeSVCB,
				 _NhlGetLayer(ditem->hlu_id),_NhlCBobjValueSet,
				 sel,DProfSetValCB,udata);
			}
		}
			
	}

/* ResTree */
        
        if (!rec->res_tree) {
                rec->res_tree = NgCreateResTree
                                (page->go,pdp->form,page->qvar,
                                 rec->class,rec->hlu_id);
                XtVaSetValues(rec->res_tree->tree,
                              XmNrightAttachment,XmATTACH_NONE,
                              XmNbottomAttachment,XmATTACH_NONE,
                              XmNtopOffset,8,
                              XmNtopAttachment,XmATTACH_WIDGET,
                              XmNtopWidget,rec->create_update,
                              NULL);
                rec->res_tree->geo_notify = AdjustHluPageGeometry;
                rec->res_tree->preview_instance =
                        rec->state == _hluCREATED ? False : True;
		_NgGOWidgetTranslations(rec->go,rec->res_tree->tree);
        }
        else {
                rec->res_tree->preview_instance =
                        rec->state == _hluCREATED ? False : True;
		XtManageChild(rec->res_tree->tree);
                NgUpdateResTree
                        (rec->res_tree,page->qvar,rec->class,rec->hlu_id);
        }
        XtVaGetValues(pdp->pane->scroller,
                      XmNhorizontalScrollBar,&rec->res_tree->h_scroll,
                      XmNverticalScrollBar,&rec->res_tree->v_scroll,
                      NULL);
        
        rec->res_tree->geo_data = (NhlPointer) page;
        
/* Create Update button */

	if (rec->state == _hluNOTCREATED && rec->class) {
		NhlBoolean work_created;
		
                int wk_id = GetWorkstation(page,&work_created);
		CreateUpdate(page,wk_id,True,True);
	}
        if (rec->state == _hluCREATED) {
		XmString xmstring =
			NgXAppCreateXmString(rec->go->go.appmgr,"Update");
		XtVaSetValues(rec->create_update,
			      XmNlabelString,xmstring,
			      NULL);
		NgXAppFreeXmString(rec->go->go.appmgr,xmstring);
	}
        return NhlNOERROR;

}

static brPageData *
NewHluPage
(
  	NgGO		go,
        brPane		*pane,
	brPage		*page
        )
{
	brPageData	*pdp;
	brHluPageRec	*rec;
        NhlString	e_text;
        int		i;

	if (!(pdp = NhlMalloc(sizeof(brPageData)))) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,_NhlName(go));
                return NULL;
	}
        pdp->dl = NULL;
	pdp->next = pane->hlu_pages;
	pane->hlu_pages = pdp;

	rec = (brHluPageRec*) NhlMalloc(sizeof(brHluPageRec));
	if (! rec) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,_NhlName(go));
                NhlFree(pdp);
                return NULL;
	}
        pdp->type_rec = (NhlPointer) rec;
        rec->go = go;
        
        NhlVAGetValues(rec->go->go.appmgr,
                       NgNappNclState,	&rec->nclstate,
                       NULL);
        
        rec->data_source_grid = NULL;
        rec->res_tree = NULL;
        rec->create_update = NULL;
        rec->auto_update = NULL;
        rec->new_data = True;
        rec->has_input_data = False;
        rec->state = _hluNOTCREATED;
        rec->do_auto_update = False;
        rec->public.data_profile = NULL;
        rec->public.class_name = NULL;
        rec->public.plot_style = NULL;
        rec->public.plot_style_dir = NULL;
        rec->class = NULL;
        rec->hlu_id = NhlNULLOBJID;
	rec->app_id = NhlNULLOBJID;
#if 0
        rec->setval_cb = NULL;
#endif
        rec->destroy_cb = NULL;
        rec->do_setval_cb = False;
	rec->data_profile = NULL;
        
	rec->data_object_count = 0;
	rec->data_objects = NULL;
	rec->datalinks = NULL;
	rec->preview_destroy = False;

	pdp->form = XtVaCreateManagedWidget
		("form",xmFormWidgetClass,pane->folder,
                 XmNy,28,
                 XmNx,2,
                 NULL);
        
	pdp->destroy_page = DestroyHluPage;
	pdp->adjust_page_geo = AdjustHluPageGeometry;
	pdp->deactivate_page = DeactivateHluPage;
        pdp->public_page_data = PublicHluPageData;
        pdp->update_page = UpdateHluPage;
        pdp->reset_page = ResetHluPage;
        pdp->page_focus_notify = HluPageFocusNotify;
        pdp->page_message_notify = HluPageMessageNotify;
        pdp->pane = pane;
        
        rec->data_source_grid = NgCreateDataSourceGrid
                (rec->go,pdp->form,page->qvar,rec->data_profile);
        XtVaSetValues(rec->data_source_grid->grid,
                      XmNbottomAttachment,XmATTACH_NONE,
                      XmNrightAttachment,XmATTACH_NONE,
                      NULL);
        
        rec->create_update = XtVaCreateManagedWidget
                ("Create/Update",xmPushButtonGadgetClass,pdp->form,
                 XmNtopAttachment,XmATTACH_WIDGET,
                 XmNtopWidget,rec->data_source_grid->grid,
                 XmNrightAttachment,XmATTACH_NONE,
                 XmNbottomAttachment,XmATTACH_NONE,
                 NULL);

        rec->auto_update = XtVaCreateManagedWidget
                ("Auto Update",xmToggleButtonGadgetClass,pdp->form,
                 XmNtopAttachment,XmATTACH_WIDGET,
                 XmNtopWidget,rec->data_source_grid->grid,
                 XmNleftAttachment,XmATTACH_WIDGET,
                 XmNleftWidget,rec->create_update,
                 XmNrightAttachment,XmATTACH_NONE,
                 XmNbottomAttachment,XmATTACH_NONE,
                 NULL);
        
        return pdp;
}

static void RestoreHluState(
	brHluPageRec	*rec,
	NgPageSaveState save_state
)
{
	brHluSaveState hs_state = (brHluSaveState) save_state->page_state;

	rec->public.class_name = hs_state->class_name;
	rec->public.plot_style = hs_state->plot_style;
	rec->public.plot_style_dir = hs_state->plot_style_dir;
	rec->hlu_id = hs_state->hlu_id;
	rec->state = hs_state->state;
	rec->do_auto_update = hs_state->do_auto_update;
	rec->public.data_profile = hs_state->data_profile;
	rec->data_object_count = hs_state->data_object_count;
	rec->data_objects = hs_state->data_objects;
	rec->datalinks = hs_state->datalinks;
	rec->has_input_data = hs_state->has_input_data;
	NhlFree(hs_state);
	NhlFree(save_state);

	return;
}        

extern brPageData *
NgGetHluPage
(
	NgGO		go,
        brPane		*pane,
	brPage		*page,
        brPage		*copy_page,
	NgPageSaveState save_state
)
{
	NgBrowse		browse = (NgBrowse)go;
	NgBrowsePart		*np = &browse->browse;
        NhlString		e_text;
	brPageData		*pdp;
	brHluPageRec		*rec,*copy_rec = NULL;
        NrmQuark		*qhlus;
        NhlBoolean		is_hlu = False;
        int			i;
        static int		first = True;
	int			hlu_id,count,*hlu_array = NULL;
	int			nclstate;
        XmString		xmstring;
	NhlBoolean		update = False;
	NhlBoolean		new = False;

        if (QFillValue == NrmNULLQUARK) {
                QFillValue = NrmStringToQuark("_FillValue"); 
		QString = NrmStringToQuark(NhlTString);
		QGenArray = NrmStringToQuark(NhlTGenArray);
		QInteger = NrmStringToQuark(NhlTInteger);
        }

	NhlVAGetValues(go->go.appmgr,
		NgNappNclState,	&nclstate,
		NULL);

	hlu_id = NgNclGetHluObjId
                (nclstate,NrmQuarkToString(page->qvar),&count,&hlu_array);
	if (hlu_id < NhlNOERROR)
		return NULL;
	if (count > 1) {
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                       "variable %s is an array: only handling first element",
                           NrmQuarkToString(page->qvar)));
		NhlFree(hlu_array);
	}

	if (copy_page) {
		copy_rec = (brHluPageRec *) copy_page->pdata->type_rec;
	}
	for (pdp = pane->hlu_pages; pdp != NULL; pdp = pdp->next) {
		if (!pdp->in_use)
		  break;
	}
        if (! pdp) {
                pdp = NewHluPage(go,pane,page);
		new = True;
	}
		
        if (! pdp)
                return NULL;
        page->pdata = pdp;
	pdp->in_use = True;
        
        rec = (brHluPageRec *) pdp->type_rec;
        
	if (hlu_id > NhlNULLOBJID && 
	    ((rec->class = NhlClassOfObject(hlu_id)) != NULL)) {
                NhlArgVal sel,user_data;
                
		rec->hlu_id = hlu_id;
		rec->public.class_name = rec->class->base_class.class_name;
                if (copy_rec && copy_rec->state == _hluPREVIEW)
                        rec->state = _hluPREVIEW;
                else
                        rec->state = _hluCREATED;
#if 0
                NhlINITVAR(sel);
                NhlINITVAR(user_data);
                sel.lngval = 0;
		rec->setval_info.pid = page->id;
		rec->setval_info.goid = page->go->base.id;
                user_data.ptrval = &rec->setval_info;
                rec->setval_cb = _NhlAddObjCallback
                        (_NhlGetLayer(hlu_id),_NhlCBobjValueSet,
                         sel,SetValCB,user_data);
#endif
	}
	if (save_state) {
		RestoreHluState(rec,save_state);
	}

        if (rec->state == _hluCREATED)
                xmstring = NgXAppCreateXmString(rec->go->go.appmgr,"Update");
        else
                xmstring = NgXAppCreateXmString(rec->go->go.appmgr,"Create");
        XtVaSetValues(rec->create_update,
                      XmNlabelString,xmstring,
                      NULL);
        NgXAppFreeXmString(rec->go->go.appmgr,xmstring);

        rec->activated = True;
        XtAddCallback
                (rec->create_update,XmNactivateCallback,CreateUpdateCB,page);
        XtAddCallback
                (rec->auto_update,XmNvalueChangedCallback,AutoUpdateCB,page);
        

        if (! copy_page) {
                if (rec->hlu_id > NhlNULLOBJID)
                        ResetHluPage(page);
		else 
			GetPageMessages(page,True);
		if (new)
			_NgGOWidgetTranslations(go,pdp->form);

                return pdp;
        }

        rec->class = copy_rec->class;
        rec->public.class_name = copy_rec->public.class_name;
        rec->public.plot_style = copy_rec->public.plot_style;
        rec->public.plot_style_dir = copy_rec->public.plot_style_dir;
        rec->hlu_id = copy_rec->hlu_id;
        rec->state = copy_rec->state;
	rec->new_data = copy_rec->new_data;
	rec->has_input_data = copy_rec->has_input_data;
        rec->do_auto_update = copy_rec->do_auto_update;
        
/* ResTree */
        
        if (rec->res_tree) {
		NgDupResTree(page->go,pdp->form,page->qvar,
			     copy_rec->class,copy_rec->hlu_id,
			     rec->res_tree,copy_rec->res_tree);
        }
        else {
                rec->res_tree = NgDupResTree
                        (page->go,pdp->form,page->qvar,
                         copy_rec->class,copy_rec->hlu_id,
                         NULL,copy_rec->res_tree);
                XtVaSetValues(rec->res_tree->tree,
                              XmNrightAttachment,XmATTACH_NONE,
                              XmNbottomAttachment,XmATTACH_NONE,
                              XmNtopOffset,8,
                              XmNtopAttachment,XmATTACH_WIDGET,
                              XmNtopWidget,rec->create_update,
                              NULL);
                rec->res_tree->geo_notify = AdjustHluPageGeometry;
        }
        XtVaGetValues(pdp->pane->scroller,
                      XmNhorizontalScrollBar,&rec->res_tree->h_scroll,
                      XmNverticalScrollBar,&rec->res_tree->v_scroll,
                      NULL);
        rec->res_tree->geo_data = (NhlPointer) page;
        rec->res_tree->preview_instance =
                (rec->state == _hluCREATED) ? False : True;


        if (rec->do_auto_update)
                XtVaSetValues(rec->auto_update,
                              XmNset,True,
                              NULL);
	if (! (rec->class && 
	       (NhlClassIsSubclass(rec->class,NhldataCommClass) ||
		NhlClassIsSubclass(rec->class,NhldataItemClass)))) {
		if (XtIsManaged(rec->data_source_grid->grid))
			XtUnmanageChild(rec->data_source_grid->grid);
	}
	else {
		rec->public.data_profile = NgCopyDataProfile
			(copy_rec->public.data_profile);
		rec->data_profile = rec->public.data_profile;
		if (rec->data_profile->n_dataitems) {
			NgUpdateDataSourceGrid
				(rec->data_source_grid,
				 page->qvar,rec->data_profile);
			if (! XtIsManaged(rec->data_source_grid->grid))
				XtManageChild(rec->data_source_grid->grid);
		}
		if (copy_rec->data_object_count) {
			rec->data_objects = NhlMalloc
				(sizeof(brDataObjInfo) * 
				 copy_rec->data_object_count);
			for (i = 0; i < copy_rec->data_object_count; i++) {
				rec->data_objects[i] = NhlMalloc
					(sizeof(brDataObjInfoRec));
				rec->data_objects[i]->id = 
				copy_rec->data_objects[i]->id;
				rec->data_objects[i]->qname = 
				copy_rec->data_objects[i]->qname;
			}
			rec->data_object_count = copy_rec->data_object_count;
		}
	}
	GetPageMessages(page,True);
	if (new)
		_NgGOWidgetTranslations(go,pdp->form);

        return pdp;
}
