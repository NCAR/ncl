/*
 *      $Id: plotpage.c,v 1.16 2000-05-16 01:59:31 dbrown Exp $
 */
/*******************************************x*****************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		plotpage.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Apr 28 17:20:23 MDT 1999
 *
 *	Description:	
 */

#include <ncarg/ngo/plotpageP.h>
#include <ncarg/ngo/nclstate.h>
#include <ncarg/ngo/graphic.h>
#include <ncarg/ngo/xinteract.h>
#include <ncarg/ngo/stringutil.h>
#include <ncarg/ngo/plotapp.h>
#include <math.h>

#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/PushBG.h>
#include <Xm/ToggleBG.h>
#include <Xm/MessageB.h>
#include <Xm/Protocols.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/XWorkstation.h>
#include <ncarg/hlu/View.h>
#include <ncarg/hlu/WorkstationI.h>
#include <ncarg/hlu/DataItem.h>
#include <ncarg/hlu/MapPlot.h>

static NrmQuark QString = NrmNULLQUARK;
static NrmQuark QGenArray = NrmNULLQUARK;
static NrmQuark QngPlotClass = NrmNULLQUARK;
static NrmQuark Qgrid_number = NrmNULLQUARK;
static NrmQuark Qcorners = NrmNULLQUARK;

static void
XRegionCB(
	NhlArgVal	cbdata,
	NhlArgVal	udata
);

static NhlBoolean IsGribVar
(
        NclApiVarInfoRec	*vinfo
)
{
	int i;

	if (Qgrid_number == NrmNULLQUARK) {
		Qgrid_number = NrmStringToQuark("grid_number");
		Qcorners = NrmStringToQuark("corners");
	}

	for (i = 0; i < vinfo->n_atts; i++) {
		if (vinfo->attnames[i] == Qgrid_number)
			return True;
	}
	return False;
}	

static double
ValToDouble
(
        NclExtValueRec	*val,
        int		index
        )
{
        char *valp = ((char *) val->value) + index * val->elem_size;
        double dout;

        switch (val->type) {
            case NCLAPI_float:
                    dout = (double)*(float*)valp;
                    return dout;
            case NCLAPI_double:
                    dout = *(double*)valp;
                    return dout;
            case NCLAPI_byte:
            case NCLAPI_char:
                    dout = (double)*(char*)valp;
                    return dout;
            case NCLAPI_int:
                    dout = (double)*(int*)valp;
                    return dout;
            case NCLAPI_short:
                    dout = (double)*(short*)valp;
                    return dout;
            case NCLAPI_long:
                    dout = (double)*(long*)valp;
                    return dout;
            default:
                    NHLPERROR((NhlWARNING,NhlEUNKNOWN,
			      "type not supported for coordinate variable\n"));
        }
        return 0.0;
}

static	NclExtValueRec *
ReadCoord
(
        NclApiVarInfoRec	*vinfo,
        NrmQuark		qfile,
	int			dim_ix,
	long			*start,
	long			*finish,
	long			*stride
)
{
	NclExtValueRec *val;

        if (vinfo->coordnames[dim_ix] <= NrmNULLQUARK)
                return NULL;
        
        if (qfile) {
                val = NclReadFileVarCoord(qfile,vinfo->name,
                                          vinfo->coordnames[dim_ix],
                                          start,finish,stride);
        }
        else {
                val = NclReadVarCoord(vinfo->name,
                                      vinfo->coordnames[dim_ix],
                                      start,finish,stride);
        }
        return val;

}

static void
SetPlotAppFunc
(
	NgDataItem ditem,
	NgResInfo  rinfo,
	int	      arg_count,
	NrmQuark      *qarg_names,
	NhlString     *values,
	NhlBoolean    disable,
	NhlBoolean    restore
	)
{
	int i,j;
	NhlBoolean modified = False;
	int newlen = 0;
	char *func,*val;

	/*
	 * if disable or restore, args are ignored
	 */

	if (disable) {
		if (ditem->vdata->set_state != _NgUSER_DISABLED) {
			rinfo->last_state = ditem->vdata->set_state;
			ditem->vdata->set_state = _NgUSER_DISABLED;
		}
		return;
	}
	else if (ditem->vdata->set_state == _NgUSER_DISABLED) {
		ditem->vdata->set_state = rinfo->last_state;
	}
	/* not handling restore yet */

	for (i = 0; i < arg_count; i++) {
		NgArgInfo arg = NULL;
		int len;
		for (j = 0; j < rinfo->argcount; j++) {
			if (qarg_names[i] != rinfo->args[j].qargname)
				continue;
			arg = &rinfo->args[j];
			break;
		}
		if (! arg)
			continue;
		len = strlen(values[i]);
		if (len > strlen(arg->sval))
			arg->sval = NhlRealloc(arg->sval,len+1);
		if (strcmp(arg->sval,values[i])) {
			strcpy(arg->sval,values[i]);
			arg->modified = True;
			modified = True;
		}
	}
	if (! modified)
		return;

	func = NrmQuarkToString(rinfo->qsym);
	newlen = strlen(func) + rinfo->argcount + 2;
	for (i = 0; i < rinfo->argcount; i++) {
                newlen += strlen(rinfo->args[i].sval);
        }
	val = ditem->vdata->expr_val;
	if (newlen > strlen(ditem->vdata->expr_val))
		val = NhlRealloc(val,newlen+1);
	sprintf(val,"%s(",func);
	for (i = 0; i < rinfo->argcount; i++) {
		sprintf(&val[strlen(val)],"%s,",rinfo->args[i].sval);
	}
	if (rinfo->argcount)
                /* write over last comma */
                sprintf(&val[strlen(val)-1],")");
        else
                sprintf(&val[strlen(val)],")");

	ditem->vdata->expr_val = val;

	ditem->vdata->cflags = _NgALL_CHANGE;
	ditem->vdata->set_state = _NgUSER_EXPRESSION;

	return;
}
	
static void
SetPlotAppFuncs
(
	NgDataProfile dprof,
	NrmQuark      qobject,
	NhlString     func_name,
	int	      arg_count,
	NrmQuark      *qarg_names,
	NhlString     *values,
	NhlBoolean    disable,
	NhlBoolean    restore
	)
{
	NgDataItem ditem;
	NgResInfo  rinfo;
	int i;

	for (i = 0; i < dprof->n_dataitems; i++) {

		ditem = dprof->ditems[i];
		rinfo = ditem->res_info;

		if (! rinfo) 
			continue;

		if (qobject > NrmNULLQUARK &&
		    qobject != ditem->qhlu_name)
			continue;
		
		if (strncmp(ditem->vdata->expr_val,
			    func_name,
			    strlen(func_name)))
			continue;

		SetPlotAppFunc(ditem,rinfo,arg_count,qarg_names,
			       values,disable,restore);
	}
}

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

static void PlotPageFocusNotify (
        brPage *page,
        NhlBoolean in
        )
{
        return;
}


static void PostDataProfileMessage
(
        brPage		*page,
	NrmQuark	qname
)
{
	brPageData	*pdp = page->pdata;
	brPlotPageRec	*rec = (brPlotPageRec *) pdp->type_rec;

	NgPostPageMessage(page->go->base.id,page->id,_NgDATAPROFILE,
			  _brPLOTVAR,NrmNULLQUARK,qname,
			  _NgDATAPROFILE,rec->data_profile,True,NULL,True);
	return;

}


static void SetInputDataFlag
(
	brPlotPageRec	*rec
)
{
	int 		i,datavar_count = 0;

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
			case _NgUSER_EXPRESSION:
				if (ditem->vdata->expr_val)
					datavar_count++;
				else if (ditem->required)
					rec->has_input_data = False;
				break;
			case _NgBOGUS_EXPRESSION:
			case _NgUSER_DISABLED:
				if (ditem->required)
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

	return;
}

static NhlErrorTypes GetDataProfileMessage
(
        brPage *page,
	NgPageMessage   message
        )
{
        brPageData	*pdp = page->pdata;
	brPlotPageRec	*rec = (brPlotPageRec	*)pdp->type_rec;
        NgPlotPage 	*pub = &rec->public;
	NgDataProfile	dprof;

	dprof = (NgDataProfile) message->message;

	if (rec->data_profile) {
		NgTransferDataProfileInfo(rec->data_profile,dprof);
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

		name = NgHasDataProfile(rec->go->base.id,pub->class_name) ?
			pub->class_name : NULL;
		if ( ! (name || dprof->class_name) ||
		    ! strcmp(name,dprof->class_name))
			rec->data_profile = dprof;
		else {
			rec->data_profile = NgNewDataProfile
				(rec->go->base.id,name);
			if (! rec->data_profile) {
				NHLPERROR((NhlFATAL,NhlEUNKNOWN,
					   "error getting data profile for %s",
					   pub->class_name));
				return NhlFATAL;
			}
			NgTransferDataProfileInfo(rec->data_profile,dprof);
		}
	}

	SetInputDataFlag(rec);
	return NhlNOERROR;
}

static NhlErrorTypes GetDoSetValCBMessage
(
        brPage *page,
	NgPageMessage   message
        )
{
        brPageData	*pdp = page->pdata;
	brPlotPageRec	*rec = (brPlotPageRec	*)pdp->type_rec;

	rec->do_setval_cb = message->message ? True : False;
/*
 * If something in this plot object was set by another object while the
 * setval_cb was disabled, then it will not have been updated correctly.
 * So do an update now, to ensure that it is.
 */

	return NhlNOERROR;
}

static NhlErrorTypes GetDataLinkReqMessage
(
        brPage *page,
	NgPageMessage   message
        )
{
        brPageData	*pdp = page->pdata;
	brPlotPageRec	*rec = (brPlotPageRec	*)pdp->type_rec;
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
	brPlotPageRec	*rec = (brPlotPageRec	*)pdp->type_rec;
	NgVarData	vdata;

	vdata = (NgVarData) message->message;

	NgSetDataProfileVar(rec->data_profile,vdata,False,True);

	SetInputDataFlag(rec);

	if (rec->new_data && page->qvar && rec->data_profile && 
	    rec->func_grid) {
		NgUpdateFuncGrid
			(rec->func_grid,
			 page->qvar,rec->data_profile);
	}
	
	if (message->message_free)
		(*message->message_free)(message->message);
	
	return NhlNOERROR;
}

void GetGribDimGrids
(
	NgVarData	vdata,
	NrmQuark	*qgridlon,
	NrmQuark	*qgridlat
	)
{
	NclExtValueRec *val;
	int grid_num;
	char buf[32];

	val = NclReadFileVarAtt(vdata->qfile,vdata->qvar,Qgrid_number);
	grid_num = (int) ValToDouble(val,0);
	NclFreeExtValue(val);

	sprintf(buf,"gridlat_%d",grid_num);
	*qgridlat = NrmStringToQuark(buf);
	sprintf(buf,"gridlon_%d",grid_num);
	*qgridlon = NrmStringToQuark(buf);
	return;
}

static NhlBoolean GetGribDataExtent
(
	NgVarData	vdata,
	int		lon_ix,
	int		lat_ix,
	float		*lmin,
	float		*rmax,
	float		*bmin,
	float		*tmax,
	NhlMapLimitMode *lmode
)
{
	NrmQuark qlon,qlat;
	NclExtValueRec *lonval,*latval;
	int fdim_size,sdim_size;

	GetGribDimGrids(vdata,&qlon,&qlat);

	if (! (qlon && qlat))
		return False;

	lonval = NclReadFileVar(vdata->qfile,qlon,NULL,NULL,NULL);
	latval = NclReadFileVar(vdata->qfile,qlat,NULL,NULL,NULL);
	
	if (! (lonval && latval))
		return False;

	fdim_size = lonval->dim_sizes[1];

	*lmin = ValToDouble(lonval,fdim_size * vdata->start[lat_ix] +
			    vdata->start[lon_ix]);
	*bmin = ValToDouble(latval,fdim_size * vdata->start[lat_ix] +
			    vdata->start[lon_ix]);
	*rmax = ValToDouble(lonval,fdim_size * vdata->finish[lat_ix] +
			    vdata->finish[lon_ix]);
	*tmax = ValToDouble(latval,fdim_size * vdata->finish[lat_ix] +
			    vdata->finish[lon_ix]);

	*lmode = NhlCORNERS;

	NclFreeExtValue(lonval);
	NclFreeExtValue(latval);

	return True;
}

static NhlBoolean GetDataExtent
(
	NgVarData	vdata,
	float		*lmin,
	float		*rmax,
	float		*bmin,
	float		*tmax,
	NhlMapLimitMode *lmode
)
{
	NclApiVarInfoRec	*vinfo = NULL;
	int i,lon_ix = -999,lat_ix = -999;
	NclExtValueRec *val;
	float tmp;

	for (i = 0; i < vdata->ndims; i++) {
		if (vdata->order_ix[i] == vdata->ndims - 1)
			lon_ix = i;
		if (vdata->order_ix[i] == vdata->ndims - 2)
			lat_ix = i;
	}
	if (lat_ix < 0 || lon_ix < 0)
		return False;

	if (! vdata->dl) {
		if (vdata->qfile > NrmNULLQUARK)
			vdata->dl = NclGetFileVarInfo
				(vdata->qfile,vdata->qvar);
		else 	
			vdata->dl = NclGetVarInfo(vdata->qvar);
		if (! vdata->dl)
			return False;
	}
	vinfo = vdata->dl->u.var;

	val = ReadCoord(vinfo,vdata->qfile,lon_ix,NULL,NULL,NULL);

	if (! val) {
		if (IsGribVar(vinfo) && vdata->qfile != NrmNULLQUARK) {
			return GetGribDataExtent(vdata,lon_ix,lat_ix,
						 lmin,rmax,bmin,tmax,lmode);
		}
		else
			return False;
	}

	*lmin = ValToDouble(val,vdata->start[lon_ix]);
	*rmax = ValToDouble(val,vdata->finish[lon_ix]);
	if (*lmin > *rmax) {
		tmp = *lmin;
		*lmin = *rmax;
		*rmax = tmp;
	}
	NclFreeExtValue(val);

	val = ReadCoord(vinfo,vdata->qfile,lat_ix,NULL,NULL,NULL);
	if (! val) 
		return False;



	*bmin = ValToDouble(val,vdata->start[lat_ix]);
	*tmax = ValToDouble(val,vdata->finish[lat_ix]);
	if (*bmin > *tmax) {
		tmp = *bmin;
		*bmin = *tmax;
		*tmax = tmp;
	}
	NclFreeExtValue(val);

	*lmode = NhlLATLON;

	return True;
}
	
static NhlErrorTypes HandleDataVarUpdateMessage
(
        brPage		*page,
	NgPageMessage   message,
	NhlBoolean	update
        )
{
        brPageData	*pdp = page->pdata;
	brPlotPageRec	*rec = (brPlotPageRec	*)pdp->type_rec;
	int		data_ix;

	data_ix = (int) message->message;

	if (data_ix >= 0 && data_ix < rec->data_var_grid->plotdata_count)
		rec->new_data = True;
	
	if (message->message_free)
		(*message->message_free)(message->message);
	
	if (rec->new_data) {
		NgVarData	vdata;
		float		lmin,rmax,bmin,tmax;
		NhlMapLimitMode lmode;
		char buf0[32],buf1[32],buf2[32],buf3[32],buf4[32];
		NhlString values[5];
		NrmQuark qargs[5];
		NgVarData	ovdata;
		float		olmin,ormax,obmin,otmax;
		NhlMapLimitMode olmode;

		values[0] = buf0;
		values[1] = buf1;
		values[2] = buf2;
		values[3] = buf3;
		values[4] = buf4;

		vdata = rec->data_var_grid->plotdata[data_ix].vdata;
		if (! GetDataExtent(vdata,&lmin,&rmax,&bmin,&tmax,&lmode))
			return NhlWARNING;
		/*
		 * Since latlon mode does not give the same results as the
		 * the NDC mode used when selecting data using the rubber
		 * box, we must be sure not to switch to LATLON mode unless
		 * necessary
		 */

		ovdata = rec->data_profile->plotdata[data_ix].vdata;
		if (! GetDataExtent
		    (ovdata,&olmin,&ormax,&obmin,&otmax,&olmode))
			return NhlWARNING;

		if (lmode == NhlLATLON) {

			if (! (lmin == olmin && rmax == ormax)) {
			
				/*
				 * Set the "NgAdjustLongitude..." functions
				 */

				sprintf(values[0],"%f",lmin);
				sprintf(values[1],"%f",rmax);
				qargs[0] = NrmStringToQuark("start_lon");
				qargs[1] = NrmStringToQuark("end_lon");
				SetPlotAppFuncs(rec->data_profile,NrmNULLQUARK,
						"NgAdjustLongitude",
						2,qargs,values,False,False);
			}
		}

		if (! (lmin == olmin && rmax == ormax &&
		       bmin == obmin && tmax == otmax &&
		       lmode == olmode)) {
			/*
			 * Set the "NgSetMapLimits" function
			 */
			if (lmode == NhlCORNERS) {
				sprintf(values[0],"%d",lmode);
			}
			else {
				sprintf(values[0],"%d",-1);
			}
			sprintf(values[1],"%f",lmin);
			sprintf(values[2],"%f",rmax);
			sprintf(values[3],"%f",bmin);
			sprintf(values[4],"%f",tmax);
			qargs[0] = NrmStringToQuark("limit_mode");
			qargs[1] = NrmStringToQuark("left_min");
			qargs[2] = NrmStringToQuark("right_max");
			qargs[3] = NrmStringToQuark("bottom_min");
			qargs[4] = NrmStringToQuark("top_max");
			SetPlotAppFuncs(rec->data_profile,NrmNULLQUARK,
					"NgSetMapLimits",5,qargs,values,
					False,False);
		}
	}

		
	return NhlNOERROR;
}

static NhlErrorTypes GetPageMessages
(
        brPage 		*page,
	NhlBoolean	update
        )
{
	NgPageMessage   *messages;
	int 		i,count;
	NhlErrorTypes	ret = NhlNOERROR,subret = NhlNOERROR;

	count = NgRetrievePageMessages
		(page->go->base.id,_brPLOTVAR,
		 NrmNULLQUARK,page->qvar,&messages);

	/*
	 * Reading in this order should ensure that Create messages are
	 * parsed and acted on before data update messages.
	 */
	for (i = count - 1; i >= 0; i--) {
		switch (messages[i]->mtype) { 
		case _NgDATAVARUPDATE:
			subret = HandleDataVarUpdateMessage
				(page,messages[i],update);
			ret = MIN(ret,subret);
			break;
		case _NgPLOTCREATE:
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,"internal error"));
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

static NhlBoolean GetDataVal
(
        brPlotPageRec   *rec,
	NgDataItem	ditem,
	NhlPointer 	*value,
	NrmQuark 	*type,
	NhlBoolean 	preview
)
{
	int i;
	NclExtValueRec *val = NULL;
	NhlString type_str;
	NgVarData vdata = ditem->vdata;


	*value = NULL;
	*type = NrmNULLQUARK;

	if (! vdata || vdata->set_state == _NgUNKNOWN_DATA ||
	    vdata->set_state == _NgBOGUS_EXPRESSION ||
		vdata->set_state == _NgUSER_DISABLED)
		return False;


	if (! preview) {
		char buf[1024];

		if (vdata->set_state == _NgEXPRESSION ||
		    vdata->set_state == _NgUSER_EXPRESSION) {
			if (! vdata->qexpr_var) {
				NhlBoolean user = 
					vdata->set_state == _NgEXPRESSION ?
					False : True;
				if (! (vdata->expr_val && 
				       vdata->goid > NhlNULLOBJID))
					return False;
				if (! NgSetExpressionVarData
				    (vdata->goid,vdata,
				     vdata->expr_val,_NgCONDITIONAL_EVAL,user))
					return False;
			}
			sprintf(buf,"%s",
				NrmQuarkToString(vdata->qexpr_var));
			*value = NhlMalloc(strlen(buf) + 1);
			strcpy((char *)*value,buf);
			*type = QString;
			return True;
		}
		else {

			if (! vdata->qvar) {
				if (ditem->required)
					return False;
				sprintf(buf,"null");
				*value = NhlMalloc(strlen(buf) + 1);
				strcpy((char *)*value,buf);
				*type = QString;
				return True;
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
				NHLPERROR((NhlWARNING,NhlEUNKNOWN,
					   "invalid var type\n"));
				return False;
			}
		}
		for (i = 0; i < vdata->ndims; i++) {
			if (abs((vdata->finish[i] - vdata->start[i]) /
				vdata->stride[i]) < 1) {
				sprintf(&buf[strlen(buf)],"%ld,",
					vdata->start[i]);
				continue;
			}
	                sprintf(&buf[strlen(buf)],"%ld:%ld:%ld,",
                        vdata->start[i],vdata->finish[i],vdata->stride[i]);
		}
		/* backing up 1 to get rid of last comma */
		sprintf(&buf[strlen(buf)-1],")");
		*value = NhlMalloc(strlen(buf) + 1);
		strcpy((char *)*value,buf);
		*type = QString;
		return True;
	}

	if (vdata->set_state == _NgEXPRESSION ||
	    vdata->set_state == _NgUSER_EXPRESSION) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"invalid var data set state"));
		return False;
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
		return False;
	}
	if (! val)
		return False;

	type_str = NgHLUTypeString(val->type);
	*value = _NhlCreateGenArray(val->value,type_str,val->elem_size,
				   val->n_dims,val->dim_sizes,!val->constant);
	NclFreeExtValue(val);
	*type = QGenArray;

	return True;
}

static NhlBoolean DataChanged
(
	NgVarData 	vdata
)
{
	NclExtValueRec *oldval = NULL;
	NclExtValueRec *newval = NULL;
	NhlBoolean ret = False;
	NhlBoolean user = vdata->set_state == _NgEXPRESSION ? False : True;

	oldval = NclReadVar(vdata->qexpr_var,NULL,NULL,NULL);

	ret = NgSetExpressionVarData(vdata->goid,vdata,
				     vdata->expr_val,_NgFORCED_EVAL,user);
	if (! ret)
		return False;
	ret = False;

	newval = NclReadVar(vdata->qexpr_var,NULL,NULL,NULL);

	if (! (newval && oldval)) {
		; /* can't do any further testing */
	}
	else if (oldval->totalelements != newval->totalelements ||
	    oldval->elem_size != newval->elem_size) {
		ret = True;
	}
	else if (memcmp(oldval->value,newval->value,
			oldval->totalelements * oldval->elem_size)) {
		ret = True;
	}
	else {
		vdata->cflags = 0;
	}
	if (oldval)
		NclFreeExtValue(oldval);
	if (newval)
		NclFreeExtValue(newval);

	return ret;
}

static NhlBoolean GetDataObjValue
(
        brPlotPageRec   *rec,
        NgDataItem      ditem,
        NhlPointer      *value,
        NrmQuark        *type,
        NhlBoolean      preview
)
{
        NgVarData       vdata;
        NrmQuark qname;
	int id,count;
	int *id_array;

        *value = NULL;
        *type = NrmNULLQUARK;
        vdata = ditem->vdata;

        if (vdata->set_state == _NgEXPRESSION ||
	    vdata->set_state == _NgUSER_EXPRESSION) {
                char buf[1024];
		NhlBoolean user = 
			vdata->set_state == _NgEXPRESSION ? False : True;

                if (! vdata->qexpr_var) {
                        if (! (vdata->expr_val && vdata->goid > NhlNULLOBJID))
                                return False;
                        if (! NgSetExpressionVarData
                            (vdata->goid,vdata,vdata->expr_val,
			     _NgCONDITIONAL_EVAL,user))
                                return False;
                }
		else if (ditem->save_to_compare) {
			if (! (DataChanged(vdata)))
				return False;
		}
                sprintf(buf,"%s",
                        NrmQuarkToString(vdata->qexpr_var));
		id = NgNclGetHluObjId(rec->nclstate,buf,&count,&id_array);
		if (id == NhlNULLOBJID)
			return False;
                *value = NhlMalloc(strlen(buf) + 1);
                strcpy((char *)*value,buf);
                *type = QString;
		if (id_array)
			NhlFree(id_array);
                return True;
        }
        qname = ditem->vdata->qvar;
        if (! qname)
                return False;

	id = NgNclGetHluObjId
		(rec->nclstate,NrmQuarkToString(qname),&count,&id_array);
	if (id == NhlNULLOBJID)
			return False;
        *value = NhlMalloc(strlen(NrmQuarkToString(qname)) + 1);
        strcpy((char *)*value,NrmQuarkToString(qname));
        *type = QString;

	if (id_array) 
		NhlFree(id_array);

        return True;
}

static NhlBoolean GetConfigValue
(
	brPlotPageRec	*rec,
	NgDataItem	ditem,
	NhlPointer 	*value,
	NrmQuark 	*type,
	NhlBoolean 	preview
)
{
	NgVarData 	vdata;
	
	*value = NULL;
	*type = NrmNULLQUARK;
	vdata = ditem->vdata;

	if (! vdata || vdata->set_state == _NgUNKNOWN_DATA ||
	    vdata->set_state == _NgBOGUS_EXPRESSION ||
		vdata->set_state == _NgUSER_DISABLED)
		return False;

	if (vdata->set_state == _NgEXPRESSION ||
	    vdata->set_state == _NgUSER_EXPRESSION) {
		char buf[1024];
		NhlBoolean user = vdata->set_state == _NgEXPRESSION ?
			False : True;

		if (! vdata->qexpr_var) {
			if (! (vdata->expr_val && vdata->goid > NhlNULLOBJID))
				return False;
			if (! NgSetExpressionVarData
			    (vdata->goid,vdata,vdata->expr_val,
			     _NgCONDITIONAL_EVAL,user))
				return False;
		}
		else if (ditem->save_to_compare) {
			if (! DataChanged(vdata))
				return False;
		}
		sprintf(buf,"%s",
			NrmQuarkToString(vdata->qexpr_var));
		*value = NhlMalloc(strlen(buf) + 1);
		strcpy((char *)*value,buf);
		*type = QString;
		return True;
	}
	if (! ditem->get_val)
		return False;

	return (*ditem->get_val)(ditem,value,type,preview);
}

static NhlBoolean GetFillValue
(
	brPlotPageRec	*rec,
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

static NhlBoolean DestroyFirstCB(
        NhlArgVal       cbdata,
        NhlArgVal       udata,
        NhlArgVal       *ret
)
{
	NgHluData 	hdata = (NgHluData) udata.ptrval; 

	if (! hdata)
		return False;

	if (hdata->xregion_cb) {
		_NhlCBDelete(hdata->xregion_cb);
	}

	return True;
}
static void
DestroyCB
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	NgHluData 	hdata = (NgHluData) udata.ptrval; 

#if DEBUG_PLOTPAGE
        fprintf(stderr,"in destroy cb\n");
#endif
	if (hdata->qplotstyle)
		NgDeletePlotAppRef(hdata->qplotstyle);

	NgCBWPDestroy(hdata->destroy_cb);

	NgFreeHluData(hdata);

        return;
}

NhlString SetGribDataRegionString
(
	brPlotPageRec	*rec,
	int		data_ix,
	int		xb_ix,
	int		xe_ix,
	int		yb_ix,
	int		ye_ix,
	NhlBoolean	full_region
	)
{
	NgPlotData pd = &rec->data_var_grid->plotdata[data_ix];
	NgVarData  vdata = pd->vdata;
	char buf[256];
	int i;
	NhlString vdata_string;

	if (! vdata->qvar)
		return NULL;

	buf[0] = '\0';
	if (vdata->qfile) {
		sprintf(buf,"%s->",NrmQuarkToString(vdata->qfile));
	}
	sprintf(&buf[strlen(buf)],"%s(",NrmQuarkToString(vdata->qvar));

	for (i = 0; i < vdata->ndims; i++) {
		if (vdata->order_ix[i] < vdata->ndims - 2) {
			sprintf(&buf[strlen(buf)],"%d,",vdata->start[i]);
		}
		else if (full_region) {
			sprintf(&buf[strlen(buf)],":,");
		}
		else if (vdata->order_ix[i] < vdata->ndims - 1) {
			sprintf(&buf[strlen(buf)],"%d:%d,",yb_ix,ye_ix);
		}
		else if (vdata->order_ix[i] < vdata->ndims) {
			if (xb_ix > xe_ix) {
				sprintf(&buf[strlen(buf)],":,");
			}
			else {
				sprintf(&buf[strlen(buf)],"%d:%d,",
					xb_ix,xe_ix);
			}
		}
	}
			
	/* remove last comma */
	sprintf(&buf[strlen(buf)-1],")"); 

	vdata_string = NhlMalloc(strlen(buf)+1);
	if (! vdata_string) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NULL;
	}
	strcpy(vdata_string,buf);
	
	return vdata_string;
}

NhlString SetVarDataRegionString
(
	brPlotPageRec	*rec,
	int		data_ix,
	float		xb,
	float		xe,
	float		yb,
	float		ye,
	NhlBoolean	full_region
	)
{
	NgPlotData pd = &rec->data_var_grid->plotdata[data_ix];
	NgVarData  vdata = pd->vdata;
	char buf[256];
	int i;
	NhlString vdata_string;
	NclApiVarInfoRec	*vinfo = NULL;

	if (! vdata->qvar)
		return NULL;

	if (! vdata->dl) {
		if (vdata->qfile > NrmNULLQUARK)
			vdata->dl = NclGetFileVarInfo
				(vdata->qfile,vdata->qvar);
		else 	
			vdata->dl = NclGetVarInfo(vdata->qvar);
		if (! vdata->dl) 
			return NULL;
	}
	vinfo = vdata->dl->u.var;

	buf[0] = '\0';
	if (vdata->qfile) {
		sprintf(buf,"%s->",NrmQuarkToString(vdata->qfile));
	}
	sprintf(&buf[strlen(buf)],"%s(",NrmQuarkToString(vdata->qvar));

	for (i = 0; i < vdata->ndims; i++) {
		NrmQuark qcoord = vinfo->coordnames[i];
		if (vdata->order_ix[i] < vdata->ndims - 2) {
			sprintf(&buf[strlen(buf)],"%d,",vdata->start[i]);
		}
		else if (full_region) {
			sprintf(&buf[strlen(buf)],":,");
		}
		else if (vdata->order_ix[i] < vdata->ndims - 1) {
			if (qcoord > NrmNULLQUARK) 
				sprintf(&buf[strlen(buf)],"{%f:%f},",yb,ye);
			else
				sprintf(&buf[strlen(buf)],"%d:%d,",
					(int)yb,(int)(ye + 1.0));
		}
		else if (vdata->order_ix[i] < vdata->ndims) {
			if (xb > xe && qcoord > NrmNULLQUARK) {
				sprintf(&buf[strlen(buf)],":,");
			}
			else if (qcoord > NrmNULLQUARK) {
				sprintf(&buf[strlen(buf)],"{%f:%f},",xb,xe);
			}
			else {
				sprintf(&buf[strlen(buf)],"%d:%d,",
					(int)xb,(int)(xe + 1.0));
			}
		}
	}
			
	/* remove last comma */
	sprintf(&buf[strlen(buf)-1],")"); 

	vdata_string = NhlMalloc(strlen(buf)+1);
	if (! vdata_string) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NULL;
	}
	strcpy(vdata_string,buf);
	
	return vdata_string;
}
	

static void
RegisterCallbacks
(
        brPage		*page,
	int		hlu_id,
	NhlBoolean	preview
)
{
	brPageData	*pdp = page->pdata;
	brPlotPageRec	*rec = (brPlotPageRec *) pdp->type_rec;
	NgHluData 	hdata,whdata;
	NhlArgVal 	sel,user_data;
	NhlLayer	xwk;

	NhlLayer l = _NhlGetLayer(hlu_id);

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
	hdata->draw_req = True;

	if (rec->public.plot_style && ! preview) {
		hdata->qplotstyle = NrmStringToQuark(rec->public.plot_style);
	}
	NhlINITVAR(sel);
	NhlINITVAR(user_data);
	sel.lngval = 0;
	user_data.ptrval = hdata;
	hdata->destroy_cb = NgCBWPAdd
		(page->go->go.appmgr,DestroyFirstCB,NULL,_NhlGetLayer(hlu_id),
		 _NhlCBobjDestroy,sel,DestroyCB,user_data);

	if (! (l->base.wkptr && _NhlIsXWorkstation(l->base.wkptr)))
		return;

	if (hlu_id != _NhlTopLevelView(hlu_id))
		return;
	if (! NhlIsTransform(hlu_id))
		return;

	xwk = NULL;
	whdata = l->base.wkptr->base.gui_data2;
	if (whdata) {
		NgWksObj wobj = (NgWksObj) whdata->gdata;
		
		if (wobj) {
			xwk = _NhlGetLayer(wobj->wks_wrap_id);
		}
	}
	if (! xwk)
		return;

	sel.lngval = hlu_id;
	user_data.intval = page->go->base.id;
	hdata->xregion_cb = _NhlAddObjCallback((NhlLayer)xwk,
			   NgCBXRegionDef,sel,XRegionCB,user_data);

#if DEBUG_PLOTPAGE
	fprintf(stderr,"adding region callback for %s\n",l->base.name);
#endif

	return;
}

static void CreatePreviewGraphic
(        
	brPage	*page,
        int	wk_id,
	int	obj_ix
)
{
	return;
}

static void
CreatePreviewInstance
(
        brPage	*page,
        int	wk_id
)
{
	brPageData	*pdp = page->pdata;
	brPlotPageRec	*rec = (brPlotPageRec *) pdp->type_rec;
	NgDataProfile	dprof = rec->data_profile;
	int		i;

	if (rec->app_id <= NhlNULLOBJID) {
		rec->app_id =  NgNewPlotAppRef
			(page->go->base.id,rec->public.plot_style,
			 rec->public.plot_style_dir,
			 rec->public.plot_style,
			 rec->public.class_name,
			 False);
	}
	
	for (i = 0; i < dprof->obj_count; i++) {
		CreatePreviewGraphic(page,wk_id,i);
	}

	return;
}

static NhlBoolean CreateGraphic
(        
	brPage	*page,
        int	wk_id,
	int	obj_ix,
	NgResData resdata
)
{
        NhlErrorTypes	ret;
	brPageData	*pdp = page->pdata;
	brPlotPageRec	*rec = (brPlotPageRec *) pdp->type_rec;
	NgDataProfile	dprof = rec->data_profile;
	NrmQuark 	qobj_name = dprof->qobjects[obj_ix];
	NhlClass	obj_class = dprof->obj_classes[obj_ix];
	int		i,count;
	NhlBoolean	preview = False;
        NgSetResProc	setresproc[2];
        NhlPointer	setresdata[2];
	char		ncl_name[128];
	NhlString	parent = NULL;
	int		hlu_id;
	NhlBoolean	required_resource_missing = False;
	NhlString 	pstyle_name = NULL;

	count = 0;
	for (i = 0; i < dprof->n_dataitems; i++) {
		NgDataItem ditem = dprof->ditems[i];
		NgVarData vdata;
	
		if (ditem->qhlu_name != qobj_name)
			continue;
		if (ditem->set_only)
			continue;
		vdata = ditem->vdata;
		switch (vdata->set_state) {
		case _NgDEFAULT_VAR:
		case _NgDEFAULT_SHAPE:
		case _NgSHAPED_VAR:
			if (! vdata->qvar) {
				if (ditem->required)
					required_resource_missing = True;
				continue;
			}
			break;
		case _NgEXPRESSION:
		case _NgUSER_EXPRESSION:
		case _NgUNKNOWN_DATA:
			if (! vdata->expr_val) {
				if (ditem->required)
					required_resource_missing = True;
				continue;
			}
			break;
		case _NgBOGUS_EXPRESSION:
		case _NgUSER_DISABLED:
		case _NgVAR_UNSET:
			if (ditem->item_type == _NgMISSINGVAL)
				break;
			if (ditem->required)
				required_resource_missing = True;
			continue;
		}
		
		if (count >= resdata->res_alloced) {
			resdata = NgReallocResData(resdata,count);
			if (! resdata)
				return False;
		}
		resdata->res[count] = ditem->resname;
		switch (ditem->item_type) {
		case _NgMISSINGVAL:
			if (!GetFillValue(rec,ditem,&resdata->values[count],
				&resdata->types[count],preview))
				continue;
			break;
		case _NgCONFIG:
			if (! GetConfigValue
			    (rec,ditem,&resdata->values[count],
			     &resdata->types[count],preview)) {
				if (ditem->required)
					required_resource_missing = True;
				continue;
			}
			break;
		case _NgDATAOBJ:
			if (!GetDataObjValue(rec,ditem,&resdata->values[count],
					     &resdata->types[count],preview)) {
				if (ditem->required)
					required_resource_missing = True;
				continue;
			}
			break;
		case _NgDATAVAR:
                case _NgCOORDVAR:
			if (preview && ! vdata->qvar)
				continue;
			if (! GetDataVal(rec,ditem,&resdata->values[count],
					 &resdata->types[count],preview)) {
				if (ditem->required)
					required_resource_missing = True;
				continue;
			}
			break;
		case _NgSYNTHETIC:
		default:
			continue;
		}
			
		resdata->vdata[count] = vdata;
		count++;
	}
	resdata->res_count = count;
	if (required_resource_missing)
		return False;

        setresproc[0] = NgAddResList;
	setresdata[0] = (NhlPointer)resdata;

	if (rec->app_id) {
		if (resdata->res_count >= resdata->res_alloced) {
			resdata = NgReallocResData(resdata,resdata->res_count);
			if (! resdata)
				return False;
		}
		resdata->res[resdata->res_count] = "objAppObj";
		pstyle_name = NgNclGetHLURef
			(rec->nclstate,rec->app_id);
		resdata->values[resdata->res_count] =
			NhlMalloc(strlen(pstyle_name)+1);
		strcpy((char *)resdata->values[resdata->res_count],
		       pstyle_name);
		resdata->types[resdata->res_count] = QString;
		resdata->res_count++;
	}
	if (NhlClassIsSubclass(obj_class,NhlviewClass) &&
	    wk_id != NhlNULLOBJID ) 
		parent = NgNclGetHLURef(rec->go->go.nclstate,wk_id);
	else
		parent = pstyle_name;

	sprintf(ncl_name,"%s_%s",
		NrmQuarkToString(page->qvar),
		NrmQuarkToString(qobj_name));

	ret = NgCreateGraphic
		(rec->go->base.id,&hlu_id,
		 NrmQuarkToString(qobj_name),ncl_name,parent,
		 obj_class->base_class.class_name,1,setresproc,setresdata);

#if DEBUG_PLOTPAGE
	fprintf(stderr,"created hlu obj with id %d\n", hlu_id);
#endif        
	if (! hlu_id || ret < NhlWARNING) {
		char buf[512];
		sprintf(buf,"%s = %s@_FillValue\n",
			ncl_name,ncl_name);
		(void)NgNclSubmitBlock(rec->nclstate,buf);
		return False;
	}
	else {
		NhlLayer l = _NhlGetLayer(hlu_id);
		int app_id;

		app_id = NgNewPlotAppRef
			(page->go->base.id,rec->public.plot_style,
			 rec->public.plot_style_dir,
			 rec->public.plot_style,
			 rec->public.class_name,
			 False);

		if (app_id != rec->app_id) {
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				   "internal error getting app reference"));
			return False;
		}
			
		rec->hlu_ids[obj_ix] = hlu_id;
		for (i = 0; i < dprof->n_dataitems; i++) {
			NgDataItem ditem = dprof->ditems[i];
			if (ditem->qhlu_name != qobj_name)
				continue;
			ditem->hlu_id = hlu_id;
			if (! ditem->set_only)
				ditem->vdata->cflags = 0;
		}
                for (i = 0; i< resdata->res_count; i++) {
                        if (resdata->values[i]) {
				if (resdata->types[i] == QGenArray)
					NhlFreeGenArray(resdata->values[i]);
				else if (resdata->types[i] == QString)
					NhlFree(resdata->values[i]);
			}
		}
		if (l) {
			char buf[2048] = "";
			sprintf(&buf[strlen(buf)],
				"%s(%d) = %s\n",
				NrmQuarkToString(page->qvar),
				obj_ix,ncl_name);
			sprintf(&buf[strlen(buf)],
				"%s@%s = \"%s\"\n",ncl_name,ndvPLOTNAME,
				NrmQuarkToString(page->qvar));
			sprintf(&buf[strlen(buf)],
				"%s@%s = \"%s\"\n",ncl_name,ndvCLASS,
				obj_class->base_class.class_name);
			if (parent) { /* it's a View class object */
				sprintf(&buf[strlen(buf)],
					"%s@%s = %s\n",ncl_name,ndvWKS,parent);
			}
			(void)NgNclSubmitBlock(rec->go->go.nclstate,buf);
		}
	}
	return True;
}


static NhlBoolean UpdateGraphic
(        
	brPage	*page,
        int	wk_id,
	int	obj_ix,
	NgResData resdata
)
{
        NhlErrorTypes	ret;
	brPageData	*pdp = page->pdata;
	brPlotPageRec	*rec = (brPlotPageRec *) pdp->type_rec;
	NgDataProfile	dprof = rec->data_profile;
	NrmQuark 	qobj_name = dprof->qobjects[obj_ix];
	int		i,count;
        NgSetResProc	setresproc[1];
        NhlPointer	setresdata[1];
	char		ncl_name[128];
	int		setresproc_count,rix;

/*
 * if the graphic doesn't exist, see if the conditions have changed that
 * caused it not to be created originally. For instance, a scalar field
 * might be added to a vector plot after it already exists.
 */
	if (! _NhlGetLayer(rec->hlu_ids[obj_ix])) {
		if (! CreateGraphic(page,wk_id,obj_ix,resdata))
			return False;
		/*
		 * Mark a change in any data profile item whose value is
		 * the object created.
		 */
		sprintf(ncl_name,"%s_%s",
			NrmQuarkToString(page->qvar),
			NrmQuarkToString(qobj_name));
		for (i = 0; i < dprof->n_dataitems; i++) {
			NgDataItem ditem = dprof->ditems[i];
			NgVarData vdata = ditem->vdata;

			if (! (ditem->item_type == _NgDATAOBJ && 
			       vdata->expr_val))
				continue;
			if (strcmp(vdata->expr_val,ncl_name))
				continue;
			vdata->cflags = _NgALL_CHANGE;
			break;
		}
		return True;
	}

	count = 0;
	for (i = 0; i < dprof->n_dataitems; i++) {
		NgDataItem ditem = dprof->ditems[i];
		NgVarData vdata;
	
		if (ditem->qhlu_name != qobj_name)
			continue;
		vdata = ditem->vdata;
		if (! vdata->cflags  && ! ditem->save_to_compare) {
			if (! (ditem->item_type == _NgCOORDVAR ||
			       ditem->item_type == _NgMISSINGVAL))
				continue;
			if (! (ditem->ref_ditem &&
			       ditem->ref_ditem->vdata->cflags))
				continue;
		}
		
		if (count >= resdata->res_alloced) {
			resdata = NgReallocResData(resdata,count);
			if (! resdata)
				return False;
		}
		resdata->res[count] = ditem->resname;
		switch (ditem->item_type) {
		case _NgMISSINGVAL:
			if (!GetFillValue(rec,ditem,&resdata->values[count],
				&resdata->types[count],False))
				continue;
			break;
		case _NgCONFIG:
			if (! GetConfigValue
			    (rec,ditem,&resdata->values[count],
			     &resdata->types[count],False))
				continue;
			break;
		case _NgDATAOBJ:
			GetDataObjValue(rec,ditem,&resdata->values[count],
					&resdata->types[count],False);

			break;
		case _NgDATAVAR:
                case _NgCOORDVAR:
			GetDataVal(rec,ditem,&resdata->values[count],
				   &resdata->types[count],False);
			break;
		case _NgSYNTHETIC:
			/* 
			 * A synthetic resource may cause several real
			 * resources to be set or none at all. Therefore
			 * count may or may not change as a result of the call.
			 */

			NgHandleSyntheticResource
				(rec->go->base.id,
				 NrmStringToQuark(rec->public.plot_style),
				 page->qvar,dprof,obj_ix,i,resdata,&count,
				 False);
#if DEBUG_PLOTPAGE
			fprintf(stderr,
				"synthetic resource: %s\n",ditem->resname);
#endif
			continue;
		default:
			continue;
		}
			
		resdata->vdata[count] = vdata;
		count++;
	}
	if (count == 0) {
		return True;
	}
	resdata->res_count = count;

	sprintf(ncl_name,"%s_%s",
		NrmQuarkToString(page->qvar),
		NrmQuarkToString(qobj_name));

	setresproc[0] = NgAddResList;
	setresdata[0] = (NhlPointer)resdata;
	
	setresproc_count = 1;
	rix = 0;
	ret = NgUpdateGraphic
		(rec->go->base.id,ncl_name,
		 setresproc_count,&setresproc[rix],&setresdata[rix]);
	if (ret < NhlWARNING)
		return False;

/*
 * there's got to be a better way, but if it's a data object we're got
 * to find the plot it belongs to and set the draw_req flag. temporary I hope.
 */
	if (NhlClassIsSubclass(dprof->obj_classes[obj_ix],NhldataItemClass)) {
		for (i = 0; i < dprof->n_dataitems; i++) {
			int j;
			NgDataItem ditem = dprof->ditems[i];
			if (! (ditem->vdata && ditem->vdata->expr_val))
				continue;
			if (strcmp(ncl_name,ditem->vdata->expr_val))
				continue;
			for (j = 0; j < dprof->obj_count; j++) {
				if (dprof->qobjects[j] == ditem->qhlu_name)
					break;
			}
			if (j == dprof->obj_count)
				continue;
			if (NgViewOn(rec->hlu_ids[j])) {
				int top_id = _NhlTopLevelView(rec->hlu_ids[j]);
				NhlLayer tl = _NhlGetLayer(top_id);
				if (tl) {
					NgHluData hdata = 
						(NgHluData) tl->base.gui_data2;
					if (hdata)
						hdata->draw_req = True;
				}
			}
		}
	}

	for (i = 0; i < dprof->n_dataitems; i++) {
		NgDataItem ditem = dprof->ditems[i];
		if (ditem->qhlu_name != qobj_name)
			continue;
		ditem->vdata->cflags = 0;
	}
	for (i = 0; i< resdata->res_count; i++) {
		if (resdata->values[i]) {
			if (resdata->types[i] == QGenArray)
				NhlFreeGenArray(resdata->values[i]);
			else if (resdata->types[i] == QString)
				NhlFree(resdata->values[i]);
		}
	}
	return True;
}

static NhlErrorTypes DoUpdateFunc
(
	brPlotPageRec	*rec,
	NgDataItem 	ditem,
	NhlBoolean	*plot_changed
)
{
	NhlErrorTypes	ret = NhlNOERROR;
	NgVarData 	vdata = ditem->vdata;
	NclExtValueRec	*val = NULL;
	NhlBoolean	user;

	*plot_changed = False;

	if (! vdata)
		return NhlNOERROR;

	if (vdata->set_state == _NgEXPRESSION)
		user = False;
	else if (vdata->set_state == _NgUSER_EXPRESSION)
		user = True;
	else
		return NhlNOERROR;

	if (! (vdata->expr_val && vdata->goid > NhlNULLOBJID))
		return NhlNOERROR;
		
	if (! NgSetExpressionVarData
	    (vdata->goid,vdata,vdata->expr_val,_NgFORCED_EVAL,user)) {
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,
	   "Disabling resource %s@%s; error evaluating expression value:\n %s",
			   NrmQuarkToString(ditem->qhlu_name),
			   NrmQuarkToString(ditem->resq),
			   vdata->expr_val));
		return NhlWARNING;
	}
	val = NclReadVar(vdata->qexpr_var,NULL,NULL,NULL);
	if (! val) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			   "Error reading temporary variable: %s",
			   NrmQuarkToString(vdata->qexpr_var)));
		ret = NhlWARNING;
	}
	else {
		switch (*(int *)val->value) {
		case -1:
			NHLPERROR((NhlWARNING,NhlEUNKNOWN,
				   "Expression %s reports error",
				   vdata->expr_val));
			ret = NhlWARNING;
			break;
		case 0:
			*plot_changed = False;
			break;
		case 1:
			*plot_changed = True;
			break;
		default: 
			break;
		}
		NclFreeExtValue(val);
	}

	/* That's all we need the value for.
	 * Since the post funcs are only evaluated for 
	 * side effects -- immediately blow away the variable
	 */

	NgDeleteExpressionVarData(vdata->goid,vdata);

	return ret;
}
	

static NhlErrorTypes DoUpdateFuncs
(        
	brPlotPageRec	*rec,
	NhlBoolean	init,
	int		obj_ix,
	int		sequence,
	int		*count
)
{
	NhlErrorTypes	subret = NhlNOERROR, ret = NhlNOERROR;
	NgDataProfile	dprof = rec->data_profile;
	NrmQuark 	qobj_name = dprof->qobjects[obj_ix];
	int		i;

	if (! _NhlGetLayer(rec->hlu_ids[obj_ix]))
		return NhlNOERROR; /* it's ok */

	*count = 0;
	for (i = 0; i < dprof->n_dataitems; i++) {
		NgDataItem ditem = dprof->ditems[i];
		NhlBoolean plot_changed;

		if (ditem->qhlu_name != qobj_name)
			continue;
		switch (ditem->item_type) {
		default:
			break;
		case _NgUPDATEFUNC:
			if ((long) sequence !=  (long) ditem->data)
				break;
			if (! init) {
				if (ditem->init_only)
					break;
			}
			subret = DoUpdateFunc(rec,ditem,&plot_changed);
			ret = MIN(ret,subret);
			if (plot_changed)
				(*count)++;
			break;
		}
	}
	return ret;
}

static NhlBoolean DoPlotTreeUpdate
(
	brPage	*page,
	int	obj_ix
)
{
        NhlErrorTypes	ret;
	brPageData	*pdp = page->pdata;
	brPlotPageRec	*rec = (brPlotPageRec *) pdp->type_rec;
	NgDataProfile	dprof = rec->data_profile;
	NrmQuark 	qobj_name = dprof->qobjects[obj_ix];
	char		ncl_name[128];
	NgPlotTreeResDataRec	ptresdata;
        NgSetResProc	setresproc;
        NhlPointer	setresdata;

	if (! _NhlGetLayer(rec->hlu_ids[obj_ix]))
		return True; /* it's ok */

	sprintf(ncl_name,"%s_%s",
		NrmQuarkToString(page->qvar),
		NrmQuarkToString(qobj_name));

	ptresdata.qname = qobj_name;
	ptresdata.plot_tree = rec->plot_tree;
	setresproc = NgPlotTreeAddResList;
	setresdata = &ptresdata;

	ret = NgUpdateGraphic
		(rec->go->base.id,ncl_name,1,
		 &setresproc,&setresdata);
	if (ret < NhlWARNING)
		return False;
	return True;
}

static NhlErrorTypes
CreateInstance
(
        brPage	*page,
        int	wk_id
)
{
	NhlErrorTypes	subret = NhlNOERROR, ret = NhlNOERROR;
	brPageData	*pdp = page->pdata;
	brPlotPageRec	*rec = (brPlotPageRec *) pdp->type_rec;
        int		i,j;
	NgDataProfile	dprof = rec->data_profile;
#if 0
	NhlBoolean	success = True;
#endif
	NgResData	resdata = NgReallocResData(NULL,0);

	if (rec->app_id <= NhlNULLOBJID) {
		rec->app_id = NgNewPlotAppRef
			(page->go->base.id,rec->public.plot_style,
			 rec->public.plot_style_dir,
			 rec->public.plot_style,
			 rec->public.class_name,
			 False);
	}
	
	for (i = 0; i < dprof->obj_count; i++) {
		resdata->res_count = 0;
		if (! CreateGraphic(page,wk_id,i,resdata))
			;
#if 0
			success = False;
#endif
	}
	/*
	 * After the create, call update to handle the set_only resources,
	 * They should be the only ones that have cflags set.
	 */
	for (i = 0; i < dprof->obj_count; i++) {
		resdata->res_count = 0;
		if (! UpdateGraphic(page,wk_id,i,resdata))
			;
	}
	NgFreeResData(resdata);

	/*
	 * Find the max update sequence number
	 */

	if (rec->max_seq_num < 0) {
		for (i = 0; i < dprof->n_dataitems; i++) {
			NgDataItem ditem = dprof->ditems[i];
			if (ditem->item_type == _NgUPDATEFUNC) {
				long seq = (long) ditem->data;
				if (seq > rec->max_seq_num)
					rec->max_seq_num = seq;
			}
		}
	}

	/*
	 * Now call the update functions. These functions may require
	 * the presence of other objects that may not yet have existed at
	 * create time. See explanation of PlotTree updates in 
	 * UpdateInstance. It's a little simpler during a create because
	 * we know the plot is going to be drawn.
	 */

	rec->plot_tree->first_vpon = True;
	for (i = 0; i < dprof->obj_count; i++) {
		if (! NhlClassIsSubclass(dprof->obj_classes[i],NhlviewClass))
			continue;
		if (! DoPlotTreeUpdate(page,i))
			continue;
	}
	for (i = 0; i <= rec->max_seq_num; i++) {
		for (j = 0; j < dprof->obj_count; j++) {
			int count;
			subret = DoUpdateFuncs(rec,True,j,i,&count);
			ret = MIN(ret,subret);
		}
	}
	rec->plot_tree->first_vpon = False;
	for (i = 0; i < dprof->obj_count; i++) {
		if (! NhlClassIsSubclass(dprof->obj_classes[i],NhlviewClass))
			continue;

		if (! DoPlotTreeUpdate(page,i))
			continue;
		RegisterCallbacks(page,rec->hlu_ids[i],False);
	}

	return ret;
/*
 * No explicit draw on create because it is handled automatically by
 * the ViewTree (mwin) create CB.
 */
}

static NhlErrorTypes
UpdateInstance
(
        brPage		*page,
        int		wk_id,
	NhlBoolean	do_draw
)
{
	NhlErrorTypes	subret = NhlNOERROR, ret = NhlNOERROR;
	brPageData	*pdp = page->pdata;
	brPlotPageRec	*rec = (brPlotPageRec *) pdp->type_rec;
	NhlLayer	wl = _NhlGetLayer(wk_id);
	NgDataProfile	dprof = rec->data_profile;
	int		i,j;
	NgResData	resdata = NgReallocResData(NULL,0);
	NhlBoolean	*hlus_on;
	NgHluData	whdata;
	NgWksObj	wks;

	for (i = 0; i < dprof->obj_count; i++) {
		resdata->res_count = 0;
		if (! UpdateGraphic(page,wk_id,i,resdata))
			;
			
		if (NgViewOn(rec->hlu_ids[i]) &&
			(do_draw || resdata->res_count)) {
			int top_id = _NhlTopLevelView(rec->hlu_ids[i]);
			NhlLayer tl = _NhlGetLayer(top_id);
			if (tl) {
				NgHluData hdata = 
					(NgHluData) tl->base.gui_data2;
				if (hdata)
					hdata->draw_req = True;
			}
		}
	}
	NgFreeResData(resdata);
	
	/*
	 * Controlling the vpOn resource at the plot page level presents a
	 * special challenge. Sometimes the plot update functions have to 
	 * turn off a plot component (e.g. map tickmarks when the map is
	 * not in an understood projection). This must override the plot page
	 * setting. But otherwise the plot page setting should rule.
	 * So it is necessary to first set the value. Then do the update
	 * funcs. Then reset the value only if an update func has turned
	 * the component on when the user has set it off via the plot page.
	 * Save the initial state to see if a new draw will be required.
	 */

	hlus_on = NhlMalloc(rec->hlu_count * sizeof(NhlBoolean));

	rec->plot_tree->first_vpon = True;
	for (i = 0; i < dprof->obj_count; i++) {
		if (! NhlClassIsSubclass(dprof->obj_classes[i],NhlviewClass))
			continue;

		NhlVAGetValues(rec->hlu_ids[i],
			       NhlNvpOn,&hlus_on[i],
			       NULL);
		if (! DoPlotTreeUpdate(page,i))
			continue;
	}

	
	/*
	 * Now call the update functions. These functions may require
	 * the presence of other objects that may not yet have existed at
	 * create time.
	 */

	if (rec->max_seq_num < 0) {
		for (i = 0; i < dprof->n_dataitems; i++) {
			NgDataItem ditem = dprof->ditems[i];
			if (ditem->item_type == _NgUPDATEFUNC) {
				long seq = (long) ditem->data;
				if (seq > rec->max_seq_num)
					rec->max_seq_num = seq;
			}
		}
	}

	for (i = 0; i <= rec->max_seq_num; i++) {
		for (j = 0; j < dprof->obj_count; j++) {
			int count;

			subret = DoUpdateFuncs(rec,False,j,i,&count);
			ret = MIN(ret,subret);

			if (count && NgViewOn(rec->hlu_ids[i])) {
				int top_id = _NhlTopLevelView(rec->hlu_ids[j]);
				NhlLayer tl = _NhlGetLayer(top_id);
				if (tl) {
					NgHluData hdata = 
						(NgHluData) tl->base.gui_data2;
					if (hdata)
						hdata->draw_req = True;
				}
			}
		}
	}

	rec->plot_tree->first_vpon = False;
	for (i = 0; i < dprof->obj_count; i++) {
		NhlBoolean new_view_on;
		if (! NhlClassIsSubclass(dprof->obj_classes[i],NhlviewClass))
			continue;

		if (! DoPlotTreeUpdate(page,i))
			continue;

		NhlVAGetValues(rec->hlu_ids[i],
			       NhlNvpOn,&new_view_on,
			       NULL);
		if (new_view_on !=  hlus_on[i]) {
			int top_id = _NhlTopLevelView(rec->hlu_ids[i]);
			NhlLayer tl = _NhlGetLayer(top_id);
			if (tl) {
				NgHluData hdata = 
					(NgHluData) tl->base.gui_data2;
				if (hdata)
					hdata->draw_req = True;
			}
		}
	}
	NhlFree(hlus_on);

	whdata = (NgHluData) wl->base.wkptr->base.gui_data2;
	wks = whdata ? (NgWksObj) whdata->gdata : NULL;

/*
 * There is no auto callback for setvalues yet, so for updates the draw
 * must occur here
 */
	if (wks && do_draw && _NhlIsClass(wl,NhlxWorkstationClass) &&
		! wks->colormap_cb_pending) {
		NgHluData hdata = (NgHluData) wl->base.gui_data2;
		NgWksObj wks = hdata ? (NgWksObj) hdata->gdata : NULL;

		if (wks->auto_refresh) {
			NgDrawGraphic(rec->go->base.id,
				      NrmQuarkToString(page->qvar),True);
		}
	}

	if (wks->colormap_cb_pending) {
		for (i = 0; i < dprof->obj_count; i++) {
			int top_id = _NhlTopLevelView(rec->hlu_ids[i]);
			NhlLayer tl = _NhlGetLayer(top_id);
			if (tl) {
				NgHluData hdata = 
					(NgHluData) tl->base.gui_data2;
				if (hdata)
					hdata->draw_req = False;
			}
		}
	}

        return ret;
}

static void DataConfigOKCB
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

static void
PostDataCompletionDialog(
	brPage		*page
)
{
        brPageData	*pdp = page->pdata;
	brPlotPageRec	*rec = (brPlotPageRec	*)pdp->type_rec;
	char    buf[512] = "";
	Arg	args[50];
	int	nargs;
        XmString xmname,xmtext;
        Widget  help,cancel;
	int status = 0;
	XtAppContext context;
	Widget data_config,shell;
	Atom WM_DELETE_WINDOW;

	sprintf(buf,"%s Requires Configuration",
		NrmQuarkToString(page->qvar));
        xmname = NgXAppCreateXmString(rec->go->go.appmgr,buf);
	xmtext = NgXAppCreateXmString
		(rec->go->go.appmgr,
		 "Undefined data variables: configuration required");
	shell = XmLShellOfWidget(pdp->form);

        nargs = 0;
	XtSetArg(args[nargs],XmNdialogTitle,xmname);nargs++;
	XtSetArg(args[nargs],XmNuserData,page);nargs++;
	XtSetArg(args[nargs],XmNmessageString,xmtext);nargs++;
	XtSetArg(args[nargs],
		 XmNdialogStyle,XmDIALOG_APPLICATION_MODAL);nargs++;

	data_config = XmCreateMessageDialog
		(XtParent(pdp->form),"DataConfig",args,nargs);

	WM_DELETE_WINDOW = XmInternAtom(XtDisplay(pdp->form),
					"WM_DELETE_WINDOW",False);
	XmAddWMProtocolCallback(shell, WM_DELETE_WINDOW, DataConfigOKCB,
		(caddr_t)&status);

	NgXAppFreeXmString(rec->go->go.appmgr,xmname);
	NgXAppFreeXmString(rec->go->go.appmgr,xmtext);

	help = XmMessageBoxGetChild(data_config,XmDIALOG_HELP_BUTTON);
	XtUnmanageChild(help);
	cancel = XmMessageBoxGetChild(data_config,XmDIALOG_CANCEL_BUTTON);
	XtUnmanageChild(cancel);

	XtAddCallback(data_config,
		      XmNokCallback,DataConfigOKCB,&status);

        XtManageChild(data_config);
        
	context = XtWidgetToApplicationContext(pdp->form);
	while (! status || XtAppPending(context))
		XtAppProcessEvent(context, XtIMAll);
	XtDestroyWidget(data_config);

	return;
}

static NhlBoolean
AllDataDefined
(
	NgPlotData plotdata,
	int 	   count
)
{
	int i;

	for (i = 0; i < count; i++) {
		NgPlotData pd = &plotdata[i];
		if (pd->required && pd->vdata->qvar <= NrmNULLQUARK)
			return False;
	}
	return True;
}

static NhlBoolean VDataChanged
(
	brPlotPageRec	*rec
)
{
	int old_count = rec->vdata_count;
	int new_count = rec->data_var_grid->plotdata_count;
	NgPlotData plotdata = rec->data_var_grid->plotdata;
	int i,j;

	if (new_count != old_count)
		return True;

	for (i = 0; i < new_count; i++) {
		NgVarData vdata = plotdata[i].vdata;
		if (rec->vdata[i]->qvar != vdata->qvar ||
		    rec->vdata[i]->qfile != vdata->qfile)
			return True;
		for (j = 0; j < vdata->ndims; j++) {
			if (rec->vdata[i]->start[j] != vdata->start[j] ||
			    rec->vdata[i]->finish[j] != vdata->finish[j] ||
			    rec->vdata[i]->stride[j] != vdata->stride[j])
				return True;
		}
	}

	return False;
}

static NhlErrorTypes UpdateVData
(
	brPlotPageRec	*rec
)
{
	int old_count = rec->vdata_count;
	int new_count = rec->data_var_grid->plotdata_count;
	int i;

	for (i = new_count; i < old_count; i++) {
		NgFreeVarData(rec->vdata[i]);
	}
	rec->vdata = NhlRealloc
		(rec->vdata,new_count * sizeof(NgVarData));
	if (! rec->vdata) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NhlFATAL;
	}
	for (i = old_count; i < new_count; i++) {
		rec->vdata[i] = NgNewVarData();
		if (! rec->vdata[i]) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return NhlFATAL;
		}
	}
	for (i = 0; i < new_count; i++) {
		NgCopyVarData(rec->vdata[i],
			      rec->data_var_grid->plotdata[i].vdata);
	}
	rec->vdata_count = new_count;	

	return NhlNOERROR;
}

static void
CreateUpdate
(
        brPage		*page,
        int		wk_id,
	NhlBoolean	do_draw
)
{
	brPageData	*pdp = page->pdata;
	brPlotPageRec	*rec = (brPlotPageRec *) pdp->type_rec;
        int		i;
	NhlLayer	wl = _NhlGetLayer(wk_id);
	

#if DEBUG_PLOTPAGE
        fprintf(stderr,"CreateUpdate -- class %s, hlu_id %d\n",
		rec->public.class_name);
#endif

        /*
	 * First complete any edits in progress in the datavargrid.
	 */

	XmLGridEditComplete(rec->data_var_grid->grid);
	/*
	 * If all required data has not been defined, do not proceed
	 */
	if (! AllDataDefined(rec->data_var_grid->plotdata,
			     rec->data_var_grid->plotdata_count)) {
		PostDataCompletionDialog(page);
		rec->public.config_required = True;
		return;
	}

	if (VDataChanged(rec) || rec->new_data) {
		UpdateVData(rec);
		NgSetPlotAppDataVars(rec->go->base.id,
				     NrmStringToQuark(rec->public.plot_style),
				     NrmQuarkToString(page->qvar),
				     rec->data_profile,NULL,NULL,
				     rec->vdata_count,rec->vdata,page->qvar);
		if (rec->func_grid)
			NgSynchronizeFuncGridState
				(rec->func_grid);
	}

	SetInputDataFlag(rec);
        rec->do_setval_cb = False;

	if (rec->state == _plotNOTCREATED)
		rec->state = _plotPREVIEW;
        if (rec->state == _plotNOTCREATED) {
		CreatePreviewInstance(page,wk_id);
                rec->state = _plotPREVIEW;
        }
        else  if (rec->state == _plotPREVIEW) {
                XmString xmstring;
#if 0
                rec->preview_destroy = True;
                if (rec->hlu_id > NhlNULLOBJID) {
                        NgDestroyPreviewGraphic(rec->go->base.id,rec->hlu_id);
		}
		rec->preview_destroy = False;
#endif
		CreateInstance(page,wk_id);
		rec->state = _plotCREATED;
		xmstring = NgXAppCreateXmString(rec->go->go.appmgr,"Update");
		XtVaSetValues(rec->create_update,
			      XmNlabelString,xmstring,
			      NULL);
		NgXAppFreeXmString(rec->go->go.appmgr,xmstring);
		if (rec->new_data) {
			for (i = 0; i < rec->data_profile->n_dataitems; i++) {
				NgDataItem ditem =rec->data_profile->ditems[i];
				NgVarData vdata = ditem->vdata;
				if ((vdata->set_state == _NgEXPRESSION ||
				     vdata->set_state == _NgUSER_EXPRESSION) &&
				    vdata->qexpr_var && 
				    ! ditem->save_to_compare) {
					NgDeleteExpressionVarData
						(rec->go->base.id,vdata);
				}
			}
		}
		for (i = 0; i < rec->data_profile->n_dataitems; i++)
			rec->data_profile->ditems[i]->vdata->cflags = 0; 
		rec->new_data = False;

        }
	else if (rec->state == _plotCREATING) {
		rec->state = _plotCREATED;
	}
	else {
                UpdateInstance(page,wk_id,do_draw);
		if (rec->new_data) {
			for (i = 0; i < rec->data_profile->n_dataitems; i++) {
				NgDataItem ditem =rec->data_profile->ditems[i];
				NgVarData vdata = ditem->vdata;
				if ((vdata->set_state == _NgEXPRESSION ||
				     vdata->set_state == _NgUSER_EXPRESSION) &&
				    vdata->qexpr_var && 
				    ! ditem->save_to_compare) {
					NgDeleteExpressionVarData
						(rec->go->base.id,vdata);
				}
			}
		}
		for (i = 0; i < rec->data_profile->n_dataitems; i++)
			rec->data_profile->ditems[i]->vdata->cflags = 0;
		rec->new_data = False;
        }

	if (do_draw && rec->state == _plotCREATED &&
	    _NhlIsClass(wl,NhlxWorkstationClass)) {
		NgHluData hdata = (NgHluData) wl->base.gui_data2;
		NgWksObj wks = hdata ? (NgWksObj) hdata->gdata : NULL;
		if (! wks) 
			return;
		for (i = 0; i < rec->hlu_count; i++) {
			int draw_id = _NhlTopLevelView(rec->hlu_ids[i]);
			NhlLayer drawl = _NhlGetLayer(draw_id);
		
			if (drawl) {
				NgSetSelectedXwkView(wks->wks_wrap_id,draw_id);
			}
		}
	}

	XSync(rec->go->go.x->dpy,False);
	return;
}

static int GetWorkstation
(
        brPage		*page,
        NhlBoolean	*work_created
        )
{
	brPageData	*pdp = page->pdata;
	brPlotPageRec	*rec = (brPlotPageRec *) pdp->type_rec;
        int wk_id = NhlNULLOBJID;
	int i;

	for (i = 0; i < rec->hlu_count; i++) {
		NhlLayer l = _NhlGetLayer(rec->hlu_ids[i]);
        
		if (l && _NhlIsView(l)) {
			wk_id = l->base.wkptr->base.id;
			break;
		}
	}
	if (wk_id <= NhlNULLOBJID) {
                wk_id = NgAppGetSelectedWork
                        (page->go->go.appmgr,True,work_created);
        }
        return wk_id;
}

static void AnalyzeGribMapLocation
(
	int map_id,
	NgVarData vdata,
	float *xloc,
	float *yloc,
	int	*sx_ix,
	int	*fx_ix,
	int	*sy_ix,
	int	*fy_ix
)
{
	float xt[2],yt[2];
	float xmp[2],ymp[2];
	float mpw,mph;
	int status;
	float oor;
	int i,j;
	NrmQuark qlon,qlat;
	NclExtValueRec *lonval,*latval;
	float xlp,xrp,ybp,ytp;
	int lon_ix,lat_ix;
	int lon_ext, lat_ext;
	float tlat,tlon,tdist,mindist;
	int fdim_size,sdim_size;
	int x_ix,y_ix;
	NhlBoolean changed;

	for (i = 0; i < vdata->ndims; i++) {
		if (vdata->order_ix[i] == vdata->ndims - 1)
			lon_ix = i;
		if (vdata->order_ix[i] == vdata->ndims - 2)
			lat_ix = i;
	}
	lat_ext = vdata->finish[lat_ix] - vdata->start[lat_ix];
	lon_ext = vdata->finish[lon_ix] - vdata->start[lon_ix];
		
	NhlVAGetValues(map_id,
		       NhlNmpLeftMapPosF,&xmp[0],
		       NhlNmpRightMapPosF,&xmp[1],
		       NhlNmpBottomMapPosF,&ymp[0],
		       NhlNmpTopMapPosF,&ymp[1],
		       NULL);
	mpw = xmp[1] - xmp[0];
	mph = ymp[1] - ymp[0];
	xlp = (xloc[0] - xmp[0]) / mpw;
	xrp = (xloc[1] - xmp[0]) / mpw;
	ybp = (yloc[0] - ymp[0]) / mph;
	ytp = (yloc[1] - ymp[0]) / mph;

	*sx_ix = vdata->start[lon_ix] + (int) (xlp * lon_ext);
	*fx_ix = vdata->start[lon_ix] + (int) (xrp * lon_ext);
	*sy_ix = vdata->start[lat_ix] + (int) (ybp * lat_ext);
	*fy_ix = vdata->start[lat_ix] + (int) (ytp * lat_ext);
	
#if DEBUG_PLOTPAGE
	fprintf(stderr,"sx %d fx %d sy %d fy %d\n",
		*sx_ix,*fx_ix,*sy_ix,*fy_ix);
#endif

	GetGribDimGrids(vdata,&qlon,&qlat);
	lonval = NclReadFileVar(vdata->qfile,qlon,NULL,NULL,NULL);
	latval = NclReadFileVar(vdata->qfile,qlat,NULL,NULL,NULL);

	NhlNDCToData(map_id,
		     xloc,yloc,2,xt,yt,NULL,NULL,&status,&oor);

	fdim_size = lonval->dim_sizes[1];
	sdim_size = lonval->dim_sizes[0];
	tlat = ValToDouble(latval,fdim_size * *sy_ix + *sx_ix);
	tlon = ValToDouble(lonval,fdim_size * *sy_ix + *sx_ix);
	mindist = (tlon - xt[0]) * (tlon - xt[0]) + 
		(tlat - yt[0]) * (tlat - yt[0]);

	changed = True;
	while (changed) {
		changed = False;
		for (i = MAX(0,*sx_ix - 1); 
		     i < MIN(fdim_size,*sx_ix + 2); i++) {
			for (j = MAX(0,*sy_ix - 1); 
				     j < MIN(sdim_size,*sy_ix+2); j++) {
				if (i == *sx_ix && j == *sy_ix)
					continue;
				tlat = ValToDouble(latval,fdim_size * j + i);
				tlon = ValToDouble(lonval,fdim_size * j + i);
				tdist = (tlon - xt[0]) * (tlon - xt[0]) + 
					(tlat - yt[0]) * (tlat - yt[0]);
				if (tdist >= mindist)
					continue;
				mindist = tdist;
				x_ix = i;
				y_ix = j;
				changed = True;
			}
		}
		if (changed) {
			*sx_ix = x_ix;
			*sy_ix = y_ix;
		}
	}


	tlat = ValToDouble(latval,fdim_size * *fy_ix + *fx_ix);
	tlon = ValToDouble(lonval,fdim_size * *fy_ix + *fx_ix);
	mindist = (tlon - xt[1]) * (tlon - xt[1]) + 
		(tlat - yt[1]) * (tlat - yt[1]);

	changed = True;
	while (changed) {
		changed = False;
		for (i = MAX(0,*fx_ix - 1); 
		     i < MIN(fdim_size,*fx_ix + 2); i++) {
			for (j = MAX(0,*fy_ix - 1); 
				     j < MIN(sdim_size,*fy_ix+2); j++) {
				if (i == *fx_ix && j == *fy_ix)
					continue;
				tlat = ValToDouble(latval,fdim_size * j + i);
				tlon = ValToDouble(lonval,fdim_size * j + i);
				tdist = (tlon - xt[1]) * (tlon - xt[1]) + 
					(tlat - yt[1]) * (tlat - yt[1]);
				if (tdist >= mindist)
					continue;
				mindist = tdist;
				x_ix = i;
				y_ix = j;
				changed = True;
			}
		}
		if (changed) {
			*fx_ix = x_ix;
			*fy_ix = y_ix;
		}
	}
#if DEBUG_PLOTPAGE
	fprintf(stderr,"after - sx %d fx %d sy %d fy %d\n",
		*sx_ix,*fx_ix,*sy_ix,*fy_ix);
#endif
	/*
	 * Given these data adjust the location so that the map is 
	 * registered correctly
	 */
	xt[0] = ValToDouble(lonval,fdim_size * *sy_ix + *sx_ix);
	yt[0] = ValToDouble(latval,fdim_size * *sy_ix + *sx_ix);
	xt[1] = ValToDouble(lonval,fdim_size * *fy_ix + *fx_ix);
	yt[1] = ValToDouble(latval,fdim_size * *fy_ix + *fx_ix);
	
	NhlDataToNDC(map_id,
		     xt,yt,2,xloc,yloc,NULL,NULL,&status,&oor);

	NclFreeExtValue(lonval);
	NclFreeExtValue(latval);

	return;
}

static void AnalyzeMapLocation
(
	int map_id,
	int cycle_point,
	float *xloc,
	float *yloc,
	float *xout,
	float *yout
)
{
	int proj;
	float clon,crot,clat;
	NhlBoolean rel_lon,rel_lat;
	NhlBoolean northpole = False, southpole = False;
	float xt[180],yt[180];
	int status;
	float oor;
	float dist;
	float xmin = 999,ymin = 999,xmax = -999,ymax = -999,lastx;
	float xmin1 = 999,xmax1 = -999;
	NhlBoolean firstpart = True;
	int i;
	
	NhlVAGetValues(map_id,
		       NhlNmpProjection,&proj,
		       NhlNmpCenterLonF,&clon,
		       NhlNmpCenterRotF,&crot,
		       NhlNmpCenterLatF,&clat,
		       NhlNmpRelativeCenterLon,&rel_lon,
		       NhlNmpRelativeCenterLat,&rel_lat,
		       NULL);

	/* if it's a straight cyleq or mercator, and the data boundary 
	   line is not crossed then the data locs need no further processing */

	if ((proj == 8 || proj == 7) && clat == 0.0 && crot == 0.0) {
		NhlNDCToData(map_id,
			     xloc,yloc,2,xout,yout,NULL,NULL,&status,&oor);
		/* no problem */
		return;
	}

	/*
	 * see if the north or south pole is inside the area
	 */
	xt[0] = xt[1] = cycle_point % 360 + 180;
	yt[0] = -90;
	yt[1] = 90;

	NhlDataToNDC(map_id,xt,yt,2,xt,yt,NULL,NULL,&status,&oor);

	if (yt[0] >= yloc[0] && yt[0] <= yloc[1] &&
	    xt[0] >= xloc[0] && xt[0] <= xloc[1])
		southpole = True;
	if (yt[1] >= yloc[0] && yt[1] <= yloc[1] &&
	    xt[1] >= xloc[0] && xt[1] <= xloc[1])
		northpole = True;

	/*
	 * sample points along each edge and determine the min and max
	 * data values
	 */

	dist = xloc[1] - xloc[0];
	for (i = 0; i < 25; i++) {
		xt[i] = xloc[0] + i * dist / 25.0;
		yt[i] = yloc[0];
	}
	dist = yloc[1] - yloc[0];
	for (i = 0; i < 25; i++) {
		xt[25+i] = xloc[1];
		yt[25+i] = yloc[0] + i * dist / 25.0;;
	}
	dist = xloc[0] - xloc[1];
	for (i = 0; i < 25; i++) {
		xt[50+i] = xloc[1] + i * dist / 25.0;
		yt[50+i] = yloc[1];
	}
	dist = yloc[0] - yloc[1];
	for (i = 0; i < 25; i++) {
		xt[75+i] = xloc[0];
		yt[75+i] = yloc[1] + i * dist / 25.0;;
	}
	NhlNDCToData(map_id,
		     xt,yt,100,xt,yt,NULL,NULL,&status,&oor);

	if (northpole) {
		for (i = 0; i < 100; i++) {
			if (ymin > yt[i]) ymin = yt[i];
		}
		xout[0] = 0;
		xout[1] = 360;
		yout[0] = ymin;
		yout[1] = 90;
		return;
	}
	else if (southpole) {
		for (i = 0; i < 100; i++) {
			if (ymax < yt[i] && yt[i] < 1e10) ymax = yt[i];
		}
		xout[0] = 0;
		xout[1] = 360;
		yout[0] = -90;
		yout[1] = ymax;
		return;
	}
	else if (cycle_point == -9999) {
		for (i = 0; i < 100; i++) {
			if (xmin > xt[i]) xmin = xt[i];
			if (xmax < xt[i] && xt[i] < 1e10) xmax = xt[i];
			if (ymin > yt[i]) ymin = yt[i];
			if (ymax < yt[i] && yt[i] < 1e10) ymax = yt[i];
		}
		xout[0] = xmin;
		xout[1] = xmax;
		yout[0] = ymin;
		yout[1] = ymax;
		return;
	}
	/*
	 * this is the most general situation; the data may cross the
	 * cyclic point, and since we're not at the poles, it needs to
	 * be determined how much of the longitudinal extent to include
	 */
	lastx = xt[0];
	for (i = 0; i < 100; i++) {
		if (abs((int)(xt[i]-lastx)) > 50	)
			firstpart = ! firstpart;
		if (firstpart) {
			if (xmin > xt[i]) xmin = xt[i];
			if (xmax < xt[i] && xt[i] < 1e10) xmax = xt[i];
		}
		else {
			if (xmin1 > xt[i]) xmin1 = xt[i];
			if (xmax1 < xt[i] && xt[i] < 1e10) xmax1 = xt[i];
		}
		if (ymin > yt[i]) ymin = yt[i];
		if (ymax < yt[i] && yt[i] < 1e10) ymax = yt[i];
		lastx = xt[i];
	}
	if (xmin1 != 999) {
		xout[0] = MAX(xmin,xmin1);
		xout[1] = MIN(xmax,xmax1);
	}
	else {
		xout[0] = xmin;
		xout[1] = xmax;
	}
	yout[0] = ymin;
	yout[1] = ymax;
	return;
}

void GetGribGridDataLimits
(
	NgVarData	vdata,
	NclApiVarInfoRec	*vinfo,
	float 		*ymin,
	float 		*ymax,
	float 		*yinc,
	float 		*xmin,
	float 		*xmax,
	float 		*xinc
	)
{
	NclExtValueRec *val;
	int i;
	NrmQuark qlon,qlat;

	GetGribDimGrids(vdata,&qlon,&qlat);

	for (i = 0; i < vdata->ndims; i++) {
		NrmQuark qgrid = vinfo->coordnames[i];
		NhlBoolean do_y = False;

		if (vdata->order_ix[i] < vdata->ndims - 2)
			continue;
		if (vdata->dstart[i] != vdata->dfinish[i])
			continue;
		if (vdata->order_ix[i] < vdata->ndims - 1)
			do_y = True;

		qgrid = do_y ?  qlat : qlon;
		val = NclReadFileVarAtt(vdata->qfile,qgrid,Qcorners);

		if (! val) {
			vdata->dstart[i] = 0;
			vdata->dfinish[i] = (double)vdata->size[i]-1;
			if (do_y) {
				*ymin = vdata->dstart[i];
				*ymax = vdata->dfinish[i];
				*yinc = 1;
			}
			else {
				*xmin = vdata->dstart[i];
				*xmax = vdata->dfinish[i];
				*xinc = 1;
			}
			continue;
		}
		if (do_y) {
			*ymin = ValToDouble(val,0);
			*ymax = ValToDouble(val,2);
			*yinc = 0;
		}
		else {
			*xmin = ValToDouble(val,0);
			*xmax = ValToDouble(val,2);
			*xinc = 0;
		}
		NclFreeExtValue(val);
	}
	return;
}	


void GetDataLimits2D
(
	NgVarData	vdata,
	float 		*ymin,
	float 		*ymax,
	float 		*yinc,
	float 		*xmin,
	float 		*xmax,
	float 		*xinc,
	NhlBoolean	*is_grib
	)
{
	int i,j;
	NclApiVarInfoRec	*vinfo = NULL;
	int sign;
	float tinc,inc;
	NhlBoolean monotonic;
		
	*is_grib = False;

	if (vdata->dl) {
		vinfo = vdata->dl->u.var;
	}
	else {
		if (vdata->qfile > NrmNULLQUARK)
			vdata->dl = NclGetFileVarInfo
				(vdata->qfile,vdata->qvar);
		else 	
			vdata->dl = NclGetVarInfo(vdata->qvar);
		vinfo = vdata->dl->u.var;
	}

	for (i = 0; i < vdata->ndims; i++) {
		NrmQuark qcoord = vinfo->coordnames[i];
		NclExtValueRec *val;
		NhlBoolean do_y = False;
		
		if (vdata->order_ix[i] < vdata->ndims - 2)
			continue;
		if (vdata->order_ix[i] < vdata->ndims - 1)
			do_y = True;
		if (qcoord <= NrmNULLQUARK) {
			/*
			 * see if there's a grid number attribute --
			 * which we will take as an indicator that the
			 * data is preprojected GRIB data -- in that case
			 * use the Grib data limits routine.
			 */
			if (IsGribVar(vinfo) && vdata->qfile) {
				*is_grib = True;
				GetGribGridDataLimits
					(vdata,vinfo,ymin,ymax,yinc,
					 xmin,xmax,xinc);
				return;
			}
			/*
			 * otherwise set the data limits to the index values
			 */
			vdata->dstart[i] = 0;
			vdata->dfinish[i] = (double)vdata->size[i]-1;
			if (do_y) {
				*ymin = vdata->dstart[i];
				*ymax = vdata->dfinish[i];
				*yinc = 1;
			}
			else {
				*xmin = vdata->dstart[i];
				*xmax = vdata->dfinish[i];
				*xinc = 1;
			}
			continue;
		}
		else {
			if (vdata->qfile)
				val = NclReadFileVarCoord
					(vdata->qfile,vinfo->name,
					 qcoord,NULL,NULL,NULL);
			else
				val = NclReadVarCoord
					(vinfo->name,qcoord,NULL,NULL,NULL);
			if (! val)
				continue;
			vdata->dstart[i] = ValToDouble(val,0);
			vdata->dfinish[i] = ValToDouble
				(val,vinfo->dim_info[i].dim_size - 1);
		}
		tinc = ValToDouble(val,1) - vdata->dstart[i];
		sign = (tinc < 0.0) ? -1.0 : 1.0;
		inc = tinc * sign;
		monotonic = True;
		for (j = 2; j < vdata->size[i]; j++) {
			tinc = ValToDouble(val,j)-ValToDouble(val,j-1);
			if (tinc * sign < 0.0) {
				monotonic = False;
				break;
			}
			if (tinc * sign > inc)
				inc = tinc * sign;
		}
		if (do_y) {
			if (! monotonic) {
				*ymin = vdata->dstart[i];
				*ymax = vdata->dfinish[i];
				*yinc = 1;
				NclFreeExtValue(val);
				continue;
			}
			*ymin = MIN(vdata->dstart[i],vdata->dfinish[i]);
			*ymax = MAX(vdata->dstart[i],vdata->dfinish[i]);
			*yinc = inc;
		}
		else {
			if (! monotonic) {
				*xmin = vdata->dstart[i];
				*xmax = vdata->dfinish[i];
				*xinc = 1;
				NclFreeExtValue(val);
				continue;
			}
			*xmin = MIN(vdata->dstart[i],vdata->dfinish[i]);
			*xmax = MAX(vdata->dstart[i],vdata->dfinish[i]);
			*xinc = inc;
		}
		NclFreeExtValue(val);
	}
	return;
}

static void
SetMapRegion
(
	brPage		*page,	
	NgVarData	vdata,
	int		map_id,
	NhlBoolean	full_region,
	float		x,
	float		y,
	float		width,
	float		height
	)
{
	brPageData	*pdp = page->pdata;
	brPlotPageRec	*rec = (brPlotPageRec	*)pdp->type_rec;
	float minlon,maxlon,loninc,minlat,maxlat,latinc;
	float mnlon,mxlon;
	float lmin,rmax,bmin,tmax;
	NhlBoolean is_grib;
	NhlMapLimitMode lmode = NhlMAXIMALAREA;
	char buf0[32],buf1[32],buf2[32],buf3[32],buf4[32];
	NhlString values[5];
	NrmQuark qargs[5];
	NhlString var_string;
	float xt[2],yt[2];
	int slon_ix,flon_ix,slat_ix,flat_ix;
	int wk_id;
	NhlBoolean work_created;

	GetDataLimits2D
		(vdata,&minlat,&maxlat,&latinc,
		 &minlon,&maxlon,&loninc,&is_grib);

	if (minlon < -360) {
		mnlon = -540; mxlon = -180;
	}
	else if (minlon < -180) {
		mnlon = -360; mxlon = 0;
	}
	else if (minlon < 0) {
		mnlon = -180; mxlon = 180;
	}
	else if (maxlon < 360) {
		mnlon =  0; mxlon = 360;
	}
	else {
		mnlon = 180; mxlon = 540;
	}
/*
 * Normally shouldn't be setting HLU values without going through NCL, but
 * these values are reset at every draw by the plot objects anyway, so the
 * state is not affected. These need to be set correctly for the current
 * data, in order for NDCToData calls to work properly.
 */

	NhlVASetValues(map_id,
		       NhlNmpDataMinLonF,mnlon,
		       NhlNmpDataMaxLonF,mxlon,
		       NULL);

/*
 * Note: full_region is used to expand the data region to the maximal
 * area that contains data. It is not necessarily the maximal possible
 * map area.
 */	
	if (full_region) {
		if (is_grib) {
			lmode = NhlCORNERS;
			slon_ix = flon_ix = slat_ix = flat_ix = 0;
		}
		else if ((int) (maxlon - minlon + loninc + 0.5) >= 360 &&
		    (int)(maxlat - minlat + 2 * latinc + 0.5) >= 180)
			lmode = NhlMAXIMALAREA;
		else
			lmode = NhlLATLON;

		lmin = minlon;
		rmax = maxlon;
		bmin = minlat;
		tmax = maxlat;
		xt[0] = xt[1] = yt[0] = yt[1] = 0.0;
	}
	else {
		float xp[2],yp[2],vx,vy,vw,vh;

		NhlVAGetValues(map_id,
			       NhlNmpLeftMapPosF,&xp[0],
			       NhlNmpRightMapPosF,&xp[1],
			       NhlNmpBottomMapPosF,&yp[0],
			       NhlNmpTopMapPosF,&yp[1],
			       NULL);
		vx = xp[0];
		vy = yp[1];
		vw = xp[1] - xp[0];
		vh = yp[1] - yp[0];
	
		xp[0] = MIN(vx+vw,MAX(vx,x));
		xp[1] = MAX(vx,MIN(x+width,vx+vw));
		yp[0] = MIN(vy,MAX(y-height,vy-vh));
		yp[1] = MAX(vy-vh,MIN(y,vy));

#if DEBUG_PLOTPAGE
		fprintf(stderr,"clipped region: %f %f %f %f\n",
			xp[0],xp[1],yp[0],yp[1]);
#endif

		if (xp[1]-xp[0] < 0.005 ||
		    yp[1]-yp[0] < 0.005)
			return;


		if (is_grib) {
			AnalyzeGribMapLocation(map_id,vdata,
					       xp,yp,
					       &slon_ix,&flon_ix,
					       &slat_ix,&flat_ix);
#if DEBUG_PLOTPAGE
			fprintf(stderr,"grib adj: %f %f %f %f\n",
				xp[0],xp[1],yp[0],yp[1]);
#endif
		}
		else {
			int cycleval;

			if ((int) (maxlon - minlon + loninc + 0.5) < 360) {
				cycleval = -9999;
			}
			else {
				cycleval = minlon;
			}
			AnalyzeMapLocation(map_id,cycleval,
					   xp,yp,xt,yt);
#if DEBUG_PLOTPAGE
			fprintf(stderr,"data coords of region: %f %f %f %f\n",
				xt[0],xt[1],yt[0],yt[1]);
#endif
		}
		lmode = NhlNDC;
		lmin = xp[0];
		rmax = xp[1];
		bmin = yp[0];
		tmax = yp[1];
	}
	values[0] = buf0;
	values[1] = buf1;
	values[2] = buf2;
	values[3] = buf3;
	values[4] = buf4;

	if (is_grib)
		var_string = SetGribDataRegionString
			(rec,0,slon_ix,flon_ix,slat_ix,flat_ix,full_region);
	else {
		var_string = SetVarDataRegionString
			(rec,0,xt[0],xt[1],yt[0],yt[1],full_region);

/*
 * Set the "NgAdjustLongitude.." functions
 */
		sprintf(values[0],"%f",xt[0]);
		sprintf(values[1],"%f",xt[1]);
		qargs[0] = NrmStringToQuark("start_lon");
		qargs[1] = NrmStringToQuark("end_lon");
		SetPlotAppFuncs(rec->data_profile,NrmNULLQUARK,
				"NgAdjustLongitude",
				2,qargs,values,False,False);
	}
/*
 * Set the "NgSetMapLimits" function
 */
	sprintf(values[0],"%d",lmode);
	sprintf(values[1],"%f",lmin);
	sprintf(values[2],"%f",rmax);
	sprintf(values[3],"%f",bmin);
	sprintf(values[4],"%f",tmax);
	qargs[0] = NrmStringToQuark("limit_mode");
	qargs[1] = NrmStringToQuark("left_min");
	qargs[2] = NrmStringToQuark("right_max");
	qargs[3] = NrmStringToQuark("bottom_min");
	qargs[4] = NrmStringToQuark("top_max");
	SetPlotAppFuncs(rec->data_profile,NrmNULLQUARK,
			"NgSetMapLimits",5,qargs,values,False,False);

	if (! rec->data_var_grid->var_string_count) {
		rec->data_var_grid->var_strings = 
			NhlMalloc(1 * sizeof(NhlString));
		if (! rec->data_var_grid->var_strings) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return;
		}
		rec->data_var_grid->var_string_count = 1;
	}
	rec->data_var_grid->var_strings[0] = var_string;

	NgUpdateDataVarGrid(rec->data_var_grid,page->qvar,
			    rec->data_var_grid->plotdata_count,
			    rec->data_var_grid->plotdata);
	
        wk_id = GetWorkstation(page,&work_created);
	CreateUpdate(page,wk_id,True);

/*
 * if the map limits were set using NDC then it's necessary to adjust
 * the map_limits function because NDC values are reset after each
 * setvalues.
 */
	if (lmode == NhlNDC) {
		SetPlotAppFuncs(rec->data_profile,NrmNULLQUARK,
				"NgSetMapLimits",0,NULL,NULL,True,False);
	}

}


static void
XRegionCB
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	int	go_id = udata.lngval; 
	NgGO go = (NgGO) _NhlGetLayer(go_id);
	int page_id;
	NgXRegionData rdata = (NgXRegionData) cbdata.ptrval;
	NhlString name;
	NrmQuark qplot;
	NhlPointer pdata;
	float x = 0.0,y = 0.0,width = 0.0,height = 0.0;
	float vx,vy,vw,vh;
	float xt[2],yt[2],xp[2],yp[2];
	int status;
	float oor;
	brPage *page;
	brPlotPageRec	*rec;
	char *var_string;
	int wk_id;
	NhlBoolean work_created;
	NgPlotData plotdata;
	NgVarData vdata;
	NhlBoolean ismap;

	if (! (go && rdata))
		return;

	/*
	 * this callback is not interested in single point selections
	 */
		
#if DEBUG_PLOTPAGE
	if (rdata->full_region)
		fprintf(stderr,"full region\n");
#endif
	if (rdata->single_point && ! rdata->full_region)
		return;
		
	name = NgNclGetHLURef(go->go.nclstate,rdata->view_id);
	qplot = NgGraphicArrayofGraphic(NrmStringToQuark(name));
	page_id = NgGetPageId(go_id,qplot,NrmNULLQUARK);
	pdata = NgPageData(go_id,page_id);
	
	if (! pdata) {
		page_id = NgOpenPage(go_id,_brPLOTVAR,&qplot,1,NULL);
	}
	if (page_id == NgNoPage)
		return;

	page = _NgGetPageRef(go_id,page_id);
	rec = (brPlotPageRec *)page->pdata->type_rec;

	/* this will need elaboration */
	plotdata = &rec->data_profile->plotdata[0];
	vdata = plotdata->vdata;

	if (! rdata->full_region) {
		NgXCoordToNDC(rdata->xwkid,&rdata->xbbox,&x,&y,&width,&height);
		if (width <= 0.0 || height <= 0.0)
			return;
	}
		
	ismap = NhlIsClass(rdata->view_id,NhlmapPlotClass);
	if (ismap) {
		SetMapRegion(page,vdata,rdata->view_id,rdata->full_region,
			     x,y,width,height);
		return;
	}
	else {
		xt[0] = xt[1] = yt[0] = yt[1] = 0.0;
		if (! rdata->full_region) {

			NhlVAGetValues(rdata->view_id,
				       NhlNvpXF,&vx,
				       NhlNvpYF,&vy,
				       NhlNvpHeightF,&vh,
				       NhlNvpWidthF,&vw,
				       NULL);

			xp[0] = MIN(vx+vw,MAX(vx,x));
			xp[1] = MAX(vx,MIN(x+width,vx+vw));
			yp[0] = MIN(vy,MAX(y-height,vy-vh));
			yp[1] = MAX(vy-vh,MIN(y,vy));

#if DEBUG_PLOTPAGE
			fprintf(stderr,"clipped region: %f %f %f %f\n",
				xp[0],xp[1],yp[0],yp[1]);
#endif
			if (xp[1]-xp[0] < 0.005 ||
			    yp[1]-yp[0] < 0.005)
				return;

			NhlNDCToData(rdata->view_id,
				     xp,yp,2,xt,yt,NULL,NULL,&status,&oor);
			if (status)
				return;

#if DEBUG_PLOTPAGE
			fprintf(stderr,"data coords of region: %f %f %f %f\n",
				xt[0],xt[1],yt[0],yt[1]);
#endif
		}
		var_string = SetVarDataRegionString
			(rec,0,xt[0],xt[1],yt[0],yt[1],rdata->full_region);
	}
	if (! rec->data_var_grid->var_string_count) {
		rec->data_var_grid->var_strings = 
			NhlMalloc(1 * sizeof(NhlString));
		if (! rec->data_var_grid->var_strings) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return;
		}
		rec->data_var_grid->var_string_count = 1;
	}
	rec->data_var_grid->var_strings[0] = var_string;

	NgUpdateDataVarGrid(rec->data_var_grid,page->qvar,
			    rec->data_var_grid->plotdata_count,
			    rec->data_var_grid->plotdata);
	
        wk_id = GetWorkstation(page,&work_created);
	CreateUpdate(page,wk_id,True);

        return;
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

#if DEBUG_PLOTPAGE
        fprintf(stderr,"in CreateUpdateCB\n");
#endif
        
        wk_id = GetWorkstation(page,&work_created);
	/*
	 * the update button should always cause a redraw even if nothing
	 * has changed. So set do_draw to True.
	 */
	CreateUpdate(page,wk_id,True);

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
	brPlotPageRec	*rec = (brPlotPageRec *) pdp->type_rec;
        Boolean		set;

#if DEBUG_PLOTPAGE
        fprintf(stderr,"in AutoUpdateCB\n");
#endif

        XtVaGetValues(w,
                      XmNset,&set,
                      NULL);
        
        rec->do_auto_update = set;
        
        return;
}

static NhlErrorTypes PlotPageMessageNotify (
        brPage *page
        )
{
        brPageData	*pdp = page->pdata;
	brPlotPageRec	*rec = (brPlotPageRec	*)pdp->type_rec;
        
        
#if DEBUG_PLOTPAGE
        fprintf(stderr,"in plot page message notify\n");
#endif
	GetPageMessages(page,True);

        if (rec->new_data &&
	    (rec->state == _plotNOTCREATED || rec->do_auto_update)) {
		int		wk_id;
		NhlBoolean 	work_created;
                
                wk_id = GetWorkstation(page,&work_created);
                
		CreateUpdate(page,wk_id,True);

        }
        
        return NhlNOERROR;
}
static NhlPointer PublicPlotPageData (
        brPage *page
        )
{
        brPageData	*pdp = page->pdata;
	brPlotPageRec	*rec = (brPlotPageRec	*)pdp->type_rec;
        NgPlotPage 	*pub = &rec->public;
        
#if DEBUG_PLOTPAGE
        fprintf(stderr,"in public plot page data\n");
#endif
        
        return (NhlPointer) pub;
}
static void
FreePlotSaveState
(
	brPlotSaveState hs_state
)
{
	FreeDataLinks(hs_state->datalinks);
	NgFreeDataProfile(hs_state->data_profile);
	NhlFree(hs_state->hlu_ids);
	NhlFree(hs_state);
	return;
}

static void
SavePlotState
(
	brPage	*page
)
{
	brPlotPageRec *rec = (brPlotPageRec *)page->pdata->type_rec;
	int hlu_id,count,*hlu_array = NULL;
	int nclstate;
	brPlotSaveState hs_state;

	NhlVAGetValues(page->go->go.appmgr,
		NgNappNclState,	&nclstate,
		NULL);

	if (! NclSymbolDefined(NrmQuarkToString(page->qvar)))
		return;

	hlu_id = NgNclGetHluObjId
                (nclstate,NrmQuarkToString(page->qvar),&count,&hlu_array);
	if (hlu_id < NhlNOERROR)
		return;
	if (hlu_array)
		NhlFree(hlu_array);
/*
 * initial stab at saving state; this is just the most important stuff.
 */
	hs_state = NhlMalloc(sizeof(brPlotSaveStateRec));
	hs_state->plot_style = rec->public.plot_style;
	hs_state->plot_style_dir = rec->public.plot_style_dir;
	hs_state->data_profile = rec->data_profile;
	hs_state->hlu_count = rec->hlu_count;
	hs_state->hlu_ids = rec->hlu_ids;
	hs_state->app_id = rec->app_id;
	hs_state->plot_delete_cb = rec->plot_delete_cb;
	hs_state->state = rec->state;
	hs_state->do_auto_update = rec->do_auto_update;
	hs_state->datalinks = rec->datalinks;
	hs_state->has_input_data = rec->has_input_data;
	rec->datalinks = NULL;
	rec->data_profile = NULL;
	rec->hlu_count = 0;
	rec->hlu_ids = NULL;

	NgSavePageState(rec->go->base.id,page->id,page->qfile,page->qvar,
			(NhlPointer)hs_state,(NhlFreeFunc) FreePlotSaveState);

	return;
}

static void
AdjustPlotPageGeometry
(
        NhlPointer	data
)
{
	brPage	*page = (brPage *) data;
        brPlotPageRec	*rec;
	Dimension		w,h,y,twidth,theight;
        Dimension		avail_width,avail_height;
        
	rec = (brPlotPageRec *)page->pdata->type_rec;
        
	twidth = 0;
	theight = 0;
        w = 0;
        y = 0;
        h = 0;
      
	if (rec->data_var_grid &&
	    XtIsManaged(rec->data_var_grid->grid)) {
                XtVaGetValues(rec->data_var_grid->grid,
                              XmNwidth,&w,
                              XmNy,&y,
                              XmNheight,&h,
                              NULL);
	}
	twidth = w;

	if (rec->plot_tree &&
	    XtIsManaged(rec->plot_tree->tree)) {
                XtVaGetValues(rec->plot_tree->tree,
                              XmNwidth,&w,
                              XmNy,&y,
                              XmNheight,&h,
                              NULL);
	}
	twidth = MAX(twidth,w);

        if (rec->func_grid && 
	    XtIsManaged(rec->func_grid->grid)) {
                XtVaGetValues(rec->func_grid->grid,
                              XmNwidth,&w,
                              XmNy,&y,
                              XmNheight,&h,
                              NULL);
        }
	else {
                XtVaGetValues(rec->link_tgl,
                              XmNwidth,&w,
                              XmNy,&y,
                              XmNheight,&h,
                              NULL);
	}		
	twidth = MAX(twidth,w);
	theight = y + h;

        NgSetFolderSize(page->pdata->pane,
                        twidth,theight,&avail_width,&avail_height);
	
	return;
}

static void LinkToggleCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
        brPage		*page = (brPage *)udata;
	brPageData	*pdp = page->pdata;
	brPlotPageRec *rec = (brPlotPageRec *) pdp->type_rec;
	XmToggleButtonCallbackStruct    *xmcb = 
		(XmToggleButtonCallbackStruct    *) cb_data;
	Position  x,y;
	Dimension width,height;
	XRectangle rect;
                
#if DEBUG_VARPAGE
        fprintf(stderr,"LinkToggleCB(IN)\n");
#endif
	if (! xmcb->set) {
		if (! rec->func_grid)
			return;
		else {
			/*the data source grid is not unmapped automatically */
			XtUnmanageChild(rec->func_grid->grid);
			XtUnmapWidget(rec->func_grid->grid);
			AdjustPlotPageGeometry((NhlPointer)page);
		}
		return;
	}
	if (! rec->func_grid) {
		rec->func_grid = NgCreateFuncGrid
			(rec->go->base.id,
			 pdp->form,page->qvar,rec->data_profile);
		XtVaSetValues(rec->func_grid->grid,
			      XmNtopAttachment,XmATTACH_WIDGET,
			      XmNtopWidget,rec->link_tgl,
			      XmNbottomAttachment,XmATTACH_NONE,
			      XmNrightAttachment,XmATTACH_NONE,
			      NULL);
	}
	NgUpdateFuncGrid
		(rec->func_grid,page->qvar,rec->data_profile);
	XtManageChild(rec->func_grid->grid);
	XtMapWidget(rec->func_grid->grid);
	(*pdp->adjust_page_geo)(page);

	XtVaGetValues(rec->func_grid->grid,
		      XmNwidth,&width,
		      XmNheight,&height,
		      XmNx,&x,
		      XmNy,&y,
		      NULL);
	rect.x = x;
	rect.y = y;
	rect.width = width;
	rect.height = height;
                
	NgPageSetVisible(rec->go->base.id,page->id,pdp->form,&rect);

        return;
        
}

static void
DeactivatePlotPage
(
	brPage	*page
)
{
	brPlotPageRec *rec = (brPlotPageRec *)page->pdata->type_rec;
	NhlLayer l;

	GetPageMessages(page,False);

/*
 * if the object has already been destroyed its callbacks will have been
 * removed. 
 */
	l = rec->hlu_count ? _NhlGetLayer(rec->hlu_ids[0]) : NULL;
        if (l) {
		SavePlotState(page);

		if (rec->setval_cb) {
			_NhlCBDelete(rec->setval_cb);
		}
	}

	rec->setval_cb = NULL;
	rec->do_setval_cb = False;
	rec->plot_delete_cb = NULL;
        
        if (rec->create_update)
                XtRemoveCallback(rec->create_update,
                                 XmNactivateCallback,CreateUpdateCB,page);
        if (rec->auto_update)
                XtRemoveCallback(rec->auto_update,
                                 XmNvalueChangedCallback,AutoUpdateCB,page);
	if (rec->link_tgl)
		XtRemoveCallback(rec->link_tgl,
				 XmNvalueChangedCallback,LinkToggleCB,page);

        rec->state = _plotNOTCREATED;
	if (rec->hlu_ids) {
		NhlFree(rec->hlu_ids);
		rec->hlu_ids = NULL;
	}
	rec->hlu_count = 0;
	rec->app_id = NhlNULLOBJID;
        rec->do_auto_update = True;
        rec->public.class_name = NULL;
        rec->public.plot_style = NULL;
	rec->public.plot_style_dir = NULL;
        rec->public.plot_style_name = NULL;
	rec->public.config_required = False;
	rec->qclass = NrmNULLQUARK;
        rec->new_data = True;
        rec->has_input_data = False;
	rec->vdata_count = 0;
			     

        
	FreeDataLinks(rec->datalinks);
	rec->datalinks = NULL;
	

	NgFreeDataProfile(rec->data_profile);
	rec->data_profile = NULL;
        rec->activated = False;
	rec->max_seq_num = -1;
	NgDeactivateDataVarGrid(rec->data_var_grid);

	XtVaSetValues(rec->link_tgl,
		      XmNset,False,
		      NULL);
	if (rec->func_grid) {
		XtUnmanageChild(rec->func_grid->grid);
		XtUnmapWidget(rec->func_grid->grid);
	}
	
}

static void DestroyPlotPage
(
	NhlPointer data
)
{
	brPlotPageRec	*rec = (brPlotPageRec *)data;
	
        NgDestroyDataVarGrid(rec->data_var_grid);
	if (rec->func_grid)
		NgDestroyFuncGrid(rec->func_grid);

	NgDestroyPlotTree(rec->plot_tree);
        
        NhlFree(data);
	return;
}

static NhlErrorTypes UpdatePlotPage
(
        brPage *page
)
{
        brPageData	*pdp = page->pdata;
        int		wk_id;
        NhlBoolean	work_created;

#if DEBUG_PLOTPAGE
        fprintf(stderr,"in UpdatePlotPage\n");
#endif
	if (page == pdp->pane->active_page)
		XtUnmapWidget(pdp->form);

        wk_id = GetWorkstation(page,&work_created);
#if 0
	CreateUpdate(page,wk_id,False,False);
#endif

	CreateUpdate(page,wk_id,False);
	if (page == pdp->pane->active_page)
		XtMapWidget(pdp->form);
        return NhlNOERROR;

}
        
static NhlErrorTypes ResetPlotPage
(
        brPage *page
        )
{
        brPageData	*pdp = page->pdata;
	brPlotPageRec	*rec = (brPlotPageRec	*)pdp->type_rec;
	int		wk_id;
	NhlBoolean	work_created;
        
#if DEBUG_PLOTPAGE
        fprintf(stderr,"in reset plot page\n");
#endif
	GetPageMessages(page,False);

#if DEBUG_PLOTPAGE
        fprintf(stderr,"%s: %d\n",rec->hlu_id);
#endif
	wk_id = GetWorkstation(page,&work_created);
	NgUpdatePlotTree(rec->plot_tree,wk_id,page->qvar,rec->data_profile);
	rec->plot_tree->geo_data = (NhlPointer) page;
	rec->plot_tree->geo_notify = AdjustPlotPageGeometry;
	rec->plot_tree->hlu_ids = rec->hlu_ids;
	if (! XtIsManaged(rec->plot_tree->tree))
			XtManageChild(rec->plot_tree->tree);

	if (rec->data_profile && rec->data_profile->plotdata_count) {
		NgUpdateDataVarGrid
			(rec->data_var_grid,page->qvar,
			 rec->data_profile->plotdata_count,
			 rec->data_profile->plotdata);
		if (! XtIsManaged(rec->data_var_grid->grid))
			XtManageChild(rec->data_var_grid->grid);
	}
	UpdateVData(rec);

	if (rec->data_profile && rec->data_profile->n_dataitems) {
		if (rec->func_grid) {
			NgUpdateFuncGrid
				(rec->func_grid,
				 page->qvar,rec->data_profile);
			if (! XtIsManaged(rec->func_grid->grid))
				XtManageChild(rec->func_grid->grid);
		}

		SetInputDataFlag(rec);
	}

        
/* Create Update button */

        if (rec->state == _plotCREATED) {
		XmString xmstring =
			NgXAppCreateXmString(rec->go->go.appmgr,"Update");
		XtVaSetValues(rec->create_update,
			      XmNlabelString,xmstring,
			      NULL);
		NgXAppFreeXmString(rec->go->go.appmgr,xmstring);
	}
	XtMapWidget(pdp->form);
	
        return NhlNOERROR;

}

static brPageData *
NewPlotPage
(
  	NgGO		go,
        brPane		*pane,
	brPage		*page
        )
{
	brPageData	*pdp;
	brPlotPageRec	*rec;
        NhlString	e_text;

	if (!(pdp = NhlMalloc(sizeof(brPageData)))) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,_NhlName(go));
                return NULL;
	}
        pdp->dl = NULL;
	pdp->next = pane->plot_pages;
	pane->plot_pages = pdp;

	rec = (brPlotPageRec*) NhlMalloc(sizeof(brPlotPageRec));
	if (! rec) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,_NhlName(go));
                NhlFree(pdp);
                return NULL;
	}
        pdp->type_rec = (NhlPointer) rec;
        rec->go = go;
        
        rec->func_grid = NULL;
        rec->create_update = NULL;
        rec->auto_update = NULL;
	rec->link_tgl = NULL;
        rec->new_data = True;
        rec->has_input_data = False;
        rec->state = _plotNOTCREATED;
        rec->do_auto_update = True;
        rec->public.class_name = NULL;
        rec->public.plot_style = NULL;
        rec->public.plot_style_dir = NULL;
        rec->public.plot_style_name = NULL;
	rec->hlu_count = 0;
        rec->hlu_ids = NULL;
	rec->app_id = NhlNULLOBJID;
	rec->qclass = NrmNULLQUARK;
        rec->setval_cb = NULL;
        rec->do_setval_cb = False;
	rec->data_profile = NULL;
	rec->max_seq_num = -1;
	rec->datalinks = NULL;
	rec->preview_destroy = False;
	rec->vdata = NULL;
	rec->vdata_count = 0;

	pdp->form = XtVaCreateManagedWidget
		("form",xmFormWidgetClass,pane->folder,
                 XmNy,28,
                 XmNx,2,
                 NULL);
        
	pdp->destroy_page = DestroyPlotPage;
	pdp->adjust_page_geo = AdjustPlotPageGeometry;
	pdp->deactivate_page = DeactivatePlotPage;
        pdp->public_page_data = PublicPlotPageData;
        pdp->update_page = UpdatePlotPage;
        pdp->reset_page = ResetPlotPage;
        pdp->page_focus_notify = PlotPageFocusNotify;
        pdp->page_message_notify = PlotPageMessageNotify;
        pdp->pane = pane;
        
	XtUnmapWidget(pdp->form);
	
	if (! rec->data_profile) {
		rec->data_var_grid = NgCreateDataVarGrid
			(rec->go->base.id,pdp->form,page->qvar,0,NULL);
	}
	else {
		rec->data_var_grid = NgCreateDataVarGrid
			(rec->go->base.id,pdp->form,page->qvar,
			 rec->data_profile->plotdata_count,
			 rec->data_profile->plotdata);
	}	
        XtVaSetValues(rec->data_var_grid->grid,
		      XmNtopOffset,8,
                      XmNbottomAttachment,XmATTACH_NONE,
                      XmNrightAttachment,XmATTACH_NONE,
                      NULL);

        rec->create_update = XtVaCreateManagedWidget
                ("Create/Update",xmPushButtonGadgetClass,pdp->form,
		 XmNtopAttachment,XmATTACH_WIDGET,
		 XmNtopWidget,rec->data_var_grid->grid,
		 XmNtopOffset,8,
                 XmNrightAttachment,XmATTACH_NONE,
                 XmNbottomAttachment,XmATTACH_NONE,
                 NULL);

        rec->auto_update = XtVaCreateManagedWidget
                ("Auto Update",xmToggleButtonGadgetClass,pdp->form,
		 XmNset,rec->do_auto_update,
		 XmNtopAttachment,XmATTACH_WIDGET,
		 XmNtopWidget,rec->data_var_grid->grid,
		 XmNtopOffset,8,
                 XmNleftAttachment,XmATTACH_WIDGET,
                 XmNleftWidget,rec->create_update,
                 XmNrightAttachment,XmATTACH_NONE,
                 XmNbottomAttachment,XmATTACH_NONE,
                 NULL);

	rec->plot_tree = NgCreatePlotTree
                                (go->base.id,pdp->form,NhlNULLOBJID,
				 page->qvar,rec->data_profile);

	XtVaSetValues(rec->plot_tree->tree,
		      XmNrightAttachment,XmATTACH_NONE,
		      XmNbottomAttachment,XmATTACH_NONE,
		      XmNtopOffset,8,
		      XmNtopAttachment,XmATTACH_WIDGET,
		      XmNtopWidget,rec->create_update,
		      NULL);

	rec->link_tgl = XtVaCreateManagedWidget
                ("Linked Resources",xmToggleButtonGadgetClass,pdp->form,
		 XmNset,False,
		 XmNtopAttachment,XmATTACH_WIDGET,
		 XmNtopWidget,rec->plot_tree->tree,
		 XmNtopOffset,8,
                 XmNrightAttachment,XmATTACH_NONE,
                 XmNbottomAttachment,XmATTACH_NONE,
                 NULL);

        return pdp;
}

static void RestorePlotState(
	brPlotPageRec	*rec,
	NgPageSaveState save_state
)
{
	brPlotSaveState hs_state = (brPlotSaveState) save_state->page_state;

	rec->public.class_name = hs_state->class_name;
	rec->public.plot_style = hs_state->plot_style;
	rec->public.plot_style_dir = hs_state->plot_style_dir;
	rec->hlu_count = hs_state->hlu_count;
	rec->hlu_ids = hs_state->hlu_ids;
	rec->app_id = hs_state->app_id;
	rec->plot_delete_cb = hs_state->plot_delete_cb;
	rec->state = hs_state->state;
	rec->do_auto_update = hs_state->do_auto_update;
	rec->data_profile = hs_state->data_profile;
	rec->datalinks = hs_state->datalinks;
	rec->has_input_data = hs_state->has_input_data;
	NhlFree(hs_state);
	NhlFree(save_state);

	return;
}        

typedef struct _NgPlotDeleteInfoRec
{
	NrmQuark qvarname;
	NrmQuark qplotstyle;
	_NhlCB	 del_cb;
} NgPlotDeleteInfoRec, *NgPlotDeleteInfo;

static void DeletePlotVarCB
(
        NhlArgVal       cbdata,
        NhlArgVal       udata
)
{
	NgNclAny        sym = (NgNclAny) cbdata.ptrval;
	NgPlotDeleteInfo del_info = (NgPlotDeleteInfo) udata.ptrval;

	if (NrmStringToQuark(sym->name) != del_info->qvarname)
		return;
	NgDeletePlotAppRef(del_info->qplotstyle);

	_NhlCBDelete(del_info->del_cb);

	NhlFree(del_info);

	return;
}

static void InitializeGraphicVars
(
	brPage		*page
)
{
        brPageData	*pdp = page->pdata;
	brPlotPageRec	*rec = (brPlotPageRec	*)pdp->type_rec;
        NgPlotPage 	*pub = &rec->public;
	NgDataProfile 	dprof = rec->data_profile;
	char buf[256];
	char name_buf[128];
	char attr_buf[1024];
	int i;
        NhlArgVal       sel,user_data;
	NhlLayer	ncl;
	NgPlotDeleteInfo del_info;
	NhlString	varname = NrmQuarkToString(page->qvar);

	/* Create the NCL graphic variables now
	 * Each ncl variable gets its own reference to the plot app.
	 * The array var uses the one that was created for the plot as a
	 * whole.
	 */

	NgNclSubmitLine(rec->nclstate,"begin\n",True);
	sprintf(buf,"%s = new(%d,graphic)\n",varname,dprof->obj_count);
	(void)NgNclSubmitLine(rec->nclstate,buf,False);
	sprintf(attr_buf,"%s@%s = (/",varname,ndvOBJECTLIST);

	for (i = 0; i < dprof->obj_count; i++) {
		NhlString obj_name = NrmQuarkToString(dprof->qobjects[i]);
		sprintf(name_buf,"%s_%s",varname,obj_name);
		sprintf(buf,"%s = new(%d,graphic)\n",name_buf,1);
		(void)NgNclSubmitLine(rec->nclstate,buf,False);
		sprintf(&attr_buf[strlen(attr_buf)],"\"%s\",",obj_name);
	}
	if (dprof->obj_count) {
		/* backing up 1 to remove the last comma */
		sprintf(&attr_buf[strlen(attr_buf)-1],"/)\n");
		(void)NgNclSubmitLine(rec->nclstate,attr_buf,False);
	}
	NgNclSubmitLine(rec->nclstate,"end\n",False);

        NhlINITVAR(sel);
        NhlINITVAR(user_data);
	sel.lngval = NgNclCBDELETE_HLUVAR;
	del_info = NhlMalloc(sizeof(NgPlotDeleteInfoRec));
	del_info->qvarname = page->qvar;
	del_info->qplotstyle = NrmStringToQuark(pub->plot_style);
	user_data.ptrval = del_info;

	ncl = _NhlGetLayer(rec->nclstate);

	del_info->del_cb = _NhlAddObjCallback
		(ncl,NgCBnsObject,sel,DeletePlotVarCB,user_data);

	return;
}
		
static NhlBoolean 
InitializePlot
(
	brPage		*page,
	NhlPointer	init_data
)
{
        brPageData	*pdp = page->pdata;
	brPlotPageRec	*rec = (brPlotPageRec	*)pdp->type_rec;
        NgPlotPage 	*pub = &rec->public;
	brPlotObjCreate	cdata = (brPlotObjCreate) init_data;
	NgVarData	*vdata = NULL;
	int		vdata_count = 0;

	if (cdata) {
		pub->class_name = cdata->class_name;
		pub->plot_style = cdata->plot_style;
		pub->plot_style_dir = cdata->plot_style_dir;
		pub->plot_style_name = cdata->plot_style_name;
		rec->app_id = cdata->app_id;
		rec->has_input_data = cdata->has_input_data;
		rec->state = cdata->state;
		rec->qclass = NrmStringToQuark(cdata->class_name);
		vdata = cdata->vdata;
		vdata_count = cdata->vdata_count;
	}
	if (rec->data_profile) {
		NgFreeDataProfile(rec->data_profile);
	}
	
	if (! rec->app_id) {
		rec->app_id = NgNewPlotAppRef
			(rec->go->base.id,pub->plot_style,pub->plot_style_dir,
			 pub->plot_style_name,pub->class_name,False);
	}
	if (! rec->app_id)
		return False;
	
	rec->data_profile = NgNewPlotAppDataProfile
			(rec->go->base.id,
			 NrmStringToQuark(pub->plot_style));
	
	pub->config_required = False;

	InitializeGraphicVars(page);

	/*
	 * Set the variable data into the data profile.
	 */
	NgSetPlotAppDataVars(rec->go->base.id,
			     NrmStringToQuark(pub->plot_style),
			     NrmQuarkToString(page->qvar),
			     rec->data_profile,NULL,NULL,
			     vdata_count,vdata,page->qvar);

	if (! AllDataDefined(rec->data_profile->plotdata,
			     rec->data_profile->plotdata_count)) {
		pub->config_required = True;
	}
	if (rec->hlu_ids) {
		NhlFree(rec->hlu_ids);
	}
	if (cdata && cdata->obj_count) {
		rec->hlu_count = cdata->obj_count;
		rec->hlu_ids = NhlMalloc(sizeof(int) * rec->hlu_count);
		memcpy(rec->hlu_ids,cdata->obj_ids,
		       sizeof(int) * rec->hlu_count);
	}
	else if (rec->data_profile->obj_count) {
		rec->hlu_count = rec->data_profile->obj_count;
		rec->hlu_ids = NhlMalloc(sizeof(int) * rec->hlu_count);
		memset(rec->hlu_ids,(char) 0,
		       sizeof(int) * rec->hlu_count);
	}
	else {
		rec->hlu_count = 0;
		rec->hlu_ids = NULL;
	}
	return True;
}
	

extern brPageData *
_NgGetPlotPage
(
	NgGO		go,
        brPane		*pane,
	brPage		*page,
        brPage		*copy_page,
	NgPageSaveState save_state,
	NhlPointer	init_data
)
{
	brPageData		*pdp;
	brPlotPageRec		*rec,*copy_rec = NULL;
        int			i;
	int			hlu_id,count,*hlu_array = NULL;
	int			nclstate;
        XmString		xmstring;
	NhlBoolean		new = False;

        if (QString == NrmNULLQUARK) {
		QString = NrmStringToQuark(NhlTString);
		QGenArray = NrmStringToQuark(NhlTGenArray);
		QngPlotClass = NrmStringToQuark(NGPLOTCLASS);
        }

/*
 * find a page to use. If necessary create one.
 */
	if (copy_page) {
		copy_rec = (brPlotPageRec *) copy_page->pdata->type_rec;
	}
	for (pdp = pane->plot_pages; pdp != NULL; pdp = pdp->next) {
		if (!pdp->in_use)
		  break;
	}
        if (! pdp) {
                pdp = NewPlotPage(go,pane,page);
		new = True;
	}
        if (! pdp)
                return NULL;
        page->pdata = pdp;
	pdp->in_use = True;
        rec = (brPlotPageRec *) pdp->type_rec;

 	NhlVAGetValues(go->go.appmgr,
		NgNappNclState,	&nclstate,
		NULL);
	rec->nclstate = nclstate;

	if (! NclSymbolDefined(NrmQuarkToString(page->qvar))) {
		if (! (init_data && InitializePlot(page,init_data))) {
			return NULL;
		}
		if (rec->qclass != QngPlotClass) {
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				   "%s: invalid plot page class",
				   "_NgGetPlotPage"));
			return NULL;
		}
	}

	hlu_id = NgNclGetHluObjId
                (nclstate,NrmQuarkToString(page->qvar),&count,&hlu_array);
	if (hlu_id < NhlNOERROR)
		return NULL;
	if (count == 1) {
		hlu_array = NhlMalloc(sizeof(int));
		hlu_array[0] = hlu_id;
	}
	rec->hlu_ids = hlu_array;
	rec->hlu_count = count;
	for (i = 0; i < rec->hlu_count; i++) {
		if (! _NhlGetLayer(rec->hlu_ids[i]))
			rec->hlu_ids[i] = NhlNULLOBJID;
	}

	if (hlu_id > NhlNULLOBJID) {

                if (copy_rec && copy_rec->state == _plotPREVIEW)
                        rec->state = _plotPREVIEW;
                else
                        rec->state = _plotCREATED;
	}
	if (save_state) {
		RestorePlotState(rec,save_state);
	}
        if (rec->state == _plotCREATED)
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
        XtAddCallback
		(rec->link_tgl,XmNvalueChangedCallback,LinkToggleCB,page);

        if (! copy_page) {
		ResetPlotPage(page);
		if (new)
			_NgGOWidgetTranslations(go,pdp->form);
                return pdp;
        }

	if (! (copy_rec->qclass = QngPlotClass && copy_rec->data_profile)) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			   "%s: invalid plot page class","_NgGetPlotPage"));
		return NULL;
	}
        rec->public.class_name = copy_rec->public.class_name;
        rec->public.plot_style = copy_rec->public.plot_style;
        rec->public.plot_style_dir = copy_rec->public.plot_style_dir;
	rec->hlu_count = copy_rec->hlu_count;
	rec->max_seq_num = copy_rec->max_seq_num;
	if (rec->hlu_ids)
		NhlFree(rec->hlu_ids);
	if (copy_rec->hlu_ids) {
		rec->hlu_ids = NhlMalloc(sizeof(int) * copy_rec->hlu_count);
		memcpy(rec->hlu_ids,copy_rec->hlu_ids,
		       sizeof(int) * copy_rec->hlu_count);
	}
        rec->state = copy_rec->state;
	rec->new_data = copy_rec->new_data;
	rec->has_input_data = copy_rec->has_input_data;
        rec->do_auto_update = copy_rec->do_auto_update;

        if (rec->do_auto_update)
                XtVaSetValues(rec->auto_update,
                              XmNset,True,
                              NULL);
		
	rec->data_profile = NgCopyDataProfile(copy_rec->data_profile);
	if (rec->data_profile->plotdata_count) {
		NgUpdateDataVarGrid(rec->data_var_grid,
				    page->qvar,
				    rec->data_profile->plotdata_count,
				    rec->data_profile->plotdata);
		if (! XtIsManaged(rec->data_var_grid->grid))
			XtManageChild(rec->data_var_grid->grid);
	}
	UpdateVData(rec);
	ResetPlotPage(page);
	if (new)
		_NgGOWidgetTranslations(go,pdp->form);
        return pdp;
}

/*
 * allocates memory that caller is responsible for freeing
 */
int *NgPlotObjGetHluIds
(
	int go_id,
	int page_id,
	int *count
)
{
	brPlotPageRec *rec = (brPlotPageRec *) NgPageData(go_id,page_id);
	int *hlu_ids;
	
	*count = 0;

	if (! rec)
		return NULL;

	if (! rec->hlu_count)
		return NULL;
	hlu_ids = NhlMalloc(rec->hlu_count * sizeof(int));

	if (!hlu_ids) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NULL;
	}
	memcpy(hlu_ids,rec->hlu_ids,rec->hlu_count * sizeof(int));

	*count = rec->hlu_count;

	return hlu_ids;
}

