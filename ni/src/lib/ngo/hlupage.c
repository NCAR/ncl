/*
 *      $Id: hlupage.c,v 1.11 1997-10-23 02:17:53 dbrown Exp $
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

#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/PushBG.h>
#include <Xm/ToggleBG.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/XWorkstation.h>
#include <ncarg/hlu/View.h>
#include <ncarg/hlu/DataItem.h>

static NrmQuark QFillValue = NrmNULLQUARK;

static void
SetValCB
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	brSetValCBInfo   *info = (brSetValCBInfo *)udata.lngval;
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
        if (rec->do_setval_cb)
                NgResTreeResUpdateComplete(rec->res_tree,rec->hlu_id,False);

        return;
}

static NhlErrorTypes
ManageScalarField
(
        brPage	*page,
	char	*sfname,
 	NhlBoolean create,
	char	*fillvalue
)
{
	brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec *) pdp->type_rec;
	char buf[256];
	char *fillvaluestring = "";
        char sfbuf[256];
        int i,dimcount = 0;
        NgVarPageOutput *ditem = rec->var_data[0];
        int block_id;
        NhlString res_names[] =
        { "sfDataArray", "sfMissingValueV" };
        NhlString values[2];
        NhlBoolean quote[2] = { False, False };
        int res_count = 1;

/*
 * Start vis block
 */
	if (create) {
                block_id = NgNclVisBlockBegin
                        (rec->nclstate,_NgCREATE,sfname,"defaultapp",
                         "scalarFieldClass");
	}
        else if (!rec->new_data)
                return NhlNOERROR;
	else {
                block_id = NgNclVisBlockBegin
                        (rec->nclstate,_NgSETVAL,sfname,NULL,NULL);
	}
/*
 * Set up res list
 */
        if (ditem->qfile > NrmNULLQUARK)
                sprintf(sfbuf,"%s->%s(",
                        NrmQuarkToString(ditem->qfile),
                        NrmQuarkToString(ditem->qvar));
                else
                        sprintf(sfbuf,"%s(",NrmQuarkToString(ditem->qvar));
        for (i=ditem->ndims-1; i>=0;i--) {
                if (abs((ditem->finish[i] - ditem->start[i])
                        /ditem->stride[i])< 1)
                        continue;
                dimcount++;
        }
        if (dimcount < 2) {
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,
			   "insufficient dimensionality"));
                return NhlFATAL;
        }
        for (i = 0; i < ditem->ndims; i++) {
                if ((ditem->finish[i] - ditem->start[i])
                    /ditem->stride[i] == 0) {
                        sprintf(&sfbuf[strlen(sfbuf)],"%d,",
                                ditem->start[i]);
                        continue;
                }
                else if (dimcount > 2) {
                        sprintf(&sfbuf[strlen(sfbuf)],"%d,",
                                ditem->start[i]);
                        dimcount--;
                        continue;
                }
                sprintf(&sfbuf[strlen(sfbuf)],"%d:%d:%d,",
                        ditem->start[i],ditem->finish[i],ditem->stride[i]);
        }
	/* backing up 1 to get rid of last comma */
        sprintf(&sfbuf[strlen(sfbuf)-1],")");
        values[0] = sfbuf;

	if (fillvalue) {
                res_count = 2;
                values[1] = fillvalue;
	}
        NgNclVisBlockAddResList(rec->nclstate,block_id,res_count,
                                res_names,values,quote);
        
        NgNclVisBlockEnd(rec->nclstate,block_id);

	return NhlNOERROR;
}


static Boolean ManagePlotObj
(
        brPage	*page,
	char 	**dataitemlist,
	int	dataitemcount,
        int	wk_id
)
{
	brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec *) pdp->type_rec;
        NgDataSinkRec *dsp = rec->public.data_info;
	char buf[512];
	char *cp = buf;
	char title[] = 
		"\"tiMainString\" : \"%s\"\n";
        int wid;
        Dimension h,w;
	char cellsizestring[16];
        int block_id;
        int res_count = 0;
        
	char *cnlineres[] = {
		"cnScalarFieldData",
		NULL 
	};
	char *cnlinevalues[] = { 
		NULL,
		NULL
	};

	char *cnfillres[] = {
		"cnScalarFieldData",
		"cnLinesOn",
		"cnLineLabelsOn",
		"cnFillOn",
		"pmLabelBarDisplayMode",
		"vpXF",
		NULL
	};
	char *cnfillvalues[] = {
		NULL,
		"False",
		"False",
		"True",
		"\"Conditional\"",
		"\"0.12\"",
		NULL
	};

	char *cnrasterres[] = {
		"cnScalarFieldData",
		"cnLinesOn",
		"cnLineLabelsOn",
		"cnRasterModeOn",
                "cnRasterSmoothingOn",
		"pmLabelBarDisplayMode",
		"vpXF",
		NULL
	};
	char *cnrastervalues[] = {
		NULL,
		"False",
		"False",
		"True",
                "False",
		"\"Conditional\"",
		"\"0.12\"",
		NULL
	};

	char *cninterpolatedrasterres[] = {
		"cnScalarFieldData",
		"cnLinesOn",
		"cnLineLabelsOn",
		"cnRasterModeOn",
                "cnRasterSmoothingOn",
		"pmLabelBarDisplayMode",
		"vpXF",
		NULL
	};
	char *cninterpolatedrastervalues[] = {
		NULL,
		"False",
		"False",
		"True",
                "True",
		"\"Conditional\"",
		"\"0.12\"",
		NULL
	};
        
	char *stres[] = {
		"stVectorFieldData",
		NULL
	};
	char *stvalues[] = {
		NULL,
		NULL
	};

	char *vclineres[] = {
		"vcVectorFieldData",
		"vcScalarFieldData",
		"vcMonoLineArrowColor",
		"vcUseScalarArray",
		"pmLabelBarDisplayMode",
		"vpXF",
		NULL
	};
	char *vclinevalues[] = {
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL
	};
	
	char *vcfillres[] = {
		"vcVectorFieldData",
		"vcScalarFieldData",
		"vcMonoFillArrowFillColor",
		"vcUseScalarArray",
		"pmLabelBarDisplayMode",
		"vpXF",
		"vcFillArrowsOn",
		NULL
	};
	
	char *vcfillvalues[] = {
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		"True",
		NULL
	};

	char *xylineres[] = {
		"xyCoordData",
		"xyXIrregularPoints",
		"xyYIrregularPoints",
		"xyXStyle",
		"xyYStyle",
		NULL
	};
	
	char *xylinevalues[] = {
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL
	};
	
	char **res;
	char **values;
	char *copy_res[16];
        NhlBoolean quote[16];
        
	int i;
	int sv_res_count[16] = { 1, 1, 1, 1, 2, 2, 3, 3 };

	switch (dsp->type) {
 	case (int)ngLINECONTOUR:
		res = cnlineres;
		values = cnlinevalues;
		break;
	case (int)ngFILLCONTOUR:
		res = cnfillres;
		values = cnfillvalues;
		break;
 	case (int)ngRASTERCONTOUR:
		res = cnrasterres;
		values = cnrastervalues;
		break;
 	case (int)ngINTERPOLATEDRASTERCONTOUR:
                res = cninterpolatedrasterres;
		values = cninterpolatedrastervalues;
		break;
 	case (int)ngSTREAMLINE:
		res = stres;
		values = stvalues;
		break;
 	case (int)ngLINEVECTOR:
		res = vclineres;
		values = vclinevalues;
		break;
 	case (int)ngFILLVECTOR:
		res = vcfillres;
		values = vcfillvalues;
		break;
 	case (int)ngLINEXY:
 	case (int)ngSCATTERXY:
		res = xylineres;
		values = xylinevalues;
		break;
	default:
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,"not supported yet"));
		return False;
	}

	for (i = 0; i < dataitemcount; i++) {
		values[i] = dataitemlist[i];
	}

        if (rec->state != _hluCREATED) {
                block_id = NgNclVisBlockBegin(rec->nclstate,_NgCREATE,
                                              NrmQuarkToString(page->qvar),
                                              (NhlString)NhlName(wk_id),
                                              rec->public.class_name);
		for (i=0; res[i] != NULL; i++){
                        res_count++;
                        quote[i] = False;
                }
        }
        else {
                block_id = NgNclVisBlockBegin(rec->nclstate,_NgSETVAL,
                                              NrmQuarkToString(page->qvar),
                                              NULL,NULL);
#if 0                
		for (i=0; i < sv_res_count[dsp->type]; i++) {
                        res_count++;
                        quote[i] = False;
                }
#endif                
        }
        NgNclVisBlockAddResList(rec->nclstate,block_id,res_count,
                                res,values,quote);
        NgResTreeAddResList(rec->nclstate,(NhlPointer)rec->res_tree,block_id);
        
        NgNclVisBlockEnd(rec->nclstate,block_id);
                
	return True;
}

static void
ContourCreateUpdate
(
        brPage		*page,
        int		wk_id
)
{
	brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec *) pdp->type_rec;
	char buf[512];
        NgDataSinkRec *dsp = rec->public.data_info;
	char *dataitemlist[2];
	char *fillvalue = NULL;
	NclApiDataList		*dl;
	NclExtValueRec *val = NULL;
	char *sval;
        int i;
        static NrmQuark QFillValue = NrmNULLQUARK;
        NhlErrorTypes ret;
        XmString xmstring;
        NhlBoolean create,already_created = False;
        
        if (QFillValue == NrmNULLQUARK) {
                QFillValue = NrmStringToQuark("_FillValue"); 
        }
        rec->do_setval_cb = False;
        
	create = False;
	if (rec->data_objects[0] <= NrmNULLQUARK) {
                char *name;
		sprintf(buf,"%s_%s",NrmQuarkToString(page->qvar),"sf");
                name = NgNclGetSymName(rec->nclstate,buf,True);
                
		rec->data_objects[0] = NrmStringToQuark(name);
		create = True;
	}
        
	dataitemlist[0] = NrmQuarkToString(rec->data_objects[0]);
        if (rec->var_data[0]->qfile > NrmNULLQUARK)
                dl = NclGetFileVarInfo(rec->var_data[0]->qfile,
                                       rec->var_data[0]->qvar);
        else
                dl = NclGetVarInfo(rec->var_data[0]->qvar);
        for (i = 0; i < dl->u.var->n_atts; i++) {
                if (dl->u.var->attnames[i] == QFillValue)
                        break;
        }
        if (i == dl->u.var->n_atts)
                fillvalue = NULL;
        else if (rec->var_data[0]->qfile > NrmNULLQUARK)
                val = NclReadFileVarAtt
                        (rec->var_data[0]->qfile,rec->var_data[0]->qvar,
                         dl->u.var->attnames[i]);
        else 
                val = NclReadVarAtt
                        (rec->var_data[0]->qvar,dl->u.var->attnames[i]);
        if (val) {
                fillvalue = NclTypeToString(val->value,val->type);
                if (val->constant != 0)
                        NclFree(val->value);
                NclFreeExtValue(val);
        }
	NclFreeDataList(dl);
	
	ret = ManageScalarField(page,dataitemlist[0],create,fillvalue);
	if (fillvalue)
		NclFree(fillvalue);
        
        if (ret < NhlWARNING)
                return;

        if (rec->state == _hluPREVIEW) {
                sprintf(buf,"destroy(%s)\n",NrmQuarkToString(page->qvar));
                (void)NgNclSubmitBlock(rec->nclstate,buf);
        }
        if  (! ManagePlotObj(page,dataitemlist,1,wk_id))
                return;

        switch (rec->state) {
            case _hluNOTCREATED:
                    rec->state = _hluPREVIEW;
                    rec->res_tree->preview_instance = True;
                    break;
            case _hluPREVIEW:
                    rec->state = _hluCREATED;
                    rec->res_tree->preview_instance = False;
                    
                    xmstring = NgXAppCreateXmString
                            (rec->go->go.appmgr,"Update");
                    XtVaSetValues(rec->create_update,
                                  XmNlabelString,xmstring,
                                  NULL);
                    NgXAppFreeXmString(rec->go->go.appmgr,xmstring);
                    break;
            case _hluCREATED:
                    already_created = True;
                    break;
        }
        if (! already_created) {
                NhlArgVal sel,user_data;
                
                val = NclGetHLUObjId(NrmQuarkToString(page->qvar));
                if (val->totalelements > 1)
			NHLPERROR((NhlINFO,NhlEUNKNOWN,
				   "var references hlu object array"));
                rec->hlu_id = ((int*)val->value)[0];
                NclFreeExtValue(val);
                NhlINITVAR(sel);
                NhlINITVAR(user_data);
                sel.lngval = 0;
		rec->setval_info.pid = page->id;
		rec->setval_info.goid = page->go->base.id;
                user_data.ptrval = &rec->setval_info;
                rec->setval_cb = _NhlAddObjCallback
                        (_NhlGetLayer(rec->hlu_id),_NhlCBobjValueSet,
                         sel,SetValCB,user_data);
        }
        NgResTreeResUpdateComplete(rec->res_tree,rec->hlu_id,False);

        if (rec->state == _hluCREATED) {
                NgDrawGraphic(rec->go->base.id,
                              NrmQuarkToString(page->qvar),True);
                rec->new_data = False;
        }
        rec->do_setval_cb = True;
        
	return;
}
static int
CreatePreviewInstance
(
        brPage	*page,
        int	wk_id
)
{
        NhlErrorTypes	ret;
	brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec *) pdp->type_rec;
        int		hlu_id;
        NgPreviewResProc	resproc[2];
        XtPointer	resdata[2];
        NhlString	parent = NULL;

        resproc[0] = NgResTreePreviewResList;
        resdata[0] = (NhlPointer)rec->res_tree;
        if (wk_id != NhlNULLOBJID)
                parent = NgNclGetHLURef(rec->go->go.nclstate,wk_id);
        
        ret = NgCreatePreviewGraphic
                (rec->go->base.id,&hlu_id,
                 NrmQuarkToString(page->qvar),parent,
                 rec->public.class_name,1,resproc,resdata);
        if (ret > NhlFATAL)
                return hlu_id;

        return ret;
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
        int		hlu_id;
        NgSetResProc	setresproc[2];
        XtPointer	setresdata[2];
        NhlString	parent = NULL;

        setresproc[0] = NgResTreeAddResList;
        setresdata[0] = (NhlPointer)rec->res_tree;

        if (wk_id != NhlNULLOBJID)
                parent = NgNclGetHLURef(rec->go->go.nclstate,wk_id);
        
        ret = NgCreateGraphic
                (rec->go->base.id,&hlu_id,
                 NrmQuarkToString(page->qvar),parent,
                 rec->public.class_name,1,setresproc,setresdata);
        if (NhlClassIsSubclass(rec->class,NhlviewClass)) {
                NgDrawGraphic
                        (rec->go->base.id,NrmQuarkToString(page->qvar),True);
        }
        if (ret > NhlFATAL)
                return hlu_id;
        
        return ret;
}

static int
UpdateInstance
(
        brPage	*page,
        int	wk_id
)
{
        NhlErrorTypes	ret;
	brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec *) pdp->type_rec;
        int		hlu_id;
        NgSetResProc	setresproc[2];
        XtPointer	setresdata[2];

        setresproc[0] = NgResTreeAddResList;
        setresdata[0] = (XtPointer)rec->res_tree;
        
        ret = NgUpdateGraphic
                (rec->go->base.id,NrmQuarkToString(page->qvar),
                 1,setresproc,setresdata);
        if (NhlClassIsSubclass(rec->class,NhlviewClass)) {
                NgDrawGraphic
                        (rec->go->base.id,NrmQuarkToString(page->qvar),True);
        }
        
        return ret;
}

static void
CreateUpdate
(
        brPage		*page,
        int		wk_id
)
{
	brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec *) pdp->type_rec;
        NhlBoolean 	already_created = False;
        int		hlu_id;
        
        rec->do_setval_cb = False;
        if (rec->state == _hluNOTCREATED) {
                if (NhlClassIsSubclass(rec->class,NhldataItemClass)) {
                        hlu_id = NhlNULLOBJID;
                }
                else {
                        hlu_id = CreatePreviewInstance(page,wk_id);
                        if (hlu_id <= NhlNULLOBJID) {
                                NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                                           "Could not create %s graphic",
                                           NrmQuarkToString(page->qvar)));
                                rec->hlu_id = NhlNULLOBJID;
                                return;
                        }
                }
                rec->hlu_id = hlu_id;
                rec->state = _hluPREVIEW;
                rec->res_tree->preview_instance = True;
        }
        else  if (rec->state == _hluPREVIEW) {
                XmString xmstring;
                
                if (rec->hlu_id > NhlNULLOBJID)
                        NgDestroyPreviewGraphic(rec->go->base.id,rec->hlu_id);
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
        }
        else {
                UpdateInstance(page,wk_id);
                already_created = True;
        }
        if (rec->hlu_id > NhlNULLOBJID && ! already_created) {
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
                rec->do_setval_cb = True;
        }
        NgResTreeResUpdateComplete(rec->res_tree,rec->hlu_id,False);
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
        
        if (NhlClassIsSubclass(rec->class,NhlworkstationClass)||
            NhlClassIsSubclass(rec->class,NhlappClass) ||
            NhlClassIsSubclass(rec->class,NhldataItemClass)) {
                wk_id = NhlNULLOBJID;
        }
        else {
                wk_id = NgAppGetSelectedWork
                        (page->go->go.appmgr,work_created);
                if (*work_created) {
                        XRaiseWindow(rec->go->go.x->dpy,
                                     XtWindow(rec->go->go.shell));
                }
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
	brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec *) pdp->type_rec;
        NgHluPage 	*pub = &rec->public;
        int		wk_id;
	char		buf[512];
        NhlBoolean	work_created;

#if DEBUG_HLUPAGE
        fprintf(stderr,"in CreateUpdateCB\n");
#endif
        
        wk_id = GetWorkstation(page,&work_created);
        if (pub->data_info &&
            !strcmp(rec->public.class_name,"contourPlotClass")) {
                ContourCreateUpdate(page,wk_id);
        }
        else {
                CreateUpdate(page,wk_id);
        }
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
static void
CopyVarData
(
        NgVarPageOutput **to_data,
        NgVarPageOutput *from_data
        )
{
        if (! from_data) {
                if (*to_data) {
                        if ((*to_data)->start)
                                NhlFree((*to_data)->start);
                        if ((*to_data)->finish)
                                NhlFree((*to_data)->finish);
                        if ((*to_data)->stride)
                                NhlFree((*to_data)->stride);
                        NhlFree((*to_data));
                }
                (*to_data) = NULL;
                return;
        }
        if (! *to_data) {
                (*to_data) = NhlMalloc(sizeof(NgVarPageOutput));
                (*to_data)->ndims = 0;
                (*to_data)->start = NULL;
                (*to_data)->finish = NULL;
                (*to_data)->stride = NULL;
        }
        if (from_data->ndims > (*to_data)->ndims) {
                (*to_data)->start = NhlRealloc
                        ((*to_data)->start,from_data->ndims * sizeof(long));
                (*to_data)->finish = NhlRealloc
                        ((*to_data)->finish,from_data->ndims * sizeof(long));
                (*to_data)->stride = NhlRealloc
                        ((*to_data)->stride,from_data->ndims * sizeof(long));
        }
        (*to_data)->qfile = from_data->qfile;
        (*to_data)->qvar = from_data->qvar;
        (*to_data)->ndims = from_data->ndims;
        (*to_data)->data_ix = from_data->data_ix;
        memcpy((*to_data)->start,
               from_data->start,(*to_data)->ndims * sizeof(long));
        memcpy((*to_data)->finish,
               from_data->finish,(*to_data)->ndims * sizeof(long));
        memcpy((*to_data)->stride,
               from_data->stride,(*to_data)->ndims * sizeof(long));

        return;
}

static NhlBoolean
UpdateVarData
(
        brHluPageRec	*rec,
        int 		var_ix,
        NgVarPageOutput *var_data
        )
{
        NgVarPageOutput *lvar_data;
        NhlBoolean update = False;
        int i;

        if (! rec->var_data[var_ix] ||
            var_data->ndims != rec->var_data[var_ix]->ndims ||
            var_data->qfile != rec->var_data[var_ix]->qfile ||
            var_data->qvar != rec->var_data[var_ix]->qvar) {
                CopyVarData(&rec->var_data[var_ix],var_data);
                return True;
        }
        
        lvar_data = rec->var_data[var_ix];
        
        for (i = 0; i < lvar_data->ndims; i++) {
                if (lvar_data->start[i] != var_data->start[i] ||
                    lvar_data->finish[i] != var_data->finish[i] ||
                    lvar_data->stride[i] != var_data->stride[i]) {
                        update = True;
                        break;
                }
        }
        if (update) {
                CopyVarData(&rec->var_data[var_ix],var_data);
                return True;
        }
        return False;
}

static void HluPageInputNotify (
        brPage *page,
        brPageType output_page_type,
 	NhlPointer output_data
        )
{
        brPageData	*pdp = page->pdata;
	brHluPageRec	*rec = (brHluPageRec	*)pdp->type_rec;
        NgHluPage 	*pub = &rec->public;
        NgVarPageOutput	*var_data = (NgVarPageOutput *)output_data;
        int		wk_id;
        NhlBoolean 	work_created,non_contour = False;
        
                
#if DEBUG_HLUPAGE
        fprintf(stderr,"in hlu page input notify\n");
#endif

        switch (output_page_type) {
            case _brNULL:
                    non_contour = True;
                    break;
            case _brREGVAR:
            case _brFILEVAR:
                    if (! UpdateVarData(rec,var_data->data_ix,var_data))
                            return;
                    NgUpdateDataSinkGrid
                            (rec->data_sink_grid,page->qvar,pub->data_info);
                    rec->new_data = True;
                    break;
            default:
		    NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			       "page type not supported for input"));
		    return;
        }
        if (rec->state < _hluCREATED || rec->do_auto_update) {
                
                wk_id = GetWorkstation(page,&work_created);
                
                if (non_contour)
                        CreateUpdate(page,wk_id);
                else
                        ContourCreateUpdate(page,wk_id);
        }
        
        return;
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
DeactivateHluPage
(
	brPage	*page
)
{
	brHluPageRec *rec = (brHluPageRec *)page->pdata->type_rec;
        int i;

        if (rec->create_update)
                XtRemoveCallback(rec->create_update,
                                 XmNactivateCallback,CreateUpdateCB,page);
        if (rec->auto_update)
                XtRemoveCallback(rec->auto_update,
                                 XmNvalueChangedCallback,AutoUpdateCB,page);

        rec->state = _hluNOTCREATED;
        rec->do_auto_update = False;
        rec->public.data_info = NULL;
        rec->public.class_name = NULL;
        rec->new_data = True;
        
        for (i=0; i <  8; i++)
                rec->data_objects[i] = NrmNULLQUARK;
        rec->activated = False;
}

static void DestroyHluPage
(
	NhlPointer data
)
{
	brHluPageRec	*hlu_rec = (brHluPageRec *)data;
        int i,j;

        NgDestroyDataSinkGrid(hlu_rec->data_sink_grid);

        if (! hlu_rec->var_data) {
                NhlFree(data);
                return;
        }
        
        for (i = 0; i < hlu_rec->var_data_count; i++) {
                if (hlu_rec->var_data[i]) {
                        NgVarPageOutput *vdata = hlu_rec->var_data[i];
                        if (vdata->start)
                                NhlFree(vdata->start);
                        if (vdata->finish)
                                NhlFree(vdata->finish);
                        if (vdata->stride)
                                NhlFree(vdata->stride);
                        NhlFree(vdata);
                }
        }
        NhlFree(hlu_rec->var_data);

        if (hlu_rec->setval_cb) {
               _NhlCBDelete(hlu_rec->setval_cb);
               hlu_rec->setval_cb = NULL;
        }
        
        NgDestroyResTree(hlu_rec->res_tree);
        
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
        if (rec->data_sink_grid) {
                XtVaGetValues(rec->data_sink_grid->grid,
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
	int		hlu_id;
        
#if DEBUG_HLUPAGE
        fprintf(stderr,"in updata hlu page\n");
#endif

        if (! pub->class_name) {
               NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                          "no class specified for graphic variable %s",
                          NrmQuarkToString(page->qvar)));
                return NhlFATAL;
        } 
                
#if DEBUG_HLUPAGE
        fprintf(stderr,"%s\n",pub->class_name);
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

        if (pub->data_info) {
		if (pub->data_info->n_dataitems > rec->var_data_count) {
                	int i;
                
			rec->var_data = NhlRealloc
				(rec->var_data,pub->data_info->n_dataitems *
				 sizeof(NgVarPageOutput *));
			for (i = rec->var_data_count;
			     i < pub->data_info->n_dataitems; i++)
				rec->var_data[i] = NULL;
		}
		rec->var_data_count = pub->data_info->n_dataitems;
        
		rec->data_sink_grid->dataitems = rec->var_data;
		NgUpdateDataSinkGrid
			(rec->data_sink_grid,page->qvar,pub->data_info);
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
        }
        else {
                rec->res_tree->preview_instance =
                        rec->state == _hluCREATED ? False : True;
                NgUpdateResTree
                        (rec->res_tree,page->qvar,rec->class,rec->hlu_id);
        }
        XtVaGetValues(pdp->pane->scroller,
                      XmNhorizontalScrollBar,&rec->res_tree->h_scroll,
                      XmNverticalScrollBar,&rec->res_tree->v_scroll,
                      NULL);
        
        rec->res_tree->geo_data = (NhlPointer) page;
        
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
        
        rec->data_sink_grid = NULL;
        rec->res_tree = NULL;
        rec->create_update = NULL;
        rec->auto_update = NULL;
        rec->var_data_count = 0;
        rec->new_data = True;
        rec->var_data = NULL;
        rec->state = _hluNOTCREATED;
        rec->do_auto_update = False;
        rec->public.data_info = NULL;
        rec->public.class_name = NULL;
        rec->class = NULL;
        rec->hlu_id = NhlNULLOBJID;
        rec->setval_cb = NULL;
        rec->do_setval_cb = False;
        
        for (i=0; i <  8; i++)
                rec->data_objects[i] = NrmNULLQUARK;
        
	pdp->form = XtVaCreateManagedWidget
		("form",xmFormWidgetClass,pane->folder,
                 XmNy,28,
                 XmNx,2,
                 NULL);
        
	pdp->destroy_page = DestroyHluPage;
	pdp->adjust_page_geo = AdjustHluPageGeometry;
	pdp->deactivate_page = DeactivateHluPage;
	pdp->page_output_notify = NULL;
        pdp->page_input_notify = HluPageInputNotify;
        pdp->public_page_data = PublicHluPageData;
        pdp->update_page = UpdateHluPage;
        pdp->pane = pane;
        
        rec->data_sink_grid = NgCreateDataSinkGrid
                (pdp->form,page->qvar,rec->public.data_info);
        XtVaSetValues(rec->data_sink_grid->grid,
                      XmNbottomAttachment,XmATTACH_NONE,
                      XmNrightAttachment,XmATTACH_NONE,
                      NULL);
        
        rec->create_update = XtVaCreateManagedWidget
                ("Create/Update",xmPushButtonGadgetClass,pdp->form,
                 XmNtopAttachment,XmATTACH_WIDGET,
                 XmNtopWidget,rec->data_sink_grid->grid,
                 XmNrightAttachment,XmATTACH_NONE,
                 XmNbottomAttachment,XmATTACH_NONE,
                 NULL);

        rec->auto_update = XtVaCreateManagedWidget
                ("Auto Update",xmToggleButtonGadgetClass,pdp->form,
                 XmNtopAttachment,XmATTACH_WIDGET,
                 XmNtopWidget,rec->data_sink_grid->grid,
                 XmNleftAttachment,XmATTACH_WIDGET,
                 XmNleftWidget,rec->create_update,
                 XmNrightAttachment,XmATTACH_NONE,
                 XmNbottomAttachment,XmATTACH_NONE,
                 NULL);
        
        return pdp;
}
        
extern brPageData *
NgGetHluPage
(
	NgGO		go,
        brPane		*pane,
	brPage		*page,
        brPage		*copy_page
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

        if (QFillValue == NrmNULLQUARK) {
                QFillValue = NrmStringToQuark("_FillValue"); 
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
        if (! pdp)
                pdp = NewHluPage(go,pane,page);
        if (! pdp)
                return NULL;
        page->pdata = pdp;
	pdp->in_use = True;
        
        rec = (brHluPageRec *) pdp->type_rec;
        
	if (hlu_id > NhlNULLOBJID) {
                NhlArgVal sel,user_data;
                
		rec->hlu_id = hlu_id;
		rec->class = NhlClassOfObject(hlu_id);
		rec->public.class_name = rec->class->base_class.class_name;
                if (copy_rec && copy_rec->state == _hluPREVIEW)
                        rec->state = _hluPREVIEW;
                else
                        rec->state = _hluCREATED;
                NhlINITVAR(sel);
                NhlINITVAR(user_data);
                sel.lngval = 0;
		rec->setval_info.pid = page->id;
		rec->setval_info.goid = page->go->base.id;
                user_data.ptrval = &rec->setval_info;
                rec->setval_cb = _NhlAddObjCallback
                        (_NhlGetLayer(hlu_id),_NhlCBobjValueSet,
                         sel,SetValCB,user_data);
	}
        else {
                rec->hlu_id = NhlNULLOBJID;
                rec->class = NULL;
                rec->public.class_name = NULL;
                rec->state = _hluNOTCREATED;
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
                        UpdateHluPage(page);
                return pdp;
        }
        rec->public.data_info = copy_rec->public.data_info;
        
/* ResTree */
        
        if (rec->res_tree) {
                 NgDupResTree(page->go,pdp->form,page->qvar,
                              rec->class,rec->hlu_id,
                              rec->res_tree,copy_rec->res_tree);
        }
        else {
                rec->res_tree = NgDupResTree
                        (page->go,pdp->form,page->qvar,
                         rec->class,rec->hlu_id,
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
        rec->do_auto_update = copy_rec->do_auto_update;

        if (rec->do_auto_update)
                XtVaSetValues(rec->auto_update,
                              XmNset,True,
                              NULL);
        for (i = 0; i < 8; i++)
                rec->data_objects[i] = copy_rec->data_objects[i];
        if (copy_rec->var_data_count > 0) {
                rec->var_data = NhlRealloc
                        (rec->var_data,
                         copy_rec->var_data_count * sizeof(NgVarPageOutput *));
                for (i = rec->var_data_count; i<copy_rec->var_data_count; i++)
                        rec->var_data[i] = NULL;
                rec->var_data_count = copy_rec->var_data_count;
                for (i = 0; i < rec->var_data_count; i++) {
                        CopyVarData(&rec->var_data[i],
                                    copy_rec->var_data[i]);
                }
                rec->data_sink_grid->dataitems = rec->var_data;
                NgUpdateDataSinkGrid
                        (rec->data_sink_grid,page->qvar,rec->public.data_info);
        }
#if 0        
        if (rec->hlu_id > NhlNULLOBJID)
                UpdateHluPage(page);
#endif        
        return pdp;
}
