/*
 *      $Id: restree.c,v 1.5 1997-10-04 01:49:51 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		restree.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Jul 28 13:18:34 MDT 1997
 *
 *	Description:	
 */
#include <float.h>

#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/MenuShellP.h>
#include <Xm/RowColumnP.h>
#include <Xm/Frame.h>
#include <Xm/Form.h>
#include <Xm/PushBG.h>
#include  <Xm/Text.h>
#include  <ncarg/ngo/Tree.h>

#include <ncarg/ngo/restreeP.h>
#include <ncarg/ngo/xutil.h>
#include <ncarg/ngo/sort.h>
#include <ncarg/ngo/stringutil.h>
#include <ncarg/ngo/MegaB.h>
#include <ncarg/hlu/AppI.h>
#include <ncarg/hlu/View.h>
#include <ncarg/hlu/DataCommP.h>
#include <ncarg/hlu/ConvertP.h>

static void EnumButtonUpAction(
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
);

static XtActionsRec myact[] = {
        { "EnumButtonUpAction", EnumButtonUpAction },
};

static NrmQuark Qlong_name;
static Dimension Row_Height;
static Dimension Char_Height;
static Pixel Foreground,Background;
static NrmQuark Qpointer,Qimmediate,Qdatalist,Qobjid,Qdataspeclist;
static NrmQuark Qgenarray,Qdouble,Qfloat,Qvariable,Qstring,Qenum,Qcolorindex;
static int Grlist;

#define PIXMAP_WIDTH 9
#define PIXMAP_HEIGHT 9

static char Check_Bits[] = {
        0x00,0x01,0x80,0x01,0xc0,0x00,0x61,0x00,0x37,0x00,0x3e,0x00,
        0x1c,0x00,0x08,0x00,0x00,0x00 
};

static char No_Check_Bits[] = {
        0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
        0x00,0x00,0x00,0x00,0x00,0x00 
};

static Pixmap Check_Pixmap, No_Check_Pixmap,Test_Pixmap;

static Pixel Black,White;

typedef struct _SResVal 
{
        NhlString res_name;
        NhlPointer actual_val;
	NhlString actual_str;
        NhlString fake_val;
} SResVal;

static SResVal SpecialResources[] = {
        {NhlNtfPlotManagerOn,(NhlPointer)False,"False", "True" },
        {NhlNpmLabelBarDisplayMode,(NhlPointer)NhlNOCREATE,"NoCreate","Never"},
        {NhlNpmLegendDisplayMode,(NhlPointer)NhlNOCREATE,"NoCreate","Never"},
        {NhlNpmTickMarkDisplayMode,(NhlPointer)NhlNOCREATE,"NoCreate","Never"},
        {NhlNpmTitleDisplayMode,(NhlPointer)NhlNOCREATE,"NoCreate","Never"}
};
                
static NhlBoolean IsInstantiatedChild
(
        NhlLayer layer,
        NhlClass class
        )
{
        _NhlAllChildList child;
        
        if (_NhlIsObj(layer))
                return False;
        child = layer->base.all_children;

        while (child) {
                NhlLayer child_layer;
                
                if (NhlClassIsSubclass(NhlClassOfObject(child->pid),class))
                        return True;
                child_layer = _NhlGetLayer(child->pid);
                if (IsInstantiatedChild(child_layer,class))
                        return True;
                child = child->next;
        }
        return False;
}
                    
                
static void MarkInstantiated
(
        NgResTreeRec	*rtp
        )

{
        int i;
        NhlLayer layer;

        if (rtp->hlu_id <= NhlNULLOBJID) {
                for (i=0; i < rtp->class_count; i++)
                        rtp->instantiated[i] = True;
                return;
        }
        else {
                for (i=0; i <= rtp->super_class_count; i++)
                        rtp->instantiated[i] = True;
        }
        layer = _NhlGetLayer(rtp->hlu_id);
        for (i = rtp->super_class_count+1; i < rtp->class_count; i++) {
                NhlClass class = rtp->classes[i];
                rtp->instantiated[i] = IsInstantiatedChild(layer,class);
        }
        return;
        
}

        
static void ExpandClassGroup 
(
        NgResTreeRec	*rtp,
        rtNodeData	*ndata,
        int		row
        )
{
        NgResTree *pub_rtp = &rtp->restree;
        int class_group = (int) ndata->info;
        int nclasses, i,j,start_ix,res_start,res_end;
        int new_level = 10 * (ndata->type / 10) + 10;
        XmLTreeRowDefinition *rowdefs;
        char buf[256];
        NhlLayer layer = _NhlGetLayer(rtp->hlu_id);

        printf("expanding class group\n");

        switch (class_group) {
            case _rtSuperClassGroup:
                    nclasses = rtp->super_class_count;
                    start_ix = 0;
                    break;
            case _rtChildClassGroup:
                    nclasses = rtp->class_count - rtp->super_class_count - 1;
                    start_ix = rtp->super_class_count + 1;
                    break;
        }
        rowdefs = NhlMalloc(nclasses * sizeof(XmLTreeRowDefinition));
        ndata->subdata = NhlMalloc(nclasses * sizeof(rtNodeData));

        MarkInstantiated(rtp);
        
        XtVaSetValues(pub_rtp->tree,
                      XmNlayoutFrozen,True,
                      NULL);
        for (i = 0; i < nclasses; i++) {
                rowdefs[i].expands = rtp->top_res_counts[start_ix+i] > 0 ?
                        True : False;
                if (! pub_rtp->preview_instance &&
                    ! rtp->instantiated[start_ix+i])
                        rowdefs[i].expands = False;
                ndata->subdata[i].type = new_level + _rtClass;
                sprintf(buf,"%s",
                        rtp->classes[start_ix+i]->base_class.class_name);
                rowdefs[i].level = new_level / 10;
                rowdefs[i].isExpanded = False;
                rowdefs[i].pixmap = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].pixmask = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].string = XmStringCreateLocalized(buf);
                ndata->subdata[i].parent = ndata;
                ndata->subdata[i].info = (void*)rtp->classes[start_ix+i];
                ndata->subdata[i].row = row+i;
                ndata->subdata[i].expanded = False;
                ndata->subdata[i].subcount = 0;
                ndata->subdata[i].subdata = NULL;
        }
        XmLTreeAddRows(pub_rtp->tree,rowdefs,nclasses,row);
        ndata->expanded = True;
        ndata->subcount = nclasses;

        res_start = 0;
        for (i = 0; i < nclasses; i++) {
                XmStringFree(rowdefs[i].string);
                XtVaSetValues(pub_rtp->tree,
                              XmNrow,row+i,
                              XmNrowUserData,&ndata->subdata[i],
                              NULL);
                if (pub_rtp->preview_instance || rtp->instantiated[start_ix+i])
                        sprintf(buf,"%d resources",
                                rtp->top_res_counts[start_ix+i]);
                else 
                        sprintf(buf,"not currently instantiated");
                XmLGridSetStringsPos
                        (pub_rtp->tree,XmCONTENT,row+i,XmCONTENT,2,buf);
                rtp->c2_width = MAX(rtp->c2_width,strlen(buf));
        }
        XtVaSetValues(pub_rtp->tree,
                      XmNcolumn,2,
                      XmNcolumnWidth,rtp->c2_width,
                      NULL);
        XtVaSetValues(pub_rtp->tree,
                      XmNlayoutFrozen,False,
                      NULL);
        
        NhlFree(rowdefs);

        return;
}
static void DoSpecialResources
(
        NgResTreeRec	*rtp
	)
{
        NgResTree *pub_rtp = &rtp->restree;
	int i;
	if (pub_rtp->preview_instance && rtp->sres_state) {
		for (i = 0; i < NhlNumber(SpecialResources); i++) {
			rtSResState *srs = &rtp->sres_state[i];
                        if (! srs->res_data)
                                continue;
			if (srs->faked && srs->res_data->ndata) {
				XmLGridSetStringsPos
				  (pub_rtp->tree,XmCONTENT,
				   srs->res_data->ndata->row,XmCONTENT,2,
				   SpecialResources[i].actual_str);
			}
		}
	}
}

static void GetInstanceResValues
(
        NgResTreeRec	*rtp,
        int		start_row,
        int		nrows,
        rtResData	*res_data
        )
{
        NgResTree *pub_rtp = &rtp->restree;
        char buf[256];
        int i;
        static NhlBoolean first = True;
        static char arraystring[] = "<array>";
        NhlPointer *values;
        rtResData *resp = res_data;
        int editable_mask;
        _NhlConvertContext context = NULL;
	NhlClass last_class = NULL;
        
        values = NhlMalloc(nrows * sizeof(NhlPointer));
        memset(values,0,nrows * sizeof(NhlPointer));

        if (pub_rtp->preview_instance)
                editable_mask = _NhlRES_NOCACCESS;
        else
                editable_mask = _NhlRES_NOSACCESS;
        
        for (i = 0; i < nrows; i++) {
                if (_NhlIsSubtypeQ(Qgenarray,resp->res->nrm_type) ||
                    resp->res->nrm_type == Qdatalist ||
                    resp->res->nrm_type == Qobjid ||
                    resp->res->nrm_type == Qdataspeclist)
                       NhlRLGet(Grlist,
                                NrmQuarkToString(resp->res->nrm_name),
                                NhlTGenArray,&values[i]);
                else
                       NhlRLGet(Grlist,
                                NrmQuarkToString(resp->res->nrm_name),
                                NhlTString,&values[i]);
                resp++;
        }
        NhlGetValues(rtp->hlu_id,Grlist);

        for (i = 0,resp = res_data; i < nrows; i++,resp++) {
                NhlBoolean null_val = False;
		if (! values[i]) {
                        null_val = True;
			sprintf(buf,"<null>");
                }
                else if (_NhlIsSubtypeQ(Qgenarray,resp->res->nrm_type)) {
			if (resp->res->nrm_type != Qvariable) {
                        	NhlGenArray ga = (NhlGenArray) values[i];
                        	sprintf(buf,"%dD array of %d elements",
                                	ga->num_dimensions,ga->num_elements);
                        	NhlFreeGenArray(ga);
			}
			else {
                                NhlString strval;
                                NrmValue from, to;
                                if (! context)
                                      context = _NhlCreateConvertContext(NULL);
                                if (resp->real_class != last_class) {
                                        _NhlConvertContextClass
                                                (context,resp->real_class);
					last_class = resp->real_class;
				}
                                from.size = sizeof(Qgenarray);
                                from.data.ptrval = values[i];
                                to.size = sizeof(NhlPointer);
                                to.data.ptrval = &strval;
                                _NhlConvertData(context,Qgenarray,Qstring,
                                                &from,&to);
                                sprintf(buf,"%s",strval);
                        }
                }
                else if (resp->res->nrm_type == Qdatalist ||
                         resp->res->nrm_type == Qobjid ||
                         resp->res->nrm_type == Qdataspeclist) {
                        NhlGenArray ga = (NhlGenArray) values[i];
                        if (ga->num_elements == 0)
                              sprintf(buf,"<null>");
                        else {
                                int hlu_id = ((int*)ga->data)[0];
                                NhlString sym = NgNclGetHLURef
                                        (rtp->go->go.nclstate,hlu_id);
                                sprintf(buf,"%s",sym);
                        }
                        NhlFreeGenArray(ga);
                }
		else {
                        sprintf(buf,"%s",(NhlString)values[i]);
                        NhlFree(values[i]);
		}
                if (! null_val && (resp->res->nrm_type == Qfloat ||
                    resp->res->nrm_type == Qdouble ||
                    resp->res->nrm_type == Qvariable)) {
                        NgFixFloat(buf);
                        NgRemoveZeros(buf);
                }
                XmLGridSetStringsPos
                        (pub_rtp->tree,XmCONTENT,start_row+i,XmCONTENT,2,buf);
                XtVaSetValues(pub_rtp->tree,
                              XmNcolumn,1,
                              XmNrow,start_row+i,
                              XmNcellPixmap,No_Check_Pixmap,
                              NULL);
                if (resp->res->res_info & editable_mask) {
                        XtVaSetValues(pub_rtp->tree,
                                      XmNcolumnRangeStart,1,
                                      XmNcolumnRangeEnd,2,
                                      XmNrow,start_row+i,
                                      XmNcellEditable,False,
                                      XmNcellBackground,Background,
                                      NULL);
                }
                rtp->c2_width = MAX(rtp->c2_width,strlen(buf));
                resp->vis = True;
        }
        NhlFree(values);
        if (context)
             _NhlFreeConvertContext(context);   

        NhlRLClear(Grlist);
	DoSpecialResources(rtp);
        
	return;
}

static void GetClassDefaultResValues
(
        NgResTreeRec	*rtp,
        int		start_row,
        int		nrows,
        rtResData	*resp
        )
{
        NgResTree *pub_rtp = &rtp->restree;
        char buf[256];
        int i;
        NhlClass last_class = NULL;
        _NhlConvertContext      context = _NhlCreateConvertContext(NULL);
        NrmValue from, to;
        char *fromtype;
        char *strval;
        static NhlBoolean first = True;
        NrmQuark qfrom;
        
        for (i = 0; i < nrows; i++) {
                if (resp->res->nrm_default_type == Qstring) {
                        sprintf(buf,"%s",resp->res->nrm_default_val.strval);
                }
                else if (resp->res->nrm_default_type == Qimmediate) {
                        if (resp->res->nhlclass != last_class) {
                                _NhlConvertContextClass
                                        (context,resp->res->nhlclass);
                                last_class = resp->res->nhlclass;
                        }
                        from.size = resp->res->nrm_size;
                        from.data.ptrval = resp->res->nrm_default_val.ptrval;
                        to.size = sizeof(NhlString);
                        to.data.ptrval = &strval;
                        qfrom = resp->res->nrm_type;
                        if (! resp->res->nrm_default_val.ptrval &&
                            (qfrom == Qstring ||
                             qfrom == Qpointer ||
                             _NhlIsSubtypeQ(Qgenarray,qfrom)))
                                sprintf(buf,"<null>");
                        else if (qfrom == Qdatalist) {
                                sprintf(buf,"<dynamic>");
                        }
                        else {
                                _NhlConvertData
                                        (context,qfrom,Qstring,
                                         &from,&to);
                                sprintf(buf,"%s",strval);
                        }
                        
                }
                else {
                        sprintf(buf,"%s %s",
                                NrmQuarkToString(resp->res->nrm_type),
                                NrmQuarkToString(resp->res->nrm_default_type));
                }
                XmLGridSetStringsPos
                        (pub_rtp->tree,XmCONTENT,start_row+i,XmCONTENT,2,buf);
                rtp->c2_width = MAX(rtp->c2_width,strlen(buf));
                resp++;
        }
        _NhlFreeConvertContext(context);
        
}

static void GetResDBResValues
(
        NgResTreeRec	*rtp,
        int		start_row,
        int		nrows,
        rtResData	*resp
        )
{
        NgResTree *pub_rtp = &rtp->restree;
        char buf[256];
        int i;
        NhlClass last_class = NULL;
        _NhlConvertContext      context = _NhlCreateConvertContext(NULL);
        NrmQuark qnames[_NhlMAXTREEDEPTH], qclasses[_NhlMAXTREEDEPTH];
        int wk_id;
        NrmValue from, to;
        char *fromtype;
        char *strval;
        NrmQuark qfrom;
        NrmDatabase resdb;
        NhlLayer app,parent;
        int depth,size = rtp->class->base_class.layer_size;
        char *base = NhlMalloc(size);
	NrmHashTable	stackslist[_NhlMAXRESLIST];
	NrmHashTable	*slist = stackslist;
	int		slistlen = _NhlMAXRESLIST;
         
       
        memset(base,0,size);
        _NhlConvertContextClass(context,rtp->class);
        app = _NhlGetCurrentApp();
#if 0        
        if (NhlClassIsSubclass(rtp->class,NhlviewClass)) {
                wk_id = NgAppGetSelectedWork(rtp->go->go.appmgr);
                parent = _NhlGetLayer(wk_id);
                resdb = _NhlGetResDB(parent);
        }
        else {
#endif                
                parent = app;
                resdb = _NhlGetResDB(NULL);
#if 0                
        }
#endif
        if ((depth = _NhlGetNamesAndClasses
            (parent,qnames,qclasses,_NhlMAXTREEDEPTH)) < 0 ||
            depth > _NhlMAXTREEDEPTH - 2) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                          "Max instance tree depth exceeded");
                return;
        }
        qnames[depth] = rtp->qhlu;
        qnames[depth+1] = NrmNULLQUARK;
        qclasses[depth] = rtp->class->base_class.nrm_class;
        qclasses[depth+1] = NrmNULLQUARK;
        
/*
 * Retrieve the levels of the ResDB that we actually need
 */

	while(!NrmQGetSearchList(resdb,qnames,qclasses,slist,slistlen)){

		if(slist == stackslist)
			slist = NULL;
		
		slistlen *= 2;
		slist = (NrmHashTable *)NhlRealloc(slist,
					sizeof(NrmHashTable) * slistlen);
		if(slist == NULL) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"Out of memory");
			return;
                }
                
	}
        
        for (i = 0; i < nrows; i++) {
		NhlBoolean	raccess;
                NrmQuark	rdbtype;
                
                if (resp->res->nhlclass != last_class) {
                        _NhlConvertContextClass(context,resp->res->nhlclass);
                        last_class = resp->res->nhlclass;
                }
		raccess = !(resp->res->res_info&_NhlRES_NORACCESS);

                if (raccess && NrmGetQResFromList
                    (slist,resp->res->nrm_name,resp->res->nrm_class,
                     &rdbtype,&from)) {
                        
                        if (rdbtype == Qstring) {
                                NhlArgVal       data;
                                NrmValue	tmp_to;
                                
                                tmp_to.size = sizeof(NhlArgVal);
                                tmp_to.data.ptrval = &data;
                                _NhlConvertData(context,rdbtype,
                                                resp->res->nrm_type,
                                                &from,&tmp_to);
                                memcpy(&(tmp_to.data),&(data),
                                       sizeof(NhlArgVal));
                                to.size = sizeof(NhlString);
                                to.data.ptrval = &strval;
                                _NhlConvertData(context,resp->res->nrm_type,
                                                Qstring,&tmp_to,&to);
                        }
                        else {
                                to.size = sizeof(NhlString);
                                to.data.ptrval = &strval;
                                _NhlConvertData(context,rdbtype,Qstring,
                                                &from,&to);
                        }
                        sprintf(buf,"%s",strval);
                }
                else if (resp->res->nrm_default_type == Qstring) {
                        sprintf(buf,"%s",resp->res->nrm_default_val.strval);
                }
                else if (resp->res->nrm_default_type == Qimmediate) {
                        from.size = resp->res->nrm_size;
                        from.data.ptrval = resp->res->nrm_default_val.ptrval;
                        to.size = sizeof(NhlString);
                        to.data.ptrval = &strval;
                        qfrom = resp->res->nrm_type;
                        if (! resp->res->nrm_default_val.ptrval &&
                            (qfrom == Qstring ||
                             qfrom == Qpointer ||
                             _NhlIsSubtypeQ(Qgenarray,qfrom)))
                                sprintf(buf,"<null>");
                        else if (qfrom == Qdatalist) {
                                sprintf(buf,"<dynamic>");
                        }
                        else {
                                _NhlConvertData
                                        (context,qfrom,Qstring,
                                         &from,&to);
                                sprintf(buf,"%s",strval);
                        }
                        
                }
                else {
                        sprintf(buf,"%s %s",
                                NrmQuarkToString(resp->res->nrm_type),
                                NrmQuarkToString(resp->res->nrm_default_type));
                }
                XmLGridSetStringsPos
                        (pub_rtp->tree,XmCONTENT,start_row+i,XmCONTENT,2,buf);
                rtp->c2_width = MAX(rtp->c2_width,strlen(buf));
                resp++;
        }
        _NhlFreeConvertContext(context);
        
        return;
}

static void ExpandClass 
(
        NgResTreeRec	*rtp,
        rtNodeData	*ndata,
        int		row
        )
{
        NgResTree *pub_rtp = &rtp->restree;
        NhlClass class = (NhlClass) ndata->info;
        int i,j,class_ix,res_start,res_end,nrows;
        int new_level = 10 * (ndata->type / 10) + 10;
        XmLTreeRowDefinition *rowdefs;
        char buf[256];
        rtResData *resp;

        printf("expanding class\n");

        res_start = 0;
        for (i=0; i < rtp->class_count; i++) {
                if (class == rtp->classes[i]) {
                        class_ix = i;
                        break;
                }
                res_start += rtp->top_res_counts[i];
        }
        nrows = rtp->top_res_counts[class_ix];
        if (nrows == 0)
                return;
        
        res_end = res_start + nrows;
        
        rowdefs = NhlMalloc(nrows * sizeof(XmLTreeRowDefinition));
        ndata->subdata = NhlMalloc(nrows * sizeof(rtNodeData));
                
        XtVaSetValues(pub_rtp->tree,
                      XmNlayoutFrozen,True,
                      NULL);
        
        resp = &rtp->res_data[res_start];
        for (i = 0; i < nrows; i++) {
                rowdefs[i].expands = True;
                ndata->subdata[i].type = new_level + _rtRes;
                sprintf(buf,"%s",NrmQuarkToString(resp->res->nrm_name));
                rowdefs[i].level = new_level / 10;
                rowdefs[i].isExpanded = False;
                rowdefs[i].pixmap = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].pixmask = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].string = XmStringCreateLocalized(buf);
                ndata->subdata[i].parent = ndata;
                ndata->subdata[i].info = (void*)resp;
                ndata->subdata[i].row = row+i;
                ndata->subdata[i].expanded = False;
                ndata->subdata[i].subcount = 0;
                ndata->subdata[i].subdata = NULL;
                resp->ndata = &ndata->subdata[i];
                resp++;
        }
        XmLTreeAddRows(pub_rtp->tree,rowdefs,nrows,row);
        ndata->expanded = True;
        ndata->subcount = nrows;
        
        XtVaSetValues(pub_rtp->tree,
                      XmNcolumnRangeStart,1,
                      XmNcolumnRangeEnd,2,
                      XmNrowRangeStart,row,
                      XmNrowRangeEnd,row + nrows -1,
                      XmNcellEditable,True,
                      XtVaTypedArg,XmNcellBackground,XmRString,"#d0d0d0",8,
                      NULL);

        for (i = 0; i < nrows; i++) {
                XmStringFree(rowdefs[i].string);
                XtVaSetValues(pub_rtp->tree,
                              XmNrow,row+i,
                              XmNrowUserData,&ndata->subdata[i],
                              NULL);
        }
        
        resp = &rtp->res_data[res_start];

        if (rtp->hlu_id > NhlNULLOBJID)
                GetInstanceResValues(rtp,row,nrows,resp);
        else {
                GetResDBResValues(rtp,row,nrows,resp);
#if 0        
                GetClassDefaultResValues(rtp,row,nrows,resp);
#endif
        
        }
        XtVaSetValues(pub_rtp->tree,
                      XmNcolumn,2,
                      XmNcolumnWidth,rtp->c2_width,
                      NULL);
        XtVaSetValues(pub_rtp->tree,
                      XmNlayoutFrozen,False,
                      NULL);
        
        NhlFree(rowdefs);
        
        return;
}

static void ExpandResGroup 
(
        NgResTreeRec	*rtp,
        rtNodeData	*ndata,
        int		row
        )
{
        NgResTree *pub_rtp = &rtp->restree;

        printf("expanding res group\n");
        
        return;
}

static void ExpandRes 
(
        NgResTreeRec	*rtp,
        rtNodeData	*ndata,
        int		row
        )
{
        NgResTree *pub_rtp = &rtp->restree;
        rtResData *resp = (rtResData *)ndata->info;
        XmLTreeRowDefinition *rowdefs;
        int new_level = 10 * (ndata->type / 10) + 10;
        int i,rowcount;
        char buf[256];
        
        printf("expanding res\n");

        rowcount = 3;
        rowdefs = NhlMalloc(rowcount * sizeof(XmLTreeRowDefinition));

        XtVaSetValues(pub_rtp->tree,
                      XmNlayoutFrozen,True,
                      NULL);
        for (i = 0; i < rowcount; i++) {
                switch (i) {
                    case 0:
                            sprintf(buf,"Resource Class");
                            break;
                    case 1:
                            sprintf(buf,"Type");
                            break;
                    case 2:
                            sprintf(buf,"Access");
                            break;
                }
                rowdefs[i].level = new_level / 10;
                rowdefs[i].expands = False;
                rowdefs[i].isExpanded = False;
                rowdefs[i].pixmap = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].pixmask = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].string = XmStringCreateLocalized(buf);
        }
        
        XmLTreeAddRows(pub_rtp->tree,rowdefs,rowcount,row);
        ndata->expanded = True;
        ndata->subcount = rowcount;
        ndata->subdata = NULL;
        
        for (i = 0; i < rowcount; i++) {
                XmStringFree(rowdefs[i].string);
                switch (i) {
                    case 0:
                            sprintf(buf,"%s",
                                    NrmQuarkToString(resp->res->nrm_class));
                            break;
                    case 1:
                            sprintf(buf,"%s",
                                    NrmQuarkToString(resp->res->nrm_type));
                            break;
                    case 2:
                            strcpy(buf,"");
                            if (! (resp->res->res_info & _NhlRES_NORACCESS))
                                    strcat(buf,"R");
                            if (! (resp->res->res_info & _NhlRES_NOCACCESS))
                                    strcat(buf,"C");
                             if (! (resp->res->res_info & _NhlRES_NOSACCESS))
                                    strcat(buf,"S");
                            if (! (resp->res->res_info & _NhlRES_NOGACCESS))
                                    strcat(buf,"G");
                            break;
                }
                XmLGridSetStringsPos(pub_rtp->tree,
                                     XmCONTENT,row+i,XmCONTENT,2,buf);
                rtp->c2_width = MAX(rtp->c2_width,strlen(buf));
        }
        XtVaSetValues(pub_rtp->tree,
                      XmNcolumn,2,
                      XmNcolumnWidth,rtp->c2_width,
                      NULL);
        NhlFree(rowdefs);
        XtVaSetValues(pub_rtp->tree,
                      XmNlayoutFrozen,False,
                      NULL);
        
        return;
}

static void UpdateRowIndexes
(
        rtNodeData	*ndata,
        int		start_ix,
        int		row_inc
        )
{
        int i;
        
        if (start_ix < 0) {
                    /* up the tree from current node */
                if (! ndata->parent)
                        return;
                for (i = 0; i < ndata->parent->subcount; i++) {
                        if (ndata == &ndata->parent->subdata[i])
                                break;
                }
                if (i >= ndata->parent->subcount - 1) {
                        UpdateRowIndexes(ndata->parent,-1,row_inc);
                }
                else {
                        UpdateRowIndexes(ndata->parent,i+1,row_inc);
                        UpdateRowIndexes(ndata->parent,-1,row_inc);
                }
                return;
        }
        if (! ndata->subdata)
                return;
            /* sibling nodes following the current node */
        for (i = start_ix; i < ndata->subcount; i++) {
                ndata->subdata[i].row += row_inc;
                if (ndata->subdata[i].subcount)
                        UpdateRowIndexes(&ndata->subdata[i],0,row_inc);
        }
        return;
}
                
static void PrintTree 
(
        NgResTreeRec	*rtp
        )
{
        int i;

        for (i = 0; i < rtp->res_data_count; i++) {
                rtResData *resp = &rtp->res_data[i];
                printf("%d %s: ",i,NrmQuarkToString(resp->res->nrm_name));
                if (resp->ndata) {
                        printf("row %d ",resp->ndata->row);
                        if (resp->ndata->info != (void *) resp) 
                                printf("addr mismatch");
                }
                printf("\n");
                
        }
}

static void ExpandTree 
(
        NgResTreeRec	*rtp,
        rtNodeData	*ndata,
        int		row
        )
{
        NgResTree *pub_rtp = &rtp->restree;
        
        if (! rtp->expand_called) {
                short		cw,ch;
                XmFontList      fontlist;
                Dimension 	h,rh;
                int		nrows;
                XmLGridRow	grid_row;

                grid_row = XmLGridGetRow(pub_rtp->tree,XmCONTENT,row);
                XtVaSetValues(pub_rtp->tree,
                              XmNrowSizePolicy,XmCONSTANT,
                              NULL);
                XtVaGetValues(pub_rtp->tree,
                              XmNrowPtr,grid_row,
                              XmNfontList,&fontlist,
                              XmNrows,&nrows,
                              XmNrowHeight,&rh,
                              XmNheight,&h,
                              NULL);

                XtVaSetValues(pub_rtp->tree,
                              XmNrowSizePolicy,XmVARIABLE,
                              NULL);
                XmLFontListGetDimensions(fontlist,&cw,&ch,True);
                Char_Height = ch;
                Row_Height = MAX(rh,h/nrows);
                printf("ch %d rh %d rh1 %d\n", ch,rh,h/nrows);
                rtp->expand_called = True;
        }

        switch (ndata->type % 10) {
            case _rtClassGroup:
                    ExpandClassGroup(rtp,ndata,row+1);
                    break;
            case _rtClass:
                    ExpandClass(rtp,ndata,row+1);
                    break;
            case _rtResGroup:
                    ExpandResGroup(rtp,ndata,row+1);
                    break;
            case _rtRes:
                    ExpandRes(rtp,ndata,row+1);
                    break;
        }
        UpdateRowIndexes(ndata,-1,ndata->subcount);
#if 0        
        PrintTree(rtp);
#endif
        
        return;
}
        

static void SetNodeDataVisFlags
(
        NgResTreeRec	*rtp,
        rtNodeData	*ndata,
        int		node_count,
        NhlBoolean	on
        )
{
        NgResTree *pub_rtp = &rtp->restree;
        int i;

        for (i = 0; i < node_count; i++) {
		if (on && ndata[i].subcount &&
                    ndata[i].subdata && ndata[i].expanded)
                        SetNodeDataVisFlags(rtp,ndata[i].subdata,
                                             ndata[i].subcount,True);
		else if (ndata[i].subcount && ndata[i].subdata)
                        SetNodeDataVisFlags(rtp,ndata[i].subdata,
                                             ndata[i].subcount,False);
                if (ndata[i].type % 10 ==_rtRes) {
                        rtResData *resp = (rtResData *)ndata[i].info;
                        resp->vis = on;
                }
        }
        return;
}
static void DetachRows
(
        rtNodeData	*ndata,
        int 		*count
        )
{
        int i;

        *count += ndata->subcount;
        if (! ndata->subdata)
                return;
        
        for (i = 0; i < ndata->subcount; i++) {
                if (ndata->subdata[i].type % 10 == _rtRes) {
                        rtResData *resp = (rtResData *) ndata->subdata[i].info;
                        resp->vis = False;
                        resp->ndata = NULL;
                }
                if (ndata->subdata[i].subcount)
                        DetachRows(&ndata->subdata[i],count);
        }
        NhlFree(ndata->subdata);
        return;
}
        
static void UpdateClassFolderState
(
        NgResTreeRec	*rtp,
        rtNodeData	*ndata
        )
{
        NgResTree *pub_rtp = &rtp->restree;
        NhlClass class = (NhlClass) ndata->info;
        int i;
        
        for (i = rtp->super_class_count+1; i < rtp->class_count; i++) {
                if (class == rtp->classes[i]) {
                        if (! rtp->instantiated[i]) {
                                if (ndata->expanded) {
                                        int dcount = 0;
                                        DetachRows(ndata,&dcount);
                                        XmLGridDeleteRows
                                                (pub_rtp->tree,XmCONTENT,
                                                 ndata->row+1,dcount);
                                        UpdateRowIndexes
                                                (ndata,-1,-dcount);
                                        ndata->subcount = 0;
                                        ndata->subdata = NULL;
                                        ndata->expanded = False;
#if 0                                        
                                        PrintTree(rtp);
#endif                                        
                                }
                                XtVaSetValues(pub_rtp->tree,
                                              XmNrow,ndata->row,
                                              XmNrowIsExpanded,False,
                                              XmNrowExpands,False,
                                              NULL);
                        }
                        else if (!ndata->expanded) {
                              XtVaSetValues(pub_rtp->tree,
                                              XmNrow,ndata->row,
                                              XmNrowExpands,True,
                                              NULL);  
                        }
                        break;
                }
        }
        return;
}
                                              
                        
static void UpdateInstantiatedStatus
(
        NgResTreeRec	*rtp,
        rtNodeData	*ndata,
        int		node_count
        )
{
        NgResTree *pub_rtp = &rtp->restree;
        int i;

        for (i = 0; i < node_count; i++) {
                NhlClass class;
		if (ndata[i].type % 10 ==_rtClass)
                        UpdateClassFolderState(rtp,&ndata[i]);
		else if (ndata[i].type % 10 < _rtClass && ndata[i].subcount)
                        UpdateInstantiatedStatus(rtp,ndata[i].subdata,
                                                 ndata[i].subcount);
        }
        return;
}


static void ExpandCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	NgResTreeRec *rtp = (NgResTreeRec *) udata;
        NgResTree *pub_rtp = &rtp->restree;
        XmLGridCallbackStruct *cbs;
        XmLGridRow	row;
        rtNodeData	*ndata;
        int		pos;
        
        cbs = (XmLGridCallbackStruct *)cb_data;
        row = XmLGridGetRow(w,XmCONTENT,cbs->row);
        XtVaGetValues(w,
                      XmNrowPtr,row,
                      XmNrowUserData,&ndata,
                      NULL);
        
        if (ndata->subcount > 0) {
                ndata->expanded = True;
                SetNodeDataVisFlags(rtp,ndata,1,True);
                if (pub_rtp->geo_notify && pub_rtp->geo_data)
                        (*pub_rtp->geo_notify)(pub_rtp->geo_data);
                return;
        }
        
        ExpandTree(rtp,ndata,cbs->row);
        
        
        if (pub_rtp->geo_notify && pub_rtp->geo_data)
                (*pub_rtp->geo_notify)(pub_rtp->geo_data);
        
        return;
}

static void CollapseCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	NgResTreeRec *rtp = (NgResTreeRec *) udata;
        NgResTree *pub_rtp = &rtp->restree;
        XmLGridCallbackStruct *cbs;
        XmLGridRow	row;
        rtNodeData	*ndata;

        cbs = (XmLGridCallbackStruct *)cb_data;
        row = XmLGridGetRow(w,XmCONTENT,cbs->row);
        XtVaGetValues(w,
                      XmNrowPtr,row,
                      XmNrowUserData,&ndata,
                      NULL);

        ndata->expanded = False;
        
        SetNodeDataVisFlags(rtp,ndata,1,False);
                            
        if (pub_rtp->geo_notify && pub_rtp->geo_data)
                (*pub_rtp->geo_notify)(pub_rtp->geo_data);
}

static void AdjustTextWidget
(
        NgResTreeRec *rtp,
	int row,
	int col
        )
{
        NgResTree *pub_rtp = &rtp->restree;
        Widget parent = pub_rtp->tree;
        Position x,y,tx=0,ty=0;
        int i;

        for (i=0; i < 4; i++) {
                XtVaGetValues(parent,
                              XmNx,&x,
                              XmNy,&y,
                              NULL);
                tx += x;
                ty += y;
                parent = XtParent(parent);
        }
        x = MAX(0,-tx);
        y = MAX(0,-ty);
        
        XtMoveWidget(rtp->text,x,y);

        return;
}
static void UnFocusCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	NgResTreeRec 	*rtp = (NgResTreeRec *) udata;
        NgResTree 	*pub_rtp = &rtp->restree;
        XmLGridRow	rowptr;
        XmLGridColumn	colptr;
        Boolean		editable;
        rtNodeData 	*ndata;
        rtResData	*res_data;
        int 		i;

        rowptr = XmLGridGetRow(pub_rtp->tree,XmCONTENT,rtp->edit_row);
        colptr = XmLGridGetColumn(pub_rtp->tree,XmCONTENT,2);
                
        XtVaGetValues(pub_rtp->tree,
                      XmNcolumnPtr,colptr,
                      XmNrowPtr,rowptr,
                      XmNcellEditable,&editable,
                      XmNuserData,&ndata,
                      NULL);

#if 0                
        if (ndata) {
                res_data = (rtResData *)ndata->info;
                printf("unfocusing edit row %d %s -- %s\n",
                       rtp->edit_row,
                       NrmQuarkToString(res_data[i].res->nrm_name),
                       editable ? "editable" : "not editable");
        }
        else {
                for (i = 0; i < rtp->res_data_count; i++)
                       if (rtp->res_data[i].ndata &&
                           rtp->edit_row == rtp->res_data[i].ndata->row) {
                               printf("unfocusing edit row %d %s -- %s\n",
                                      rtp->edit_row,
                                      NrmQuarkToString
                                      (rtp->res_data[i].res->nrm_name),
                                      editable ? "editable" : "not editable");
                               break;
                       }
        }
        
#endif        
        if (! rtp->enum_info.up) {
                if (editable) {
                        XtVaSetValues(pub_rtp->tree,
                                      XmNcolumn,2,
                                      XmNrow,rtp->edit_row,
                                      XtVaTypedArg,XmNcellBackground,
                                      XmRString,"#d0d0d0",8,
                                      NULL);
                        XtVaSetValues(rtp->text,
                                      XtVaTypedArg,XmNbackground,
                                      XmRString,"#d0d0d0",8,
                                      NULL);
                }
                else {
                        XtVaSetValues(pub_rtp->tree,
                                      XmNcolumn,2,
                                      XmNrow,rtp->edit_row,
                                      XmNcellBackground,Background,
                                      NULL);
                        XtVaSetValues(rtp->text,
                                      XmNbackground,Background,
                                      NULL);
                }
        }

	if (rtp->manual_edit_started) {
		XmLGridEditCancel(pub_rtp->tree);
	}
	return;
}
static void FocusCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	NgResTreeRec *rtp = (NgResTreeRec *) udata;
        NgResTree *pub_rtp = &rtp->restree;
        XmLGridCallbackStruct *cb = (XmLGridCallbackStruct *)cb_data;
        Boolean		editable;
        XmLGridRow	rowptr;
        XmLGridColumn	colptr;

        if (cb->column == 1)
               XmLGridSetFocus(w,cb->row,2);
        if (cb->column != 2)
                return;
        
        if (cb->reason == XmCR_CELL_FOCUS_OUT) {
                printf("focus out: row %d col %d \n",cb->row,cb->column);
	}
        if (cb->reason == XmCR_CELL_FOCUS_IN) {
                printf("focus in: row %d col %d \n",cb->row,cb->column);
        }
        AdjustTextWidget(rtp,cb->row,cb->column);
        
        return;
}

static void RemoveFromSetValList 
(
        NgResTreeRec	*rtp,
        rtSetValNode	*svp
        )
{
        rtSetValNode *csvp, *lastsvp = NULL;

        for (csvp = rtp->set_val_list; csvp != NULL; csvp = csvp->next) {
                if (svp == csvp) {
                        if (lastsvp)
                                lastsvp->next = csvp->next;
                        else
                                rtp->set_val_list = csvp->next;
                        XtFree(svp->res_data->value);
                        NhlFree(svp);
                        break;
                }
                lastsvp = csvp;
        }
        return;
}

static void AddToSetValList 
(
        NgResTreeRec	*rtp,
        rtResData	*res_data,
	XtPointer	value
        )
{
        rtSetValNode *setvalp;

	/* if already in the list just replace the value, updating the
	   row as well in case it is out of date */
	for (setvalp = rtp->set_val_list; 
	     setvalp != NULL; setvalp = setvalp->next) {
		if (res_data == setvalp->res_data) {
			XtFree(res_data->value);
			res_data->value = value;
			return;
		}
	}

        setvalp = NhlMalloc(sizeof(rtSetValNode));
	res_data->value = value;
        setvalp->res_data = res_data;
        setvalp->next = rtp->set_val_list;
        rtp->set_val_list = setvalp;
        return;
}
        

void EmptySetValList
(
        NgResTreeRec *rtp
        )
{
        while (rtp->set_val_list)
                RemoveFromSetValList(rtp,rtp->set_val_list);
                
        rtp->set_val_list = NULL;
        return;
}

static NhlBoolean CheckToggleSetState
(
        NgResTreeRec *rtp,
	int row
        )
{
	rtSetValNode *svp;
        NgResTree *pub_rtp = &rtp->restree;
        NhlPointer value = NULL;
        rtNodeData *ndata;
        rtResData *row_res_data;
        XmLGridRow	rowptr;
        char buf[256];
        NhlBoolean null_val = False;
        
                
        rowptr = XmLGridGetRow(pub_rtp->tree,XmCONTENT,row);
        XtVaGetValues(pub_rtp->tree,
                      XmNrowPtr,rowptr,
                      XmNrowUserData,&ndata,
                      NULL);
        row_res_data = (rtResData *)ndata->info;
        
	for (svp = rtp->set_val_list; svp != NULL; svp = svp->next) {
                if (row_res_data == svp->res_data)
                        break;
        }
        if (!svp)
                return False;
        
        XtVaSetValues(pub_rtp->tree,
                      XmNcolumn,1,
                      XmNrow,row,
                      XmNcellPixmap,No_Check_Pixmap,
                      NULL);
        
        if (_NhlIsSubtypeQ(Qgenarray,svp->res_data->res->nrm_type))
                NhlRLGet(Grlist,
                         NrmQuarkToString(svp->res_data->res->nrm_name),
                         NhlTGenArray,&value);
        else
                NhlRLGet(Grlist,
                         NrmQuarkToString(svp->res_data->res->nrm_name),
                         NhlTString,&value);
        
        NhlGetValues(rtp->hlu_id,Grlist);
        NhlRLClear(Grlist);

        if (! value) {
                sprintf(buf,"<null>");
                null_val = True;
        }
        else if (_NhlIsSubtypeQ(Qgenarray,svp->res_data->res->nrm_type)) {
                if (svp->res_data->res->nrm_type != Qvariable) {
                        NhlGenArray ga = (NhlGenArray) value;
                        sprintf(buf,"%dD array of %d elements",
                                ga->num_dimensions,ga->num_elements);
                        NhlFreeGenArray(ga);
                }
                else {
                        NhlString strval;
                        NrmValue from, to;
                        _NhlConvertContext context
                                = _NhlCreateConvertContext(NULL);
                        _NhlConvertContextClass(context,
                                                svp->res_data->real_class);
                        from.size = sizeof(Qgenarray);
                        from.data.ptrval = value;
                        to.size = sizeof(NhlPointer);
                        to.data.ptrval = &strval;
                        _NhlConvertData(context,Qgenarray,Qstring,
                                         &from,&to);
                        sprintf(buf,"%s",strval);
                        _NhlFreeConvertContext(context);
                }
        }
        else {
                sprintf(buf,"%s",(NhlString)value);
                NhlFree(value);
        }
        if (! null_val && (svp->res_data->res->nrm_type == Qfloat ||
                          svp->res_data->res->nrm_type == Qdouble ||
                          svp->res_data->res->nrm_type == Qvariable)) {
                NgFixFloat(buf);
                NgRemoveZeros(buf);
        }
        XmLGridSetStringsPos
                (pub_rtp->tree,XmCONTENT,row,XmCONTENT,2,buf);

        RemoveFromSetValList(rtp,svp);

        return True;
}
/* the mega button version */
static void RestoreSensitivity
(
	NgResTreeRec	*rtp,
        XButtonEvent	*xev
)
{
        NgResTree *pub_rtp = &rtp->restree;
	rtEnumInfoRec		*ep = &rtp->enum_info;
	XRectangle	rect;
        Position	root_x,root_y;
        NhlString	param1 = "BEGIN",param2 = "END";

#if 0
        AdjustTextWidget(rtp,rtp->edit_row,2);
        XmLGridSetFocus(pub_rtp->tree,rtp->edit_row,2);
#endif
	XmLGridRowColumnToXY
                (pub_rtp->tree,
                 XmCONTENT,rtp->edit_row,XmCONTENT,2,False,&rect);
	XtTranslateCoords(pub_rtp->tree,(Position) 0,(Position) 0,
                          &root_x,&root_y);
        xev->type = ButtonPress;
        xev->window = XtWindow(pub_rtp->tree);
        xev->x = rect.x +rect.width / 2;
        xev->y = rect.y +rect.height / 2;
        xev->x_root = root_x + xev->x;
        xev->y_root = root_y + xev->y;
        xev->button = Button1;
        xev->state = 0;
        xev->same_screen = True;
        
        printf("calling action proc\n");
        XtCallActionProc
                (pub_rtp->tree,"XmLGridSelect",(XEvent*)&xev,&param1,1);

        xev->type = ButtonRelease;
        xev->state = Button1Mask;
        XtCallActionProc
                (pub_rtp->tree,"XmLGridSelect",(XEvent*)xev,&param2,1);
	return;
}

static void EnumEdCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	NgResTreeRec *rtp = (NgResTreeRec *) udata;
        NgResTree *pub_rtp = &rtp->restree;
	rtEnumInfoRec		*ep = &rtp->enum_info;
        int		i,count = 0;
        XmString	string;
        char 		*sval;
        XmLGridRow	rowptr;
        rtNodeData	*ndata;
        rtResData	*resp;
        XmMegaButtonCallbackStruct *cb = (XmMegaButtonCallbackStruct *)cb_data;
        XButtonEvent	*xev = &cb->event->xbutton;
        
        printf("in enum ed cb\n");
        
        rowptr = XmLGridGetRow(pub_rtp->tree,XmCONTENT,rtp->edit_row);
        
        XtVaGetValues(pub_rtp->tree,
                      XmNrowPtr,rowptr,
                      XmNrowUserData,&ndata,
                      NULL);
        resp = (rtResData *)ndata->info;
        sval = XtMalloc(strlen(ep->strings[cb->pos])+1);
        strcpy(sval,ep->strings[cb->pos]);
        
        AddToSetValList(rtp,resp,(XtPointer)sval);
        
        XtVaSetValues(pub_rtp->tree,
                      XmNcolumn,1,
                      XmNrow,rtp->edit_row,
                      XmNcellPixmap,Check_Pixmap,
                      NULL);
        string = NgXAppCreateXmString(rtp->go->go.appmgr,sval);
        XtVaSetValues(pub_rtp->tree,
                      XmNcolumn,2,
                      XmNrow,rtp->edit_row,
                      XmNcellString,string,
                      NULL);
        NgXAppFreeXmString(rtp->go->go.appmgr,string);
        
        return;
        
}
static void UnmapEnumEdCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	NgResTreeRec	*rtp = (NgResTreeRec *) udata;
        NgResTree	*pub_rtp = &rtp->restree;
	rtEnumInfoRec	*ep = &rtp->enum_info;
	XmRowColumnCallbackStruct *cb = (XmRowColumnCallbackStruct*)cb_data;

        printf("destroying enum popup\n");
        
        XtDestroyWidget(ep->popup);

        ep->up = False;
	RestoreSensitivity(rtp,(XButtonEvent *)cb->event);
        return;
        
}
static void SetUpColorList
(
        NgResTreeRec	*rtp,
        char		*current_val
)
{
        NgResTree 	*pub_rtp = &rtp->restree;
	rtEnumInfoRec	*ep = &rtp->enum_info;
        int		ncolors;
        NhlGenArray	ga;
        float		*cmap;
        NhlLayer	l = _NhlGetLayer(rtp->hlu_id);
        int		i;
        int		cur_ix;

        if (ep->cmap) {
                NhlFree(ep->cmap);
                ep->cmap = NULL;
        }
        if (! l || _NhlIsObj(l) || ! l->base.wkptr) {
                return;
        }
        if (ep->selected < 0) {
                sscanf(current_val,"%d",&cur_ix);
                ep->selected = cur_ix + 1;
        }
        
        NhlRLGet(Grlist,"wkColorMapLen",NhlTInteger,&ncolors);
        NhlRLGet(Grlist,"wkColorMap",NhlTGenArray,&ga);
        NhlGetValues(l->base.wkptr->base.id,Grlist);
        NhlRLClear(Grlist);
                
        ep->cmap = NhlMalloc(sizeof(float) * 3*(ncolors+1));
        ep->strings = NhlRealloc
                (ep->strings,sizeof(NhlString) * (ncolors+1));
        ep->count = ncolors+1;
        ep->cmap[0] = ep->cmap[1] = ep->cmap[2] = -1.0;
        memcpy(&(ep->cmap[3]),ga->data,3*ncolors*sizeof(float));
        NhlFreeGenArray(ga);
        for (i = 2; i < ncolors; i++) {
                char	buf[20];
                sprintf(buf,"%d",i);
                ep->strings[i+1] = NhlMalloc(strlen(buf)+1);
                strcpy(ep->strings[i+1],buf);
        }
        return;
                               
}
static void
ButtonReleaseEH
(
	Widget		w,
	XtPointer	udata,
	XEvent		*event,
	Boolean		*cont
)
{
	NgResTreeRec	*rtp = (NgResTreeRec *) udata;
        NgResTree *pub_rtp = &rtp->restree;
	rtEnumInfoRec		*ep = &rtp->enum_info;

        printf("in edit enum button release EH\n");

        if (event->type == ButtonRelease) {
                if (event->xbutton.x < ep->x ||
                    event->xbutton.x > ep->x + ep->width ||
                    event->xbutton.y < ep->y ||
                    event->xbutton.y > ep->y + ep->height) {
	        	RestoreSensitivity(rtp,(XButtonEvent *)event);
                        return;
                }
        }
#if 0
                
        if (event->xbutton.time - ep->time <
            XtGetMultiClickTime(XtDisplay(w))) {
                XmProcessTraversal(w, XmTRAVERSE_CURRENT);
                XAllowEvents(XtDisplay(w), SyncPointer, event->xbutton.time);
                return;
        }
        else {
                XAllowEvents(XtDisplay(w), ReplayPointer, event->xbutton.time);
        }
        
#endif
        return;
        

}

static void EditEnum
(
        NgResTreeRec	*rtp,
	int 		row,
	rtResData 	*resp
        )
{
        NgResTree *pub_rtp = &rtp->restree;
	rtEnumInfoRec		*ep = &rtp->enum_info;
        NhlErrorTypes		ret;
	NhlConvertArgList       args;
	int			i,j,nargs,noptions = 0;
	XRectangle		rect;
	Boolean			unique[128];
        int			ivalue,current_arg,cur_button_num;
        XmString 		xmname;
        Widget			form,button,last_set;
        Dimension		height,width,border;
	NhlBoolean		new = False;
	Dimension		root_w,root_h;
	Position		root_x,root_y,x,y;
        char 			*current_val;
        XmString		*xmstrings;
        int mode		= (int) XmMODE_TOGGLE_BUTTON;
	String			menupost,which;
        
	XmLGridRowColumnToXY(pub_rtp->tree,XmCONTENT,row,XmCONTENT,2,False,
			     &rect);
	XtTranslateCoords(pub_rtp->tree,(Position) 0,(Position) 0,
                          &root_x,&root_y);
        root_w = WidthOfScreen(XtScreen(rtp->go->go.shell));
        root_h = HeightOfScreen(XtScreen(rtp->go->go.shell));

	ret = _NhlConverterGetArgs
                (resp->real_class,Qstring,resp->res->nrm_type,&args,&nargs);
        if (ret < NhlWARNING || ! nargs) {
                NhlPError(ret,NhlEUNKNOWN,"Can't get enum arguments\n");
		return;
        }
	
        XmStringGetLtoR
                (rtp->selected_row_xmstr,XmFONTLIST_DEFAULT_TAG,&current_val);

        for (i = ep->str_assigned_count; i < ep->count; i++)
                NhlFree(ep->strings[i]);
        
        ep->strings = NhlRealloc
                (ep->strings,sizeof(NhlString) * nargs);
        if (!ep->strings) {
                NHLPERROR((NhlFATAL,ENOMEM,NULL));
                return;
        }
        
                
        ep->selected = -1;
	for (i = 0; i < nargs; i++) {
		unique[i] = True;
		for (j=i-1;j>-1;j--) {
		  if (args[j].size == args[i].size) {
				unique[i] = False;
				break;
		  }
		}
		if (unique[i]){
			ep->strings[noptions] = args[i].data.strval;
                        if (!strcmp(current_val,args[i].data.strval)) {
                                current_arg = i;
                                ep->selected = noptions;
                        }
			noptions++;
                }
	}
	ep->count = ep->str_assigned_count = noptions;
            /* ep->count will be increased for mixed enumerations */
        if (resp->res->nrm_type == Qcolorindex) {
                SetUpColorList(rtp,current_val);
                mode = (int) XmMODE_RECT_AND_BUTTON;
        }
        xmstrings = NhlMalloc(sizeof(XmString) * ep->count);
        for (i = 0; i < ep->count; i++) {
                xmstrings[i] = NgXAppCreateXmString
                        (rtp->go->go.appmgr,ep->strings[i]);
        }
#if 0        
        ep->popup = XmCreatePopupMenu(rtp->go->go.manager,
                                      "EnumInfo",NULL,0);
#endif        
        ep->popup =  XtVaCreatePopupShell
                ("EnumPopup",xmMenuShellWidgetClass,
                 pub_rtp->tree,
                 XmNwidth,		5,
                 XmNheight,		5,
                 XmNallowShellResize,	True,
                 XtNoverrideRedirect,	True,
                 XmNuserData,		rtp,
                 NULL);
        printf("ep popup %x parent window %x\n",ep->popup,
               XtWindow(pub_rtp->tree));
        
	ep->menu = XtVaCreateWidget
                ("EnumMenu",xmRowColumnWidgetClass,ep->popup,
                 XmNrowColumnType,XmMENU_POPUP,
                 XmNuserData,		rtp,
		 XmNwhichButton,	1,
                 NULL);
	XtVaGetValues(ep->menu,
		      XmNmenuPost,&menupost,
		      XmNwhichButton,&which,
		      NULL);
	printf("menupost: %s %d\n", menupost,which);
	XtFree(menupost);
        ep->mega = XtVaCreateManagedWidget
                ("mega",xmMegaButtonWidgetClass,ep->menu,
                 XmNitems,xmstrings,
                 XmNpixmaps,ep->pixmaps,
                 XmNrgbVals,ep->cmap,
                 XmNxcb,rtp->go->go.xcb,
                 XmNitemCount,ep->count,
                 XmNsetPosition,ep->selected,
                 XmNbuttonMode,mode,
                 XmNselectColor,White,
                 XmNalignment,XmALIGNMENT_CENTER,
                 XmNuserData,		rtp,
                 NULL);

        for (i = 0; i < ep->count; i++) {
                NgXAppFreeXmString(rtp->go->go.appmgr,xmstrings[i]);
        }
        NhlFree(xmstrings);
        
        XtVaGetValues(ep->mega,
		      XmNheight,&height,
                      XmNwidth,&width,
                      XmNborderWidth,&border,
                      NULL);
        width +=border;
        height += border;
        
	x = MAX(0,MIN(root_w-width,root_x+rect.x));
	y = MAX(0,MIN(root_h-height,root_y+rect.y-
		      (height*((float)ep->selected/(float)ep->count))));
        ep->x = x;
        ep->y = y;
        ep->width = width;
        ep->height = height;
        
        XtVaSetValues(ep->menu,
                       XmNx,x,
                       XmNy,y,
                       NULL);

        XtAddCallback(ep->mega,XmNactivateCallback,EnumEdCB,rtp);
        XtAddCallback(ep->menu,XmNunmapCallback,UnmapEnumEdCB,rtp);
#if 0
        XtAddEventHandler(ep->popup,
                          ButtonReleaseMask,False,
                          ButtonReleaseEH,rtp);
#endif
        XtManageChild(ep->menu);
	XtPopup(ep->popup,XtGrabNonexclusive);
#if 0
        XtPopupSpringLoaded(ep->popup);
        
        XtManageChild(ep->menu);
        XtAddGrab(ep->popup, True, True);
        printf("managing enum editor\n");



#endif        
	ep->up = True;
        XtFree(current_val);
        
	return;
}

static void SelectCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	NgResTreeRec *rtp = (NgResTreeRec *) udata;
        NgResTree *pub_rtp = &rtp->restree;
        static XmString cell_string = NULL;
        XmLGridCallbackStruct *cb = (XmLGridCallbackStruct *)cb_data;
        NhlBoolean	off = False;
        Boolean		editable;
        XmLGridRow	rowptr;
        XmLGridColumn	colptr;
        rtNodeData	*ndata;
	rtResData 	*resp;

        printf("in select callback, col %d row %d %x %x %d\n",
               cb->column,cb->row,rtp,
               &rtp->enum_info,cb->event->xbutton.time);
        if (rtp->enum_info.up) {
                printf("unmanaging enum editor\n");
#if 0                
                XtPopdown(rtp->enum_info.popup);
#endif                
                rtp->enum_info.up = False;
        }
        rtp->enum_info.time = cb->event->xbutton.time;
        
        if (! (cb->column == 2 || cb->column == 1))
                return;
                
        rowptr = XmLGridGetRow(pub_rtp->tree,XmCONTENT,cb->row);
        colptr = XmLGridGetColumn(pub_rtp->tree,XmCONTENT,2);
                
        XtVaGetValues(pub_rtp->tree,
                      XmNcolumnPtr,colptr,
                      XmNrowPtr,rowptr,
                      XmNcellEditable,&editable,
                      NULL);

        if (editable && cb->column == 1)
                off = CheckToggleSetState(rtp,cb->row);

        XtVaGetValues(pub_rtp->tree,
                      XmNrowPtr,rowptr,
                      XmNrowUserData,&ndata,
                      NULL);
	resp = (rtResData *)ndata->info;


        if (rtp->edit_row != cb->row) {
                if (editable)
                        XtVaSetValues(pub_rtp->tree,
                                      XmNrow,rtp->edit_row,
                                      XmNcolumnRangeStart,1,
                                      XmNcolumnRangeEnd,2,
                                      XtVaTypedArg,XmNcellBackground,
                                      XmRString,"#d0d0d0",8,
                                      NULL);
                else
                        XtVaSetValues(pub_rtp->tree,
                                      XmNcolumnRangeStart,1,
                                      XmNcolumnRangeEnd,2,
                                      XmNrow,rtp->edit_row,
                                      XmNcellBackground,Background,
                                      NULL);
        }
        if (! off && editable) {
                if (rtp->selected_row_xmstr)
                        XmStringFree(rtp->selected_row_xmstr);
                XtVaGetValues
                        (pub_rtp->tree,
                         XmNcolumnPtr,colptr,
                         XmNrowPtr,rowptr,
                         XmNcellString,&rtp->selected_row_xmstr,
                         NULL);
                XtVaSetValues(pub_rtp->tree,
                              XmNcolumn,2,
                              XmNrow,cb->row,
                              XtVaTypedArg,XmNcellBackground,
                              XmRString,"lightsalmon",12,
                              NULL);
                rtp->edit_row = cb->row;
                if (_NhlIsSubtypeQ(Qenum,resp->res->nrm_type))
                        EditEnum(rtp,cb->row,resp);
        }
        return;
}
        
static void EditCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	NgResTreeRec *rtp = (NgResTreeRec *) udata;
        NgResTree *pub_rtp = &rtp->restree;
        XmLGridCallbackStruct *cb;
        XmLGridRow	rowptr;
        XmLGridColumn	colptr;
        rtNodeData	*ndata;
        NhlBoolean	update = True;

        cb = (XmLGridCallbackStruct *)cb_data;
        rowptr = XmLGridGetRow(pub_rtp->tree,XmCONTENT,cb->row);
        colptr = XmLGridGetColumn(pub_rtp->tree,XmCONTENT,cb->column);

        switch (cb->reason) {
            case XmCR_EDIT_BEGIN:
                    fprintf(stderr,"edit begin\n");
                    XtVaSetValues(rtp->text,
                                  XtVaTypedArg,XmNbackground,
                                  XmRString,"lightsalmon",12,
                                  NULL);
                    rtp->edit_row = cb->row;
                    rtp->manual_edit_started = True;
                    return;
            case XmCR_EDIT_CANCEL:
                    fprintf(stderr,"edit cancel\n");
                    update = False;
		    rtp->manual_edit_started = False;
                    break;
            case XmCR_EDIT_COMPLETE:
                    fprintf(stderr,"edit complete\n");
		    rtp->manual_edit_started = False;
                    break;
            case XmCR_EDIT_INSERT:
                    fprintf(stderr,"edit insert\n");
		    rtp->manual_edit_started = True;
                    XtVaSetValues(rtp->text,
                                  XmNcursorPosition,0,
                                  XmNborderWidth,2,
                                  XmNcursorPositionVisible,True,
                                  XtVaTypedArg,XmNbackground,
                                  XmRString,"lightsalmon",12,
                                  NULL);
                    XmTextSetInsertionPosition(rtp->text,0);
                    
                    return;
        }
        if (update) {
                rtResData *resp;
                char *string;
                
                XtVaGetValues(w,
                              XmNrowPtr,rowptr,
                              XmNrowUserData,&ndata,
                              NULL);
                string = XmTextGetString(rtp->text);
                resp = (rtResData *)ndata->info;
                AddToSetValList(rtp,resp,(XtPointer)string);
                XtVaSetValues(w,
                              XmNcolumn,1,
                              XmNrow,cb->row,
                              XmNcellPixmap,Check_Pixmap,
                              NULL);
        }
        XtVaSetValues(pub_rtp->tree,
                      XmNcolumn,2,
                      XmNrow,cb->row,
                      XtVaTypedArg,XmNcellBackground,
                      XmRString,"#d0d0d0",8,
                      NULL);
        XtVaSetValues(rtp->text,
                      XmNvalue,rtp->selected_row_xmstr,
                      XtVaTypedArg,XmNbackground,
                      XmRString,"#d0d0d0",8,
                      NULL);

        return;
        
}

static void ScrollCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	NgResTreeRec *rtp = (NgResTreeRec *) udata;

        printf("in scroll callback\n");
#if 0
        AdjustTextWidget(rtp);
#endif        
        return;
}

static void CountChildren
(
        NhlClass class,
        int	 *count    /* count is not initialized */
        )
{
        _NhlChildResList        child;

        if (class->base_class.class_inited & _NhlObjClassFlag)
                return;
        
        child = class->base_class.child_resources;

        while (child) {
                (*count)++;
                CountUniqueSuperClasses(child,count);
                CountChildren(child->class,count);
                child = child->next;
        }
        return;
}
static void AddUniqueSuperClasses
(
        NhlClass class,
        NhlClass *classes,
        int	 *index
        )
{
        NhlClass sclass;
        int i;
        
        sclass = class->base_class.superclass;
        if (! sclass->base_class.superclass)
                return;

        for (i = *index - 1; i > -1; i--)
                if (sclass == classes[i])
                        return;
        classes[*index] = sclass;
        (*index)++;
        AddUniqueSuperClasses(sclass,classes,index);
        return;
}
            

static void AddChildrenToList
(
        NhlClass class,
        NhlClass *classes,
        int	 *index
        )
{
        _NhlChildResList        child;

        if (class->base_class.class_inited & _NhlObjClassFlag)
                return;
        
        child = class->base_class.child_resources;

        while (child) {
                AddUniqueSuperClasses(child->class,classes,index);
                classes[*index] = child->class;
                (*index)++;
                AddChildrenToList(child->class,classes,index);
                child = child->next;
        }
        return;
}
        
static void OrderClasses
(
        NgResTreeRec *rtp
        )
{
        NgResTree *pub_rtp = &rtp->restree;
	NhlClass class = rtp->class;
        int count;

        rtp->class_count = 0;

        while (class) {
                rtp->class_count++;
                class = class->base_class.superclass;
        }
        rtp->super_class_count = rtp->class_count - 1;
        rtp->classes = NhlMalloc((rtp->class_count + 10) * sizeof(NhlClass));
        class = rtp->class;
        count = rtp->super_class_count;
        while (count + 1) {
                rtp->classes[count--] = class;
                class = class->base_class.superclass;
        }
        count = rtp->super_class_count+1;
        AddChildrenToList(rtp->class,rtp->classes,&count);
        rtp->class_count = count;
        rtp->instantiated = NhlMalloc(rtp->class_count * sizeof(NhlBoolean));
        rtp->top_res_counts = NhlMalloc(rtp->class_count * sizeof(int));

        return;
}
static NhlClass *Classes = NULL;
static int Class_Count;

static int ClassSort
(
        const void *p1,
        const void *p2
)
{
        rtResData rd1 = *(rtResData *)p1;
        rtResData rd2 = *(rtResData *)p2;
        int i1,i2;

        if (rd1.real_class == rd2.real_class) {
                return strcmp(NrmQuarkToString(rd1.res->nrm_name),
                              NrmQuarkToString(rd2.res->nrm_name));
        }
        for (i1 = 0; i1 < Class_Count; i1++)
                if (rd1.real_class == Classes[i1])
                        break;
        for (i2 = 0; i2 < Class_Count; i2++)
                if (rd2.real_class == Classes[i2])
                        break;
        if (i1 < i2)
                return -1;
	
        return 1;
}

static void OrderResources
(
        NgResTreeRec *rtp
        )
{
        NgResTree *pub_rtp = &rtp->restree;
        int i,j,count = 0;

        OrderClasses(rtp);
        rtp->res_data_alloc_count = rtp->qnames_count;
        rtp->res_data = NhlMalloc
                (rtp->res_data_alloc_count * sizeof(rtResData));

        for (i = 0; i < rtp->qnames_count; i++) {
                NrmResource *res = _NhlGetResInfo(rtp->class,rtp->qnames[i]);

                if (count == rtp->res_data_alloc_count) {
                        rtp->res_data_alloc_count += 10;
                        rtp->res_data = NhlRealloc
                                (rtp->res_data,
                                 rtp->res_data_alloc_count*sizeof(rtResData));
                }
                if (res->res_info & _NhlRES_INTERCEPTED) {
                        NrmResource *native_res[8];
                        int j, native_res_count = 0;
                        _NhlGetNativeResInfo(rtp->class,rtp->qnames[i],
                                             &native_res_count,native_res);
#if 0                        
                        for (j = 0; j < native_res_count; j++) {
                                rtp->res_data[count].res = res;
                                rtp->res_data[count].real_class =
                                        native_res[j]->nhlclass;
                                count++;
                        }
#endif
                        rtp->res_data[count].res = res;
                        rtp->res_data[count].real_class =
                                        native_res[0]->nhlclass;
                        rtp->res_data[count].value = NULL;
                        rtp->res_data[count].vis = False;
                        rtp->res_data[count].ndata = NULL;
                        count++;
                        
                }
                else {
                        rtp->res_data[count].res = res;  
                        rtp->res_data[count].real_class = res->nhlclass;
                        rtp->res_data[count].value = NULL;
                        rtp->res_data[count].vis = False;
                        rtp->res_data[count].ndata = NULL;
                        count++;
                }
        }
        rtp->res_data_count = count;
        Classes = rtp->classes;
        Class_Count = rtp->class_count;
        
        qsort(rtp->res_data,rtp->res_data_count,sizeof(rtResData),ClassSort);

        j = 0;
        for (i = 0; i < rtp->class_count; i++) {
                rtp->instantiated[i] = 0;
                rtp->top_res_counts[i] = 0;
                while (j < rtp->res_data_count &&
                       rtp->res_data[j].real_class == rtp->classes[i]) {
                        rtp->top_res_counts[i]++;
                        j++;
                }
                printf("%s %d\n",rtp->classes[i]->base_class.class_name,
                       rtp->top_res_counts[i]);
        }

	if (NhlClassIsSubclass(rtp->class,NhltransformClass)) {
		if (! rtp->sres_state) {
			rtp->sres_state = NhlMalloc
				(NhlNumber(SpecialResources)
				 * sizeof(rtSResState));
		}
		for (i = 0; i < NhlNumber(SpecialResources ); i++) {
                	NrmResource *res;
			SResVal *srv = &SpecialResources[i];
			rtSResState *srs = &rtp->sres_state[i];
			srs->res_data = NULL;
			srs->faked = False;
			res = _NhlGetResInfo(rtp->class,
					     NrmStringToQuark(srv->res_name));
			for (j = 0; j < rtp->res_data_count; j++) {
                        	if (res == rtp->res_data[j].res) {
                                	srs->res_data = &rtp->res_data[j];
					break;
				}
			}
		}
	}
        return;
}

NhlErrorTypes NgResTreeResUpdateComplete
(
        NgResTree	*res_tree,
        int		hlu_id,
        NhlBoolean	update_all
        )
{
        NgResTreeRec *rtp;
        NgResTree *pub_rtp;
	rtSetValNode *svp;
        int i,res_count = 0,row_count;
        NhlPointer *values;
        char buf[256];
        rtResData *resp;
        rtResData **res_data;
        int row = 0;
        NhlBoolean done = False;
        int getval_count = 0;
        int editable_mask;
        _NhlConvertContext context = NULL;
	NhlClass last_class = NULL;
        
	printf("in res tree res update complete\n");

        rtp = (NgResTreeRec *) res_tree;
        if (!rtp) return NhlFATAL;
        pub_rtp = &rtp->restree;
        rtp->hlu_id = hlu_id;

        if (pub_rtp->preview_instance)
                editable_mask = _NhlRES_NOCACCESS;
        else
                editable_mask = _NhlRES_NOSACCESS;
        
        XtVaSetValues(pub_rtp->tree,
                      XmNlayoutFrozen,True,
                      NULL);
        
        if (!pub_rtp->preview_instance) {
                MarkInstantiated(rtp);
                UpdateInstantiatedStatus(rtp,&rtp->top,1);
        }
        
        XtVaGetValues(pub_rtp->tree,
                      XmNrows,&row_count,
                      NULL);
        values = NhlMalloc(rtp->res_data_count * sizeof(NhlPointer));
        res_data = NhlMalloc(rtp->res_data_count * sizeof(rtResData *));
        memset(values,0,rtp->res_data_count * sizeof(NhlPointer));

        for (i=0,resp = rtp->res_data; i < rtp->res_data_count; i++,resp++) {
                NhlBoolean found = False;
                rtNodeData *ndata;
                
                
                if (! resp->vis)
                        continue;
                ndata = resp->ndata;
                
                XtVaSetValues(pub_rtp->tree,
                              XmNcolumn,1,
                              XmNrow,ndata->row,
                              XmNcellPixmap,No_Check_Pixmap,
                              NULL);
                if (_NhlIsSubtypeQ(Qgenarray,resp->res->nrm_type) ||
                    resp->res->nrm_type == Qdatalist ||
                    resp->res->nrm_type == Qobjid ||
                    resp->res->nrm_type == Qdataspeclist)
                       NhlRLGet(Grlist,
                                NrmQuarkToString(resp->res->nrm_name),
                                NhlTGenArray,&values[res_count]);
                else
                       NhlRLGet(Grlist,
                                NrmQuarkToString(resp->res->nrm_name),
                                NhlTString,&values[res_count]);
                res_data[res_count] = resp;
                res_count++;
                
        }
        printf("res count is %d; getval_count is %d\n",res_count,getval_count);

        if (res_count)
                NhlGetValues(rtp->hlu_id,Grlist);
        
        printf("updating tree\n");
        for (i=0; i < res_count; i++,resp++) {
                NhlBoolean null_val = False;
                
                resp = res_data[i];
                
		if (! values[i]) {
			sprintf(buf,"<null>");
                        null_val = True;
                }
                else if (_NhlIsSubtypeQ(Qgenarray,resp->res->nrm_type)) {
                        if (resp->res->nrm_type != Qvariable) {
                                NhlGenArray ga = (NhlGenArray) values[i];
                                sprintf(buf,"%dD array of %d elements",
                                        ga->num_dimensions,ga->num_elements);
                                NhlFreeGenArray(ga);
                        }
                        else {
                                NhlString strval;
                                NrmValue from, to;
                                if (! context)
                                      context = _NhlCreateConvertContext(NULL);
                                if (resp->real_class != last_class) {
                                        _NhlConvertContextClass
                                                (context,resp->real_class);
					last_class = resp->real_class;
				}
                                from.size = sizeof(Qgenarray);
                                from.data.ptrval = values[i];
                                to.size = sizeof(NhlPointer);
                                to.data.ptrval = &strval;
                                _NhlConvertData(context,Qgenarray,Qstring,
                                                &from,&to);
                                sprintf(buf,"%s",strval);
                        }
                }
                else if (resp->res->nrm_type == Qdatalist ||
                         resp->res->nrm_type == Qobjid ||
                         resp->res->nrm_type == Qdataspeclist) {
                        NhlGenArray ga = (NhlGenArray) values[i];
                        if (ga->num_elements == 0)
                              sprintf(buf,"<null>");
                        else {
                                int hlu_id = ((int*)ga->data)[0];
                                NhlString sym = NgNclGetHLURef
                                        (rtp->go->go.nclstate,hlu_id);
                                sprintf(buf,"%s",sym);
                        }
                        NhlFreeGenArray(ga);
                }
		else {
                        sprintf(buf,"%s",(NhlString)values[i]);
                        NhlFree(values[i]);
		}
                if (! null_val && (resp->res->nrm_type == Qfloat ||
                    resp->res->nrm_type == Qdouble ||
                    resp->res->nrm_type == Qvariable)) {
                        NgFixFloat(buf);
                        NgRemoveZeros(buf);
                }
                XmLGridSetStringsPos
                        (pub_rtp->tree,XmCONTENT,
                         resp->ndata->row,XmCONTENT,2,buf);
                if (resp->res->res_info & editable_mask) {
                        XtVaSetValues(pub_rtp->tree,
                                      XmNcolumnRangeStart,1,
                                      XmNcolumnRangeEnd,2,
                                      XmNrow,resp->ndata->row,
                                      XmNcellEditable,False,
                                      XmNcellBackground,Background,
                                      NULL);
                }
                rtp->c2_width = MAX(rtp->c2_width,strlen(buf));
        }
	DoSpecialResources(rtp);
        XtVaSetValues(pub_rtp->tree,
                      XmNcolumn,2,
                      XmNcolumnWidth,rtp->c2_width,
                      NULL);
        XtVaSetValues(pub_rtp->tree,
                      XmNlayoutFrozen,False,
                      NULL);

        if (context)
             _NhlFreeConvertContext(context);   
        NhlFree(res_data);
        NhlFree(values);
	EmptySetValList(rtp);
        NhlRLClear(Grlist);

	printf("leaving res tree res update complete\n");
        return NhlNOERROR;
        
}

static void GetActualSpecialValues
(
        NgResTreeRec	*rtp
        )
{
        NgResTree *pub_rtp = &rtp->restree;
        char buf[256];
        int i;
        NhlClass last_class = NULL;
        _NhlConvertContext      context = _NhlCreateConvertContext(NULL);
        NrmQuark qnames[_NhlMAXTREEDEPTH], qclasses[_NhlMAXTREEDEPTH];
        int wk_id;
        NrmValue from, to;
        char *fromtype;
        char *strval;
        NrmQuark qfrom;
        NrmDatabase resdb;
        NhlLayer app,parent;
        int depth,size = rtp->class->base_class.layer_size;
        char *base = NhlMalloc(size);
	NrmHashTable	stackslist[_NhlMAXRESLIST];
	NrmHashTable	*slist = stackslist;
	int		slistlen = _NhlMAXRESLIST;
       
        memset(base,0,size);
        _NhlConvertContextClass(context,rtp->class);
        app = _NhlGetCurrentApp();
        parent = app;
        resdb = _NhlGetResDB(NULL);
        if ((depth = _NhlGetNamesAndClasses
            (parent,qnames,qclasses,_NhlMAXTREEDEPTH)) < 0 ||
            depth > _NhlMAXTREEDEPTH - 2) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                          "Max instance tree depth exceeded");
                return;
        }
        qnames[depth] = rtp->qhlu;
        qnames[depth+1] = NrmNULLQUARK;
        qclasses[depth] = rtp->class->base_class.nrm_class;
        qclasses[depth+1] = NrmNULLQUARK;
        
/*
 * Retrieve the levels of the ResDB that we actually need
 */

	while(!NrmQGetSearchList(resdb,qnames,qclasses,slist,slistlen)){

		if(slist == stackslist)
			slist = NULL;
		
		slistlen *= 2;
		slist = (NrmHashTable *)NhlRealloc(slist,
					sizeof(NrmHashTable) * slistlen);
		if(slist == NULL) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"Out of memory");
			return;
                }
                
	}
        
        for (i = 0; i < NhlNumber(SpecialResources); i++) {
		NhlBoolean	raccess;
                NrmQuark	rdbtype;
                rtSResState	*srs = &rtp->sres_state[i];
                rtResData	*resp = rtp->sres_state[i].res_data;

                if (! resp)
                        continue;
                if (resp->res->nhlclass != last_class) {
                        _NhlConvertContextClass(context,resp->res->nhlclass);
                        last_class = resp->res->nhlclass;
                }
		raccess = !(resp->res->res_info&_NhlRES_NORACCESS);
                srs->dbres_value = NULL;
                if (raccess && NrmGetQResFromList
                    (slist,resp->res->nrm_name,resp->res->nrm_class,
                     &rdbtype,&from)) {
                        NhlArgVal       data;
                        NrmValue	tmp_to;
                                
                        tmp_to.size = sizeof(NhlArgVal);
                        tmp_to.data.ptrval = &data;
                        _NhlConvertData(context,rdbtype,
                                        resp->res->nrm_type,
                                        &from,&tmp_to);
                        srs->dbres_value = data.ptrval;
                }
                resp++;
        }
        _NhlFreeConvertContext(context);
        
        return;
}

void NgResTreePreviewResList
(
        int		setrl_id,
        NhlPointer	res_tree
        )
{
        NgResTreeRec *rtp;
        NgResTree *pub_rtp;
        int i;

	printf("in res tree add res list\n");

        rtp = (NgResTreeRec *) res_tree;
        if (!rtp) return;
        pub_rtp = &rtp->restree;
/*
 * special resource hack.
 * all the "special resources" have immediate mode defaults; this code
 * would have to change if string or other values were added.
 */
	if (rtp->sres_state) {
                GetActualSpecialValues(rtp);
        	for (i = 0; i < NhlNumber(SpecialResources ); i++) {
                	SResVal *srv = &SpecialResources[i];
			rtSResState *srs = &rtp->sres_state[i];
                        if (! srs->res_data)
                                continue;
                        else if (srs->dbres_value && 
                                 srs->dbres_value !=
                                 srs->res_data->res->nrm_default_val.ptrval)
                                continue;
                        else if (srv->actual_val !=
                                 srs->res_data->res->nrm_default_val.ptrval)
                                continue;
                        NhlRLSet(setrl_id,srv->res_name,
                                 NhlTString,srv->fake_val);
                        srs->faked = True;
		}
	}
        return;
}

void NgResTreeAddResList
(
        int		nclstate,
        NhlPointer	res_tree,
        int		block_id
        )
{
        NgResTreeRec *rtp;
        NgResTree *pub_rtp;
	rtSetValNode *svp;
        int i,res_count = 0;
        NhlString *res_names;
        NhlString *values;
        NhlBoolean *quote;

	printf("in res tree add res list\n");

        rtp = (NgResTreeRec *) res_tree;
        if (!rtp) return;
        pub_rtp = &rtp->restree;
        
        for (svp = rtp->set_val_list; svp != NULL; svp = svp->next) {
                res_count++;
        }
        values = NhlMalloc(res_count * sizeof(NhlString));
        res_names = NhlMalloc(res_count * sizeof(NhlString));
        quote = NhlMalloc(res_count * sizeof(NhlBoolean));
        
        for (svp = rtp->set_val_list,i=0; svp != NULL; svp = svp->next,i++) {
                res_names[i] = NrmQuarkToString(svp->res_data->res->nrm_name);
                values[i] = svp->res_data->value;
                if (_NhlIsSubtypeQ(Qenum,svp->res_data->res->nrm_type) ||
                    _NhlIsSubtypeQ(Qstring,svp->res_data->res->nrm_type)) {
                        if (values[i][0] != ']')
                                quote[i] = True;
                        else {
                                values[i] = &(values[i][1]);
                                quote[i] = False;
                        }
                }
                else {
                        if (values[i][0] != ']')
                                quote[i] = False;
                        else {
                                values[i] = &(values[i][1]);
                                quote[i] = True;
                        }
                }
#if 0                
		if (values[i][0] == '(' && values[i][1] == '/')
			quote[i] = False;
                else if (svp->res_data->res->nrm_type == Qdatalist ||
                         svp->res_data->res->nrm_type == Qdataspeclist ||
                         svp->res_data->res->nrm_type == Qobjid)
			quote[i] = False;
		else
			quote[i] = True;
#endif                
        }
        NgNclVisBlockAddResList(nclstate,block_id,res_count,
                                res_names,values,quote);
        NhlFree(res_names);
        NhlFree(values);
        NhlFree(quote);
        
        return;
}

static void FreeSubNodes
(
        rtNodeData	*ndata
        )
{
        int i;

        if (! ndata->subdata)
                return;
        for (i = 0; i < ndata->subcount; i++) {
                FreeSubNodes(&ndata->subdata[i]);
        }
        NhlFree(ndata->subdata);
        return;
}
static int ExpandNodeDataList
(
        NgResTreeRec	*rtp,
        rtNodeData	*to_ndata,
        rtNodeData	*from_ndata,
        int		node_count,
        int		row
        )
{
        NgResTree *pub_rtp = &rtp->restree;
        int i;

        for (i = 0; i < node_count; i++) {
		if (from_ndata[i].expanded) {
			XtVaSetValues(pub_rtp->tree,
				      XmNrow,row,
				      XmNrowIsExpanded,True,
				      NULL);
		}
		if (from_ndata[i].subcount)
			ExpandTree(rtp,&to_ndata[i],row);
		to_ndata[i].expanded = from_ndata[i].expanded;
		row++;
                if (from_ndata[i].subdata)
                        row = ExpandNodeDataList
                                (rtp,to_ndata[i].subdata,
                                 from_ndata[i].subdata,
                                 from_ndata[i].subcount,row);
                else
                        row += to_ndata[i].subcount;
        }
        
        return row;
}

static void DupSetValState
(
        NgResTreeRec *tortp,
        NgResTreeRec *fromrtp
        )
{
        rtSetValNode *setvalp;
        rtResData	*fromresp,*toresp;
        NhlString	value;
        int 		i;
        
	for (setvalp = fromrtp->set_val_list; 
	     setvalp != NULL; setvalp = setvalp->next) {
                fromresp = setvalp->res_data;
                for (i = 0,toresp = tortp->res_data;
                     i < tortp->res_data_count; i++,toresp++) {
                        if (toresp->res->nrm_name != fromresp->res->nrm_name)
                                continue;
                        value = XtMalloc(strlen(fromresp->value)+1);
                        strcpy(value,fromresp->value);
                        AddToSetValList(tortp,toresp,(XtPointer)value);
                        if (toresp->vis) {
                                XtVaSetValues(tortp->restree.tree,
                                              XmNcolumn,1,
                                              XmNrow,toresp->ndata->row,
                                              XmNcellPixmap,Check_Pixmap,
                                              NULL);
                                XmLGridSetStringsPos
                                        (tortp->restree.tree,XmCONTENT,
                                         toresp->ndata->row,XmCONTENT,2,value);
                        }
                        break;
                }
        }
        return;
}

/*
 * Copies the state of an existing restree to another restree. If
 * to_res_tree is NULL, a new restree is created. 
 */

NgResTree *NgDupResTree
(
        NgGO			go,
        Widget			parent,
        NrmQuark		qhlu,
        NhlClass		class,
        int			hlu_id,
	NgResTree		*to_res_tree,
        NgResTree		*from_res_tree
        )
{
        NgResTreeRec *fromrtp,*tortp;

        fromrtp = (NgResTreeRec *) from_res_tree;
        if (!fromrtp)
                return NULL;

	if (to_res_tree) {
		NgUpdateResTree(to_res_tree,qhlu,class,hlu_id);
		tortp = (NgResTreeRec *) to_res_tree;
	}
	else
		tortp = (NgResTreeRec *) 
			NgCreateResTree
				(go,parent,qhlu,class,hlu_id);

        tortp->restree.preview_instance = fromrtp->restree.preview_instance;
        
        if (!fromrtp->expand_called)
                return (NgResTree *) tortp;

        ExpandNodeDataList
                (tortp,tortp->top.subdata,
                 fromrtp->top.subdata,fromrtp->top.subcount,0);

        if (fromrtp->set_val_list)
                DupSetValState(tortp,fromrtp);
        
	return (NgResTree *) tortp;
        
}
        
NhlErrorTypes NgUpdateResTree
(
        NgResTree		*res_tree,
        NrmQuark		qhlu,
        NhlClass		class,
        int			hlu_id
        )
{
        NhlErrorTypes ret;
        NgResTreeRec *rtp;
        NgResTree *pub_rtp;
        int	i,nrows,nvisrows;
        static Dimension height;
        NhlBoolean first = True;
        int	nattrs,ndims,tcount;
        XmLTreeRowDefinition *rowdefs;
        char	buf[256];
        Dimension width = 0;
        rtNodeData	*ndata;
        
        rtp = (NgResTreeRec *) res_tree;
        if (!rtp) return NhlFATAL;
        pub_rtp = &rtp->restree;
        
        XmLGridDeleteAllRows(pub_rtp->tree,XmCONTENT);
        EmptySetValList(rtp);

        rtp->qhlu = qhlu;
        rtp->hlu_id = hlu_id;
        rtp->edit_row = 0;
        
        ndata = &rtp->top;
        ndata->parent = NULL;
        ndata->type = _rtTop;
        ndata->info = (void *)qhlu;
        ndata->expanded = True;

        if (ndata->subdata)
                FreeSubNodes(ndata);

        if (class != rtp->class) {
                if (rtp->qnames)
                        NhlFree(rtp->qnames);
                if (rtp->classes)
                        NhlFree(rtp->classes);
                if (rtp->top_res_counts)
                        NhlFree(rtp->top_res_counts);
                if (rtp->res_data)
                        NhlFree(rtp->res_data);
                rtp->qnames = _NhlGetUserResources(class,&rtp->qnames_count);
                rtp->class = class;
                OrderResources(rtp);
        }

        tcount = 3;
        rowdefs = NhlMalloc
                (tcount * sizeof(XmLTreeRowDefinition));
        ndata->subdata = NhlMalloc(tcount * sizeof(rtNodeData));
        ndata->subcount = tcount;

        
        for (i = 0; i < tcount; i++) {
                NhlBoolean expands = True;

                switch (i) {
                    case 0:
                            sprintf(buf,"Super Classes");
                            break;
                    case 1:
                            sprintf(buf,"%s",
                                    rtp->class->base_class.class_name);
                            expands =
                             rtp->top_res_counts[rtp->super_class_count] > 0 ?
                                    True : False;
                            break;
                    case 2:
                            sprintf(buf,"Child Classes");
                            expands = rtp->class_count >
                                    rtp->super_class_count + 1 ? True : False;
                            break;
                }
                rowdefs[i].level = 1;
                rowdefs[i].expands = expands;
                rowdefs[i].isExpanded = False;
                rowdefs[i].pixmap = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].pixmask = XmUNSPECIFIED_PIXMAP;
                rowdefs[i].string = XmStringCreateLocalized(buf);
        }
        XmLTreeAddRows(pub_rtp->tree,rowdefs,tcount,0);

        for (i = 0; i < tcount; i++) {
                XtPointer udata;
                
                switch (i) {
                    case 0:
                            sprintf(buf,"%d",rtp->super_class_count);
                            ndata->subdata[i].type = _rtClassGroup;
                            ndata->subdata[i].info =
                                    (void *) _rtSuperClassGroup;
                            break;
                    case 1:
                            sprintf(buf,"%d resources",
                                  rtp->top_res_counts[rtp->super_class_count]);
                            ndata->subdata[i].type = _rtClass;
                            ndata->subdata[i].info = (void *) rtp->class;
                            break;
                    case 2:
                            sprintf(buf,"%d",rtp->class_count -
                                    rtp->super_class_count - 1);
                            ndata->subdata[i].type = _rtClassGroup;
                            ndata->subdata[i].info =
                                    (void *) _rtChildClassGroup;
                            break;
                }
                ndata->subdata[i].type += _rtLevel1;
                ndata->subdata[i].row = i;
                ndata->subdata[i].parent = ndata;
                ndata->subdata[i].expanded = False;
                ndata->subdata[i].subcount = 0;
                ndata->subdata[i].subdata = NULL;
                
                width = MAX(width,strlen(buf));
                XtVaSetValues(pub_rtp->tree,
                              XmNrow,i,
                              XmNrowUserData,&ndata->subdata[i],
                              NULL);
                XmLGridSetStringsPos
                        (pub_rtp->tree,XmCONTENT,i,XmCONTENT,2,buf);
                XmStringFree(rowdefs[i].string);
        }

        
        rtp->c2_width = MAX(width,14);
        XtVaSetValues(pub_rtp->tree,
                      XmNcolumn,2,
                      XmNcolumnWidth,rtp->c2_width,
                      NULL);
        rtp->created = True;
        rtp->expand_called = False;
        NhlFree(rowdefs);

        if (! rtp->scroll_cbs_installed && pub_rtp->h_scroll) {
                XtAddCallback(pub_rtp->h_scroll,
                              XmNvalueChangedCallback,ScrollCB,rtp);
                XtAddCallback(pub_rtp->v_scroll,
                              XmNvalueChangedCallback,ScrollCB,rtp);
                rtp->scroll_cbs_installed = True;
        }
        
        return NhlNOERROR;
}


NgResTree *NgCreateResTree
(
        NgGO			go,
        Widget			parent,
        NrmQuark		qhlu,
        NhlClass		class,
        int			hlu_id
        )
{
        NhlErrorTypes ret;
        NgResTreeRec *rtp;
        NgResTree *pub_rtp;
        static NhlBoolean first = True;
        static XrmValue from_black,to_black,from_white,to_white;
 
        if (first) {
		XtAppAddActions(go->go.x->app,myact,NhlNumber(myact));
                Qlong_name = NrmStringToQuark("long_name");
                Qgenarray = NrmStringToQuark(NhlTGenArray);
                Qdouble = NrmStringToQuark(NhlTDouble);
                Qfloat = NrmStringToQuark(NhlTFloat);
                Qvariable = NrmStringToQuark(NhlTVariable);
                Qstring = NrmStringToQuark(NhlTString);
                Qpointer = NrmStringToQuark(NhlTPointer);
                Qimmediate = NrmStringToQuark(NhlTImmediate);
                Qdatalist = NrmStringToQuark(_NhlTDataList);
                Qdataspeclist = NrmStringToQuark(_NhlTDataSpecList);
		Qenum = NrmStringToQuark(NhlTEnum);
		Qcolorindex = NrmStringToQuark(NhlTColorIndex);
                Qobjid = NrmStringToQuark(NhlTObjId);
                Grlist = NhlRLCreate(NhlGETRL);

                XtVaGetValues(go->go.shell,
                              XmNforeground,&Foreground,
                              XmNbackground,&Background,
                              NULL);
                from_black.size = sizeof(String);
                from_black.addr = (XPointer) "black";
                to_black.size = sizeof(Pixel);
                to_black.addr = (XPointer)&Black;
                if (! XtConvertAndStore
                    (go->go.shell,XtRString,&from_black,XtRPixel,&to_black))
                        printf("convert error\n");
                
                from_white.size = sizeof(String);
                from_white.addr = (XPointer) "white";
                to_white.size = sizeof(Pixel);
                to_white.addr = (XPointer)&White;
                XtConvertAndStore(go->go.shell,
                                  XtRString,&from_white,XtRPixel,&to_white);
                first = False;
        }
        
        rtp = NhlMalloc(sizeof(NgResTreeRec));
        if (!rtp) return NULL;

        NhlVAGetValues(go->go.appmgr,
                       NgNappNclState,	&rtp->nclstate,
                       NULL);
        
        pub_rtp = &rtp->restree;
        pub_rtp->geo_notify = NULL;
        pub_rtp->geo_data = NULL;
        pub_rtp->h_scroll = NULL;
        pub_rtp->v_scroll = NULL;
        
        rtp->created = False;
        rtp->class = NULL;
        rtp->qhlu = NrmNULLQUARK;
        rtp->go = go;
        rtp->qnames = NULL;
        rtp->top.subdata = NULL;
        rtp->top.subcount = 0;
        rtp->qnames = NULL;
        rtp->classes = NULL;
        rtp->top_res_counts = NULL;
        rtp->res_data = NULL;
        rtp->set_val_list = NULL;
        rtp->scroll_cbs_installed = False;
        rtp->manual_edit_started = False;
	rtp->text = NULL;
        rtp->selected_row_xmstr = NULL;
	rtp->enum_info.mega = NULL;
	rtp->enum_info.strings = NULL;
	rtp->enum_info.pixmaps = NULL;
	rtp->enum_info.cmap = NULL;
        rtp->enum_info.count = 0;
        rtp->enum_info.str_assigned_count = 0;
	rtp->enum_info.up = False;
	rtp->sres_state = NULL;
                
        pub_rtp->tree = XtVaCreateManagedWidget
                ("ResTree",
                 xmlTreeWidgetClass,parent,
                 XmNselectionPolicy,XmSELECT_NONE,
                 XmNverticalSizePolicy,XmVARIABLE,
                 XmNhorizontalSizePolicy,XmVARIABLE,
                 XmNcolumns, 3,
                 NULL);
        
        XtVaSetValues(pub_rtp->tree,
                      XmNcellDefaults,True,
                      XmNcellRightBorderType,XmBORDER_NONE,
                      XmNcellTopBorderType,XmBORDER_NONE,
                      XmNcellBottomBorderType,XmBORDER_NONE,
                      XmNcellAlignment,XmALIGNMENT_LEFT,
                      NULL);

        XtVaSetValues(pub_rtp->tree,
		      XmNcellDefaults,True,
		      XmNcolumn,2,
		      XmNcellLeftBorderType,XmBORDER_NONE,
                      XmNcellMarginLeft,0,
		      NULL);
        XtVaSetValues(pub_rtp->tree,
		      XmNcellDefaults,True,
		      XmNcolumn,1,
                      XmNcellMarginLeft,3,
		      XmNcellType,XmPIXMAP_CELL,
		      XmNcolumnWidth,2,
		      NULL);

#if 0        
        black = BlackPixelOfScreen(XtScreen(go->go.shell));
        white = WhitePixelOfScreen(XtScreen(go->go.shell));
#endif        
        Check_Pixmap = XCreatePixmapFromBitmapData
                (XtDisplay(go->go.shell),
                 DefaultRootWindow(XtDisplay(go->go.shell)),
                 Check_Bits,PIXMAP_WIDTH,PIXMAP_HEIGHT,Black,White,
                 DefaultDepthOfScreen(XtScreen(go->go.shell)));
        No_Check_Pixmap = XCreatePixmapFromBitmapData
                (XtDisplay(go->go.shell),
                 DefaultRootWindow(XtDisplay(go->go.shell)),
                 No_Check_Bits,PIXMAP_WIDTH,PIXMAP_HEIGHT,
                 Black,White,
                 DefaultDepthOfScreen(XtScreen(go->go.shell)));
        Test_Pixmap = XCreatePixmapFromBitmapData
                (XtDisplay(go->go.shell),
                 DefaultRootWindow(XtDisplay(go->go.shell)),
                 No_Check_Bits,1,1,Foreground,Background,
                 DefaultDepthOfScreen(XtScreen(go->go.shell)));
                                   
        XtAddCallback(pub_rtp->tree,XmNexpandCallback,ExpandCB,rtp);
        XtAddCallback(pub_rtp->tree,XmNcollapseCallback,CollapseCB,rtp);
        XtAddCallback(pub_rtp->tree,XmNeditCallback,EditCB,rtp);
        XtAddCallback(pub_rtp->tree,XmNselectCallback,SelectCB,rtp);
        XtAddCallback(pub_rtp->tree,XmNcellFocusCallback,FocusCB,rtp);

        XtVaGetValues(pub_rtp->tree,
                      XmNtextWidget,&rtp->text,
                      NULL);
        XtAddCallback(rtp->text,XmNlosingFocusCallback,UnFocusCB,rtp);
        ret = NgUpdateResTree((NgResTree*)rtp,qhlu,class,hlu_id);

        if (ret < NhlWARNING) {
                NhlFree(rtp);
                return NULL;
        }
        return (NgResTree *) rtp;
}

void NgDestroyResTree
(
        NgResTree		*res_tree
        )
{
        NgResTreeRec *rtp;
        
        rtp = (NgResTreeRec *) res_tree;
        if (!rtp) return;

        EmptySetValList(rtp);
        FreeSubNodes(&rtp->top);

        NhlFree(rtp);
        
        return;
}

static void EnumButtonUpAction(
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
        )
{
        NgResTreeRec *rtp;
	rtEnumInfoRec		*ep;
        
        printf("in enum button up action %x\n",XtWindow(w));

        XtVaGetValues(w,
                      XmNuserData,&rtp,
                      NULL);
        if (!rtp) {
                printf("invalid user data\n");
                return;
        }
        ep = &rtp->enum_info;

        printf("%x %x %d %d %d \n",rtp,ep,xev->xbutton.time,
               XtGetMultiClickTime(XtDisplay(w)), ep->time);
        
}

