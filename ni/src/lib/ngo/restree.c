/*
 *      $Id: restree.c,v 1.1 1997-08-21 16:33:03 dbrown Exp $
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
#include <Xm/MenuShell.h>
#include <Xm/RowColumn.h>
#include <Xm/PushBG.h>
#include  <Xm/Text.h>
#include  <XmL/Tree.h>

#include <ncarg/ngo/restreeP.h>
#include <ncarg/ngo/xutil.h>
#include <ncarg/ngo/sort.h>
#include <ncarg/ngo/stringutil.h>
#include <ncarg/hlu/AppI.h>
#include <ncarg/hlu/View.h>
#include <ncarg/hlu/DataCommP.h>


static NrmQuark Qlong_name;
static Dimension Row_Height;
static Dimension Char_Height;
static Pixel Foreground,Background;
static NrmQuark Qpointer,Qimmediate,Qdatalist;
static NrmQuark Qgenarray,Qdouble,Qfloat,Qvariable,Qstring,Qenum;
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

static Pixmap Check_Pixmap, No_Check_Pixmap;


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
        
        values = NhlMalloc(nrows * sizeof(NhlPointer));
        memset(values,0,nrows * sizeof(NhlPointer));

        if (pub_rtp->preview_instance)
                editable_mask = _NhlRES_NOCACCESS;
        else
                editable_mask = _NhlRES_NOSACCESS;
        
        for (i = 0; i < nrows; i++) {
                if (_NhlIsSubtypeQ(Qgenarray,resp->res->nrm_type) &&
                    ! (resp->res->nrm_type == Qvariable))
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
        resp = res_data;
        for (i = 0; i < nrows; i++) {
		if (! values[i]) {
			sprintf(buf,"<null>");
                }
                else if (_NhlIsSubtypeQ(Qgenarray,resp->res->nrm_type) &&
                         ! (resp->res->nrm_type == Qvariable)) {
                        NhlGenArray ga = (NhlGenArray) values[i];
                        sprintf(buf,"%dD array of %d elements",
                                ga->num_dimensions,ga->num_elements);
                }
		else {
                        sprintf(buf,"%s",(NhlString)values[i]);
		}
                if (values[i] && (resp->res->nrm_type == Qfloat ||
                    resp->res->nrm_type == Qdouble ||
                    resp->res->nrm_type == Qvariable)) {
                        NgFixFloat(buf);
                        NgRemoveZeros(buf);
                }
                XmLGridSetStringsPos
                        (pub_rtp->tree,XmCONTENT,start_row+i,XmCONTENT,2,buf);
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
                if (values[i])
                        NhlFree(values[i]);
                resp->vis = True;
                resp->row = start_row+i;
                resp++;
        }
        NhlFree(values);

        NhlRLClear(Grlist);
        
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
                ndata->subdata[i].expanded = False;
                ndata->subdata[i].subcount = 0;
                ndata->subdata[i].subdata = NULL;
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

static void ExpandTree 
(
        NgResTreeRec	*rtp,
        rtNodeData	*ndata,
        int		row
        )
{
        NgResTree *pub_rtp = &rtp->restree;
#if 0        
        static NhlBoolean first = True;
        
        if (first) {
                XmLGridColumn colptr;
                XmLGridRow rowptr;

                first = False;
                colptr = XmLGridGetColumn(pub_rtp->tree,XmCONTENT,0);
                rowptr = XmLGridGetRow(pub_rtp->tree,XmCONTENT,0);
                XtVaGetValues(pub_rtp->tree,
                              XmNcolumnPtr,colptr,
                              XmNrowPtr,rowptr,
                              XmNcellForeground,&Foreground,
                              XmNcellBackground,&Background,
                              NULL);
        }
#endif        
        
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
	NgResTreeRec *rtp = (NgResTreeRec *) udata;
        NgResTree *pub_rtp = &rtp->restree;

        if (rtp->edit_row > -1) {
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
                rtp->edit_row = -1;
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
        int		row
        )
{
        rtSetValNode *svp, *lastsvp = NULL;

        for (svp = rtp->set_val_list; svp != NULL; svp = svp->next) {
                if (row == svp->row) {
                        if (lastsvp)
                                lastsvp->next = svp->next;
                        else
                                rtp->set_val_list = svp->next;
                        XtFree(svp->res_data->value);
                        svp->res_data->value = NULL;
                        NhlFree(svp);
                        break;
                }
                lastsvp = svp;
        }
        return;
}

static void AddToSetValList 
(
        NgResTreeRec	*rtp,
        int		row,
        rtResData	*res_data
        )
{
        rtSetValNode *setvalp;

        setvalp = NhlMalloc(sizeof(rtSetValNode));

        setvalp->row = row;
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
                RemoveFromSetValList(rtp,rtp->set_val_list->row);
                
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
        
        if (_NhlIsSubtypeQ(Qgenarray,svp->res_data->res->nrm_type) &&
            ! (svp->res_data->res->nrm_type == Qvariable))
                NhlRLGet(Grlist,
                         NrmQuarkToString(svp->res_data->res->nrm_name),
                         NhlTGenArray,&value);
        else
                NhlRLGet(Grlist,
                         NrmQuarkToString(svp->res_data->res->nrm_name),
                         NhlTString,&value);
        
        NhlGetValues(rtp->hlu_id,Grlist);
        
        if (! value) {
                sprintf(buf,"<null>");
        }
        else if (_NhlIsSubtypeQ(Qgenarray,
                                svp->res_data->res->nrm_type) &&
                 ! (svp->res_data->res->nrm_type == Qvariable)) {
                NhlGenArray ga = (NhlGenArray) value;
                sprintf(buf,"%dD array of %d elements",
                        ga->num_dimensions,ga->num_elements);
        }
        else {
                sprintf(buf,"%s",(NhlString)value);
        }
        if (value && (svp->res_data->res->nrm_type == Qfloat ||
                          svp->res_data->res->nrm_type == Qdouble ||
                          svp->res_data->res->nrm_type == Qvariable)) {
                NgFixFloat(buf);
                NgRemoveZeros(buf);
        }
        XmLGridSetStringsPos
                (pub_rtp->tree,XmCONTENT,row,XmCONTENT,2,buf);

        if (value)
                NhlFree(value);
        
        RemoveFromSetValList(rtp,svp->row);

        return True;
}

static void EditEnum
(
        NgResTreeRec	*rtp,
	int 		row,
	rtResData 	*resp
        )
{
        NgResTree *pub_rtp = &rtp->restree;
	NhlConvertArgList       args;
	int			lastopt,i,nargs,noptions = 0;
	XRectangle		rect;

	_NhlConverterGetArgs(resp->real_class,resp->res->nrm_type,Qstring,
			     &args,&nargs);
	lastopt = -999999;
	for (i = 0; i < nargs; i++) {
		if (args[i].size != lastopt) {
			noptions++;
			lastopt = args[i].size;
		}
	}
	XmLGridRowColumnToXY(pub_rtp->tree,XmCONTENT,row,XmCONTENT,1,False,
			     &rect);
	if (! rtp->enum_ed) {
		Widget menush,menu;
		menush = XtVaCreatePopupShell
			("enumMenush",xmMenuShellWidgetClass,
			 pub_rtp->tree,
			 XmNwidth,5,
			 XmNheight,5,
			 XmNallowShellResize,True,
			 XmNoverrideRedirect,True,
			 NULL);

		rtp->enum_menu = XtVaCreateWidget
			("enumMenu",xmRowColumnWidgetClass,menush,
			 XmNrowColumnType,XmMENU_PULLDOWN,
			 NULL);
		rtp->enum_ed = XtVaCreateManagedWidget
			  ("enumOptMenu",
			   xmRowColumnWidgetClass,pub_rtp->tree,
			   XmNspacing,0,
			   XmNrowColumnType,XmMENU_OPTION,
			   XmNsubMenuId,rtp->enum_menu,
			   NULL);
		rtp->enum_buttons_used = 0;
		rtp->enum_buttons_alloced = 0;
		rtp->enum_buttons = NULL;
	}
	XtMoveWidget(rtp->enum_ed,rect.x,rect.y);

	if (rtp->enum_buttons_used) 
		XtUnmanageChildren(rtp->enum_buttons,rtp->enum_buttons_used);
	if (noptions > rtp->enum_buttons_alloced) {
		rtp->enum_buttons = NhlRealloc(rtp->enum_buttons,
					       noptions * sizeof(Widget));
		for (i = rtp->enum_buttons_alloced; i < noptions; i++)
			rtp->enum_buttons[i] = XtVaCreateWidget
				("enum_option",xmPushButtonGadgetClass,
				 rtp->enum_menu,NULL);
		rtp->enum_buttons_alloced = noptions;
	}

	lastopt = -999999;
	noptions = 0;
	for (i = 0; i < nargs; i++) {
	  	XmString xmname;
		if (args[i].size != lastopt) {
			lastopt = args[i].size;

			xmname = NgXAppCreateXmString(rtp->go->go.appmgr,
						      args[i].data.strval);
                
			XtVaSetValues(rtp->enum_buttons[noptions++],
				      XmNlabelString,xmname,
				      NULL);
			NgXAppFreeXmString(rtp->go->go.appmgr,xmname);
		}
	}

	rtp->enum_buttons_used = noptions;
	XtManageChildren(rtp->enum_buttons,noptions);

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

        printf("in select callback, col %d row %d\n",cb->column,cb->row);
        
        if (cb->column != 2 & cb->column != 1)
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

	if (_NhlIsSubtypeQ(Qenum,resp->res->nrm_type))
		EditEnum(rtp,cb->row,resp);

        if (rtp->edit_row > -1) {
                XtVaSetValues(pub_rtp->tree,
                              XmNcolumn,2,
                              XmNrow,rtp->edit_row,
                              XtVaTypedArg,XmNcellBackground,
                              XmRString,"#d0d0d0",8,
                              NULL);
                rtp->edit_row = -1;
        }
        if (! off && editable) {
                XtVaSetValues(pub_rtp->tree,
                              XmNcolumn,2,
                              XmNrow,cb->row,
                              XtVaTypedArg,XmNcellBackground,
                              XmRString,"lightsalmon",12,
                              NULL);
                rtp->edit_row = cb->row;
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
        static XmString cell_string = NULL;
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
                    if (cell_string)
                            XmStringFree(cell_string);
                    XtVaGetValues
                            (pub_rtp->tree,
                             XmNcolumnPtr,colptr,
                             XmNrowPtr,rowptr,
                             XmNcellString,&cell_string,
                             NULL);
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
                    if (cell_string)
                            XmStringFree(cell_string);
                    XtVaGetValues
                            (pub_rtp->tree,
                             XmNcolumnPtr,colptr,
                             XmNrowPtr,rowptr,
                             XmNcellString,&cell_string,
                             NULL);
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
                resp->value = string;
                AddToSetValList(rtp,cb->row,resp);
                XtVaSetValues(w,
                              XmNcolumn,1,
                              XmNrow,cb->row,
                              XmNcellPixmap,Check_Pixmap,
                              NULL);
        }
        rtp->edit_row = -1;
        XtVaSetValues(pub_rtp->tree,
                      XmNcolumn,2,
                      XmNrow,cb->row,
                      XtVaTypedArg,XmNcellBackground,
                      XmRString,"#d0d0d0",8,
                      NULL);
        XtVaSetValues(rtp->text,
                      XmNvalue,cell_string,
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
        
        if (!fromrtp->expand_called)
                return (NgResTree *) tortp;

        ExpandNodeDataList
                (tortp,tortp->top.subdata,
                 fromrtp->top.subdata,fromrtp->top.subcount,0);

	return (NgResTree *) tortp;
        
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
                CountChildren(child->class,count);
                child = child->next;
        }
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
        CountChildren(rtp->class,&rtp->class_count);

        rtp->classes = NhlMalloc(rtp->class_count * sizeof(NhlClass));
        rtp->instantiated = NhlMalloc(rtp->class_count * sizeof(NhlBoolean));
        rtp->top_res_counts = NhlMalloc(rtp->class_count * sizeof(int));

        class = rtp->class;
        count = rtp->super_class_count;
        while (count + 1) {
                rtp->classes[count--] = class;
                class = class->base_class.superclass;
        }
        count = rtp->super_class_count+1;
        AddChildrenToList(rtp->class,rtp->classes,&count);

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
                        rtp->res_data[count].row = 0;
                        count++;
                        
                }
                else {
                        rtp->res_data[count].res = res;  
                        rtp->res_data[count].real_class = res->nhlclass;
                        rtp->res_data[count].value = NULL;
                        rtp->res_data[count].vis = False;
                        rtp->res_data[count].row = 0;
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
                
        return;
}

#if 0
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
        
	printf("in res tree res update complete\n");

        rtp = (NgResTreeRec *) res_tree;
        if (!rtp) return NhlFATAL;
        pub_rtp = &rtp->restree;
        rtp->hlu_id = hlu_id;
        
        for (svp = rtp->set_val_list; svp != NULL; svp = svp->next) {
                res_count++;
        }
        values = NhlMalloc(res_count * sizeof(NhlPointer));
        memset(values,0,res_count * sizeof(NhlPointer));
        
        XtVaSetValues(pub_rtp->tree,
                      XmNlayoutFrozen,True,
                      NULL);
        XtVaGetValues(pub_rtp->tree,
                      XmNrows,&row_count,
                      NULL);
        
 	for (svp = rtp->set_val_list,i=0; svp != NULL; svp = svp->next,i++) {
                    /*opening up the grid can change the row number */
                while (svp->row < row_count) {
                        rtNodeData *ndata;
                        XmLGridRow rowptr = XmLGridGetRow
                                (pub_rtp->tree,XmCONTENT,svp->row);
                        
                        XtVaGetValues(pub_rtp->tree,
                                      XmNrowPtr,rowptr,
                                      XmNrowUserData,&ndata,
                                      NULL);
                        if (svp->res_data == (rtResData *)ndata->info)
                                break;
                        svp->row++;
                }
                XtVaSetValues(pub_rtp->tree,
                              XmNcolumn,1,
                              XmNrow,svp->row,
                              XmNcellPixmap,No_Check_Pixmap,
                              NULL);
                if (_NhlIsSubtypeQ(Qgenarray,svp->res_data->res->nrm_type) &&
                    ! (svp->res_data->res->nrm_type == Qvariable))
                       NhlRLGet(Grlist,
                                NrmQuarkToString(svp->res_data->res->nrm_name),
                                NhlTGenArray,&values[i]);
                else
                       NhlRLGet(Grlist,
                                NrmQuarkToString(svp->res_data->res->nrm_name),
                                NhlTString,&values[i]);
        }
        
        NhlGetValues(rtp->hlu_id,Grlist);
        
 	for (svp = rtp->set_val_list,i=0; svp != NULL; svp = svp->next,i++) {
		if (! values[i]) {
			sprintf(buf,"<null>");
                }
                else if (_NhlIsSubtypeQ(Qgenarray,
                                        svp->res_data->res->nrm_type) &&
                         ! (svp->res_data->res->nrm_type == Qvariable)) {
                        NhlGenArray ga = (NhlGenArray) values[i];
                        sprintf(buf,"%dD array of %d elements",
                                ga->num_dimensions,ga->num_elements);
                }
		else {
                        sprintf(buf,"%s",(NhlString)values[i]);
		}
                if (values[i] && (svp->res_data->res->nrm_type == Qfloat ||
                    svp->res_data->res->nrm_type == Qdouble ||
                    svp->res_data->res->nrm_type == Qvariable)) {
                        NgFixFloat(buf);
                        NgRemoveZeros(buf);
                }
                XmLGridSetStringsPos
                        (pub_rtp->tree,XmCONTENT,svp->row,XmCONTENT,2,buf);
                if (values[i])
                        NhlFree(values[i]);
        }
        XtVaSetValues(pub_rtp->tree,
                      XmNlayoutFrozen,False,
                      NULL);

        NhlFree(values);
	EmptySetValList(rtp);
        NhlRLClear(Grlist);
        
}
#endif

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
        int row = 0,last_row = 0;
        NhlBoolean done = False;
        int getval_count = 0;
        
	printf("in res tree res update complete\n");

        rtp = (NgResTreeRec *) res_tree;
        if (!rtp) return NhlFATAL;
        pub_rtp = &rtp->restree;
        rtp->hlu_id = hlu_id;
        
        XtVaSetValues(pub_rtp->tree,
                      XmNlayoutFrozen,True,
                      NULL);
        XtVaGetValues(pub_rtp->tree,
                      XmNrows,&row_count,
                      NULL);
        values = NhlMalloc(rtp->res_data_count * sizeof(NhlPointer));
        res_data = NhlMalloc(rtp->res_data_count * sizeof(rtResData *));
        memset(values,0,res_count * sizeof(NhlPointer));

        for (i=0,resp = rtp->res_data; i < rtp->res_data_count; i++,resp++) {
                NhlBoolean found = False;

                if (! resp->vis)
                        continue;

                resp->row = MAX(resp->row,last_row+1);
                while (resp->row < row_count) {
                        rtNodeData *ndata;
                        XmLGridRow rowptr = XmLGridGetRow
                                (pub_rtp->tree,XmCONTENT,resp->row);

                        getval_count++;
                        XtVaGetValues(pub_rtp->tree,
                                      XmNrowPtr,rowptr,
                                      XmNrowUserData,&ndata,
                                      NULL);
                        if (ndata && resp == (rtResData *)ndata->info) {
                                found = True;
                                break;
                        }
                        resp->row++;
                }
                if (!found) {
                        printf("big problem\n");
                }
                
                XtVaSetValues(pub_rtp->tree,
                              XmNcolumn,1,
                              XmNrow,resp->row,
                              XmNcellPixmap,No_Check_Pixmap,
                              NULL);
                if (_NhlIsSubtypeQ(Qgenarray,resp->res->nrm_type) &&
                    ! (resp->res->nrm_type == Qvariable))
                       NhlRLGet(Grlist,
                                NrmQuarkToString(resp->res->nrm_name),
                                NhlTGenArray,&values[res_count]);
                else
                       NhlRLGet(Grlist,
                                NrmQuarkToString(resp->res->nrm_name),
                                NhlTString,&values[res_count]);
                res_data[res_count] = resp;
                res_count++;
                last_row = resp->row;
                
        }
        printf("res count is %d; getval_count is %d\n",res_count,getval_count);
        
        NhlGetValues(rtp->hlu_id,Grlist);
        
        printf("updating tree\n");
        for (i=0; i < res_count; i++,resp++) {
                resp = res_data[i];
                
		if (! values[i]) {
			sprintf(buf,"<null>");
                }
                else if (_NhlIsSubtypeQ(Qgenarray,
                                        resp->res->nrm_type) &&
                         ! (resp->res->nrm_type == Qvariable)) {
                        NhlGenArray ga = (NhlGenArray) values[i];
                        sprintf(buf,"%dD array of %d elements",
                                ga->num_dimensions,ga->num_elements);
                }
		else {
                        sprintf(buf,"%s",(NhlString)values[i]);
		}
                if (values[i] && (resp->res->nrm_type == Qfloat ||
                    resp->res->nrm_type == Qdouble ||
                    resp->res->nrm_type == Qvariable)) {
                        NgFixFloat(buf);
                        NgRemoveZeros(buf);
                }
                XmLGridSetStringsPos
                        (pub_rtp->tree,XmCONTENT,resp->row,XmCONTENT,2,buf);
                if (values[i])
                        NhlFree(values[i]);
        }
        XtVaSetValues(pub_rtp->tree,
                      XmNlayoutFrozen,False,
                      NULL);
        
        NhlFree(res_data);
        NhlFree(values);
	EmptySetValList(rtp);
        NhlRLClear(Grlist);

	printf("leaving res tree res update complete\n");
        return NhlNOERROR;
        
}
NhlErrorTypes NgResTreeAddResList
(
        NgResTree	*res_tree,
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
        if (!rtp) return NhlFATAL;
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
                quote[i] = True;
        }
        NgNclVisBlockAddResList(rtp->nclstate,block_id,res_count,
                                res_names,values,quote);
        NhlFree(res_names);
        NhlFree(values);
        NhlFree(quote);
        
        return NhlNOERROR;
}

#if 0
#define MAXBUFSIZE 512

NhlErrorTypes NgResTreeSetValues
(
        NgResTree	*res_tree
        )
{
        NgResTreeRec *rtp;
        NgResTree *pub_rtp;
	rtSetValNode *svp;
        char buf[MAXBUFSIZE];
        int i,buflen = 512;
	char *val, *res;
        static NhlBoolean first = True;
	static int srlist;
        int res_count = 0;
        NhlPointer *values;
        
        if (first) {
                srlist = NhlRLCreate(NhlSETRL);
                first = False;
        }

	printf("in set values\n");

        rtp = (NgResTreeRec *) res_tree;
        if (!rtp) return NhlFATAL;
        pub_rtp = &rtp->restree;
/*
 * If it's the preview instance, don't go through ncl. Otherwise, the
 * ncl needs to know about what was set. CAN't do setvalues before create!!!
 */
        if (pub_rtp->preview_instance) {
                for (svp = rtp->set_val_list; svp != NULL; svp = svp->next) {
                        val = svp->res_data->value;
                        res = NrmQuarkToString(svp->res_data->res->nrm_name);
                        NhlRLSetString(srlist,res,val);
                        svp->res_data->set = True;
                        res_count++;
                }
                NhlSetValues(rtp->hlu_id,srlist);
        }
        else {
                char *endstr = "end setvalues\n";
                int startlen,endlen = strlen(endstr);
                NhlBoolean out = True;
                
                sprintf(buf,"setvalues %s\n",NrmQuarkToString(rtp->qhlu));
                startlen = buflen = strlen(buf);
                for (svp = rtp->set_val_list; svp != NULL; svp = svp->next) {
                        if (out) {
                                out = False;
                        }
                        val = svp->res_data->value;
                        res = NrmQuarkToString(svp->res_data->res->nrm_name);
                        if (strlen(val) + strlen(res) + 5 + endlen + buflen >
                            MAXBUFSIZE) {
                                strcat(buf,endstr);
                                (void)NgNclSubmitBlock(rtp->nclstate,buf);
                                sprintf(buf,"setvalues %s\n",
                                        NrmQuarkToString(rtp->qhlu));
                                startlen = buflen = strlen(buf);
                        }
                        sprintf(&buf[buflen],"\"%s\" : \"%s\"\n",res,val);
                        buflen = strlen(buf);
                        svp->res_data->set = True;
                        res_count++;
                }
                if (buflen > startlen) {
                        strcat(buf,endstr);
                        (void)NgNclSubmitBlock(rtp->nclstate,buf);
                }
        }

            /* reset the check pixmap to the unset state and
               retrieve values in order to replace strings with canonical
               form */
        
        values = NhlMalloc(res_count * sizeof(NhlPointer));
        memset(values,0,res_count * sizeof(NhlPointer));
        
        XtVaSetValues(pub_rtp->tree,
                      XmNlayoutFrozen,True,
                      NULL);
 	for (svp = rtp->set_val_list,i=0; svp != NULL; svp = svp->next,i++) {
                XtVaSetValues(pub_rtp->tree,
                              XmNcolumn,1,
                              XmNrow,svp->row,
                              XmNcellPixmap,No_Check_Pixmap,
                              NULL);
                if (_NhlIsSubtypeQ(Qgenarray,svp->res_data->res->nrm_type) &&
                    ! (svp->res_data->res->nrm_type == Qvariable))
                       NhlRLGet(Grlist,
                                NrmQuarkToString(svp->res_data->res->nrm_name),
                                NhlTGenArray,&values[i]);
                else
                       NhlRLGet(Grlist,
                                NrmQuarkToString(svp->res_data->res->nrm_name),
                                NhlTString,&values[i]);
        }
        
        NhlGetValues(rtp->hlu_id,Grlist);
        
 	for (svp = rtp->set_val_list,i=0; svp != NULL; svp = svp->next,i++) {
		if (! values[i]) {
			sprintf(buf,"<null>");
                }
                else if (_NhlIsSubtypeQ(Qgenarray,
                                        svp->res_data->res->nrm_type) &&
                         ! (svp->res_data->res->nrm_type == Qvariable)) {
                        NhlGenArray ga = (NhlGenArray) values[i];
                        sprintf(buf,"%dD array of %d elements",
                                ga->num_dimensions,ga->num_elements);
                }
		else {
                        sprintf(buf,"%s",(NhlString)values[i]);
		}
                if (values[i] && (svp->res_data->res->nrm_type == Qfloat ||
                    svp->res_data->res->nrm_type == Qdouble ||
                    svp->res_data->res->nrm_type == Qvariable)) {
                        NgFixFloat(buf);
                        NgRemoveZeros(buf);
                }
                XmLGridSetStringsPos
                        (pub_rtp->tree,XmCONTENT,svp->row,XmCONTENT,2,buf);
                if (values[i])
                        NhlFree(values[i]);
        }
        XtVaSetValues(pub_rtp->tree,
                      XmNlayoutFrozen,False,
                      NULL);

        NhlFree(values);
	EmptySetValList(rtp);
        NhlRLClear(srlist);
        NhlRLClear(Grlist);
        
        return;
}
#endif        
        
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

        rtp->qhlu = qhlu;
        rtp->hlu_id = hlu_id;
        rtp->edit_row = -1;
        
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
        Pixel black,white;
 
        if (first) {
                Qlong_name = NrmStringToQuark("long_name");
                Qgenarray = NrmStringToQuark(NhlTGenArray);
                Qdouble = NrmStringToQuark(NhlTDouble);
                Qfloat = NrmStringToQuark(NhlTFloat);
                Qvariable = NrmStringToQuark(NhlTVariable);
                Qstring = NrmStringToQuark(NhlTString);
                Qpointer = NrmStringToQuark(NhlTPointer);
                Qimmediate = NrmStringToQuark(NhlTImmediate);
                Qdatalist = NrmStringToQuark(_NhlTDataList);
		Qenum = NrmStringToQuark(NhlTEnum);
                Grlist = NhlRLCreate(NhlGETRL);

                XtVaGetValues(go->go.shell,
                              XmNforeground,&Foreground,
                              XmNbackground,&Background,
                              NULL);
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
	rtp->enum_ed = NULL;
	rtp->enum_buttons_alloced = 0;
                
        pub_rtp->tree = XtVaCreateManagedWidget
                ("ResTree",
                 xmlTreeWidgetClass,parent,
                 XmNselectionPolicy,XmSELECT_NONE,
                 XmNverticalSizePolicy,XmVARIABLE,
                 XmNhorizontalSizePolicy,XmVARIABLE,
                 XmNcolumns, 3,
		 XmNheadingRows,1,
		 XmNsimpleHeadings,"Resource|Set|Value",
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

        black = BlackPixelOfScreen(XtScreen(go->go.shell));
        white = WhitePixelOfScreen(XtScreen(go->go.shell));
        
        Check_Pixmap = XCreatePixmapFromBitmapData
                (XtDisplay(go->go.shell),
                 DefaultRootWindow(XtDisplay(go->go.shell)),
                 Check_Bits,PIXMAP_WIDTH,PIXMAP_HEIGHT,Foreground,Background,
                 DefaultDepthOfScreen(XtScreen(go->go.shell)));
        No_Check_Pixmap = XCreatePixmapFromBitmapData
                (XtDisplay(go->go.shell),
                 DefaultRootWindow(XtDisplay(go->go.shell)),
                 No_Check_Bits,PIXMAP_WIDTH,PIXMAP_HEIGHT,
                 Foreground,Background,
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

        FreeSubNodes(&rtp->top);

        NhlFree(rtp);
        
        return;
}
