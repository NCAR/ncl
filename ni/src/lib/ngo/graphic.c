/*
 *      $Id: graphic.c,v 1.15 2000-01-21 05:18:52 dbrown Exp $
 */
/*******************************************x*****************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		graphic.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Sep 10 18:38:01 MDT 1997
 *
 *	Description:	
 */

#include <ncarg/ngo/graphic.h>
#include <ncarg/ngo/goP.h>
#include <ncarg/hlu/ViewP.h>
#include <ncarg/ngo/xwk.h>
#include <ncarg/ngo/nclstate.h>
#include <ncarg/ngo/mwin.h>
#include <ncarg/ngo/xinteract.h>

/*
 * Function:	NgCreatePreviewGraphic
 *
 * Description:	creates a preview instance of graphic (HLU) objects. Since
 *              this is supposed to be a 'behind the scenes' kind of
 *              operation, direct HLU calls, rather than ncl block are used
 *              to create the object. Through this interface, the only
 *              resources that can be set are those that can be converted
 *              from string to the correct type. Basically this means that
 *              only resources that can be set from resource files can be
 *              set in the preview instance. In particular, this means you
 *              cannot easily create usable DataItem instances with this
 *              interface.
 *
 * In Args:	set ncl_parent to NULL if no parent is needed for object
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */

NhlErrorTypes NgCreatePreviewGraphic
(
        int		goid,
        int		*hlu_id,
	NhlString	hlu_name,
	NhlString	ncl_graphic,
	NhlString	ncl_parent,
	NhlString	classname,
        int		pres_proc_count,
        NgPreviewResProc *pres_procs,
        NhlPointer	*pres_proc_data
        )
{
	NhlErrorTypes ret;
        static int srlist;
        int i,parent_id;
        int count,*id_array;
        static NhlBoolean first = True;
        NhlClass class;
        NgGO go = (NgGO)_NhlGetLayer(goid);
	NgHluData 	hdata = NULL;

        if (!go) {
                NHLPERROR((NhlWARNING,NhlEUNKNOWN,"invalid graphic object"));
                return (NhlErrorTypes) NhlFATAL;
        }
        
        *hlu_id = NhlNULLOBJID;
        
        class = NgNclHluClassPtrFromName(go->go.nclstate,classname);
        if (! class) {
                NHLPERROR((NhlWARNING,NhlEUNKNOWN,"invalid class name"));
                return (NhlErrorTypes) NhlFATAL;
        }

        if (first) {
                srlist = NhlRLCreate(NhlSETRL);
                first = False;
        }
        else {
                NhlRLClear(srlist);
        }
	/* 
	 * For now only views and workstations need to carry around the 
	 * NgHluData struct. Set preview True.
	 */

	if(NhlClassIsSubclass(class,NhlworkstationClass) ||
	   NhlClassIsSubclass(class,NhlviewClass)) {
		hdata = NgGetHluData();
		if (! hdata)
			return NhlFATAL;
		hdata->preview = True;
		NhlRLSet(srlist,_NhlNguiData2,NhlTPointer,hdata);
	}
        for (i = 0; i < pres_proc_count; i++) {
                (*pres_procs[i])(srlist,pres_proc_data[i]);
        }
        if (ncl_parent == NULL)
                parent_id = NhlDEFAULT_APP;
        else {
                parent_id = NgNclGetHluObjId
                        (go->go.nclstate,ncl_parent,&count,&id_array);
                if (count > 1) {
                        NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                       "variable %s is an array: only handling first element",
                                   ncl_parent));
                        NhlFree(id_array);
                }
                if (parent_id <= NhlNULLOBJID) {
                        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                                   "unable to retrieve valid parent id"));
                        return NhlFATAL;
                }
        }
        ret = NhlCreate(hlu_id,hlu_name,class,parent_id,srlist);
	
	if (ret < NhlWARNING || *hlu_id <= NhlNULLOBJID 
	    || ! _NhlGetLayer(*hlu_id)) {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                           "Unable to create preview graphic for %s",
			   ncl_graphic));
                return NhlFATAL;
        }

	return ret;
}

/*
 * Function:	NgUpdatePreviewGraphic
 *
 * Description:	updates a preview instance of graphic (HLU) objects. Since
 *              this is supposed to be a 'behind the scenes' kind of
 *              operation, direct HLU calls, rather than ncl block are used
 *              to create the object. Through this interface, the only
 *              resources that can be set are those that can be converted
 *              from string to the correct type. Basically this means that
 *              only resources that can be set from resource files can be
 *              set in the preview instance. In particular, this means you
 *              cannot easily create usable DataItem instances with this
 *              interface.
 *
 * In Args:	set ncl_parent to NULL if no parent is needed for object
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */

NhlErrorTypes NgUpdateePreviewGraphic
(
        int		goid,
        int		hlu_id,
        int		pres_proc_count,
        NgPreviewResProc *pres_procs,
        NhlPointer	*pres_proc_data
        )
{
        static int srlist;
        int i;
        static NhlBoolean first = True;
        NgGO go = (NgGO)_NhlGetLayer(goid);

        if (!go) {
                NHLPERROR((NhlWARNING,NhlEUNKNOWN,"invalid graphic object"));
                return (NhlErrorTypes) NhlFATAL;
        }

        if (first) {
                srlist = NhlRLCreate(NhlSETRL);
                first = False;
        }
        else {
                NhlRLClear(srlist);
        }
        for (i = 0; i < pres_proc_count; i++) {
                (*pres_procs[i])(srlist,pres_proc_data[i]);
        }
        return NhlSetValues(hlu_id,srlist);
}

NhlErrorTypes NgDestroyPreviewGraphic
(
        int		goid,
        int		hlu_id
        )
{
        NgGO go = (NgGO)_NhlGetLayer(goid);

        if (!go) {
                NHLPERROR((NhlWARNING,NhlEUNKNOWN,"invalid graphic object"));
                return (NhlErrorTypes) NhlFATAL;
        }
        return NhlDestroy(hlu_id);
}

/*
 * Function:	NgCreateGraphic
 *
 * Description:	creates a graphic (HLU) object. Resources are added by
 *		any number of SetRecProc s that are called in order of
 *		appearance in the list. 
 * 
 *
 * In Args:	set ncl_parent to NULL if no parent is needed for object.
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */

NhlErrorTypes NgCreateGraphic
(
        int		goid,
        int		*hlu_id,
	NhlString	hlu_name,
	NhlString	ncl_graphic,
	NhlString	ncl_parent,
	NhlString	classname,
        int		res_proc_count,
        NgSetResProc	*res_procs,
        NhlPointer	*res_proc_data
        )
{
        int i,count,*id_array;
        int block_id;
        NhlString hlu_parent = "defaultapp";
        NgGO go = (NgGO)_NhlGetLayer(goid);

        if (!go) {
                NHLPERROR((NhlWARNING,NhlEUNKNOWN,"invalid graphic object"));
                return (NhlErrorTypes) NhlFATAL;
        }
        
        *hlu_id = NhlNULLOBJID;
        
            /* ncl identifier might not be the same as hlu name */

        if (ncl_parent) {
                int parent_id = NgNclGetHluObjId
                        (go->go.nclstate,ncl_parent,&count,&id_array);
                if (count > 1) {
                        NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                       "variable %s is an array: only handling first element",
                                   ncl_parent));
                        NhlFree(id_array);
                }
                if (parent_id <= NhlNULLOBJID) {
                        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                                   "unable to retrieve valid parent id"));
                        return NhlFATAL;
                }
                hlu_parent = (NhlString)NhlName(parent_id);
        }
        block_id = NgNclVisBlockBegin(go->go.nclstate,_NgCREATE,hlu_name,
				      ncl_graphic,hlu_parent,classname);
        for (i = 0; i < res_proc_count; i++) {
                (*res_procs[i])(go->go.nclstate,res_proc_data[i],block_id);
        }
        NgNclVisBlockEnd(go->go.nclstate,block_id);
                
        *hlu_id = NgNclGetHluObjId(go->go.nclstate,
				   ncl_graphic,&count,&id_array);
        if (count > 1) {
                NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                       "variable %s is an array: only handling first element",
                           ncl_graphic));
                NhlFree(id_array);
        }
        if (*hlu_id <= NhlNULLOBJID || ! _NhlGetLayer(*hlu_id)) {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                           "unable to create graphic object %s",ncl_graphic));
                return NhlFATAL;
        }
        return NhlNOERROR;
}

NhlBoolean NgViewOn(
	int view_id
)
{
	NhlLayer l = _NhlGetLayer(view_id);
	NgHluData hdata;
	NgViewObj vobj;

	if (! (l && _NhlIsView(l)))
		return False;
	hdata = l->base.gui_data2;

	if (hdata) {
		vobj = (NgViewObj)hdata->gdata;
		if (vobj && vobj->visible)
			return True;
	}
	return False;
}


NhlErrorTypes NgUpdateGraphic
(
        int		goid,
	NhlString	ncl_graphic,
        int		res_proc_count,
        NgSetResProc	*res_procs,
        NhlPointer	*res_proc_data
        )
{
        int i;
        int block_id;
        NgGO go = (NgGO)_NhlGetLayer(goid);
	int tcount = 0;
	int hlu_id, *id_array, count;

        if (!go) {
                NHLPERROR((NhlWARNING,NhlEUNKNOWN,"invalid graphic object"));
                return (NhlErrorTypes) NhlFATAL;
        }
        
        block_id = NgNclVisBlockBegin(go->go.nclstate,_NgSETVAL,NULL,
				      ncl_graphic,NULL,NULL);
        for (i = 0; i < res_proc_count; i++) {
                tcount += (*res_procs[i])
			(go->go.nclstate,res_proc_data[i],block_id);
        }
	if (! tcount)
		return NhlNOERROR;

        NgNclVisBlockEnd(go->go.nclstate,block_id);

        hlu_id = NgNclGetHluObjId
                (go->go.nclstate,ncl_graphic,&count,&id_array);

        for (i = 0; i < count; i++) {
		NhlLayer l;
                if (count > 1)
                        hlu_id = id_array[i];
		l = _NhlGetLayer(hlu_id);
		if (l && _NhlIsView(l) && NgViewOn(hlu_id)) {
			NhlLayer tl;
			int top_id = _NhlTopLevelView(hlu_id);
			tl = _NhlGetLayer(top_id);
			if (tl) {
				NgHluData hdata = 
					(NgHluData) tl->base.gui_data2;
				if (hdata)
					hdata->draw_req = True;
			}
		}
	}	
        if (count > 1) {
                NhlFree(id_array);
        }
                
        return NhlNOERROR;
}
/*
 * Sets the selected view to some other view than the current one
 */
static void SetNewSelectedView
(
	NgWksState      wks_state,
	int		xwk_wrap_id,
	int		xwk_id,
	int		current_selected_id
)
{
	int top_level_count;
	int *top_level_views;
	int i,ix;

	top_level_count = NgTopLevelViews
		(wks_state,xwk_id,&top_level_views);

	if (top_level_count <= 1) { /* there's only one view */
		NgSetSelectedXwkView(xwk_wrap_id,NhlNULLOBJID);
		return;
	}
		
	for (i = 0; i < top_level_count; i++) {
		if (top_level_views[i] == current_selected_id) {
			ix = (i < top_level_count - 1) ? i + 1 : i - 1;
			break;
		}
	}
	if (i == top_level_count)
		ix = 0;

	NgSetSelectedXwkView(xwk_wrap_id,top_level_views[ix]);

	return;
}

NhlErrorTypes DestroyGraphicArray
(
	NgGO		go,
	NhlString	ncl_graphic,
	int		count,
	int		*id_array
)
{
	NhlErrorTypes ret = NhlNOERROR;
	NclExtValueRec *attval;
	NhlLayer l;
	int i;
        char buf[512];
	NgWksState      wks_state;

	attval = NclReadVarAtt(NrmStringToQuark(ncl_graphic),
			       NrmStringToQuark(ndvOBJECTLIST));
	if (! attval) {
		ret = NhlFATAL;
		goto err_ret;
	}
	NhlVAGetValues(go->go.appmgr,
		       NgNappWksState,&wks_state,
		       NULL);
/*
 * first clear any top level views
 */
        for (i = 0; i < count; i++) {
		l = _NhlGetLayer(id_array[i]);
		if (! l)
			continue;
		if (_NhlIsView(l) && _NhlIsXWorkstation(l->base.wkptr) &&
		    (_NhlTopLevelView(id_array[i]) == id_array[i])) {
			NgWksObj wkobj;
			int selected_view_id;
			NhlBoolean draw_selected_only;
				
			NgHluData hdata = 
				(NgHluData) l->base.wkptr->base.gui_data2;

			wkobj = hdata ? (NgWksObj) hdata->gdata : NULL;
			if (! wkobj)
				continue;
			NhlVAGetValues(wkobj->wks_wrap_id,
				       NgNxwkSelectedView,&selected_view_id,
				       NgNxwkDrawSelectedViewOnly,
				       &draw_selected_only,
				       NULL);
			if (! draw_selected_only ||
			    (draw_selected_only && 
			     selected_view_id == id_array[i])) {
				NgViewObj vobj = NULL;

				SetNewSelectedView
					(wks_state,wkobj->wks_wrap_id,
					 l->base.wkptr->base.id,id_array[i]);
				NgClearXwkView(wkobj->wks_wrap_id,id_array[i]);
				/*
				 * Turn off the view so that clearing other
				 * views does not cause this view to be 
				 * redrawn.
				 */
				vobj = NgGetView(wks_state,id_array[i]);
				if (vobj)
					vobj->visible = False;
				
			}
		}
	}
/*
 * then remove all plots, making sure not to do any more drawing.
 */
        for (i = 0; i < count; i++) {
		NhlBoolean destroy = True;
		NhlLayer l,xwkl = NULL;
		NhlString obj_name;
		NgWksObj wkobj = NULL;
		NhlBoolean save_auto_refresh;
		

		obj_name = NrmQuarkToString(((NrmQuark *)attval->value)[i]);
		l = _NhlGetLayer(id_array[i]);
		if (! l) {
			sprintf(buf,"%s_%s",ncl_graphic,obj_name);
			if (! NclSymbolDefined(buf))
				continue;
			else
				destroy = False;
		}
		else {
			if (_NhlIsXWorkstation(l))
				xwkl = l;
			else if (_NhlIsView(l)) { 
				NgViewObj vobj;

				if ( _NhlIsXWorkstation(l->base.wkptr))
					xwkl = l->base.wkptr;

				/*
				 * Since the view has already been cleared
				 * set its viewport offscreen so that the
				 * destroy callback can't do another clear.
				 */
				vobj = NgGetView(wks_state,id_array[i]);
				if (vobj) {
					vobj->xbbox.p0.x = 
						vobj->xbbox.p0.y = 10000;
					vobj->xbbox.p1.x = 
						vobj->xbbox.p0.x + 1;
					vobj->xbbox.p1.y = 
						vobj->xbbox.p0.y + 1;
					vobj->xvp = vobj->xbbox;
				}
			}
		}
		if (xwkl) {
			NgHluData hdata = 
				(NgHluData) xwkl->base.gui_data2;
			wkobj = hdata ? (NgWksObj) hdata->gdata : NULL;
			save_auto_refresh = wkobj->auto_refresh;
		}
		if (wkobj)
			wkobj->auto_refresh = False;
		if (destroy)
			sprintf(buf,"destroy(%s_%s)\ndelete(%s_%s)\n",
				ncl_graphic,obj_name,ncl_graphic,obj_name);
		else
			sprintf(buf,"delete(%s_%s)\n",ncl_graphic,obj_name);
		(void)NgNclSubmitBlock(go->go.nclstate,buf);
		if (wkobj)
			wkobj->auto_refresh = save_auto_refresh;
	}	
	sprintf(buf,"delete(%s)\n",ncl_graphic);
	(void)NgNclSubmitBlock(go->go.nclstate,buf);

 err_ret:

	if (attval)
		NclFreeExtValue(attval);
	NhlFree(id_array);

	return ret;
}
	
NhlErrorTypes NgDestroyGraphic
(
        int		goid,
	NhlString	ncl_graphic
        )
{
        char buf[512];
        NgGO go = (NgGO)_NhlGetLayer(goid);
	int hlu_id, *id_array, count;
	NhlLayer l;

        if (!(go && ncl_graphic)) {
                NHLPERROR((NhlWARNING,NhlEUNKNOWN,"invalid graphic object"));
                return (NhlErrorTypes) NhlFATAL;
        }

	if (! NclSymbolDefined(ncl_graphic)) {
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,
        "undefined symbol,probably a preview object,no way to find hlu id\n"));
		return NhlWARNING;
	}
		
        hlu_id = NgNclGetHluObjId
                (go->go.nclstate,ncl_graphic,&count,&id_array);

	if (count > 1) {
		return(DestroyGraphicArray(go,ncl_graphic,count,id_array));
	}
	l = _NhlGetLayer(hlu_id);
	if (l && _NhlIsXWorkstation(l)) {
		NgWksObj wkobj;
		NgHluData hdata = (NgHluData) l->base.gui_data2;
		wkobj = hdata ? (NgWksObj) hdata->gdata : NULL;
		if (wkobj)
			wkobj->auto_refresh = False;
	}
	
        sprintf(buf,"destroy(%s)\ndelete(%s)\n",ncl_graphic,ncl_graphic);
        (void)NgNclSubmitBlock(go->go.nclstate,buf);
                
        return NhlNOERROR;
}

NhlErrorTypes DrawGraphicArray
(
	NgGO		go,
	NgWksState 	wks_state,
	NhlString	ncl_graphic,
	int		count,
	int		*id_array,
        NhlBoolean	clear
)
{
	NhlLayer l;
	int i;
        char buf[512];
	int last_top_id = NhlNULLOBJID;
	NgHluData hdata;
	int top_id,wk_id;

	for (i = 0; i < count; i++) {
		if (! NhlIsView(id_array[i]))
		    continue;

		top_id = _NhlTopLevelView(id_array[i]);
		if (top_id <= NhlNULLOBJID)
			continue;

		l = _NhlGetLayer(top_id);
		if (! l)
			continue;
		wk_id = l->base.wkptr->base.id;
		hdata = (NgHluData) l->base.gui_data2;

		if (! hdata->draw_req || top_id == last_top_id) {
			NgUpdateViewBB(wks_state,id_array[i]);
			hdata->draw_req = False;
			if (id_array[i] != top_id) {
				l = _NhlGetLayer(id_array[i]);
				hdata = (NgHluData) l->base.gui_data2;
				hdata->draw_req = False;
			}
			continue;
		}
		else if (NhlIsClass(wk_id,NhlxWorkstationClass)) {
			NhlLayer wkl = _NhlGetLayer(wk_id);
			NgWksObj wko;

			hdata = (NgHluData) wkl->base.gui_data2;

			wko = hdata ? (NgWksObj) hdata->gdata : NULL;
			if (! wko) {
				NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				   "graphic not drawable %s",ncl_graphic));
				return NhlFATAL;
			}
			NgDrawXwkView(wko->wks_wrap_id,top_id,clear);
		}
		else {
			char *wk_name;
			char topname[128];

			strcpy(topname,NgNclGetHLURef(go->go.nclstate,top_id));
			wk_name = NgNclGetHLURef(go->go.nclstate,wk_id);
			if (clear)
				sprintf(buf,"clear(%s)\ndraw(%s)\n",
					wk_name,topname);
			else
				sprintf(buf,"draw(%s)\n",topname);
			(void)NgNclSubmitBlock(go->go.nclstate,buf);
			NgUpdateViewBB(wks_state,id_array[i]);
		}

		hdata = (NgHluData) l->base.gui_data2;
		hdata->draw_req = False;
		last_top_id = top_id;
	}
	/*
	 * if there wasn't at least one drawable item, consider it
	 * a FATAL error (for now -- it should maybe just be a warning).
	 */
	return (last_top_id == NhlNULLOBJID) ? NhlFATAL : NhlNOERROR;
}
NhlErrorTypes NgDrawGraphic
(
        int		goid,
	NhlString	ncl_graphic,
        NhlBoolean	clear
        )
{
        char buf[512];
        int hlu_id,wk_id,base_id,count,*id_array;
        NhlLayer layer;
        NhlString wk_name;
	char base_name[512];
        NgGO go = (NgGO)_NhlGetLayer(goid);
	NgWksState wks_state;
	NgHluData hdata;

        if (!go) {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"invalid graphic object"));
                return (NhlErrorTypes) NhlFATAL;
        }
 	NhlVAGetValues(go->go.appmgr,
		       NgNappWksState,&wks_state,
		       NULL);

        hlu_id = NgNclGetHluObjId
                (go->go.nclstate,ncl_graphic,&count,&id_array);
        if (count > 1) {
                NhlErrorTypes ret =  DrawGraphicArray
			(go,wks_state,ncl_graphic,count,id_array,clear);
		NhlFree(id_array);
		return ret;
        }
	layer = _NhlGetLayer(hlu_id);

	if (! (layer && _NhlIsView(layer))) {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                           "graphic not drawable %s",ncl_graphic));
                return NhlFATAL;
        }
	wk_id = layer->base.wkptr->base.id;


	base_id = _NhlTopLevelView(hlu_id);
	base_id =  base_id != NhlNULLOBJID ? base_id : hlu_id;

	if (NhlIsClass(wk_id,NhlxWorkstationClass)) {
		NhlLayer wkl = _NhlGetLayer(wk_id);
		NgWksObj wko;
		hdata = (NgHluData) wkl->base.gui_data2;

		wko = hdata ? (NgWksObj) hdata->gdata : NULL;
		if (! wko) {
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				   "graphic not drawable %s",ncl_graphic));
			return NhlFATAL;
		}
		NgDrawXwkView(wko->wks_wrap_id,base_id,clear);
	}
	else {
        
		strcpy(base_name,NgNclGetHLURef(go->go.nclstate,base_id));
        
		wk_name = NgNclGetHLURef(go->go.nclstate,wk_id);
		if (clear)
			sprintf(buf,"clear(%s)\ndraw(%s)\n",wk_name,base_name);
		else
			sprintf(buf,"draw(%s)\n",base_name);
		(void)NgNclSubmitBlock(go->go.nclstate,buf);
		NgUpdateViewBB(wks_state,hlu_id);
	}
        

	hdata = (NgHluData) layer->base.gui_data2;
	hdata->draw_req = False;

        return NhlNOERROR;
}


NhlErrorTypes NgDrawView
(
        int		goid,
	int		view_id,
	NhlBoolean	clear
)
{
        NgGO go = (NgGO)_NhlGetLayer(goid);
	NhlString vname;

        if (!go) {
                NHLPERROR((NhlWARNING,NhlEUNKNOWN,"invalid graphic object"));
                return (NhlErrorTypes) NhlFATAL;
        }

        vname = NgNclGetHLURef(go->go.nclstate,view_id);
	if (! vname) {
                NHLPERROR((NhlWARNING,NhlEUNKNOWN,"invalid view object"));
                return (NhlErrorTypes) NhlFATAL;
        }

	return NgDrawGraphic(goid,vname,clear);
}


extern NgHluData NgGetHluData
(
	void
)
{
	NgHluData hlu_data = NhlMalloc(sizeof(NgHluDataRec));

	if (! hlu_data) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NULL;
	}

	hlu_data->page_id = 0;
	hlu_data->q_sym = NrmNULLQUARK;
	hlu_data->ddata = NULL;
	hlu_data->gdata = NULL;
	hlu_data->go_id = NhlNULLOBJID;
	hlu_data->preview = False;
	hlu_data->qplotstyle = NrmNULLQUARK;
	hlu_data->destroy_cb = NULL;
	hlu_data->draw_req = False;

	return hlu_data;
}
	


void NgFreeHluData
(
	NgHluData hlu_data
)

{
/*
 * Don't free the gdata -- it's only a copy of the main window ndata pointer
 */
#if 0
	if (hlu_data->gdata)
		NhlFree(hlu_data->gdata);
#endif
	NhlFree(hlu_data);
	return;
}


/*
 * These ResList functions are passed as function pointers to the 
 * NgCreate..Graphic and NgUpdate..Graphic routines.
 */

void NgPreviewResList
(
        int		setrl_id,
        NhlPointer	data
        )
{
	NgResData res_data = (NgResData ) data;
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

int NgAddResList
(
        int		nclstate,
        NhlPointer	data,
        int		block_id
        )
{
	NgResData res_data = (NgResData ) data;
	NhlBoolean *quote;
	int i,lcount = 0;
	NhlString *res,*values;

	if (! res_data->res_count)
		return 0;

	quote = NhlMalloc(res_data->res_count * sizeof(NhlBoolean));
	res = NhlMalloc(res_data->res_count * sizeof(NhlString));
	values = NhlMalloc(res_data->res_count * sizeof(NhlString));

	if (! (quote && res && values)) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return 0;
	}

	for (i = 0; i < res_data->res_count; i++) {
		if (res_data->values[i]) {
			res[lcount] = res_data->res[i];
			values[lcount] = (NhlString) res_data->values[i];
			quote[lcount] = False;
			lcount++;
		}
	}

        NgNclVisBlockAddResList(nclstate,block_id,lcount,res,values,quote);

	NhlFree(quote);
	NhlFree(res);
	NhlFree(values);

	return lcount;
}

/*
 * This function returns a new NgResData structure if passed a NULL
 * NgResData pointer, with enough entries to hold 
 * MAX(rescount,NgRESDATA_ALLOC_UNIT) resources. if resdata is not NULL,
 * and rescount >= resdata->res_alloced the allocation is increased by
 * MAX(rescount - resdata->res_alloced,NgRESDATA_ALLOC_UNIT).
 * Except for initialization, the res_count field is left to the caller
 * to handle.
 */

extern NgResData NgReallocResData
(
	NgResData resdata,
	int	  rescount
)
{
	int i,count;

	if (! resdata) {
		resdata = NhlMalloc(sizeof(NgResDataRec));
		if (! resdata) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return NULL;
		}
		resdata->res_alloced = 0;
		resdata->res_count = 0;
		resdata->res = NULL;
		resdata->values = NULL;
		resdata->types = NULL;
		resdata->vdata = NULL;
	}
	if (rescount < resdata->res_alloced) 
		return resdata;

	count = resdata->res_alloced +
		MAX(rescount - resdata->res_alloced,NgRESDATA_ALLOC_UNIT);

	resdata->res = NhlRealloc(resdata->res,count * sizeof(char*));
	resdata->values = NhlRealloc(resdata->values, 
				     count * sizeof(NhlPointer));
	resdata->types = NhlRealloc(resdata->types,
				    count * sizeof(NrmQuark));
	resdata->vdata = NhlRealloc(resdata->vdata,
				    count * sizeof(NgVarData));

	if (! (resdata->res && resdata->values && 
	       resdata->types && resdata->vdata)) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		NhlFree(resdata);
		return NULL;
	}
	for (i = resdata->res_alloced; i < count; i++) {
		resdata->res[i] = NULL;
		resdata->values[i] = NULL;
		resdata->types[i] = NrmNULLQUARK;
		resdata->vdata[i] = NULL;
	}
	resdata->res_alloced = count;

	return resdata;
}

extern void NgFreeResData
(
	NgResData resdata
)
{

	NhlFree(resdata->res);
	NhlFree(resdata->values);
	NhlFree(resdata->types);
	NhlFree(resdata->vdata);
	NhlFree(resdata);

	return;
}
