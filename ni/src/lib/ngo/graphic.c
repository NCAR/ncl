/*
 *      $Id: graphic.c,v 1.10 1999-02-23 03:56:48 dbrown Exp $
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
	char namebuf[128];
	NhlLayer l;

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
	/* special handling for the Xworkstation -- add a prefix to the
	   name so we will know not to Popup the preview XWorkstation. It
	   also means that we have to specially handle its resources that 
	   by default have the name attached. Hopefully this can be 
	   eliminated when we implement class reference objects. */

        if (first) {
                srlist = NhlRLCreate(NhlSETRL);
                first = False;
        }
        else {
                NhlRLClear(srlist);
        }
	if(NhlClassIsSubclass(class,NhlworkstationClass) ||
	   NhlClassIsSubclass(class,NhlviewClass)) {
		strcpy(namebuf,"_NgPreview_");	
		strcat(namebuf,ncl_graphic);
	}
	else {
		strcpy(namebuf,ncl_graphic);
	}
	if (class == NhlxWorkstationClass) {
		NhlRLSetString(srlist,NhlNwkIconTitle,ncl_graphic);
		NhlRLSetString(srlist,NhlNwkTitle,ncl_graphic);
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
        ret = NhlCreate(hlu_id,namebuf,class,parent_id,srlist);
	
	if (ret < NhlWARNING || *hlu_id <= NhlNULLOBJID 
	    || ! (l = _NhlGetLayer(*hlu_id))) {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                           "unable to create graphic object %s",ncl_graphic));
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
	NhlLayer l;

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
        block_id = NgNclVisBlockBegin(go->go.nclstate,_NgCREATE,ncl_graphic,
                                      hlu_parent,classname);
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
        if (*hlu_id <= NhlNULLOBJID || ! (l = _NhlGetLayer(*hlu_id))) {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                           "unable to create graphic object %s",ncl_graphic));
                return NhlFATAL;
        }
        return NhlNOERROR;
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

        if (!go) {
                NHLPERROR((NhlWARNING,NhlEUNKNOWN,"invalid graphic object"));
                return (NhlErrorTypes) NhlFATAL;
        }
        
        block_id = NgNclVisBlockBegin(go->go.nclstate,_NgSETVAL,ncl_graphic,
                                      NULL,NULL);
        for (i = 0; i < res_proc_count; i++) {
                (*res_procs[i])(go->go.nclstate,res_proc_data[i],block_id);
        }
        NgNclVisBlockEnd(go->go.nclstate,block_id);
                
        return NhlNOERROR;
}

NhlErrorTypes NgDestroyGraphic
(
        int		goid,
	NhlString	ncl_graphic
        )
{
        char buf[512];
        NgGO go = (NgGO)_NhlGetLayer(goid);
	int i,hlu_id, *id_array, count;

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
        for (i = 0; i < count; i++) {
		NhlLayer l;
                if (count > 1)
                        hlu_id = id_array[i];
		l = _NhlGetLayer(hlu_id);
		if (l && _NhlIsXWorkstation(l)) {
			NgWksObj wkobj;
			NgHluData hdata = (NgHluData) l->base.gui_data2;
			wkobj = hdata ? (NgWksObj) hdata->gdata : NULL;
			if (wkobj)
				wkobj->auto_refresh = False;
		}
	}	
        if (count > 1) {
                NhlFree(id_array);
        }
	
        sprintf(buf,"destroy(%s)\ndelete(%s)\n",ncl_graphic,ncl_graphic);
        (void)NgNclSubmitBlock(go->go.nclstate,buf);
                
        return NhlNOERROR;
}
NhlErrorTypes NgDrawGraphic
(
        int		goid,
	NhlString	ncl_graphic,
        NhlBoolean	clear
        )
{
        char buf[512];
        int i,hlu_id,wk_id,base_id,count,*id_array;
        NhlBoolean has_drawable = False;
        NhlLayer layer;
        NhlString wk_name;
	char base_name[512];
        NgGO go = (NgGO)_NhlGetLayer(goid);
	NgWksState wks_state;

        if (!go) {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"invalid graphic object"));
                return (NhlErrorTypes) NhlFATAL;
        }
	NhlVAGetValues(go->go.appmgr,
		       NgNappWksState,&wks_state,
		       NULL);

        hlu_id = NgNclGetHluObjId
                (go->go.nclstate,ncl_graphic,&count,&id_array);
        for (i = 0; i < count; i++) {
                if (count > 1)
                        hlu_id = id_array[i];
                if (hlu_id > NhlNULLOBJID) {
                        NhlClass class = NhlClassOfObject(hlu_id);
                        if (NhlClassIsSubclass(class,NhlviewClass)) {
                                has_drawable = True;
                                layer = _NhlGetLayer(hlu_id);
                                wk_id = layer->base.wkptr->base.id;
                                break;
                        }
                }
        }
        if (count > 1) {
                NhlFree(id_array);
        }
        if (! has_drawable) {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                           "graphic not drawable %s",ncl_graphic));
                return NhlFATAL;
        }

	base_id = _NhlBasePlot(hlu_id);
	base_id =  base_id != NhlNULLOBJID ? base_id : hlu_id;

	if (NhlIsClass(wk_id,NhlxWorkstationClass)) {
		NhlLayer wkl = _NhlGetLayer(wk_id);
		NgWksObj wko;
		NgHluData hdata = (NgHluData) wkl->base.gui_data2;

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

	return hlu_data;
}
	


void NgFreeHluData
(
	NgHluData hlu_data
)

{
	NhlFree(hlu_data);
	return;
}



