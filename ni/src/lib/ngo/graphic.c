/*
 *      $Id: graphic.c,v 1.2 1998-01-08 01:19:25 dbrown Exp $
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
#include <ncarg/ngo/nclstate.h>

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
        static int srlist;
        int i,parent_id;
        int count,*id_array;
        static NhlBoolean first = True;
        NhlClass class;
        NgGO go = (NgGO)_NhlGetLayer(goid);

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
        return NhlCreate(hlu_id,ncl_graphic,class,parent_id,srlist);
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
        if (*hlu_id <= NhlNULLOBJID) {
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

        if (!go) {
                NHLPERROR((NhlWARNING,NhlEUNKNOWN,"invalid graphic object"));
                return (NhlErrorTypes) NhlFATAL;
        }
        sprintf(buf,"destroy(%s)\ndelete(%s)\n",ncl_graphic);
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

        if (!go) {
                NHLPERROR((NhlWARNING,NhlEUNKNOWN,"invalid graphic object"));
                return (NhlErrorTypes) NhlFATAL;
        }
        

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
                return NhlWARNING;
        }
	base_id = _NhlBasePlot(hlu_id);
	strcpy(base_name,NgNclGetHLURef(go->go.nclstate,base_id));

        wk_name = NgNclGetHLURef(go->go.nclstate,wk_id);

        if (clear)
                sprintf(buf,"clear(%s)\ndraw(%s)\n",wk_name,base_name);
        else
                sprintf(buf,"draw(%s)\n",wk_name,base_name);
        
        (void)NgNclSubmitBlock(go->go.nclstate,buf);

        return NhlNOERROR;
}


                        



