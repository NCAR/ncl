/*
 *      $Id: varmenus.c,v 1.1 1997-06-04 18:08:34 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		varmenus.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Mar 17 20:52:04 MST 1997
 *
 *	Description:	
 */

#include <ncarg/ngo/varmenusP.h>
#include <ncarg/ngo/xutil.h>
#include <ncarg/ngo/sort.h>

#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/RowColumn.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>
#include <Xm/MenuShell.h>

static void
FileVarMenu(
	NgVarMenus	vmenus,
        NrmQuark	qfile
);
        
static void
CreateCB
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	NgVarRec	*vrec = (NgVarRec *)udata.ptrval;

        vrec->varcount++;
	vrec->modified = True;

        if (vrec->varcount > 0)
                XtSetSensitive(vrec->mbutton,True);
        return;

}

static void
DeleteCB
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	NgVarRec	*vrec = (NgVarRec *)udata.ptrval;

        vrec->varcount--;
	vrec->modified = True;

        if (vrec->varcount < 1)
                XtSetSensitive(vrec->mbutton,False);
        return;
}

        
static void
FileVarMenuEH
(
	Widget		w,
	XtPointer	udata,
	XEvent		*event,
	Boolean		*cont
)
{
        NgVarMenus	vmenus = (NgVarMenus) udata;
	NrmQuark	qfile;
        
#if	DEBUG_VAR_MENUS & DEBUG_ENTRY
	fprintf(stderr,"FileVarMenuEH(IN)\n");
#endif
        XtVaGetValues(w,
                      XmNuserData,&qfile,
                      NULL);
        
#if	DEBUG_VAR_MENUS & DEBUG_MENUS
	fprintf(stderr"%s filevars\n", NrmQuarkToString(qfile));
#endif
        
        FileVarMenu(vmenus,qfile);
        
        return;
        
}

static void FileVarMenuCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
        )
{
        NgVarMenus	vmenus = (NgVarMenus) udata;
	NrmQuark	qfile;
        
#if	DEBUG_VAR_MENUS & DEBUG_ENTRY
	fprintf(stderr,"FileVarMenuCB(IN)\n");
#endif
        XtVaGetValues(w,
                      XmNuserData,&qfile,
                      NULL);
#if	DEBUG_VAR_MENUS & DEBUG_MENUS
	fprintf(stderr,"%s filevars\n", NrmQuarkToString(qfile));
#endif
        FileVarMenu(vmenus,qfile);

        return;
}

#define MAX_MENU_ENTRIES 37 /* not counting "More ..." */
static void
FileVarMenu
(
	NgVarMenus	vmenus,
        NrmQuark	qfile
        )
{
	VarMenusRec	*vp = (VarMenusRec *)vmenus->vmenu_data;
	NgVarRec	*vrec;
        int		i;
        NgFileVarRec	*fvar;
        NclApiFileInfoRec *finfo;
        static int	tvarcount = 0;
        static int	menu_count = 0;
        static NrmQuark	*qvars = NULL;
        int		count,tcount;
        NhlBoolean	cont = False;
        Widget		cont_submenu;
        XmString	xmname;
        
#if	DEBUG_VAR_MENUS & DEBUG_MENUS
	fprintf(stderr," %s filevars\n", NrmQuarkToString(qfile));
#endif
        
        vrec = &vp->filevars;
        fvar = (NgFileVarRec*) vrec->priv;

        if (fvar->qfile == qfile)
                return;
        else if (qfile == NrmNULLQUARK) { /* cont menu */
                qfile = fvar->qfile;
                finfo = fvar->dl->u.file;
                if (tvarcount >= finfo->n_vars) /* all menus updated */
                        return;
        }
        else {
                if (fvar->in_use > 0)
                        NclFreeDataList(fvar->dl);
                fvar->dl = NclGetFileInfo(qfile);
                tvarcount = 0;
                menu_count = 0;
                finfo = fvar->dl->u.file;
                qvars = finfo->var_names;
                NgSortQuarkList(qvars,finfo->n_vars,False);
        }
        vmenus->qfile = qfile;
#if	DEBUG_VAR_MENUS & DEBUG_MENUS
        fprintf(stderr,"tvarcount %d menu_count %d n_vars %d\n",
               tvarcount,menu_count,finfo->n_vars);
#endif
        
        for (i = 0; i < menu_count; i++)
                fvar = fvar->next;
        if (fvar->in_use > 0)
                XtUnmanageChildren(fvar->vbuttons,fvar->in_use);

        count = MIN(MAX_MENU_ENTRIES,finfo->n_vars - tvarcount);
        if (finfo->n_vars - tvarcount > MAX_MENU_ENTRIES)
                cont = True;
        tcount = cont ? count + 1 : count;
        
        if (tcount > fvar->alloced) {
                fvar->vbuttons = NhlRealloc(fvar->vbuttons,
                                            tcount * sizeof(Widget));
                for (i = fvar->alloced; i < count; i++) {
                        fvar->vbuttons[i] = XtVaCreateWidget
                                ("vbutton",xmCascadeButtonWidgetClass,
                                 fvar->submenu,NULL);
                        XtAddCallback
                                (fvar->vbuttons[i],
                                 XmNactivateCallback,
                                 vp->filevar_cb,vp->udata);
                }
                fvar->alloced = tcount;
                if (cont) {
                        xmname = NgXAppCreateXmString
                                (vp->appmgr,"More ...");
                        fvar->vbuttons[count] = XtVaCreateWidget
                                ("vbutton",xmCascadeButtonWidgetClass,
                                 fvar->submenu,
                                 XmNlabelString,xmname,
                                 NULL);
                        NgXAppFreeXmString(vp->appmgr,xmname);
                        XtAddCallback(fvar->vbuttons[count],
                                      XmNcascadingCallback,FileVarMenuCB,
                                      vmenus);
                        XtAddEventHandler(fvar->vbuttons[count],
                                          ButtonPressMask,False,
                                          FileVarMenuEH,vmenus);
                }
        }
        
        for (i = 0; i < count; i++) {
                xmname = NgXAppCreateXmString
                        (vp->appmgr,
                         NrmQuarkToString(qvars[tvarcount+ i]));
                XtVaSetValues(fvar->vbuttons[i],
                              XmNlabelString,xmname,
			      XmNuserData,qvars[tvarcount+ i],
                              NULL);
                NgXAppFreeXmString(vp->appmgr,xmname);
        }
        if (cont) {
                if (! fvar->next) {
                        NgFileVarRec* prev = fvar;
                        
                        fvar->next = NhlMalloc(sizeof(NgFileVarRec));
                        fvar = fvar->next;
                        fvar->next = NULL;
                        fvar->override_sh = XtVaCreatePopupShell
                                ("override_sh",xmMenuShellWidgetClass,
                                 prev->submenu,
                                 XmNwidth,		5,
                                 XmNheight,		5,
                                 XmNallowShellResize,	True,
                                 XtNoverrideRedirect,	True,
                                 NULL);
                        cont_submenu = fvar->submenu = XtVaCreateWidget
                                ("FileVars",xmRowColumnWidgetClass,
                                 fvar->override_sh,
                                 XmNrowColumnType,XmMENU_PULLDOWN,
                                 NULL);
                        fvar->qfile = prev->qfile;
                        fvar->vbuttons = NULL;
                        fvar->dl = prev->dl;
                        fvar->alloced = 0;
                        fvar->in_use = 0;
                        fvar = prev;
                        XtVaSetValues(fvar->vbuttons[count],
                                      XmNsubMenuId,cont_submenu,
                                      NULL);
                }
                XtVaSetValues(fvar->vbuttons[count],
                              XmNuserData,NrmNULLQUARK,
                              NULL);
                menu_count++;
        }
        XtManageChildren(fvar->vbuttons,tcount);
        fvar->in_use = tcount;
        fvar->qfile = qfile;
        tvarcount += count;

        return;
}

static void VarMenuCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
        NgVarMenus	vmenus = (NgVarMenus) udata;
	VarMenusRec	*vp = (VarMenusRec *)vmenus->vmenu_data;
	NgVarRec	*vrec;
        int		i,count;
        NgFileVarRec	*fvar;

#if	DEBUG_VAR_MENUS & DEBUG_ENTRY
	fprintf(stderr,"VarMenuCB(IN)\n");
#endif

	XtVaGetValues(w,
		      XmNuserData,&vrec,
		      NULL);

	if (! vrec->modified)
		return;
        
        if (vrec->in_use > 0) {
                NclFree(vrec->qvars);
                XtUnmanageChildren(vrec->vbuttons,vrec->in_use);
        }
	switch (vrec->vtype) {
	case _brREGULAR:
		vrec->qvars = NclGetVarSymNames(&count);
		break;
	case _brFILEREF:
	case _brFILEVAR:
		vrec->qvars = NclGetFileSymNames(&count);
		break;

	}
        NgSortQuarkList(vrec->qvars,count,False);
        
        if (count > vrec->alloced) {
                vrec->vbuttons = NhlRealloc(vrec->vbuttons,
                                            count * sizeof(Widget));
                switch (vrec->vtype) {
                case _brREGULAR:
                        for (i = vrec->alloced; i < count; i++) {
                                vrec->vbuttons[i] = XtVaCreateWidget
                                        ("vbutton",xmCascadeButtonGadgetClass,
                                         vrec->menu,NULL);
                                XtAddCallback
                                        (vrec->vbuttons[i],
                                         XmNactivateCallback,vp->regvar_cb,
                                         vp->udata);
                        }
                        break;
                case _brFILEREF:
                        for (i = vrec->alloced; i < count; i++) {
                                vrec->vbuttons[i] = XtVaCreateWidget
                                        ("vbutton",xmCascadeButtonGadgetClass,
                                         vrec->menu,NULL);
                                XtAddCallback
                                        (vrec->vbuttons[i],
                                         XmNactivateCallback,vp->fileref_cb,
                                         vp->udata);
                        }
                        break;
                case _brFILEVAR:
                        fvar = (NgFileVarRec *)vrec->priv;
                        if (!fvar->override_sh) {
                                fvar->override_sh = XtVaCreatePopupShell
                                        ("override_sh",xmMenuShellWidgetClass,
                                         vrec->menu,
                                         XmNwidth,		5,
                                         XmNheight,		5,
                                         XmNallowShellResize,	True,
                                         XtNoverrideRedirect,	True,
                                         NULL);
                                fvar->submenu = XtVaCreateWidget
                                        ("FileVars",xmRowColumnWidgetClass,
                                         fvar->override_sh,
                                         XmNrowColumnType,XmMENU_PULLDOWN,
                                         NULL);
                        }
                        for (i = vrec->alloced; i < count; i++) {
                                vrec->vbuttons[i] = XtVaCreateWidget
                                        ("vbutton",xmCascadeButtonWidgetClass,
                                         vrec->menu,
                                         XmNsubMenuId,fvar->submenu,
                                         NULL);
                                XtAddCallback
                                        (vrec->vbuttons[i],
                                         XmNcascadingCallback,
                                         FileVarMenuCB,
                                         vmenus);
                                XtAddEventHandler(vrec->vbuttons[i],
                                                  ButtonPressMask,
                                                  False,
                                                  FileVarMenuEH,vmenus);
                        }
                        break;
                }
                vrec->alloced = count;
        }
        for (i = 0; i < count; i++) {
                XmString xmname;
                xmname = NgXAppCreateXmString
                        (vp->appmgr,NrmQuarkToString(vrec->qvars[i]));
                
                XtVaSetValues(vrec->vbuttons[i],
                              XmNlabelString,xmname,
			      XmNuserData,vrec->qvars[i],
                              NULL);
                NgXAppFreeXmString(vp->appmgr,xmname);
        }
        XtManageChildren(vrec->vbuttons,count);
        vrec->in_use = count;
	vrec->modified = False;

        return;
        
}

NgVarMenus
NgCreateVarMenus
(
        int		appmgr,
        int		nsid,
        Widget		parent,
        XtCallbackProc	regvar_cb,
        XtCallbackProc	fileref_cb,
        XtCallbackProc	filevar_cb,
        XtPointer	udata
)
{
	VarMenusRec	*vp;
        NgVarMenus	vmenus;
        Widget		menush;
        XmString	xmstring;
	NhlArgVal	sel,user_data;
        NrmQuark	*qvars;
        int		count;
        NgFileVarRec	*fvar;
	NhlLayer	ncl = _NhlGetLayer(nsid);

        vp = NhlMalloc(sizeof(VarMenusRec));
        vmenus = NhlMalloc(sizeof(NgVarMenusRec));
        vmenus->vmenu_data = (NhlPointer)vp;
        vmenus->qfile = NrmNULLQUARK;
        vp->appmgr = appmgr;
        vp->nsid = nsid;
        vp->regvar_cb = regvar_cb;
        vp->fileref_cb = fileref_cb;
        vp->filevar_cb = filevar_cb;
        vp->udata = udata;

        vp->regvars.varcount = 0;
        vp->regvars.alloced = 0;
        vp->regvars.in_use = 0;
        vp->regvars.vbuttons = NULL;
        vp->regvars.modified = True;
        vp->regvars.vtype = _brREGULAR;
        vp->regvars.priv = NULL;
        
        vp->filerefs.varcount = 0;
        vp->filerefs.alloced = 0;
        vp->filerefs.in_use = 0;
        vp->filerefs.vbuttons = NULL;
        vp->filerefs.modified = True;
        vp->filerefs.vtype = _brFILEREF;
        vp->filevars.priv = NULL;

        vp->filevars.varcount = 0;
        vp->filevars.alloced = 0;
        vp->filevars.in_use = 0;
        vp->filevars.vbuttons = NULL;
        vp->filevars.modified = True;
        vp->filevars.vtype = _brFILEVAR;

        fvar = NhlMalloc(sizeof(NgFileVarRec));
        fvar->next = NULL;
        fvar->override_sh = NULL;
        fvar->submenu = NULL;
        fvar->qfile = NrmNULLQUARK;
        fvar->vbuttons = NULL;
        fvar->alloced = 0;
        fvar->in_use = 0;
        
        vp->filevars.priv = (NhlPointer) fvar;
        
	menush = XtVaCreatePopupShell
                ("override_sh",xmMenuShellWidgetClass,
                 parent,
                 XmNwidth,		5,
                 XmNheight,		5,
                 XmNallowShellResize,	True,
                 XtNoverrideRedirect,	True,
                 NULL);

        vp->regvars.menu = XtVaCreateWidget
                ("Reg Vars",xmRowColumnWidgetClass,menush,
                 XmNrowColumnType,	XmMENU_PULLDOWN,
		 XmNuserData,	&vp->regvars,
                 NULL);
	XtAddCallback(vp->regvars.menu,
		      XmNmapCallback,VarMenuCB,vmenus);

	vmenus->regvars_mbutton = vp->regvars.mbutton =
                XtVaCreateManagedWidget
                ("Regular Vars",xmCascadeButtonGadgetClass,
                 parent,
                 XmNsubMenuId,	vp->regvars.menu,
                 NULL);

        vp->filerefs.menu = XtVaCreateWidget
                ("Files",xmRowColumnWidgetClass,menush,
                 XmNrowColumnType,	XmMENU_PULLDOWN,
		 XmNuserData,	&vp->filerefs,
                 NULL);
	XtAddCallback(vp->filerefs.menu,
		      XmNmapCallback,VarMenuCB,vmenus);

	vmenus->filerefs_mbutton = vp->filerefs.mbutton =
                XtVaCreateManagedWidget
                ("Files",xmCascadeButtonGadgetClass,
                 parent,
                 XmNsubMenuId,	vp->filerefs.menu,
                 NULL);
        
        vp->filevars.menu = XtVaCreateWidget
                ("File Vars",xmRowColumnWidgetClass,menush,
                 XmNrowColumnType,	XmMENU_PULLDOWN,
		 XmNuserData,	&vp->filevars,
                 NULL);
	XtAddCallback(vp->filevars.menu,
		      XmNmapCallback,VarMenuCB,vmenus);

	vmenus->filevars_mbutton = vp->filevars.mbutton =
                XtVaCreateManagedWidget
                ("File Vars",xmCascadeButtonGadgetClass,
                 parent,
                 XmNsubMenuId,	vp->filevars.menu,
                 NULL);
        
	XtManageChild(vp->regvars.menu);
	XtManageChild(vp->filerefs.menu);
	XtManageChild(vp->filevars.menu);

        qvars = NclGetVarSymNames(&count);
        vp->regvars.varcount = count;
	XtSetSensitive(vp->regvars.mbutton,count > 0);
        NclFree(qvars);
        
        qvars = NclGetFileSymNames(&count);
        vp->filerefs.varcount = count;
        vp->filevars.varcount = count;
	XtSetSensitive(vp->filerefs.mbutton,count > 0);
	XtSetSensitive(vp->filevars.mbutton,count > 0);
        NclFree(qvars);

	NhlINITVAR(sel);
	NhlINITVAR(user_data);
        
	user_data.ptrval = &vp->regvars;
	sel.lngval = NgNclCBCREATE_VAR;
	_NhlAddObjCallback(ncl,NgCBnsObject,sel,CreateCB,user_data);
	sel.lngval = NgNclCBDELETE_VAR;
	_NhlAddObjCallback(ncl,NgCBnsObject,sel,DeleteCB,user_data);
        
	user_data.ptrval = &vp->filerefs;
	sel.lngval = NgNclCBCREATE_FILEVAR;
	_NhlAddObjCallback(ncl,NgCBnsObject,sel,CreateCB,user_data);
	sel.lngval = NgNclCBDELETE_FILEVAR;
	_NhlAddObjCallback(ncl,NgCBnsObject,sel,DeleteCB,user_data);
        
	user_data.ptrval = &vp->filevars;
	sel.lngval = NgNclCBCREATE_FILEVAR;
	_NhlAddObjCallback(ncl,NgCBnsObject,sel,CreateCB,user_data);
	sel.lngval = NgNclCBDELETE_FILEVAR;
	_NhlAddObjCallback(ncl,NgCBnsObject,sel,DeleteCB,user_data);

        return vmenus;
        
}
