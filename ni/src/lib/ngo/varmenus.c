/*
 *      $Id: varmenus.c,v 1.9 1998-09-18 23:47:40 boote Exp $
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
#include <ncarg/ngo/CascadeBG.h>
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
        NgNclAny	sym = (NgNclAny) cbdata.ptrval;
                
#if	DEBUG_VAR_MENUS & DEBUG_ENTRY
	fprintf(stderr,"CreateCB - symbol: %s\n",sym->name);
#endif
        if (! strncmp(sym->name,"_Ng",3))
                return;
        if (! vrec->varcount)
              XtSetSensitive(vrec->mbutton,True);  
        vrec->varcount++;
	vrec->modified = True;

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
        NgNclAny	sym = (NgNclAny) cbdata.ptrval;
                
#if	DEBUG_VAR_MENUS & DEBUG_ENTRY
	fprintf(stderr,"DeleteCB - symbol: %s\n",sym->name);
#endif
        if (! strncmp(sym->name,"_Ng",3))
                return;
        vrec->varcount--;
	vrec->modified = True;

        if (! vrec->varcount)
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
	fprintf(stderr,"%s filevars\n", NrmQuarkToString(qfile));
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
                                ("vbutton",xmCascadeButtonGadgetClass,
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
                                ("vbutton",xmCascadeButtonGadgetClass,
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
			XmNdepth,		XcbGetDepth(vp->go->go.xcb),
			XmNcolormap,		XcbGetColormap(vp->go->go.xcb),
			XmNvisual,		XcbGetVisual(vp->go->go.xcb),
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
        int		i,count,used;
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
	case _vmHLUVAR:
		vrec->qvars = NclGetHLUVarSymNames(&count);
		break;
	case _vmREGULAR:
		vrec->qvars = NclGetVarSymNames(&count);
		break;
	case _vmFILEREF:
	case _vmFILEVAR:
		vrec->qvars = NclGetFileSymNames(&count);
		break;
	}
#if DEBUG_VAR_MENUS        
	fprintf(stderr,"symbol count %d\n",count);
#endif        
        NgSortQuarkList(vrec->qvars,count,False);
        
        if (count > vrec->alloced) {
                vrec->vbuttons = NhlRealloc(vrec->vbuttons,
                                            count * sizeof(Widget));
                switch (vrec->vtype) {
                case _vmHLUVAR:
                        for (i = vrec->alloced; i < count; i++) {
                                vrec->vbuttons[i] = XtVaCreateWidget
                                        ("vbutton",xmCascadeButtonGadgetClass,
                                         vrec->menu,NULL);
                                XtAddCallback
                                        (vrec->vbuttons[i],
                                         XmNactivateCallback,vp->hluvar_cb,
                                         vp->udata);
                        }
                        break;
                case _vmREGULAR:
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
                case _vmFILEREF:
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
                case _vmFILEVAR:
                        fvar = (NgFileVarRec *)vrec->priv;
                        if (!fvar->override_sh) {
                                fvar->override_sh = XtVaCreatePopupShell
                                        ("override_sh",xmMenuShellWidgetClass,
                                        vrec->menu,
                                        XmNwidth,		5,
                                        XmNheight,		5,
                                        XmNallowShellResize,	True,
                                        XtNoverrideRedirect,	True,
					XmNdepth,XcbGetDepth(vp->go->go.xcb),
					XmNcolormap,
						XcbGetColormap(vp->go->go.xcb),
					XmNvisual,XcbGetVisual(vp->go->go.xcb),
                                        NULL);
                                fvar->submenu = XtVaCreateWidget
                                        ("FileVars",xmRowColumnWidgetClass,
                                         fvar->override_sh,
                                         XmNrowColumnType,XmMENU_PULLDOWN,
                                         NULL);
                        }
                        for (i = vrec->alloced; i < count; i++) {
                                vrec->vbuttons[i] = XtVaCreateWidget
                                        ("vbutton",xmCascadeButtonGadgetClass,
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
        used = 0;
        for (i = 0; i < count; i++) {
                XmString xmname;
                char *name = NrmQuarkToString(vrec->qvars[i]);

#if DEBUG_VAR_MENUS        
		fprintf(stderr,"symbol name %s\n",name);
#endif        
                if (strncmp(name,"_Ng",3) == 0)
                        continue;
                
                xmname = NgXAppCreateXmString(vp->appmgr,name);
                
                XtVaSetValues(vrec->vbuttons[used],
                              XmNlabelString,xmname,
			      XmNuserData,vrec->qvars[i],
                              NULL);
                NgXAppFreeXmString(vp->appmgr,xmname);
                used++;
        }
        XtManageChildren(vrec->vbuttons,used);
        vrec->in_use = used;
	vrec->modified = False;

        return;
        
}

static void InitVarRec
(
        NgVarRec 	*vrec,
        NgvmVarType     vtype
        )
{
        vrec->vtype = vtype;
        vrec->menu = NULL;
        vrec->mbutton = NULL;
        vrec->varcount = 0;
        vrec->vbuttons = NULL;
        vrec->qvars = NULL;
        vrec->alloced = 0;
        vrec->in_use = 0;
        vrec->modified = True;
        vrec->priv = NULL;
        vrec->create_cb = NULL;
        vrec->delete_cb = NULL;
        return;
}

static void
NSDestroyCB
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	VarMenusRec	*vp = (VarMenusRec*)udata.ptrval;
        NgVarRec	*vrec;
        int i;
        
        for (i = 0; i < 4; i++) {
                switch (i) {
                    case 0:
                            vrec = &vp->hluvars;
                            break;
                    case 1:
                            vrec = &vp->regvars;
                            break;
                    case 2:
                            vrec = &vp->filerefs;
                            break;
                    case 3:
                            vrec = &vp->filevars;
                            break;
                }
                if (vrec->create_cb){
                        _NhlCBDelete(vrec->create_cb);
			vrec->create_cb = NULL;
		}
                if (vrec->delete_cb){
                        _NhlCBDelete(vrec->delete_cb);
			vrec->delete_cb = NULL;
		}
	}

	_NhlCBDelete(vp->nsdestroycb);
	vp->nsdestroycb = NULL;

        return;
}

NgVarMenus
NgCreateVarMenus
(
        Widget		parent,
        XtCallbackProc	hluvar_cb,
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
	NhlLayer	ncl;
	int		goid = NgGOWidgetToGoId(parent);
        NgGO        	go = (NgGO)_NhlGetLayer(goid);
        
        if (!go)
                return NULL;
        
	NhlINITVAR(sel);
	NhlINITVAR(user_data);

        vp = NhlMalloc(sizeof(VarMenusRec));
        vmenus = NhlMalloc(sizeof(NgVarMenusRec));
        vmenus->vmenu_data = (NhlPointer)vp;
        vmenus->qfile = NrmNULLQUARK;
	vp->go = go;
        vp->appmgr = go->go.appmgr;
        vp->nsid = go->go.nclstate;
        ncl = _NhlGetLayer(vp->nsid);

	user_data.ptrval = vp;
	vp->nsdestroycb = _NhlAddObjCallback(ncl,_NhlCBobjDestroy,sel,
							NSDestroyCB,user_data);
        vp->hluvar_cb = hluvar_cb;
        vp->regvar_cb = regvar_cb;
        vp->fileref_cb = fileref_cb;
        vp->filevar_cb = filevar_cb;
        vp->udata = udata;

        InitVarRec(&vp->hluvars,_vmHLUVAR);
        InitVarRec(&vp->regvars,_vmREGULAR);
        InitVarRec(&vp->filerefs,_vmFILEREF);
        InitVarRec(&vp->filevars,_vmFILEVAR);

        fvar = NhlMalloc(sizeof(NgFileVarRec));
        fvar->next = NULL;
        fvar->override_sh = NULL;
        fvar->submenu = NULL;
        fvar->qfile = NrmNULLQUARK;
        fvar->vbuttons = NULL;
        fvar->alloced = 0;
        fvar->in_use = 0;
        
        vp->filevars.priv = (NhlPointer) fvar;
        
	menush = XtVaCreatePopupShell("override_sh",xmMenuShellWidgetClass,
						                 parent,
		XmNwidth,		5,
		XmNheight,		5,
		XmNallowShellResize,	True,
		XtNoverrideRedirect,	True,
		XmNdepth,		XcbGetDepth(vp->go->go.xcb),
		XmNcolormap,		XcbGetColormap(vp->go->go.xcb),
		XmNvisual,		XcbGetVisual(vp->go->go.xcb),
		NULL);

        if (vp->hluvar_cb) {
                vp->hluvars.menu = XtVaCreateWidget
                        ("Hlu Vars",xmRowColumnWidgetClass,menush,
                         XmNrowColumnType,	XmMENU_PULLDOWN,
                         XmNuserData,	&vp->hluvars,
                         NULL);
                XtAddCallback(vp->hluvars.menu,
                              XmNmapCallback,VarMenuCB,vmenus);

                vmenus->hluvars_mbutton = vp->hluvars.mbutton =
                        XtVaCreateManagedWidget
                        ("Hlu Vars",xmCascadeButtonGadgetClass,
                         parent,
                         XmNsubMenuId,	vp->hluvars.menu,
                         NULL);

                qvars = NclGetHLUVarSymNames(&count);
                vp->hluvars.varcount = count;
                XtSetSensitive(vp->hluvars.mbutton,count > 0);
                NclFree(qvars);

                user_data.ptrval = &vp->hluvars;
                sel.lngval = NgNclCBCREATE_HLUVAR;
                vp->hluvars.create_cb = _NhlAddObjCallback
                        (ncl,NgCBnsObject,sel,CreateCB,user_data);
                sel.lngval = NgNclCBDELETE_HLUVAR;
                vp->hluvars.delete_cb = _NhlAddObjCallback
                        (ncl,NgCBnsObject,sel,DeleteCB,user_data);
        }

        if (vp->regvar_cb) {
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
                qvars = NclGetVarSymNames(&count);
                vp->regvars.varcount = count;
                XtSetSensitive(vp->regvars.mbutton,count > 0);
                NclFree(qvars);
        
                user_data.ptrval = &vp->regvars;
                sel.lngval = NgNclCBCREATE_VAR;
                vp->regvars.create_cb = _NhlAddObjCallback
                        (ncl,NgCBnsObject,sel,CreateCB,user_data);
                sel.lngval = NgNclCBDELETE_VAR;
                vp->regvars.delete_cb = _NhlAddObjCallback
                        (ncl,NgCBnsObject,sel,DeleteCB,user_data);
        }

        qvars = NULL;
        if (vp->fileref_cb) {
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

                qvars = NclGetFileSymNames(&count);
                vp->filerefs.varcount = count;
                XtSetSensitive(vp->filerefs.mbutton,count > 0);
        
                user_data.ptrval = &vp->filerefs;
                sel.lngval = NgNclCBCREATE_FILEVAR;
                vp->filerefs.create_cb = _NhlAddObjCallback
                        (ncl,NgCBnsObject,sel,CreateCB,user_data);
                sel.lngval = NgNclCBDELETE_FILEVAR;
                vp->filerefs.delete_cb = _NhlAddObjCallback
                        (ncl,NgCBnsObject,sel,DeleteCB,user_data);
        }
        
        if (vp->filevar_cb) {
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
        
                if (!qvars)
                        qvars = NclGetFileSymNames(&count);
                vp->filevars.varcount = count;
                XtSetSensitive(vp->filevars.mbutton,count > 0);
                user_data.ptrval = &vp->filevars;
                sel.lngval = NgNclCBCREATE_FILEVAR;
                vp->filevars.create_cb = _NhlAddObjCallback
                        (ncl,NgCBnsObject,sel,CreateCB,user_data);
                sel.lngval = NgNclCBDELETE_FILEVAR;
                vp->filevars.delete_cb = _NhlAddObjCallback
                        (ncl,NgCBnsObject,sel,DeleteCB,user_data);
        }
        if (qvars)
                NclFree(qvars);

        return vmenus;
        
}

void NgDestroyVarMenus
(
        NgVarMenus vmenus
        )
{
	VarMenusRec	*vp = (VarMenusRec *)vmenus->vmenu_data;
        NgVarRec	*vrec;
        NgFileVarRec	*fvar;
        int i;
        
        for (i = 0; i < 4; i++) {
                fvar = NULL;
                switch (i) {
                    case 0:
                            vrec = &vp->hluvars;
                            break;
                    case 1:
                            vrec = &vp->regvars;
                            break;
                    case 2:
                            vrec = &vp->filerefs;
                            break;
                    case 3:
                            vrec = &vp->filevars;
                            fvar = (NgFileVarRec *)vrec->priv;
                            break;
                }
                if (vrec->create_cb)
                        _NhlCBDelete(vrec->create_cb);
                if (vrec->delete_cb)
                        _NhlCBDelete(vrec->delete_cb);
                if (fvar) {
                        NgFileVarRec *ftmp;
                        while (fvar) {
                                ftmp = fvar;
                                fvar = fvar->next;
                                NhlFree(ftmp);
                        }
                }
        }
	_NhlCBDelete(vp->nsdestroycb);
        NhlFree(vp);
        NhlFree(vmenus);
                            
        return;
}
