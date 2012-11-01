
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/Callbacks.h>
#include "defs.h"
#include "Symbol.h"
#include "NclHLUObj.h"
#include "HLUSupport.h"

static NclHLULookUpTable hlu_tab[NCL_SYM_TAB_SIZE];


extern NclHLULookUpTable *_NclGetHLURefInfo
#if     NhlNeedProto
(int id)
#else
(id)
int id;
#endif
{
        int index;
        NclHLULookUpTable *tmp = NULL;
        index = id % NCL_SYM_TAB_SIZE;
        tmp = &hlu_tab[index];
        while(tmp != NULL) {
                if(tmp->hlu_id == id) {
                        return(tmp);
                } else {
                        tmp = tmp->next;
                }
        }
        return(NULL);
}



NhlErrorTypes _NclAddHLURef
#if NhlNeedProto
(int ncl_id, NclQuark vq, NclQuark aq, int off,int level)
#else
(ncl_id, vq, aq, off,level)
int ncl_id;
NclQuark vq;
NclQuark aq;
int off;
int level;
#endif
{
	static int first = 1;
	int i,j = -1,k,itmp,ktmp;
	int index;
	NclHLULookUpTable *tmp,*prev;
	NclHLUObj tmp_ho;
	int id;

	if (ncl_id < 0) {
		/* missing or undefined */
		return NhlNOERROR;
	}

	if(first) {
		first = 0;
		for(i = 0 ; i < NCL_SYM_TAB_SIZE; i++) {
			hlu_tab[i].hlu_id = -1;
			hlu_tab[i].ncl_hlu_id = -1;
			hlu_tab[i].n_entries = 0;
			hlu_tab[i].ref_list = NULL;
			hlu_tab[i].next = NULL;
		}
	}
	tmp_ho = (NclHLUObj)_NclGetObj(ncl_id);
	if(tmp_ho != NULL) {
		id = tmp_ho->hlu.hlu_id;
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclAddHLURef: internal error: ncl object %d does not exist for vname %s",ncl_id,NrmQuarkToString(vq));
		return(NhlFATAL);
	}
	index = id % NCL_SYM_TAB_SIZE;
	

	if(hlu_tab[index].hlu_id != -1){
		prev = &hlu_tab[index];
		tmp = &hlu_tab[index];
		while(tmp != NULL) {
			if(tmp->hlu_id == id) {
				if (tmp->ncl_hlu_id != ncl_id) {
					NclObj obj;
					obj = _NclGetObj(tmp->ncl_hlu_id);
					if (obj) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclAddHLURef: internal error: NCL object %d exists but is different from %d, both have hlu object id %d, vname %s",
							  tmp->ncl_hlu_id,ncl_id,tmp->hlu_id,NrmQuarkToString(vq));
						return NhlFATAL;
					}
					else {
						/* this means that an hlu object was deleted but it did not get removed from the list */
						for(i = 0; i < tmp->n_entries; i++) {
							NclFree(tmp->ref_list[i].refs);
						}
						NclFree(tmp->ref_list);
						tmp->n_entries = 0;
						tmp->ref_list = NULL;
						tmp->ncl_hlu_id = ncl_id;
					}
				}
				for(i = 0; i < tmp->n_entries; i++) {
					if((tmp->ref_list[i].vq == vq)&&(tmp->ref_list[i].aq == aq)&&(tmp->ref_list[i].level == level)) {
						for(j = 0; j < tmp->ref_list[i].n_refs; j++ ) {
							if(tmp->ref_list[i].refs[j] == off) {
								return(NhlNOERROR);
							} else if(tmp->ref_list[i].refs[j] > off) {
								itmp = tmp->ref_list[i].refs[j];
								tmp->ref_list[i].refs[j] = off;
								for(k = j; k < tmp->ref_list[i].n_refs + 1;k++) {
									ktmp = tmp->ref_list[i].refs[k];
									tmp->ref_list[i].refs[k] = itmp;
									itmp = ktmp;
								}
								tmp->ref_list[i].n_refs++;
								if(tmp->ref_list[i].n_refs == tmp->ref_list[i].refs_size) {
									tmp->ref_list[i].refs_size *= 2;
									tmp->ref_list[i].refs = NclRealloc(tmp->ref_list[i].refs,tmp->ref_list[i].refs_size*sizeof(char*));

								} 
								return(NhlNOERROR);
							}
						}
						tmp->ref_list[i].refs[j] = off;
						tmp->ref_list[i].n_refs++;
						if(tmp->ref_list[i].n_refs == tmp->ref_list[i].refs_size) {
                                                         tmp->ref_list[i].refs_size *= 2;
                                                         tmp->ref_list[i].refs = NclRealloc(tmp->ref_list[i].refs,tmp->ref_list[i].refs_size*sizeof(char*));
                                                }
                                                return(NhlNOERROR);
					}
				}
				if(tmp->ref_list == NULL) {
					tmp->ref_list =  NclMalloc(sizeof(NclHLURefList)*REF_LIST_SIZE);
					tmp->ref_list_size = REF_LIST_SIZE;

				}
				if(i == tmp->n_entries) {
					tmp->ref_list[i].vq = vq;
					tmp->ref_list[i].aq = aq;
					tmp->ref_list[i].level = level;
					tmp->ref_list[i].n_refs = 1;
					tmp->ref_list[i].refs_size = REF_LIST_SIZE;
					tmp->ref_list[i].refs = NclMalloc(sizeof(int)*REF_LIST_SIZE);
					tmp->ref_list[i].refs[0] = off;
					tmp->n_entries++;
					if(i == tmp->ref_list_size) {
						tmp->ref_list_size *= 2;
						tmp->ref_list = NclRealloc(tmp->ref_list,tmp->ref_list_size*sizeof(char*));
					}
					return(NhlNOERROR);
 
				}
			} else {
				prev = tmp;
				tmp = tmp->next;
			}
		}
		if((tmp == NULL)&&(prev->next == NULL)) {
			prev->next = NclMalloc(sizeof(NclHLULookUpTable));
			prev->next->hlu_id = id;	
			prev->next->ncl_hlu_id = ncl_id;
			prev->next->n_entries = 1;
			prev->next->ref_list_size = REF_LIST_SIZE;
			prev->next->ref_list =  NclMalloc(sizeof(NclHLURefList)*REF_LIST_SIZE);
			prev->next->ref_list[0].vq = vq;
			prev->next->ref_list[0].aq = aq;
			prev->next->ref_list[0].level = level;
			prev->next->ref_list[0].n_refs = 1;
			prev->next->ref_list[0].refs_size = REF_LIST_SIZE;
			prev->next->ref_list[0].refs = NclMalloc(sizeof(int)*REF_LIST_SIZE);
			prev->next->ref_list[0].refs[0] = off;
			prev->next->next =  NULL;
			/*printf("%d  hlu id %d  ncl id added: %s %s\n", id, ncl_id,NhlClassName(id),NhlName(id));*/
			return(NhlNOERROR);
		}
	} else {
		hlu_tab[index].hlu_id = id;
		hlu_tab[index].ncl_hlu_id = ncl_id;
		hlu_tab[index].n_entries = 1;
		hlu_tab[index].ref_list_size = REF_LIST_SIZE;
		hlu_tab[index].ref_list = NclMalloc(sizeof(NclHLURefList)*REF_LIST_SIZE);
		hlu_tab[index].ref_list[0].vq = vq;
		hlu_tab[index].ref_list[0].aq = aq;
		hlu_tab[index].ref_list[0].level =level;
		hlu_tab[index].ref_list[0].n_refs = 1;
		hlu_tab[index].ref_list[0].refs_size = REF_LIST_SIZE;
		hlu_tab[index].ref_list[0].refs = NclMalloc(sizeof(int)*REF_LIST_SIZE);
		hlu_tab[index].ref_list[0].refs[0] = off;
		hlu_tab[index].next = NULL;
		/*printf("%d  hlu id %d  ncl id added: %s %s\n", id, ncl_id,NhlClassName(id),NhlName(id));*/
		return(NhlNOERROR);
	}
	NHLPERROR((NhlFATAL,NhlEUNKNOWN,"internal error"));
	return(NhlFATAL);
}
NclHLUObj _NclLookUpHLU 
#if NhlNeedProto
(int id)
#else
(id)
	int id;
#endif
{
	int index;
	NclHLULookUpTable *tmp = NULL;
	index = id % NCL_SYM_TAB_SIZE;
	tmp = &hlu_tab[index];
	while(tmp != NULL) {
		if(tmp->hlu_id == id) {
			return((NclHLUObj)_NclGetObj(tmp->ncl_hlu_id));
		} else {
			tmp = tmp->next;
		}
	}
	return(NULL);
}

NhlErrorTypes _NclDelHLURef
#if NhlNeedProto
(int ncl_id, NclQuark vq, NclQuark aq, int off,int level)
#else
(ncl_id, vq, aq, off)
int ncl_id;
NclQuark vq;
NclQuark aq;
int off;
int level;
#endif
{
	int i,j = -1,k;
	int index;
	NclHLULookUpTable *tmp = NULL,*prev = NULL;
	NclHLUObj tmp_ho;
	int id;

	if (ncl_id < 0) {
		/* missing or undefined */
		return NhlNOERROR;
	}

	tmp_ho = (NclHLUObj)_NclGetObj(ncl_id);
	if(tmp_ho != NULL) {
		id = tmp_ho->hlu.hlu_id;
	} else {
		/* the object has already been deleted from the NCL object table so see if the ncl_id is still present in the HLU table */
		id = -9999;
		for (i = 0; i < NCL_SYM_TAB_SIZE; i++) {
			tmp = &hlu_tab[i];
			while (tmp) {
				if (tmp->ncl_hlu_id == ncl_id) {
					id = tmp->hlu_id;
					break;
				}
				tmp = tmp->next;
			}
			if (id != -9999)
				break;
		}
		if (id == -9999)
			return(NhlNOERROR);
	}
	index = id % NCL_SYM_TAB_SIZE;
	tmp = &hlu_tab[index];
	while(tmp != NULL) {
		if(tmp->hlu_id == id) {
			/*printf("%d  hlu id %d  ncl id deleted %s %s\n", id, ncl_id,NhlClassName(id),NhlName(id));*/
			for(i = 0; i < tmp->n_entries; i++) {
				if((tmp->ref_list[i].vq == vq)&&(tmp->ref_list[i].aq == aq)) {
					for(j = 0; j < tmp->ref_list[i].n_refs; j++) {
						if(tmp->ref_list[i].refs[j] == off) {
							tmp->ref_list[i].n_refs--;
							for(k = j ; k <tmp->ref_list[i].n_refs; k++) {
								tmp->ref_list[i].refs[k] = tmp->ref_list[i].refs[k+1];
							}
							if(tmp->ref_list[i].n_refs == 0) {
								NclFree(tmp->ref_list[i].refs);
								tmp->n_entries--;
								for(k = i; k < tmp->n_entries; k++) {
									tmp->ref_list[k] = tmp->ref_list[k+1];
								}
								if(tmp->n_entries == 0) {
									if(prev != NULL) {
										prev->next = tmp->next;
										NclFree(tmp->ref_list);
										tmp->n_entries = 0;
										tmp->ref_list = NULL;

										NclFree(tmp);
									} else {
										NclFree(hlu_tab[index].ref_list);
										hlu_tab[index].hlu_id = -1;
										hlu_tab[index].ncl_hlu_id = -1;
										hlu_tab[index].n_entries = 0;
										hlu_tab[index].ref_list = NULL;
										hlu_tab[index].next = NULL;
									}
								}
							}
							return(NhlNOERROR);
						}
					}
				}
			}
			/*printf("%d  hlu id %d  ncl id delete failed\n", id, ncl_id,NhlClassName(id),NhlName(id));*/
			return(NhlFATAL);
		} else {
			prev = tmp;
			tmp = tmp->next;
		}
	}
	/*printf("%d  hlu id %d  ncl id delete failed\n", id, ncl_id,NhlClassName(id),NhlName(id));*/
	return(NhlFATAL);
}

NhlErrorTypes _NclRemoveAllRefs
#if NhlNeedProto
(int ncl_id)
#else
(ncl_id)
	int ncl_id;
#endif 
{
	int i;
	int index;
	NclHLULookUpTable *tmp = NULL,*prev = NULL;
	NclHLUObj tmp_obj;
	int id;
	
	if (ncl_id < 0) {
		/* missing or undefined */
		return NhlNOERROR;
	}

	tmp_obj = (NclHLUObj)_NclGetObj(ncl_id);	
	if(tmp_obj == NULL) {
		return(NhlFATAL);
	} else {
		id = tmp_obj->hlu.hlu_id;
	}

	index = id % NCL_SYM_TAB_SIZE;

	tmp = &hlu_tab[index];
	while(tmp != NULL) {
		if(tmp->hlu_id == id) {
			for(i = 0; i < tmp->n_entries; i++) {
				NclFree(tmp->ref_list[i].refs);
			}
			NclFree(tmp->ref_list);
			if(prev == NULL) {
				hlu_tab[index].hlu_id = -1;
				hlu_tab[index].ncl_hlu_id = -1;
				hlu_tab[index].n_entries = 0;
				hlu_tab[index].ref_list = NULL;
				hlu_tab[index].next = NULL;
			} else {
				prev->next = tmp->next;
				NclFree(tmp);
			}
			return(NhlNOERROR);
		} else {
			prev = tmp;
			tmp = tmp->next;
		}
	}
	/*printf("%d  hlu id %d  ncl id no refs found %s %s \n", id, ncl_id,NhlClassName(id),NhlName(id));*/
	return(NhlNOERROR);
}

NhlErrorTypes _NclAddHLUToExpList
#if	NhlNeedProto
(NclHLUObj ptmp,int nclhlu_id)
#else
(ptmp,nclhlu_id)
NclHLUObj ptmp;
int nclhlu_id;
#endif
{
	NclHLUObjClass oc;

	if(ptmp == NULL) {
		return(NhlFATAL);
	} else {
		oc = (NclHLUObjClass)ptmp->obj.class_ptr;
	}
	while(oc != (NclHLUObjClass)&nclObjClassRec) {
		if(oc->hlu_class.add_exp_child!= NULL) {
			return((*oc->hlu_class.add_exp_child)(ptmp,nclhlu_id));
		} else {
			oc = (NclHLUObjClass)oc->obj_class.super_class;
		}
	}
	return(NhlFATAL);
}



NhlErrorTypes _NclDelHLUChild
#if	NhlNeedProto
(NclHLUObj ptmp,int nclhlu_id)
#else
(ptmp,nclhlu_id)
NclHLUObj ptmp;
int nclhlu_id;
#endif
{
	NclHLUObjClass oc;

	if(ptmp == NULL) {
		return(NhlFATAL);
	} else {
		oc = (NclHLUObjClass)ptmp->obj.class_ptr;
	}
	while(oc != (NclHLUObjClass)&nclObjClassRec) {
		if(oc->hlu_class.del_hlu_child != NULL) {
			return((*oc->hlu_class.del_hlu_child)(ptmp,nclhlu_id));
		} else {
			oc = (NclHLUObjClass)oc->obj_class.super_class;
		}
	}
	return(NhlFATAL);
}


NhlErrorTypes _NclAddHLUChild
#if	NhlNeedProto
(NclHLUObj ptmp,int nclhlu_id)
#else
(ptmp,nclhlu_id)
NclHLUObj ptmp;
int nclhlu_id;
#endif
{
	NclHLUObjClass oc;

	if(ptmp == NULL) {
		return(NhlFATAL);
	} else {
		oc = (NclHLUObjClass)ptmp->obj.class_ptr;
	}
	while(oc != (NclHLUObjClass)&nclObjClassRec) {
		if(oc->hlu_class.add_hlu_child != NULL) {
			return((*oc->hlu_class.add_hlu_child)(ptmp,nclhlu_id));
		} else {
			oc = (NclHLUObjClass)oc->obj_class.super_class;
		}
	}
	return(NhlFATAL);
}

void _NclPrintHLURefs
#if	NhlNeedProto
(void)
#else
(void)
#endif
{
	int i,j;
        NclHLULookUpTable *tmp = NULL;

	for (i = 0; i < NCL_SYM_TAB_SIZE; i++) {
		tmp = &hlu_tab[i];
		while (tmp) {
			if (tmp->hlu_id > 0)  {
				printf("hlu_id %d, ncl_hlu_id %d, %s %s\n", tmp->hlu_id,tmp->ncl_hlu_id,NhlClassName(tmp->hlu_id),NhlName(tmp->hlu_id));
				for (j = 0; j < tmp->n_entries; j++) {
					printf("\tvname %s", NrmQuarkToString(tmp->ref_list[j].vq));
					if (tmp->ref_list[j].aq > -1)
						printf("   attname %s  level %d\n", NrmQuarkToString(tmp->ref_list[j].aq),tmp->ref_list[j].level);
					else
						printf("  level %d\n",tmp->ref_list[j].level);
				}
			}
			tmp = tmp->next;
		}
	}
}

