
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include "defs.h"
#include "Symbol.h"
#include "NclHLUObj.h"
#include "HLUSupport.h"



NhlErrorTypes _NclDelHLUChild
#if __STDC__
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
#if __STDC__
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


