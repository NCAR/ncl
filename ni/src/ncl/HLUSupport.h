#ifndef HLUSupport_h
#define HLUSupport_h

typedef struct _NclHLUUData {
	NclQuark vq;
	NclQuark aq;
	int level;
}NclHLUUData;
typedef struct _NclHLUCbData {
	int prev_id;
	int ncl_id;
	int off;
	int kind; /* 0 = Assign normal, 1 = Assign missing */
}NclHLUCbData;
typedef struct _NclHLURefList {
	NclQuark vq;
	NclQuark aq;
	int level;
	int n_refs;
	int refs_size;
	int *refs;
}NclHLURefList;

typedef struct _NclHLULookUpTableNode {
	int	hlu_id;
	int	ncl_hlu_id;
	int	n_entries;
	int ref_list_size;
	struct  _NclHLURefList *ref_list;
	struct  _NclHLULookUpTableNode *next;
}NclHLULookUpTable;

#define REF_LIST_SIZE 20

extern NhlErrorTypes _NclAddHLUToExpList(
#if NhlNeedProto
NclHLUObj /*ptmp*/,
int /*nclhlu_id*/
#endif
);

extern NhlErrorTypes _NclDelHLUChild(
#if NhlNeedProto
NclHLUObj /*ptmp*/,
int /*nclhlu_id*/
#endif
);

extern NhlErrorTypes _NclAddHLUChild(
#if NhlNeedProto
NclHLUObj /*ptmp*/,
int /*nclhlu_id*/
#endif
);

extern NhlErrorTypes _NclAddHLURef(
#if NhlNeedProto
int 	/*ncl_id*/,
NclQuark /*vq*/,
NclQuark /*aq*/,
int	/*off*/,
int	/*level*/
#endif
);

extern NhlErrorTypes _NclDelHLURef(
#if NhlNeedProto
int 	/*id*/,
NclQuark /*vq*/,
NclQuark /*aq*/,
int	/*off*/,
int	/*level*/
#endif
);

extern NhlErrorTypes _NclRemoveAllRefs(
#if NhlNeedProto
int 	/*id*/
#endif
);

extern NclHLUObj _NclLookUpHLU(
#if NhlNeedProto
int 	/*id*/
#endif
);

extern NclHLULookUpTable * _NclGetHLURefInfo(
#if	NhlNeedProto
int /*id*/
#endif
);

#endif /* HLUSupport_h */

