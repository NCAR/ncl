#ifndef HLUSupport_h
#define HLUSupport_h

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

#endif /* HLUSupport_h */
