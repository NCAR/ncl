%{
#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/ncl/defs.h>
#include <ncarg/ncl/NclDataDefs.h>
#include "WSymbol.h"
#include "fstrings.h"
extern char *yytext;
void yyerror(const char *);
extern char* gettxt();
typedef struct wrap_src_node_list {
	void *node;
	struct wrap_src_node_list *next;
}WrapSrcListNode;

typedef struct int_list {
	int val;
	struct int_list *next;
}IntList;
extern char *wrapname;
int nargs;
int w_nargs;
extern WWrapRec *current;

WParamLoc* theparam = NULL;

WrapSrcListNode *WMakeListNode(void) 
{
	WrapSrcListNode *tmp = (WrapSrcListNode*)malloc(sizeof(WrapSrcListNode));
	tmp->next = NULL;
	tmp->node = NULL;
	return(tmp);
}

extern WCentry *WNewVDef(char* ,int , int ,char* ,int);
extern WGetArgValue *WNewArgVal();
extern WCallRec* WNewCallRec(char* ,int );
extern WSrcList* WNewAdditionalSrc(char*,int);
extern WParamLoc* NewParamLoc(NclSymbol*);

extern void DoTotal(NclSymbol*);
extern void DoDimsizes(NclSymbol*);


int FType(char *fname) {
	switch(fname[0]) {
	case 'i':
	case 'I':
	case 'j':
	case 'J':
	case 'k':
	case 'K':
	case 'l':
	case 'L':
	case 'm':
	case 'M':
	case 'n':
	case 'N':
		return(NCL_int);
	default:
		return(NCL_float);
	}
}
char *NType(int datatype) {
	switch(datatype) {
	case NCL_byte:
		return("NCL_byte");
	case NCL_char:
		return("NCL_char");
	case NCL_float:
		return("NCL_float");
	case NCL_double:
		return("NCL_double");
	case NCL_int:
		return("NCL_int");
	case NCL_short:
		return("NCL_short");
	case NCL_long: 
		return("NCL_long");
	case NCL_none:
		return("NclANY");
	case NCL_string: 
		return("NrmQuark");
	default:
		return("void");
	}
}
char *CType(int datatype) {
	switch(datatype) {
	case NCL_byte:
		return("byte");
	case NCL_char:
		return("char");
	case NCL_float:
		return("float");
	case NCL_double:
		return("double");
	case NCL_int:
		return("int");
	case NCL_short:
		return("short");
	case NCL_long: 
		return("long");
	case NCL_string: 
		return("NrmQuark");
	case NCL_none:
	default:
		return("void");
	}
}


%}
%union {
	int integer;
	char *str;
	struct _NclSymbol *sym;
	struct _DimVal *dim;
	struct _FTypeVal *ftyp;
	void *src_node;
	WParamLoc  *ploc;
	WDSpecList *dsp;
	WModList *mod;
	struct int_list *ilist;
	struct wrap_src_node_list *list;
}

%token <void> '@' '&' ':' ',' '(' ')' '*' '[' ']' '=' '/' '#' '^' EOLN ENDOFIF 
%token <integer> INT 
%token <sym> BYTE1 INTEGER2 INTEGER4 INTEGER8 REAL4 REAL8 DIMENSION DATA SAVE COMMON IMPLICIT
%token <sym> WPARAM FNAME UNDEF SUBROUTINE FUNCTION NEW IN OUT INOUT TYPE
%token <sym> NOMISSING DIMSIZES PROCEDURE MISSING RETURN REAL PRECISION
%token <sym> INTEGER FLOAT LONG DOUBLE BYTE CHARACTER STRNG 
%token <sym> SHORT LOGICAL FARG FUNC  SUBR PROC XFARG CVAR

%type <sym> subroutine_def function_def param_def declaration
%type <integer> datatype
%type <void> data_defs

%type <dim> dim
%type <ftyp> ftype otyp
%type <dsp> dspec_list vspec
%type <src_node> interface wrap_interface  
%type <src_node> len farg_map 
%type <src_node> normal_param
%type <mod> mod mods_list omods
%type <ploc> param_loc rparam
%type <list> interface_list arg_list name_list dim_list arg_dec_list
%type <list> parg_map_list farg_map_list 
%type <ilist> dim_size_list
%%

interface_list : interface ENDOFIF
	{
		WVDefList *c_vdefs;
		WArgValList *c_argval;
		WSrcList *additional_src;
		WCallRec *c_callrec;
		char buffer[BUFFSIZE];
		char upper[BUFFSIZE];
		char lower[BUFFSIZE];
		int i;


		c_callrec = current->c_callrec;
		fprintf(stdout,fdef_fmt,current->c_defstring);
		c_vdefs = current->c_vdefs;
		while(c_vdefs != NULL) {
			sprintf(buffer,"%s ",CType(c_vdefs->def->datatype));
			if(c_vdefs->def->isptr) {
				strcat(buffer,"*");
			}
			strcat(buffer,c_vdefs->def->string);
			if(c_vdefs->def->array_spec != NULL) {
				strcat(buffer,c_vdefs->def->array_spec);
			}
			fprintf(stdout,"\t%s;\n",buffer);
			c_vdefs = c_vdefs->next;
		}
		
		c_argval = current->c_argval;
		while(c_argval != NULL) {
			fprintf(stdout,argval_fmt,
				c_argval->arec->assign_to,
				CType(c_argval->arec->datatype),
				c_argval->arec->pnum,
				c_argval->arec->totalp,
				c_argval->arec->ndims==NULL?"NULL":c_argval->arec->ndims,
				c_argval->arec->dimsizes==NULL?"NULL":c_argval->arec->dimsizes,
				c_argval->arec->missing==NULL?"NULL":c_argval->arec->missing,
				c_argval->arec->hasmissing==NULL?"NULL":c_argval->arec->hasmissing,
				c_argval->arec->type==NULL?"NULL":c_argval->arec->type,
				c_argval->arec->rw);
			additional_src = c_argval->arec->additional_src;
			while(additional_src != NULL) {
				if(additional_src->src) {
					fprintf(stdout,"%s",additional_src->src);
				}
				additional_src = additional_src->next;
			}
			c_argval = c_argval->next;	
		}
		c_vdefs = current->c_vdefs;
		while(c_vdefs != NULL) {
			additional_src = c_vdefs->def->additional_src;
			while(additional_src != NULL) {
				if(additional_src->order == 0) {
					if(additional_src->src) {
						fprintf(stdout,"%s",additional_src->src);
					}
				}
				additional_src = additional_src->next;
			}
			c_vdefs = c_vdefs->next;
		}
		c_vdefs = current->c_vdefs;
		while(c_vdefs != NULL) {
			additional_src = c_vdefs->def->additional_src;
			while(additional_src != NULL) {
				if(additional_src->order == 1) {
					if(additional_src->src) {
						fprintf(stdout,"%s",additional_src->src);
					}
				}
				additional_src = additional_src->next;
			}
			c_vdefs = c_vdefs->next;
		}
		c_callrec = current->c_callrec;
		fprintf(stdout,"\t%s",c_callrec->callstring);
		sprintf(buffer,"(");
		for(i = 0; i < c_callrec->n_args;i++) {
			if(c_callrec->arg_strings[i] == NULL) {
				strcat(buffer,"NULL");
			} else if(!c_callrec->arg_strings[i]->isptr) {
				strcat(buffer,"&");
				strcat(buffer,c_callrec->arg_strings[i]->string);
			} else {
				strcat(buffer,c_callrec->arg_strings[i]->string);
			}
			if(i!= c_callrec->n_args-1) {
				strcat(buffer,",");
			} 
		}
		if(c_callrec->nstrs != 0) {
			strcat(buffer,",");
			for(;i < c_callrec->n_args+c_callrec->nstrs;i++) {
				strcat(buffer,c_callrec->arg_strings[i]->string);
				if(i!= c_callrec->n_args + c_callrec->nstrs -1) {
					strcat(buffer,",");
				} 
			}
		}
		strcat(buffer,");\n\n");
		fprintf(stdout,"%s",buffer);
		c_vdefs = current->c_vdefs;
		while(c_vdefs != NULL) {
			additional_src = c_vdefs->def->additional_src;
			while(additional_src != NULL) {
				if(additional_src->order == 2) {
					if(additional_src->src) {
						fprintf(stdout,"%s",additional_src->src);
					}
				}
				additional_src = additional_src->next;
			}
			c_vdefs = c_vdefs->next;
		}
		fprintf(stdout,"\t%s\n",current->rtrn);
		fprintf(stdout,"}\n");
		
		nargs = 0;
		w_nargs = 0;
	}
	| interface_list interface ENDOFIF
	{
		WVDefList *c_vdefs;
		WArgValList *c_argval;
		WSrcList *additional_src;
		WCallRec *c_callrec;
		char buffer[BUFFSIZE];
		char upper[BUFFSIZE];
		char lower[BUFFSIZE];
		int i;


		c_callrec = current->c_callrec;
		fprintf(stdout,fdef_fmt,current->c_defstring);
		c_vdefs = current->c_vdefs;
		while(c_vdefs != NULL) {
			sprintf(buffer,"%s ",CType(c_vdefs->def->datatype));
			if(c_vdefs->def->isptr) {
				strcat(buffer,"*");
			}
			strcat(buffer,c_vdefs->def->string);
			if(c_vdefs->def->array_spec != NULL) {
				strcat(buffer,c_vdefs->def->array_spec);
			}
			fprintf(stdout,"\t%s;\n",buffer);
			c_vdefs = c_vdefs->next;
		}
		
		c_argval = current->c_argval;
		while(c_argval != NULL) {
			fprintf(stdout,argval_fmt,
				c_argval->arec->assign_to,
				CType(c_argval->arec->datatype),
				c_argval->arec->pnum,
				c_argval->arec->totalp,
				c_argval->arec->ndims==NULL?"NULL":c_argval->arec->ndims,
				c_argval->arec->dimsizes==NULL?"NULL":c_argval->arec->dimsizes,
				c_argval->arec->missing==NULL?"NULL":c_argval->arec->missing,
				c_argval->arec->hasmissing==NULL?"NULL":c_argval->arec->hasmissing,
				c_argval->arec->type==NULL?"NULL":c_argval->arec->type,
				c_argval->arec->rw);
			if(c_argval->arec->additional_src != NULL) {
				fprintf(stdout,"%s",c_argval->arec->additional_src);
			}
			c_argval = c_argval->next;
		}
		c_vdefs = current->c_vdefs;
		while(c_vdefs != NULL) {
			additional_src = c_vdefs->def->additional_src;
			while(additional_src != NULL) {
				if(additional_src->order==0 ) {
					if(additional_src->src) {
						fprintf(stdout,"%s",additional_src->src);
					}
				} 
				additional_src = additional_src->next;
			}
			c_vdefs = c_vdefs->next;
		}
		c_vdefs = current->c_vdefs;
		while(c_vdefs != NULL) {
			additional_src = c_vdefs->def->additional_src;
			while(additional_src != NULL) {
				if(additional_src->order==1 ) {
					if(additional_src->src) {
						fprintf(stdout,"%s",additional_src->src);
					}
				} 
				additional_src = additional_src->next;
			}
			c_vdefs = c_vdefs->next;
		}
		c_callrec = current->c_callrec;
		fprintf(stdout,"\t%s",c_callrec->callstring);
		sprintf(buffer,"(");
		for(i = 0; i < c_callrec->n_args;i++) {
			if(c_callrec->arg_strings[i] == NULL) {
				strcat(buffer,"NULL");
			} else if(!c_callrec->arg_strings[i]->isptr) {
				strcat(buffer,"&");
				strcat(buffer,c_callrec->arg_strings[i]->string);
			} else {
				strcat(buffer,c_callrec->arg_strings[i]->string);
			}
		
			if((i!= c_callrec->n_args-1)) {
				strcat(buffer,",");
			} 
		}
		if(c_callrec->nstrs != 0) {
			strcat(buffer,",");
			for(;i < c_callrec->n_args+c_callrec->nstrs;i++) {
				strcat(buffer,c_callrec->arg_strings[i]->string);
				if(i!= c_callrec->n_args + c_callrec->nstrs -1) {
					strcat(buffer,",");
				} 
			}
		}
		strcat(buffer,");\n\n");
		fprintf(stdout,"%s",buffer);
		c_vdefs = current->c_vdefs;
		while(c_vdefs != NULL) {
			additional_src = c_vdefs->def->additional_src;
			while(additional_src != NULL) {
				if(additional_src->order == 2) {
					if(additional_src->src) {
						fprintf(stdout,"%s",additional_src->src);
					}
				}
				additional_src = additional_src->next;
			}
			c_vdefs = c_vdefs->next;
		}
		fprintf(stdout,"\t%s\n",current->rtrn);
		fprintf(stdout,"}\n");
		
		nargs = 0;
		w_nargs = 0;
	}
interface : function_def odata_defs
	{
/*
* No wrapper info, so output records have not been built.
*/
		int i,j,tmpj,nstrs;
		NclSymbol *s,*s0;
		NclSymbol *wp;
		char buffer[BUFFSIZE];
		char upper[BUFFSIZE];
		char lower[BUFFSIZE];
		WGetArgValue *arg;
		WCallRec *thecall;
		NclSymbol **tmp;
		WCentry *tmpc;
		WSrcList *tmp_src;

		thecall = WNewCallRec($1->name,nargs);
/*
* Now since its a one-one function the wrapper parameter information must
* be take from the FORTRAN information This list would otherwise be created
* already.
*/
		for(i = 0; i < strlen($1->name); i++) {
                        if((!isdigit($1->name[i]))&&(isalpha($1->name[i]))) {
                        	if(isupper($1->name[i])) {
                               		upper[i] = $1->name[i];
                               		lower[i] = (char)((int)$1->name[i] + 32);
                        	} else {
                                	lower[i] = $1->name[i];
                                	upper[i] = (char)((int)$1->name[i] - 32);
                        	}
			} else {
				upper[i] = lower[i] = $1->name[i];
			}
                }
		lower[i] = '\0';
		upper[i] = '\0';
		fprintf(stdout,"extern %s NGCALLF(%s,%s)();\n",CType($1->u.func->datatype),lower,upper);

		current->f_or_p = 1;
		current->c_defstring = malloc(strlen($1->name)+1);
		strcpy(current->c_defstring, $1->name);

		sprintf(buffer,"%s_ret",$1->name);
		tmpc = WNewVDef(buffer,$1->u.func->datatype,0,NULL,0);

		sprintf(buffer,"%s = NGCALLF(%s,%s)",tmpc->string,lower,upper);
		thecall->callstring = malloc(strlen(buffer)+1);
		strcpy(thecall->callstring,buffer);

		sprintf(buffer,"%s_ret_dimsizes",$1->name);
		(void)WNewVDef(buffer,NCL_long,0,"[NCL_MAX_DIMENSIONS]",0);
		

		for (i = 0; i < $1->u.func->n_args; i++) {
			s = $1->u.func->args[i];
/*
* Also since no wrapper only need to retrieve pointer to var
* no need for ndims,dimsizes,missing....
*/
			wp = _NclAddInScope(current->wrec,(char*)s->name,WPARAM);
			wp->u.wparam = (WParamInfo*)malloc(sizeof(WParamInfo));


			arg = WNewArgVal();
			if(s->u.farg->datatype == NCL_none) {
				s->u.farg->datatype = wp->u.wparam->datatype = FType(s->name);
			} else {
				wp->u.wparam->datatype = s->u.farg->datatype;
			}
			wp->u.wparam->n_dims = s->u.farg->n_dims;
			for(j = 0; j < s->u.farg->n_dims; j++) {
				wp->u.wparam->dim_sizes[j] = s->u.farg->dim_sizes[j];
			}
			arg->assign_to = s->name;
			arg->datatype = s->u.farg->datatype;
			arg->pnum = i;
			arg->totalp = $1->u.func->n_args;
			arg->rw = 1;
			arg->nd[0] = '\t';
			arg->nd[1] = ')';
			arg->nd[2] = ';';
			arg->nd[3] = '\n';
			arg->nd[4] = '\0';
			wp->u.wparam->getarg = arg;
			thecall->arg_strings[i] = WNewVDef(s->name,s->u.farg->datatype,1,NULL,0);
		}
/*
* This loop searches for string arguments and defines the length argument needed by FORTRAN
*/
		nstrs = 0;
		for (i = 0; i < $1->u.func->n_args; i++) {
			s = $1->u.func->args[i];
			if(s->u.farg->datatype == NCL_string) {
				sprintf(buffer,"%s_len",s->name);
				thecall->arg_strings[$1->u.func->n_args+nstrs] = WNewVDef(buffer,NCL_int,0,NULL,0);
				sprintf(buffer,"\t%s_len = strlen(NrmQuarkToString(*%s));\n",s->name,s->name);
				tmp_src = thecall->arg_strings[$1->u.func->n_args+nstrs]->additional_src;
				thecall->arg_strings[$1->u.func->n_args+nstrs]->additional_src = WNewAdditionalSrc(buffer,1);
				thecall->arg_strings[$1->u.func->n_args+nstrs]->additional_src->next = tmp_src;


				sprintf(buffer,"%s_str",s->name);
				thecall->arg_strings[i] = WNewVDef(buffer,NCL_char,1,NULL,1);
				sprintf(buffer,"\t%s_str = malloc(%s_len + 1);\n\tstrcpy(%s_str,NrmQuarkToString(*%s));\n",s->name,s->name,s->name,s->name);
				tmp_src = thecall->arg_strings[i]->additional_src;
				
				thecall->arg_strings[i]->additional_src = WNewAdditionalSrc(buffer,1);
				thecall->arg_strings[i]->additional_src->next = tmp_src;
				sprintf(buffer,"\t%s_str[%s_len] = \'\\0\';\n\t*%s = NrmStringToQuark(%s_str);\n\tfree(%s_str);\n",s->name,s->name,s->name,s->name,s->name);
				tmp_src = thecall->arg_strings[i]->additional_src;
				thecall->arg_strings[i]->additional_src = WNewAdditionalSrc(buffer,2);
				thecall->arg_strings[i]->additional_src->next = tmp_src;

				nstrs++;
			} else {
				for(j = 0; j < $1->u.func->args[i]->u.farg->n_dims; j++) {
					if($1->u.func->args[i]->u.farg->dim_refs[j] != NULL) {
						s0 = _NclLookUpInScope(current->wrec,$1->u.func->args[i]->name);
						DoDimsizes(s0);
						sprintf(buffer,"\tif(*%s != (int)%s_dimsizes[%d]) {\n\t\tNhlPError(NhlFATAL,NhlEUNKNOWN,\"%s: dimension size of dimension (%d) of %s must be equal to the value of %s\");\n\t\treturn(NhlFATAL);\n\t}\n",$1->u.func->args[i]->u.farg->dim_refs[j]->name,$1->u.func->args[i]->name,j,$1->name,j,$1->u.func->args[i]->name,$1->u.func->args[i]->u.farg->dim_refs[j]->name);
						tmp_src = thecall->arg_strings[i]->additional_src;
						thecall->arg_strings[i]->additional_src = WNewAdditionalSrc(buffer,1);
						thecall->arg_strings[i]->additional_src->next = tmp_src;
					}
				}
			}
		}
		thecall->nstrs = nstrs;
		sprintf(buffer,"%s_ret_dimsizes[0] = 1;\n\treturn(NclReturnValue(&%s_ret,1,%s_ret_dimsizes,NULL,%s,1));",$1->name,$1->name,$1->name,NType($1->u.func->datatype));
		current->rtrn = malloc(strlen(buffer)+1);
		strcpy(current->rtrn,buffer);
		
	}	
	| subroutine_def odata_defs
	{
/*
* No wrapper info, so output records have not been built.
*/
		int i,j,nstrs,tmpj;
		NclSymbol *s,*s0;
		NclSymbol *wp;
		char buffer[BUFFSIZE];
		char lower[BUFFSIZE];
		char upper[BUFFSIZE];
		WGetArgValue *arg;
		WCallRec *thecall;
		NclSymbol **tmp;
		WSrcList *tmp_src;

		thecall = WNewCallRec($1->name,nargs);
/*
* Now since its a one-one function the wrapper parameter information must
* be take from the FORTRAN information This list would otherwise be created
* already.
*/
		for(i = 0; i < strlen($1->name); i++) {
			if((!isdigit($1->name[i]))&&(isalpha($1->name[i]))) {
                        	if(isupper($1->name[i])) {
                                	upper[i] = $1->name[i];
                                	lower[i] = (char)((int)$1->name[i] + 32);
                        	} else {
                                	lower[i] = $1->name[i];
                                	upper[i] = (char)((int)$1->name[i] - 32);
                        	}
			} else {
                                	upper[i] = lower[i] = $1->name[i];
			}
                }
		lower[i] = '\0';
		upper[i] = '\0';
		sprintf(buffer,"NGCALLF(%s,%s)",lower,upper);
		thecall->callstring = malloc(strlen(buffer)+1);
		strcpy(thecall->callstring,buffer);
		current->f_or_p = 0;
		current->c_defstring = malloc(strlen($1->name)+1);
		strcpy(current->c_defstring, $1->name);
		for (i = 0; i < $1->u.subr->n_args; i++) {
			s = $1->u.subr->args[i];
/*
* Also since no wrapper only need to retrieve pointer to var
* no need for ndims,dimsizes,missing....
*/
			wp = _NclAddInScope(current->wrec,(char*)s->name,WPARAM);
			wp->u.wparam = (WParamInfo*)malloc(sizeof(WParamInfo));


			arg = WNewArgVal();
			if(s->u.farg->datatype == NCL_none) {
				s->u.farg->datatype = wp->u.wparam->datatype = FType(s->name);
			} else {
				wp->u.wparam->datatype = s->u.farg->datatype;
			}
			wp->u.wparam->n_dims = s->u.farg->n_dims;
			for(j = 0; j < s->u.farg->n_dims; j++) {
				wp->u.wparam->dim_sizes[j] = s->u.farg->dim_sizes[j];
			}
			arg->assign_to = s->name;
			arg->datatype = s->u.farg->datatype;
			arg->pnum = i;
			arg->totalp = $1->u.subr->n_args;
			arg->rw = 1;
			arg->nd[0] = '\t';
			arg->nd[1] = ')';
			arg->nd[2] = ';';
			arg->nd[3] = '\n';
			arg->nd[4] = '\0';
			wp->u.wparam->getarg = arg;
			thecall->arg_strings[i] = WNewVDef(s->name,s->u.farg->datatype,1,NULL,0);
		}
/*
* This loop searches for string arguments and defines the length argument needed by FORTRAN
*/
		nstrs = 0;
		for (i = 0; i < $1->u.subr->n_args; i++) {
			s = $1->u.subr->args[i];
			if(s->u.farg->datatype == NCL_string) {
				sprintf(buffer,"%s_len",s->name);
				thecall->arg_strings[$1->u.subr->n_args+nstrs] = WNewVDef(buffer,NCL_int,0,NULL,0);
				sprintf(buffer,"\t%s_len = strlen(NrmQuarkToString(*%s));\n",s->name,s->name);
				tmp_src = thecall->arg_strings[$1->u.subr->n_args+nstrs]->additional_src;
				thecall->arg_strings[$1->u.subr->n_args+nstrs]->additional_src = WNewAdditionalSrc(buffer,1);
				thecall->arg_strings[$1->u.subr->n_args+nstrs]->additional_src->next = tmp_src;


				sprintf(buffer,"%s_str",s->name);
				thecall->arg_strings[i] = WNewVDef(buffer,NCL_char,1,NULL,1);
				sprintf(buffer,"\t%s_str = malloc(%s_len + 1);\n\tstrcpy(%s_str,NrmQuarkToString(*%s));\n",s->name,s->name,s->name,s->name);
				tmp_src = thecall->arg_strings[i]->additional_src;
				
				thecall->arg_strings[i]->additional_src = WNewAdditionalSrc(buffer,1);
				thecall->arg_strings[i]->additional_src->next = tmp_src;
				sprintf(buffer,"\t%s_str[%s_len] = \'\\0\';\n\t*%s = NrmStringToQuark(%s_str);\n\tfree(%s_str);\n",s->name,s->name,s->name,s->name,s->name);
				tmp_src = thecall->arg_strings[i]->additional_src;
				thecall->arg_strings[i]->additional_src = WNewAdditionalSrc(buffer,2);
				thecall->arg_strings[i]->additional_src->next = tmp_src;
				nstrs++;
			} else {
				for(j = 0; j < $1->u.subr->args[i]->u.farg->n_dims; j++) {
					if($1->u.subr->args[i]->u.farg->dim_refs[j] != NULL) {
						s0 = _NclLookUpInScope(current->wrec,$1->u.subr->args[i]->name);
						DoDimsizes(s0);
						sprintf(buffer,"\tif(*%s != (int)%s_dimsizes[%d]) {\n\t\tNhlPError(NhlFATAL,NhlEUNKNOWN,\"%s: dimension size of dimension (%d) of %s must be equal to the value of %s\");\n\t\treturn(NhlFATAL);\n\t}\n",$1->u.subr->args[i]->u.farg->dim_refs[j]->name,$1->u.subr->args[i]->name,j,$1->name,j,$1->u.subr->args[i]->name,$1->u.subr->args[i]->u.farg->dim_refs[j]->name);
						tmp_src = thecall->arg_strings[i]->additional_src;
						thecall->arg_strings[i]->additional_src = WNewAdditionalSrc(buffer,1);
						thecall->arg_strings[i]->additional_src->next = tmp_src;
					}
				}
			}
		}
		thecall->nstrs = nstrs;
		current->rtrn = malloc(strlen("return(NhlNOERROR);")+1);
		strcpy(current->rtrn,"return(NhlNOERROR);");
		
		
	}
	| function_def odata_defs wrap_interface
        {
        }
        | subroutine_def odata_defs { (void)WNewCallRec($1->name,nargs); } wrap_interface
        {
        }
;

function_def : otyp FUNCTION UNDEF len '(' arg_list ')' oEOLN_list
	{
		WrapSrcListNode* tmp,*tmp2;
		int count = 0;
		$3->type = FUNC;
		$3->u.func = (WFuncInfo*)malloc(sizeof(WFuncInfo));

                tmp = $6;
                while(tmp != NULL) {
                        count++;
                        tmp = tmp->next;
                }
		$3->u.func->n_args = count;
		$3->u.func->args = (NclSymbol**)malloc(sizeof(NclSymbol*)*count);
		if($1 == NULL) {
			$3->u.func->datatype = FType($3->name);
		} else {
			$3->u.func->datatype = $1->datatype;
		}
		tmp = $6;
		for(count = 0; count < $3->u.func->n_args; count++) {
			$3->u.func->args[count] = tmp->node;
			tmp2 = tmp;
			tmp = tmp->next;
			free(tmp2);
		}
		$$ = $3;
		

	} 
	| otyp FUNCTION UNDEF len '(' ')'  oEOLN_list
	{
		$3->type = FUNC;
		$3->u.func = (WFuncInfo*)malloc(sizeof(WFuncInfo));
		$3->u.func->n_args = 0;
		$3->u.func->args = NULL;
		if($1 == NULL) {
			$3->u.func->datatype = FType($3->name);
		} else {
			$3->u.func->datatype = $1->datatype;
		}
		$$ = $3;
	}
;

subroutine_def : SUBROUTINE UNDEF '(' arg_list ')' oEOLN_list
	{
		WrapSrcListNode* tmp,*tmp2;
		int count = 0;
		$2->type = SUBR;
		$2->u.subr = (WSubrInfo*)malloc(sizeof(WSubrInfo));
	
		tmp = $4;
		while(tmp != NULL) {
			count++;
			tmp = tmp->next;
		}
		$2->u.subr->args = (NclSymbol**)malloc(sizeof(NclSymbol*)*count);
		$2->u.subr->n_args = count;
		tmp = $4;
		for( count = 0; count < $2->u.subr->n_args; count++ ) {
			$2->u.subr->args[count] = tmp->node;
			tmp2 =tmp;
			tmp = tmp->next;
			free(tmp2);
		}
		$$ = $2;
	}
	| SUBROUTINE UNDEF '(' ')' oEOLN_list
	{
		$2->type = SUBR;
		$2->u.subr = (WSubrInfo*)malloc(sizeof(WSubrInfo));
		$2->u.subr->args = NULL;
		$2->u.subr->n_args = 0;
		$$ = $2;
	}
;
oEOLN_list :
	{
	}
	| EOLN_list
	{
	}

EOLN_list : EOLN
	{
	}
	| EOLN_list EOLN
	{
	}
;
arg_list : UNDEF
	{
		NclSymbol *s;
		char buffer[10];
		$1->type = FARG;
		$1->u.farg = (WFargInfo*)malloc(sizeof(WFargInfo));
		$1->u.farg->arg_num = nargs;
		$1->u.farg->dim_sizes[0] = 1;
		$1->u.farg->dim_refs[0] = NULL;
		$1->u.farg->n_dims = 1;
		sprintf(buffer,"FARG%d",nargs++);
#if  YYDEBUG
#endif
		s = _NclAddInScope(current->wrec,buffer,XFARG);
		s->u.xref = $1;
		$$ = WMakeListNode();
		$$->node = $1;
		$$->next = NULL;
	}
	|   UNDEF ',' arg_list 
	{
		NclSymbol *s;
		char buffer[BUFFSIZE];
		$1->u.farg = (WFargInfo*)malloc(sizeof(WFargInfo));
		$1->type = FARG;
		$1->u.farg->arg_num = nargs;
		$1->u.farg->dim_sizes[0] = 1;
		$1->u.farg->dim_refs[0] = NULL;
		$1->u.farg->n_dims = 1;
		sprintf(buffer,"FARG%d",nargs++);
#if  YYDEBUG
#endif

		s = _NclAddInScope(current->wrec,buffer,XFARG);
		s->u.xref = $1;
		$$ = WMakeListNode();
		$$->node = $1;
		$$->next = $3;
	}
;
odata_defs : 
	{
	}
	| data_defs
	{
	}
;

data_defs :  ftype name_list oEOLN_list 
	{
		WrapSrcListNode * tmp,*tmp2;
/*
*------> This is where problems with MD char arrays happens
*/
		tmp = $2;
		while(tmp != NULL) {
			tmp2 = tmp;
			((NclSymbol*)tmp->node)->u.farg->datatype = $1->datatype;
			if(($1->datatype == NCL_char)&&($1->size != -1)) {
				((NclSymbol*)tmp->node)->u.farg->dim_sizes[0] = $1->size;
				((NclSymbol*)tmp->node)->u.farg->dim_refs[0] = NULL;
			}
			tmp = tmp->next;
			free(tmp2);
		}

	}
	| data_defs ftype name_list oEOLN_list {
		WrapSrcListNode * tmp,*tmp2;

		tmp = $3;
		while(tmp != NULL) {
			tmp2 = tmp;
			((NclSymbol*)tmp->node)->u.farg->datatype = $2->datatype;
			if(($2->datatype == NCL_char)&&($2->size != -1)) {
				((NclSymbol*)tmp->node)->u.farg->dim_sizes[0] = $2->size;
				((NclSymbol*)tmp->node)->u.farg->dim_refs[0] = NULL;
			}
			tmp = tmp->next;
			free(tmp2);
		}
	}
;
ftype : BYTE
	{	$$ = (FTypeVal*)malloc(sizeof(FTypeVal));
		$$->datatype = NCL_byte;
		$$->size = -1;
	}
	| INTEGER
	{
		$$ = (FTypeVal*)malloc(sizeof(FTypeVal));
		$$->datatype = NCL_int;
		$$->size = -1;
	}
	| LOGICAL
	{
		$$ = (FTypeVal*)malloc(sizeof(FTypeVal));
		$$->datatype = NCL_logical;
		$$->size = -1;
	}
	| DOUBLE PRECISION
	{
		$$ = (FTypeVal*)malloc(sizeof(FTypeVal));
		$$->datatype = NCL_double;
		$$->size = -1;
	}
	| REAL
	{
		$$ = (FTypeVal*)malloc(sizeof(FTypeVal));
		$$->datatype = NCL_float;
		$$->size = -1;
	}
	| DATA
	{
		$$ = (FTypeVal*)malloc(sizeof(FTypeVal));
		$$->datatype = NCL_none;
		$$->size = -1;
	}
	| IMPLICIT
	{
		$$ = (FTypeVal*)malloc(sizeof(FTypeVal));
		$$->datatype = NCL_none;
		$$->size = -1;
	}
	| SAVE
	{
		$$ = (FTypeVal*)malloc(sizeof(FTypeVal));
		$$->datatype = NCL_none;
		$$->size = -1;
	}
	| DIMENSION
	{
		$$ = (FTypeVal*)malloc(sizeof(FTypeVal));
		$$->datatype = NCL_none;
		$$->size = -1;
	}
	| CHARACTER '*' INT {
		$$ = (FTypeVal*)malloc(sizeof(FTypeVal));
		$$->datatype =  NCL_char;
		$$->size = $3;
	}
	| CHARACTER
	{
		$$ = (FTypeVal*)malloc(sizeof(FTypeVal));
		$$->datatype =  NCL_char;
		$$->size = -1;
	}
	| CHARACTER '*' '(' '*' ')'
	{
		$$ = (FTypeVal*)malloc(sizeof(FTypeVal));
		$$->datatype = NCL_string;
		$$->size = -1;
	}
	| BYTE '*' INT
	{
		$$ = (FTypeVal*)malloc(sizeof(FTypeVal));
		$$->size = -1;
		switch($3) {
		case 1:
			$$->datatype = NCL_byte;
			break;
		case 2:
			$$->datatype = NCL_short;
			break;
		case 4:
			$$->datatype = NCL_int;
			break;
		default:
			$$->datatype = NCL_none;
		}
	}
	| INTEGER '*' INT
	{
		$$ = (FTypeVal*)malloc(sizeof(FTypeVal));
		$$->size = -1;
		switch($3){
		case 1:
			$$->datatype = NCL_byte;
			break;
		case 2:
			$$->datatype = NCL_short;
			break;
		case 4:
			$$->datatype = NCL_int;
			break;
		case 8:
			if(sizeof(long)==8) {
				$$->datatype = NCL_long;
			} else {
				$$->datatype = NCL_none;
			}
		default:
			$$ = NCL_none;
		}
	}
	| LOGICAL '*' INT
	{
		$$ = (FTypeVal*)malloc(sizeof(FTypeVal));
		$$->size = -1;
		switch($3){
		case 1:
			$$->datatype = NCL_byte;
			break;
		case 2:
			$$->datatype = NCL_short;
			break;
		case 4:
			$$->datatype = NCL_logical;
			break;
		default:
			$$->datatype = NCL_none;
		}
	}
	| REAL '*' INT
	{
		$$ = (FTypeVal*)malloc(sizeof(FTypeVal));
		$$->size = -1;
		switch($3){
		case 1:
		case 2:
			$$->datatype = NCL_none;
			break;
		case 4:
			$$->datatype = NCL_float;
			break;
		case 8:
			$$->datatype = NCL_double;
			break;
		default:
			$$ = NCL_none;
		}
	}
;


name_list : param_def
	{
		if($1 != NULL) {
			$$ = (WrapSrcListNode*) malloc(sizeof(WrapSrcListNode));
			$$->node = $1;
			$$->next = NULL;
		} else {
			$$ = NULL;
		}
	}
	| name_list ',' param_def
	{
		if($3 != NULL) {
			$$ = (WrapSrcListNode*) malloc(sizeof(WrapSrcListNode));
			$$->node = $3;
			$$->next = $1;
		} else {
			$$ = $1;
		}
	}
;

param_def : FARG
	{
		$1->u.farg->n_dims = 1;
		$1->u.farg->dim_sizes[0] = 1;
		$$ = $1;
	}
	| FARG '*' INT 
	{
		$1->u.farg->n_dims = 1;
		$1->u.farg->dim_sizes[0] = $3;
		$$ = $1;
	}
	| FARG '*' '(' '*' ')'
	{
		$1->u.farg->n_dims = 1;
		$1->u.farg->dim_sizes[0] = -1;
		$$ = $1;
	}
	| FARG '(' dim_list ')'
	{
		int n_dims=0;
		int i;
		WrapSrcListNode* tmp;
		tmp = $3;
		while(tmp != NULL) {
			n_dims++;
			tmp = tmp->next;
		}
		$1->u.farg->n_dims = n_dims;
		tmp = $3;
		for(i = 0; i < n_dims; i++) {
			if(((DimVal*)tmp->node)->kind) {
				$1->u.farg->dim_sizes[i]  = -1;
				$1->u.farg->dim_refs[i] = ((DimVal*)tmp->node)->u.sym;
			} else {
				$1->u.farg->dim_refs[i]  = NULL;
				$1->u.farg->dim_sizes[i] = ((DimVal*)tmp->node)->u.val;
			}
			tmp = tmp->next;
		}
		$$ = $1;
	}
	| UNDEF '/' INT '/'
	{
		$$ = NULL;
	}
	| UNDEF 
	{
		$$ = NULL;
	}
	| UNDEF '(' dim_list ')'
	{
		$$ = NULL;
	}
;

dim_list : dim
	{
		$$ = (WrapSrcListNode*)malloc(sizeof(WrapSrcListNode));
		$$->node = $1;
		$$->next = NULL;
	}
	|  dim_list ',' dim
	{
		$$ = (WrapSrcListNode*)malloc(sizeof(WrapSrcListNode));
		$$->node = $3;
		$$->next = $1;
	}
;

dim : FARG
	{
		$$ = (DimVal*)malloc(sizeof(DimVal));
		$$->kind = 1;
		$$->u.sym = $1;
	}
	| INT
	{
		$$ = (DimVal*)malloc(sizeof(DimVal));
		$$->kind = 0;
		$$->u.val = $1;
	}
	| '*'
	{
		$$ = (DimVal*)malloc(sizeof(DimVal));
		$$->kind = 0;
		$$->u.val = -1;
	}
;

otyp :
	{
		$$ = NULL;
	}
	| ftype
	{
		if($1->datatype == NCL_none) {
			fprintf(stderr,"Unsupported FORTRAN type detected\n");
		}
		$$ = $1;
	}
;


len : 
	{
	}
	| '*'
	{
	}
	| INT
	{
	}
;

wrap_interface : PROCEDURE UNDEF '(' arg_dec_list ')' EOLN parg_map_list
	{
		WrapSrcListNode* tmp;
		NclSymbol *s;

		tmp = $4;

		while(tmp!= NULL) {
			s = (NclSymbol*)tmp->node;		
			s->u.wparam->getarg->totalp = w_nargs;
			tmp = tmp->next;
		}
		$2->type = PROC;
		current->f_or_p = 0;
		current->c_defstring = malloc(strlen($2->name)+1);
		strcpy(current->c_defstring,$2->name);

	}
	| FUNCTION UNDEF '(' arg_dec_list ')' EOLN farg_map_list
	{
		WrapSrcListNode* tmp;
		NclSymbol *s;

		tmp = $4;

		while(tmp!= NULL) {
			s = (NclSymbol*)tmp->node;		
			s->u.wparam->getarg->totalp = w_nargs;
			tmp = tmp->next;
		}
		$2->type = FUNC;
		current->f_or_p = 1;
		current->c_defstring = malloc(strlen($2->name)+1);
		strcpy(current->c_defstring,$2->name);
	}
	| PROCEDURE UNDEF '(' ')' EOLN  
	{
		$2->type = PROC;
		current->f_or_p = 0;
		current->c_defstring = malloc(strlen($2->name)+1);
		strcpy(current->c_defstring,$2->name);
	}
	| FUNCTION UNDEF '(' ')' EOLN farg_map_list 
	{
		$2->type = FUNC;
		current->f_or_p = 1;
		current->c_defstring = malloc(strlen($2->name)+1);
		strcpy(current->c_defstring,$2->name);
	}
;

arg_dec_list  : declaration
	{
		$$ = (WrapSrcListNode*)malloc(sizeof(WrapSrcListNode));
		$$->next = NULL;
		$$->node = (void*)$1;
	}
	| arg_dec_list ',' declaration
	{
		$$ = (WrapSrcListNode*)malloc(sizeof(WrapSrcListNode));
		$$->next = $1;
		$$->node = (void*)$3;
	}
;
declaration : UNDEF {
		WGetArgValue *tmp;
		char buffer[BUFFSIZE];
		$1->type = WPARAM;	
		$1->u.wparam = malloc(sizeof(WParamInfo));
		$1->u.wparam->n_dims = -1;
		$1->u.wparam->dim_sizes[0] = -1;
		$1->u.wparam->datatype = -1;
		$1->u.wparam->getarg = tmp = WNewArgVal();
		tmp->assign_to = $1->name;
		tmp->pnum = w_nargs;
		tmp->datatype = -1;
		tmp->rw = 1;
		$$ = $1;
		(void)WNewVDef($1->name,-1,1,NULL,0);
		w_nargs++;
	}
	|  UNDEF ':' datatype
	{
		WGetArgValue *tmp;
		WSrcList *srclist;
		char buffer[BUFFSIZE];

		$1->type = WPARAM;
		$1->u.wparam = malloc(sizeof(WParamInfo));
		$1->u.wparam->n_dims = -1;
		$1->u.wparam->dim_sizes[0] = -1;
		$1->u.wparam->datatype = $3;
		$1->u.wparam->getarg = tmp = WNewArgVal();
		(void)WNewVDef($1->name,$3,1,NULL,0);

		tmp->assign_to = $1->name;
		tmp->pnum = w_nargs;
		tmp->datatype = $3;
		tmp->rw = 1;
                sprintf(buffer,"&%s_type",$1->name);
                tmp->type = malloc(strlen(buffer)+1);
                strcpy(tmp->type,buffer);


		sprintf(buffer,"%s_type",$1->name);
		(void)WNewVDef(buffer,NCL_int,0,NULL,0);

		$$ = $1;
		w_nargs++;
	}
	| UNDEF dim_size_list
	{	
		WGetArgValue *tmp;
		char buffer[BUFFSIZE];
		IntList *itmp;
		int i = 0 ;

		$1->type = WPARAM;
		$1->u.wparam = malloc(sizeof(WParamInfo));
		itmp = $2;
		while(itmp != NULL) {
			$1->u.wparam->dim_sizes[i] = itmp->val;	
			i++;
			itmp = itmp->next;
		}
		$1->u.wparam->n_dims = i;
		$1->u.wparam->datatype = -1;
		$1->u.wparam->getarg = tmp = WNewArgVal();
		sprintf(buffer,"%s",$1->name);
		(void)WNewVDef(buffer,-1,1,NULL,0);

		tmp->assign_to = $1->name;
		tmp->pnum = w_nargs;
		tmp->datatype = -1;
		tmp->rw = 1;

		sprintf(buffer,"&%s_ndims",$1->name);
		tmp->ndims = malloc(strlen(buffer)+1);
		strcpy(tmp->ndims,buffer);


		sprintf(buffer,"%s_ndims",$1->name);
		(void)WNewVDef(buffer,NCL_int,0,NULL,0);

		sprintf(buffer,"%s_dimsizes",$1->name);
		(void)WNewVDef(buffer,NCL_long,0,"[NCL_MAX_DIMENSIONS]",0);
		tmp->dimsizes = malloc(strlen(buffer)+1);
		strcpy(tmp->dimsizes,buffer);


		$$ = $1;
		w_nargs++;
	}
	| UNDEF dim_size_list ':' datatype
	{	
		WGetArgValue *tmp;
		char buffer[BUFFSIZE];
		IntList *itmp;
		int i = 0 ;
		$1->type = WPARAM;
		$1->u.wparam = malloc(sizeof(WParamInfo));
		itmp = $2;
		while(itmp != NULL) {
			$1->u.wparam->dim_sizes[i] = itmp->val;	
			i++;
			itmp = itmp->next;
		}
		$1->u.wparam->n_dims = i;
		$1->u.wparam->datatype = $4;
		$1->u.wparam->getarg = tmp = WNewArgVal();
		sprintf(buffer,"%s",$1->name);
		(void)WNewVDef(buffer,$4,1,NULL,0);

		tmp->assign_to = $1->name;
		tmp->pnum = w_nargs;
		tmp->datatype = $4;
		tmp->rw = 1;

		sprintf(buffer,"&%s_ndims",$1->name);	
		tmp->ndims = malloc(strlen(buffer)+1);
		strcpy(tmp->ndims,buffer);
		sprintf(buffer,"%s_ndims",$1->name);
		(void)WNewVDef(buffer,NCL_int,0,NULL,0);


		sprintf(buffer,"%s_dimsizes",$1->name);
		tmp->dimsizes= malloc(strlen(buffer)+1);
		strcpy(tmp->dimsizes,buffer);
		(void)WNewVDef(buffer,NCL_long,0,"[NCL_MAX_DIMENSIONS]",0);

		sprintf(buffer,"&%s_type",$1->name);
		tmp->type = malloc(strlen(buffer)+1);
		strcpy(tmp->type,buffer);

		sprintf(buffer,"%s_type",$1->name);
		(void)WNewVDef(buffer,NCL_int,0,NULL,0);

		$$ = $1;
		w_nargs++;
	}
;
dim_size_list : '[' INT ']'
	{
                $$ = (IntList*) malloc(sizeof(IntList));
                $$->val = $2;
                $$->next = NULL;
	}
	| '[' '*' ']'
	{
                $$ = (IntList*) malloc(sizeof(IntList));
                $$->val = -1;
                $$->next = NULL;
	}
	| dim_size_list '[' '*' ']'
	{
                $$ = (IntList*) malloc(sizeof(IntList));
                $$->val = -1;
                $$->next = $1;
	}
	| dim_size_list '[' INT ']'
	{
                $$ = (IntList*) malloc(sizeof(IntList));
                $$->val = $3;
                $$->next = $1;
	}
;

datatype : FLOAT        { $$ = NCL_float; }
        | LONG          { $$ = NCL_long; }
        | INTEGER       { $$ = NCL_int; }
        | SHORT         { $$ = NCL_short; }
        | DOUBLE        { $$ = NCL_double; }
        | CHARACTER     { $$ = NCL_char; }
        | BYTE          { $$ = NCL_byte; }
        | STRNG         { $$ = NCL_string; }
        | LOGICAL       { $$ = NCL_logical; }
;


farg_map_list : farg_map EOLN
	{
		$$ = (WrapSrcListNode*) malloc(sizeof(WrapSrcListNode));
		$$->node = (void*)$1;
		$$->next = NULL;
	}
	| farg_map_list farg_map EOLN
	{
		$$ = (WrapSrcListNode*) malloc(sizeof(WrapSrcListNode));
		$$->node = (void*)$2;
		$$->next = $1;
	}
;

farg_map : normal_param {
	} 
	| rparam {theparam = $1;} omods {
		
	}
;

rparam : RETURN ':' WPARAM
	{
		char buffer[BUFFSIZE];
		WParamLoc *loc = (WParamLoc*)malloc(sizeof(WParamLoc));
		NclSymbol *s = _NclLookUpInScope(current->crec,$3->name);
		int i;

		loc->type = RETURNPARAM;
		loc->datatype = $3->u.wparam->datatype;
		loc->xfarg = NULL;
		loc->cdef = s->u.centry;
		loc->wsym = $3;
		
		if($3->u.wparam->n_dims == -1) {
			loc->typeofdim = 1;
			loc->n_dims = 1;
			sprintf(buffer,"%s_dimsizes",$3->name);	
			s = _NclLookUpInScope(current->crec,buffer);
			loc->altdimsref = s->u.centry;
			sprintf(loc->altndimref,"%s_ndims",$3->name);
		} else {
			loc->typeofdim = 0;
			loc->n_dims = $3->u.wparam->n_dims;
			for(i = 0; i < $3->u.wparam->n_dims; i++) {
				loc->dim_sizes[i] = $3->u.wparam->dim_sizes[i];
				if(loc->dim_sizes[i] = -1) {
					sprintf(loc->dim_refs[i],"(%s_dimsizes[%d])",$3->name,i);
				} else {
					sprintf(loc->dim_refs[i],"(%d)",loc->dim_sizes[i]);
				}
			}
		}
		$$ = loc;
	}
	| RETURN ':' '/' WPARAM
	{
	}
	| RETURN ':' '#' WPARAM
	{
	}
        | RETURN ':' '/' WPARAM '[' INT ']'
        {
        }
/*
	| RETURN ':' XFARG
	{
	}
	| RETURN ':' '#' XFARG
	{
	}
	| RETURN ':' '/' XFARG
	{
	}
        | RETURN ':' '/' XFARG '[' INT ']'
        {
        }
*/
;


parg_map_list : normal_param EOLN
	{
		$$ = (WrapSrcListNode*) malloc(sizeof(WrapSrcListNode));
		$$->node = (void*)$1;
		$$->next = NULL;
	}
	| parg_map_list normal_param EOLN
	{
		$$ = (WrapSrcListNode*) malloc(sizeof(WrapSrcListNode));
		$$->node = (void*)$2;
		$$->next = $1;
	}
;

normal_param : param_loc { theparam = $1; } omods
	{
		char buffer[BUFFSIZE];
		int i,j;
		WSrcList *tmp;
		WModList *mods;


		switch($1->type) {
		case FARGNEW:
			current->c_callrec->arg_strings[$1->xfarg->u.xref->u.farg->arg_num] = $1->cdef;
			sprintf(buffer,"\t%s = (%s*)malloc(sizeof(%s)",$1->cdef->string,CType($1->datatype),CType($1->datatype));
			for(i = 0; i < $1->n_dims; i++) {
				strcat(buffer," * ");
				strcat(buffer,$1->dim_refs[i]);
			}
			strcat(buffer,");\n");
			if($1->cdef->additional_src == NULL) {
				$1->cdef->additional_src = WNewAdditionalSrc(buffer,1);
			} else {
				tmp =  WNewAdditionalSrc(buffer,1);
				tmp->next = $1->cdef->additional_src;
				$1->cdef->additional_src = tmp;
			}
			break;
		case PARAMVAL:
		case TOTALPARAM:
			current->c_callrec->arg_strings[$1->xfarg->u.xref->u.farg->arg_num] = $1->cdef;
			break;
		case DIMSPARAM:
			break;
		case DIMINDEXPARAM:
			break;
		}

	}
;

param_loc :  XFARG ':'  NEW '=' dspec_list
	{
		WParamLoc *loc = NewParamLoc($1);
		char buffer[BUFFSIZE];
		WDSpecList *tmp = $5;
		WCentry *ctmp;
		int j;
	
		loc->type = FARGNEW;
		loc->xfarg = $1;
		loc->cdef = WNewVDef($1->name,$1->u.xref->u.farg->datatype,1,NULL,1);
		sprintf(loc->call_string,"%s",$1->name);

		loc->n_dims = 0	;
		while(tmp != NULL) {

			switch(tmp->type) {
			case INTDIMSIZES:
				sprintf(loc->dim_refs[loc->n_dims],"(%d)",tmp->u.val);
				loc->n_dims++;
				break;
			case PARAMVALDIMSIZES:
				sprintf(loc->dim_refs[loc->n_dims],"(*%s)",tmp->u.pval.wsym->name);
				loc->n_dims++;
				break;
			case TOTALPARAMDIMSIZES:
				sprintf(loc->dim_refs[loc->n_dims],"(%s_total)",tmp->u.tpval.wsym->name);
				loc->n_dims++;
				break;
			case DIMSPARAMDIMSIZES:
				if(tmp->u.dpval.wsym->u.wparam->n_dims == -1) {
					DoTotal(tmp->u.dpval.wsym);
					sprintf(loc->dim_refs[loc->n_dims],"(%s_total)",tmp->u.dpval.wsym->name);
					loc->n_dims++;
				} else {
					for(j = 0; j < tmp->u.dpval.wsym->u.wparam->n_dims; j++) {
						sprintf(loc->dim_refs[loc->n_dims],"(%s_dimsizes[%d])",tmp->u.dpval.wsym->name,j);	
						loc->n_dims++;
					}
				}
				break;
			case DIMINDEXPARAMDIMSIZES:
				sprintf(loc->dim_refs[loc->n_dims],"(%s_dimsizes[%d])",tmp->u.dipval.wsym->name,tmp->u.dipval.index);	
				loc->n_dims++;
				break;
			}
			tmp = tmp->next;
		}
		$$ = loc;
	}
	| XFARG ':' WPARAM
	{

		WParamLoc *loc = NewParamLoc($1);
		NclSymbol *tmp;
		int i;
		char buffer[BUFFSIZE];

		DoDimsizes($3);
		loc->type = PARAMVAL;
		loc->xfarg = $1;
		tmp = _NclLookUpInScope(current->crec,$3->name);
		loc->cdef = tmp->u.centry;
		sprintf(loc->call_string,"%s",$3->name);
		loc->n_dims = $3->u.wparam->n_dims;
		for(i = 0; i < $3->u.wparam->n_dims; i++) {
			loc->dim_sizes[i] = $3->u.wparam->dim_sizes[i];
			sprintf(loc->dim_refs[i],"(%s_dimsizes[%d])",tmp->name,i);
		}
		loc->datatype = $3->u.wparam->datatype;
		$$ = loc;
		
	}
	| XFARG ':' '#' WPARAM
	{
		WParamLoc *loc = NewParamLoc($1);
		WCentry *tmp;
		char buffer[BUFFSIZE];
		NclSymbol *s;

		DoTotal($4);
		loc->type = TOTALPARAM;
		loc->xfarg = $1;
		loc->n_dims = 1;
		loc->datatype = NCL_int;
		loc->dim_sizes[0] = 1;
		sprintf(loc->dim_refs[0],"(1)");
		sprintf(buffer,"%s_total",$4->name);
		s = _NclLookUpInScope(current->crec,buffer);
		loc->cdef = s->u.centry;
		sprintf(loc->call_string,"&%s",buffer);
		$$ = loc;
	}
	| XFARG ':' '/' WPARAM
	{
                WParamLoc *loc = NewParamLoc($1);
		char buffer[BUFFSIZE];
		NclSymbol *s;
		DoDimsizes($4);

                loc->type = DIMSPARAM;
                loc->xfarg = $1;
		loc->n_dims = 1;
		loc->datatype = NCL_int;
		loc->dim_sizes[0] = $4->u.wparam->n_dims;
		sprintf(loc->dim_refs[0],"(%d)",$4->u.wparam->n_dims);
		sprintf(buffer,"%s_dimsizes",$4->name);
		s = _NclLookUpInScope(current->crec,buffer);
                loc->cdef = s->u.centry;
		sprintf(loc->call_string,"%s",buffer);
		$$ = loc;

	}
	| XFARG ':' '/' WPARAM '[' INT ']'
	{
                WParamLoc *loc = NewParamLoc($1);
		char buffer[BUFFSIZE];
		NclSymbol *s;

		DoDimsizes($4);

                loc->type = DIMINDEXPARAM;
                loc->xfarg = $1;
		loc->n_dims = 1;
		loc->dim_sizes[0] = 1;
		sprintf(loc->dim_refs[0],"(1)");
		sprintf(buffer,"%s_dimsizes",$4->name);
		s = _NclLookUpInScope(current->crec,buffer);
                loc->cdef = s->u.centry;
		sprintf(loc->call_string,"&(%s[%d])",buffer,$6);
		$$ = loc;
	}
/*
	| XFARG ':' XFARG
	{
	}
	| XFARG ':' '#' XFARG
	{
	}
	| XFARG ':' '/' XFARG
	{
	}
	| XFARG ':' '/' XFARG '[' INT ']'
	{
	}
*/
;

dspec_list : INT
	{
		$$ = (WDSpecList*)malloc(sizeof(WDSpecList));
		$$->type = INTDIMSIZES;
		$$->u.val = $1;
		$$->next = NULL;
	}
	| WPARAM
	{
		$$ = (WDSpecList*)malloc(sizeof(WDSpecList));
		$$->type = PARAMVALDIMSIZES;
		$$->u.pval.wsym = $1;
		$$->next = NULL;
	}
	| '#' WPARAM
	{
		char buffer[BUFFSIZE];
		WCentry *tmp;
		$$ = (WDSpecList*)malloc(sizeof(WDSpecList));
		$$->type = TOTALPARAMDIMSIZES;
		$$->u.tpval.wsym = $2;
		$$->next = NULL;
		DoTotal($2);
	}
	| '/' WPARAM
	{
		char buffer[BUFFSIZE];
		$$ = (WDSpecList*)malloc(sizeof(WDSpecList));
		$$->type = DIMSPARAMDIMSIZES;
		$$->u.dpval.wsym = $2;
		$$->next = NULL;
		DoDimsizes($2);
	}
	| '/' WPARAM '[' INT ']'
	{
		char buffer[BUFFSIZE];
		$$ = (WDSpecList*)malloc(sizeof(WDSpecList));
		$$->type = DIMINDEXPARAMDIMSIZES;
		$$->u.dipval.wsym = $2;
		$$->u.dipval.index= $4;
		$$->next = NULL;
		DoDimsizes($2);
	}
	| dspec_list ',' WPARAM
	{
		char buffer[BUFFSIZE];
		$$ = (WDSpecList*)malloc(sizeof(WDSpecList));
		$$->type = PARAMVALDIMSIZES;
		$$->u.pval.wsym = $3;
		$$->next = $1;
	}
	| dspec_list ',' '#' WPARAM
	{
		char buffer[BUFFSIZE];
		WCentry *tmp;
		$$ = (WDSpecList*)malloc(sizeof(WDSpecList));
		$$->type = TOTALPARAMDIMSIZES;
		$$->u.pval.wsym = $4;
		$$->next = $1;
		DoTotal($4);
	}
	| dspec_list ',' '/' WPARAM
	{
		char buffer[BUFFSIZE];
		$$ = (WDSpecList*)malloc(sizeof(WDSpecList));
		$$->type = DIMSPARAMDIMSIZES;
		$$->u.dpval.wsym = $4;
		$$->next = $1;
		DoDimsizes($4);
	}
	| dspec_list ',' '/' WPARAM '[' INT ']'
	{
		char buffer[BUFFSIZE];
		$$ = (WDSpecList*)malloc(sizeof(WDSpecList));
		$$->type = DIMINDEXPARAMDIMSIZES;
		$$->u.dipval.wsym = $4;
		$$->u.dipval.index= $6;
		$$->next = $1;
		DoDimsizes($4);
	}
	| dspec_list ',' INT
	{
		char buffer[BUFFSIZE];
		$$ = (WDSpecList*)malloc(sizeof(WDSpecList));
		$$->type = INTDIMSIZES;
		$$->u.val = $3;
		$$->next = $1;
	}
/*
	| XFARG
	{
	}
	| '#' XFARG
        {
        }
        | '/' XFARG
        {
        }
        | '/' XFARG '[' INT ']'
        {
        }
	| dspec_list ',' XFARG 
	{
	}
	| dspec_list ',' '#' XFARG
        {
        }
        | dspec_list ',' '/' XFARG
        {
        }
        | dspec_list ',' '/' XFARG '[' INT ']'
        {
        }
*/
;

vspec : WPARAM
	{
		$$ = (WDSpecList*)malloc(sizeof(WDSpecList));
		$$->type = PARAMVALDIMSIZES;
		$$->u.pval.wsym = $1;
		$$->next = NULL;
	}
	| '#' WPARAM
	{
		char buffer[BUFFSIZE];
		WCentry *tmp;
		$$ = (WDSpecList*)malloc(sizeof(WDSpecList));
		$$->type = TOTALPARAMDIMSIZES;
		$$->u.tpval.wsym = $2;
		$$->next = NULL;
		DoTotal($2);
	}
	| '/' WPARAM
	{
		char buffer[BUFFSIZE];
		$$ = (WDSpecList*)malloc(sizeof(WDSpecList));
		$$->type = DIMSPARAMDIMSIZES;
		$$->u.dpval.wsym = $2;
		$$->next = NULL;
		DoDimsizes($2);
	}
	| '/' WPARAM '[' INT ']'
	{
		char buffer[BUFFSIZE];
		$$ = (WDSpecList*)malloc(sizeof(WDSpecList));
		$$->type = DIMINDEXPARAMDIMSIZES;
		$$->u.dipval.wsym = $2;
		$$->u.dipval.index= $4;
		$$->next = NULL;
		DoDimsizes($2);
	}
	| XFARG
	{
	}
        | '#' XFARG
        {
        }
        | '/' XFARG
        {
        }
        | '/' XFARG '[' INT ']'
        {
        }

;

/*
int_list : INT
	{
		$$ = (IntList*)malloc(sizeof(IntList));
		$$->val = $1;
		$$->next = NULL;
	}
	| int_list ',' INT
	{
		$$ = (IntList*)malloc(sizeof(IntList));
		$$->val = $3;
		$$->next = $1;
	}
*/

omods : {
		$$ = NULL;
	}
	| mods_list
	{
		$$= $1;
	}
;

mods_list : ':' mod
	{
		$2->next = NULL;
		$$ = $2;
	}
	| mods_list ':' mod
	{
		$3->next = $1;
		$$ = $3;
	}
;

mod : MISSING '=' vspec
	{
		$$ = NULL;
	}
	| DIMSIZES '=' vspec
	{
		int i;
		char buffer[BUFFSIZE];
		NclSymbol *s;

		$$ = (WModList*)malloc(sizeof(WModList));
		$$->type = DIMSIZESOF;
		switch($3->type) {
		case PARAMVALDIMSIZES:
			$$->u.dims.sym = $3->u.pval.wsym;
			$$->u.dims.index = -1;
			$$->u.dims.kind = 0;
			if($3->u.pval.wsym->u.wparam->n_dims == 1) {
				if($3->u.pval.wsym->u.wparam->dim_sizes[0] != -1) {
					theparam->typeofdim = 0;
					theparam->n_dims = $3->u.pval.wsym->u.wparam->dim_sizes[0];
					for(i = 0; i < $3->u.pval.wsym->u.wparam->dim_sizes[0]; i++) {
						sprintf(theparam->dim_refs[i],"(%s[%d])",$3->u.pval.wsym->name);
						theparam->dim_sizes[i] = -1;
					}
				} else {
					theparam->n_dims = 1;
					theparam->dim_sizes[0] = -1;
					theparam->typeofdim = 1;
					sprintf(buffer,"%s",$3->u.pval.wsym->name);
					s = _NclLookUpInScope(current->crec,buffer);
					theparam->altdimsref = s->u.centry; 

					sprintf(theparam->altndimref,"%s_dimsizes[0]",$3->u.pval.wsym->name);
				}
			} else if($3->u.pval.wsym->u.wparam->n_dims == -1) {
				theparam->n_dims = 1;
				theparam->dim_sizes[0] = -1;
				theparam->typeofdim = 1;
                                sprintf(buffer,"%s",$3->u.pval.wsym->name);
                                s = _NclLookUpInScope(current->crec,buffer);
                                theparam->altdimsref = s->u.centry; 
                                sprintf(theparam->altndimref,"%s_dimsizes[0]",$3->u.pval.wsym->name); 
			} else {
				fprintf(stderr,"Can not use a multidimensional variable (%s) to specify dimension sizes",$3->u.pval.wsym->name);
			}
		break;
		case DIMSPARAMDIMSIZES:
			$$->u.dims.sym = $3->u.dpval.wsym;
			$$->u.dims.index = -1;
			$$->u.dims.kind = 1;
			theparam->typeofdim = 0;
			if($3->u.dpval.wsym->u.wparam->n_dims != -1) {
				for(i = 0; i <  $3->u.dpval.wsym->u.wparam->n_dims; i++) {
					if($3->u.dpval.wsym->u.wparam->dim_sizes[i] == -1) {
						sprintf(theparam->dim_refs[i],"(%s_dimsizes[%d])",$3->u.dpval.wsym->name);
						theparam->dim_sizes[i] = -1;
					} else {
						sprintf(theparam->dim_refs[i],"(%d)",$3->u.dpval.wsym->u.wparam->dim_sizes[i]);
						theparam->dim_sizes[i] = $3->u.dpval.wsym->u.wparam->dim_sizes[i];
					}
				}
			} else {
				theparam->n_dims = -1;
				theparam->dim_sizes[0] = -1;
				theparam->typeofdim = 1;
                                sprintf(buffer,"%s_dimsizes",$3->u.dpval.wsym->name);
                                s = _NclLookUpInScope(current->crec,buffer);
                                theparam->altdimsref = s->u.centry;
                                sprintf(theparam->altndimref,"%s_ndims",$3->u.dpval.wsym->name);

			}
		break;
		case TOTALPARAMDIMSIZES:
			$$->u.dims.sym = $3->u.tpval.wsym;
			$$->u.dims.index = -1;
			$$->u.dims.kind = 2;
			theparam->dim_sizes[0] = -1;
			theparam->typeofdim = 0;
			theparam->n_dims = 1;
			sprintf(theparam->dim_refs[0],"(%s_total)",$3->u.tpval.wsym->name);
		break;
		case DIMINDEXPARAMDIMSIZES:
			$$->u.dims.sym = $3->u.dipval.wsym;
			$$->u.dims.index = $3->u.dipval.index;
			$$->u.dims.kind = 3;
			theparam->typeofdim = 0;
			theparam->dim_sizes[0] = $3->u.dpval.wsym->u.wparam->dim_sizes[ $3->u.dipval.index];
			theparam->n_dims = 1;
			sprintf(theparam->dim_refs[0],"(%s_dimsizes[%d])",$3->u.dipval.wsym->name,$3->u.dipval.index);
		break;
		} 

		
	}
	| IN
	{
		$$ = NULL;
	}
	| OUT
	{
		$$ = NULL;
	}
	| INOUT
	{
		$$ = NULL;
	}
	| TYPE '=' datatype
	{
		$$ = (WModList*)malloc(sizeof(WModList));
		$$->type = TYPEOF;
		$$->u.type.datatype = $3;
	}
	| TYPE '=' vspec
	{
		$$ = (WModList*)malloc(sizeof(WModList));
		$$->type = TYPEOF;
		switch($3->type) {
		case PARAMVALDIMSIZES:
			$$->u.type.datatype = $3->u.pval.wsym->u.wparam->datatype;
			theparam->datatype = $3->u.pval.wsym->u.wparam->datatype;
		break;
		case DIMSPARAMDIMSIZES:
			$$->u.type.datatype = NCL_int;
			theparam->datatype = NCL_int;
		break;
		case TOTALPARAMDIMSIZES:
			$$->u.type.datatype = NCL_int;
			theparam->datatype = NCL_int;
		break;
		case DIMINDEXPARAMDIMSIZES:
			$$->u.type.datatype = NCL_int;
			theparam->datatype = NCL_int;
		break;
		} 
	}
	| '@' UNDEF '=' vspec
	{
		$$ = NULL;
	}
	| '&' UNDEF '=' vspec
	{
		$$ = NULL;
	}
	| '@' UNDEF
	{
		$$ = NULL;
	}
	| '&' UNDEF
	{
		$$ = NULL;
	}
;


	




%%
void yyerror
#if __STDC__
(const char *s)
#else
(s)
	char *s;
#endif
{
	char error_buffer[1024];
	int i;

	
	fprintf(stderr,"A syntax error occurred while parsing: %s\n",yytext);
	exit(0);
}
