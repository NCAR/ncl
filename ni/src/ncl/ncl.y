%{
#include <stdio.h>
#include <ncarg/hlu/hluP.h>
#include <data_objs/NclData.h>
#include <defs.h>
#include <errno.h>
#include <Symbol.h>
#include <ctype.h>
#include <SrcTree.h>
int scopelevel = 0;
extern int yydebug;
extern char yytext[];
extern int yylineno;
extern FILE *thefptr;
extern FILE *theoptr;
extern int cmd_line;
extern int cur_line_length;
extern int cur_line_number;
extern int last_line_length;
extern char cur_line_text[512];
extern int ok_to_start_vsblk;
#define ERROR(x)  NhlPError(FATAL,E_UNKNOWN,"%s",(x))
int is_error = 0;

extern int rec; 
extern FILE* recfp;
extern FILE* yyin;

int loading = 0;
int top_level_line;
char *cur_load_file = NULL;

%}
%union {
	int integer;
	double real;
	char  str[NCL_MAX_STRING];
	struct _NclSymbol *sym;
	void *src_node;
	struct src_node_list *list;
	struct ncl_rcl_list *array;
}

%token	<void> EOLN 
%token	<void> RP LP RBC LBC RBK LBK COLON ',' '*' SEMI MARKER LPSLSH SLSHRP
%token <integer> INT DIMNUM
%token <real> REAL
%token <str> STRING DIM DIMNAME ATTNAME COORD FVAR 
%token <sym> INTEGER FLOAT LONG DOUBLE BYTE CHARACTER NUMERIC FILETYPE SHORT
%token <sym> UNDEF VAR WHILE DO QUIT PROC EPROC NPROC UNDEFFILEVAR BREAK
%token <sym> BGIN END FUNC EFUNC NFUNC FDIM IF THEN VBLKNAME FILEVAR CONTINUE
%token <sym> DFILE KEYFUNC KEYPROC ELSE EXTERNAL RETURN VSBLKGET LOAD
%token <sym> OBJNAME OBJTYPE RECORD VSBLKCREATE VSBLKSET LOCAL STOP
%token '='
%token OR
%token XOR
%token AND
%token GT 
%token GE 
%token LT 
%token LE 
%token EQ 
%token NE
%token '<' 
%token '>'
%token '+' 
%token '-'
%token '*' 
%token '#' 
%token '/' 
%token '%'
%token '^'
%token UNOP 
%token NOT
%right '='
%left OR XOR
%left AND
%left GT GE LT LE EQ NE
%left '<' '>'
%left '+' '-'
%left '*' '#' '/' '%'
%right '^'
%left UNOP NOT
%type <array> expr_list
%type <src_node> statement assignment 
%type <src_node> procedure function_def procedure_def block do conditional
%type <src_node> visblk statement_list
%type <src_node> declaration identifier expr
%type <src_node> subscript0 break_cont
%type <src_node> subscript1 subexpr primary function array error
%type <list> the_list arg_dec_list subscript_list opt_arg_list 
%type <list> block_statement_list resource_list dim_size_list vcreate 
%type <list> arg_list do_stmnt resource vset vget get_resource get_resource_list
%type <sym> datatype pfname vname
%type <sym> func_identifier proc_identifier 
%%

statement_list :  statement eoln			{	
								int strt;

								if(($1 != NULL)&&!(is_error)) {
									_NclPrintTree($1,thefptr);
									strt = _NclTranslate($1,thefptr);
									_NclTransTerminate();
									_NclPrintMachine(strt,-1,theoptr);
									_NclExecute(strt);
									_NclResetNewSymStack();
									_NclFreeTree();
#ifdef MAKEAPI
									return(0);
#endif
								} else {
									_NclDeleteNewSymStack();
									_NclFreeTree();
									is_error = 0;
#ifdef MAKEAPI
									return(1);
#endif
								}
								if(cmd_line) {
									fprintf(stdout,"ncl %d> ",cur_line_number);
								}
							}
	| statement_list statement eoln			{		
								int strt;

								if(($2 != NULL) && !(is_error)) {
									_NclPrintTree($2,thefptr);
									strt = _NclTranslate($2,thefptr);
									_NclTransTerminate();
									_NclPrintMachine(strt,-1,theoptr);
									_NclExecute(strt);
									_NclResetNewSymStack();
									_NclFreeTree();
#ifdef MAKEAPI
									return(0);
#endif 
								} else {
									_NclDeleteNewSymStack();
									_NclFreeTree();
									is_error = 0;
#ifdef MAKEAPI
									return(1);
#endif
								}
								if(cmd_line)
									fprintf(stdout,"ncl %d> ",cur_line_number);
							}
	| statement_list RECORD STRING eoln		{ 
/*
* These record statments have to occur here so that the record command isn't written out
* by the scanner. The scanner writes each line when an EOLN is scanned.
*/
								recfp = fopen(_NhlResolvePath($3),"w"); 
								if(recfp != NULL){ 
									rec =1;
								} else {
									NhlPError(WARNING,errno,"Could not open record file");
									rec = 0;
								}
								if(cmd_line)
									fprintf(stdout,"ncl %d> ",cur_line_number);
#ifdef MAKEAPI
									return(0);
#endif 
							}
	| RECORD STRING eoln				{ 
								recfp = fopen(_NhlResolvePath($2),"w"); 
								if(recfp != NULL){ 
									rec =1;
								} else {
									NhlPError(WARNING,errno,"Could not open record file");
									rec = 0;
								}
								if(cmd_line) {
									fprintf(stdout,"ncl %d> ",cur_line_number);
								}
#ifdef MAKEAPI
									return(0);
#endif 
							}
/*
* This can not be used through the API because the API has as different way of
* handling I/O which doesn't support "swapping out" the input stream with
* a new one.
*/
	| LOAD STRING eoln				{
#ifndef MAKEAPI
								FILE *tmp_file;
	

								if(loading) {
									NhlPError(WARNING,E_UNKNOWN,"Recursive script file loading is not supported");
								} else {
									tmp_file = fopen(_NhlResolvePath($2),"r");	
									if(tmp_file != NULL) {
										top_level_line = cur_line_number + 1;
										cur_line_number = 0;
										yyin = tmp_file;
										cmd_line = isatty(fileno(tmp_file));
										loading = 1;
										cur_load_file = (char*)NclMalloc((unsigned)strlen($2)+1);
										strcpy(cur_load_file,$2);
									} else {
										NhlPError(WARNING,E_UNKNOWN,"Could not open %s",$2);
										loading = 0;
									}
								}
							}
	| statement_list LOAD STRING eoln				{
								FILE *tmp_file;
	

								if(loading) {
									NhlPError(WARNING,E_UNKNOWN,"Recursive script file loading is not supported");
								} else {
									tmp_file = fopen(_NhlResolvePath($3),"r");	
									if(tmp_file != NULL) {
										top_level_line = cur_line_number + 1;
										cur_line_number = 0;
										yyin = tmp_file;
										loading = 1;
										cur_load_file = (char*)NclMalloc(strlen((char*)$3)+1);
										cmd_line = isatty(fileno(tmp_file));
										strcpy(cur_load_file,$3);
									} else {
										NhlPError(WARNING,E_UNKNOWN,"Could not open %s",$3);
										loading = 0;
									}
								}
#endif
							}
;

block_statement_list : statement eoln { 	
								
								if(cmd_line) {
									if(is_error) {
										_NclDeleteNewSymStack();	
										is_error = 0;
									} else {
										_NclResetNewSymStack();
									}
									fprintf(stdout,"ncl %d> ",cur_line_number);
								}
								if($1 != NULL) {
									$$ = _NclMakeNewListNode();
									$$->next = NULL;
									$$->node = $1;
/*
									$$->next = _NclMakeNewListNode();
									$$->next->node = $2;
									$$->next->next = NULL;
*/
								} else {
									$$ = NULL;
								}
							}
	| block_statement_list statement eoln {	
/*
* This looping is necessary because ordering needs to be maintained for statement_lists
*/
								NclSrcListNode *step;
								if(cmd_line){
									if(is_error) {
										_NclDeleteNewSymStack();	
										is_error = 0;
									} else {
										_NclResetNewSymStack();
									}
									fprintf(stdout,"ncl %d> ",cur_line_number);
								}
								if($1 == NULL) {
									if($2 != NULL) {
										$$ = _NclMakeNewListNode();
										$$->next = NULL;
										$$->node = $2;
/*
										$$->next = _NclMakeNewListNode();
										$$->next->node = $3;
										$$->next->next = NULL;
*/
									} else if($2 == NULL) {
										$$ = NULL;
									}
								} else if($2 != NULL){
									step = $1;
									while(step->next != NULL) {
										step = step->next;
									}
									step->next = _NclMakeNewListNode();
									step= step->next;
/*
									step->next = _NclMakeNewListNode();
									step->next->node = $3;
									step->next->next = NULL;
*/
									step->next = NULL;
									step->node = $2;
									$$ = $1;
								} else {
									$$ = $1;
								}
							}
	| block_statement_list RECORD STRING eoln				{ 
								recfp = fopen(_NhlResolvePath($3),"w"); 
								if(recfp != NULL){ 
									rec =1;
								} else {
									NhlPError(WARNING,errno,"Could not open record file");
									rec = 0;
								}
								$$ = $1;
								if(cmd_line)
									fprintf(stdout,"ncl %d> ",cur_line_number);
							}
	| RECORD STRING eoln				{ 
								recfp = fopen(_NhlResolvePath($2),"w"); 
								if(recfp != NULL){ 
									rec =1;
								} else {
									NhlPError(WARNING,errno,"Could not open record file");
									rec = 0;
								}
								if(cmd_line)
									fprintf(stdout,"ncl %d> ",cur_line_number);
								$$ = NULL;
							}
/*
* This can not be used through the API because the API has as different way of
* handling I/O which doesn't support "swapping out" the input stream with
* a new one.
*/
	| LOAD STRING eoln				{
#ifndef MAKEAPI
								FILE *tmp_file;
	

								if(loading) {
									NhlPError(WARNING,E_UNKNOWN,"Recursive script file loading is not supported");
								} else {
									tmp_file = fopen(_NhlResolvePath($2),"r");	
									if(tmp_file != NULL) {
										top_level_line = cur_line_number +1;
										cur_line_number = 0;
										yyin = tmp_file;
										cmd_line = isatty(fileno(tmp_file));
										loading = 1;
										cur_load_file = (char*)NclMalloc((unsigned)strlen($2)+1);
										strcpy(cur_load_file,$2);
									} else {
										NhlPError(WARNING,E_UNKNOWN,"Could not open %s",$2);
										loading = 0;
									}
								}
#endif
								$$ = NULL;
							}
	| block_statement_list LOAD STRING eoln				{
#ifndef MAKEAPI
								FILE *tmp_file;
	

								if(loading) {
									NhlPError(WARNING,E_UNKNOWN,"Recursive script file loading is not supported");
								} else {
									tmp_file = fopen(_NhlResolvePath($3),"r");	
									if(tmp_file != NULL) {
										top_level_line = cur_line_number +1;
										cur_line_number = 0;
										yyin = tmp_file;
										cmd_line = isatty(fileno(tmp_file));
										loading = 1;
										cur_load_file = (char*)NclMalloc((unsigned)strlen((char*)$3)+1);
										strcpy(cur_load_file,$3);
									} else {
										NhlPError(WARNING,E_UNKNOWN,"Could not open %s",$3);
										loading = 0;
									}
								}
#endif
								$$ = $1;
							}
;

opt_eoln : 		{ /* do nothing */ }
	| eoln		{ 
				yyerrok; 
				if(cmd_line)
                                      fprintf(stdout,"ncl %d> ",cur_line_number);
			}
;

eoln : EOLN 						{ yyerrok; }

statement :     					{ $$ = NULL; }
	| 	assignment 				{
								$$ = $1; 
							}
	|	procedure 				{
								$$ = $1;
							}
	|	function_def 				{
								$$ = $1;
							}
	|	procedure_def 				{
								$$ = $1;
							}
	| 	block 					{
								$$ = $1;
							}
	|	do 					{
								$$ = $1;
							}
	| 	conditional				{
								$$ = $1;
							}
	| 	break_cont				{
								$$ = $1;
							}
	|	visblk 					{
								$$ = $1;
							}
	|	RETURN expr 				{
								$$ = _NclMakeReturn($2); 
							}
	| 	QUIT 					{ 
								exit(0);
							}
	| 	error 					{ 
								$$ = NULL ; 
								ERROR("error in statement"); 
							}
	| 	STOP RECORD				{
/*
* this goes here so that rec gets set to one before eoln comes from scanner.
*/
								if(rec ==1 ) {
									fclose(recfp);
								} 
								$$ = NULL;
							}
;

break_cont : BREAK  {
				$$ = _NclMakeBreakCont($1);
			}
	| CONTINUE {
				$$ = _NclMakeBreakCont($1);
		}

conditional : IF expr then block_statement_list END IF				{  $$ = _NclMakeIfThen($2,$4);  }
	| IF expr then block_statement_list ELSE block_statement_list END IF	{  $$ = _NclMakeIfThenElse($2,$4,$6);  }
	| IF expr then statement END IF						{  
											NclSrcListNode *tmp = NULL;	
											if($4 != NULL) {
												tmp = _NclMakeNewListNode();
												tmp->next = NULL;
												tmp->node = $4;
											} 
											$$ = _NclMakeIfThen($2,tmp);  
										}
	| IF expr then statement ELSE block_statement_list END IF	 	{  
											NclSrcListNode *tmp = NULL;
	
											if($4 != NULL) {
                                                                                                tmp = _NclMakeNewListNode();
                                                                                                tmp->next = NULL;
                                                                                                tmp->node = $4;
											} 
											$$ = _NclMakeIfThenElse($2,tmp,$6);  
										}
											

	| IF expr then statement ELSE statement END IF	 	{  
										NclSrcListNode *tmp = NULL ,*tmp1 = NULL ;
										if($4 != NULL) {
                                                                                        tmp = _NclMakeNewListNode();
                                                                                        tmp->next = NULL;
                                                                                        tmp->node = $4;
										}
										if($6 != NULL) {
                                                                                        tmp1 = _NclMakeNewListNode();
                                                                                        tmp1->next = NULL;
                                                                                        tmp1->node = $6;
										}		
										$$ = _NclMakeIfThenElse($2,tmp,tmp1);  
								}
	| IF expr then block_statement_list ELSE statement END IF	 	{  
											NclSrcListNode *tmp = NULL ;
											if($6 != NULL) {
                                                                                        	tmp = _NclMakeNewListNode();
                                                                                        	tmp->next = NULL;
                                                                                        	tmp->node = $6;
	                                                                                } 
											$$ = _NclMakeIfThenElse($2,$4,tmp);  

										}

;

then : 
	| THEN 
;

visblk :  vcreate	{
				$$ = $1;
			}
	| vset		{
				$$ = $1;
			}
	| vget		{
				$$ = $1;
			}
;
vcreate : VSBLKCREATE identifier identifier resource_list END VSBLKCREATE	{   
									$$ = _NclMakeVis($2,$3,$4,Ncl_VISBLKCREATE);
								}
	| VSBLKCREATE identifier identifier resource END VSBLKCREATE 		{   
									$$ = _NclMakeVis($2,$3,$4,Ncl_VISBLKCREATE); 
								}
	| VSBLKCREATE error 					{
										$$ = NULL;
								}
;

vset :  VSBLKSET identifier resource END VSBLKSET		{
									$$ = _NclMakeVis($2,NULL,$3,Ncl_VISBLKSET); 
								}
	| VSBLKSET identifier resource_list END VSBLKSET	{
									$$ = _NclMakeVis($2,NULL,$3,Ncl_VISBLKSET);
								}
	| VSBLKSET error 					{
										$$ = NULL;
								}
;
vget : VSBLKGET identifier get_resource END VSBLKGET		{
									$$ = _NclMakeVis($2,NULL,$3,Ncl_VISBLKGET); 
								}
	| VSBLKGET identifier get_resource_list END VSBLKGET  	{
									$$ = _NclMakeVis($2,NULL,$3,Ncl_VISBLKGET);
								}
	| VSBLKGET error {
										$$ = NULL;
								}
;


get_resource_list : get_resource eoln		{
							if($1 != NULL) {
						 		$$ = _NclMakeNewListNode();
								$$->next = NULL;
								$$->node = $1;
							} else {
								$$ = NULL;
							}
						}
	| get_resource_list get_resource eoln	{
							if($1 == NULL) {
								if($2 != NULL) {
									$$ = _NclMakeNewListNode();
									$$->next = NULL;
								 	$$->node = $2;
								} else {
									$$ = NULL;
								}
							} else if($2 != NULL) {
								$$ = _NclMakeNewListNode();
								$$->next = $1;
								$$->node = $2;
							} else {
								$$ = $1;
							}
						}
;

get_resource : 					{
							$$ = NULL;
						}
	| STRING COLON UNDEF			{
						 	$$ = _NclMakeGetResource($1,$3);
						}
	| STRING COLON VAR			{
						 	$$ = _NclMakeGetResource($1,$3);
						}
	| error					{	
							$$ = NULL;
						}
;


resource_list : resource eoln			{
							if($1 != NULL) {
						 		$$ = _NclMakeNewListNode();
								$$->next = NULL;
								$$->node = $1;
							} else {
								$$ = NULL;
							}
						}
						
	| resource_list resource eoln		{
							if($1 == NULL) {
								if($2 != NULL) {
									$$ = _NclMakeNewListNode();
									$$->next = NULL;
								 	$$->node = $2;
								} else {
									$$ = NULL;
								}
							} else if($2 != NULL) {
								$$ = _NclMakeNewListNode();
								$$->next = $1;
								$$->node = $2;
							} else {
								$$ = $1;
							}
						}	
;

resource : 					{
							$$ = NULL;
						}
	| STRING COLON expr 			{
						 	$$ = _NclMakeResource($1,$3);
						}
/*
	| STRING COLON RKEY FVAR		{
						 	$$ = NULL;
						}
	| STRING COLON RKEY FVAR LP subscript_list RP	{
					 		$$ = NULL;
						}
	| STRING COLON RKEY COORD		{	
						 	$$ = NULL;
						}
	| STRING COLON RKEY COORD LP subscript_list RP  	{
							 	$$ = NULL;
							}
	| STRING COLON RKEY ATTNAME		{
						 	$$ = NULL;
						}
	| STRING COLON RKEY ATTNAME LP subscript_list RP	{
						 		$$ = NULL;
							}
*/
	| error					{
							$$ = NULL;
						}
;

do_stmnt : block_statement_list						{
										$$ = $1;
									}
	| statement							{
										NclSrcListNode * tmp = NULL;
										if($1 != NULL ) {
											tmp = _NclMakeNewListNode();
											tmp->next = NULL;
											tmp->node = $1;
										}
										$$ = tmp;
									}
;

do : DO identifier '=' expr ',' expr do_stmnt END DO 			 	{ 
										((NclGenericRefNode*)$2)->ref_type = Ncl_WRITEIT;
										
										$$ = _NclMakeDoFromTo($2,$4, $6, $7);
									}
	| DO identifier '=' expr ',' expr ',' expr do_stmnt END DO	 { 
										((NclGenericRefNode*)$2)->ref_type = Ncl_WRITEIT;
										$$ = _NclMakeDoFromToStride($2,$4,$6,$8,$9);
									}
	| DO WHILE expr block_statement_list END DO {   
								$$ = _NclMakeWhile($3,$4);
							}
	| DO WHILE expr statement END DO {   
								NclSrcListNode *tmp = NULL ;
								if($4 != NULL) {
                                                               		tmp = _NclMakeNewListNode();
                                                                       	tmp->next = NULL;
                                                                       	tmp->node = $4;
	                                                        } 
								$$ = _NclMakeWhile($3,tmp);
							}
;

block : BGIN block_statement_list END	{ $$ = _NclMakeBlock($2); }
	| BGIN statement END	{ 
					NclSrcListNode *tmp = NULL ;
					if($2 != NULL) {
                                       		tmp = _NclMakeNewListNode();
                                        	tmp->next = NULL;
                                               	tmp->node = $2;
	                                } 
					$$ = _NclMakeBlock(tmp); 
				}
;

procedure : PROC opt_arg_list	{ 
						NclSrcListNode *step;
						int count = 0;
					
						step = $2;
						while(step != NULL) {
							count++;
							step = step->next;
						}
						if(count != $1->u.procfunc->nargs) {
							is_error += 1;
							NhlPError(FATAL,E_UNKNOWN,"syntax error: procedure %s expects %d arguments, got %d",$1->name,$1->u.procfunc->nargs,count);
							$$ = NULL;
						} else {
							$$ = _NclMakeProcCall($1,$2,Ncl_BUILTINPROCCALL); 
						}
				}
	| EPROC opt_arg_list	{ $$ = _NclMakeProcCall($1,$2,Ncl_EXTERNALPROCCALL); }
	| NPROC opt_arg_list	{ $$ = _NclMakeProcCall($1,$2,Ncl_PROCCALL); }
	| PROC 			{ 
					$$ = _NclMakeProcCall($1,NULL,Ncl_BUILTINPROCCALL); 
				}
	| EPROC 		{ $$ = _NclMakeProcCall($1,NULL,Ncl_EXTERNALPROCCALL); }
	| NPROC 		{ 
						if($1->u.procfunc->nargs != 0) {
							is_error += 1;
							NhlPError(FATAL,E_UNKNOWN,"syntax error: procedure %s expects %d arguments, got %d",$1->name,$1->u.procfunc->nargs,0);
							$$ = NULL;
						} else {
							$$ = _NclMakeProcCall($1,NULL,Ncl_PROCCALL); 
						}
				}
/*---------------------------------------------ERROR HANDLING BELOW THIS LINE-----------------------------------------------------*/
/*
	| identifier opt_arg_list	{ ERROR("syntax error: <identifier> IS NOT A PROCEDURE"); }
*/
	| FUNC opt_arg_list	{ ERROR("syntax error: <identifier> IS A FUNCTION NOT A PROCEDURE"); }
;

opt_arg_list : LP arg_list RP			{ $$ = $2;    }
	| LP RP					{ $$ = NULL;    }
;

arg_list: expr					{ 
						/* Code to check type of expression, iff its and identifier then stamp it with
							the Ncl_PARAMIT tag so the translator can add extra code */
							if(((NclGenericNode*)$1)->kind == Ncl_IDNEXPR) {
								((NclGenericRefNode*)((NclIdnExpr*)$1)->idn_ref_node)->ref_type =
									Ncl_PARAMIT;
							}
							$$ = _NclMakeNewListNode();
							$$->next = NULL;
							$$->node = $1;
						}
	| arg_list ',' expr  			{
							NclSrcListNode * step;
						/* 
						* ordering is important because arguments eventually must be pushed on stack in
						* appropriate order 
						*/
							step = $1;
							while(step->next != NULL) {
								step = step->next;
							}
						/* Code to check type of expression, iff its and identifier then stamp it with
							the Ncl_PARAMIT tag so the translator can add extra code */
							if(((NclGenericNode*)$3)->kind == Ncl_IDNEXPR) {
								((NclGenericRefNode*)((NclIdnExpr*)$3)->idn_ref_node)->ref_type =
									Ncl_PARAMIT;
							}
							step->next = _NclMakeNewListNode();
							step->next->next = NULL;
							step->next->node = $3;
							$$ = $1;
						}
;
func_identifier: KEYFUNC UNDEF { _NclNewScope(); $$ = $2; }
;

local_list: vname {
			/* have to make sure that items in the local list are not added twice !! */
			int lv = _NclGetCurrentScopeLevel();

			if($1->level != lv) {
				_NclAddSym($1->name,UNDEF);
			}
		}
	| pfname {
			int lv = _NclGetCurrentScopeLevel();
			if($1->level != lv) {
				_NclAddSym($1->name,UNDEF);
			}
		}
	| local_list ',' vname {
			int lv = _NclGetCurrentScopeLevel();
			if($3->level != lv) {
				_NclAddSym($3->name,UNDEF);
			}
			}
	| local_list ',' pfname {
			int lv = _NclGetCurrentScopeLevel();
			if($3->level != lv) {
				_NclAddSym($3->name,UNDEF);
			}
			}
;
function_def :  func_identifier LP arg_dec_list  RP opt_eoln block		
								{  
									NclSymTableListNode *tmp;

									if(is_error) {
										_NclDeleteNewSymStack();
										tmp = _NclPopScope();	
										$$ = NULL;
									}else {
										tmp = _NclPopScope();	
										$$ = _NclMakeNFunctionDef(_NclChangeSymbolType($1,NFUNC),$3,$6,tmp);  
									}
								}
	|  func_identifier LP arg_dec_list  RP opt_eoln LOCAL local_list opt_eoln block		
								{  
									NclSymTableListNode *tmp;

									if(is_error) {
										_NclDeleteNewSymStack();
										tmp = _NclPopScope();	
										$$ = NULL;
									}else {
										tmp = _NclPopScope();	
										$$ = _NclMakeNFunctionDef(_NclChangeSymbolType($1,NFUNC),$3,$9,tmp);  
									}
								}
	| EXTERNAL func_identifier LP arg_dec_list  RP opt_eoln STRING 
								{  
									NclSymTableListNode *tmp;
									if(is_error) {
										_NclDeleteNewSymStack();
										tmp = _NclPopScope();	
										$$ = NULL;
									} else {
										tmp = _NclPopScope();	
										$$ = _NclMakeEFunctionDef(_NclChangeSymbolType($2,EFUNC),$4,$7,tmp);  
									}
								}

/*---------------------------------------------ERROR HANDLING BELOW THIS LINE-----------------------------------------------------*/
	| func_identifier error {
			is_error += 1;
/*
* Need to call this before new scope is poped so symbols can be found and freed
*/
			_NclDeleteNewSymStack();
/*
* Need to call function to free scope
*/
			(void)_NclPopScope();
	}
/*
	| EXTERNAL func_identifier LP arg_dec_list RP opt_eoln local_arg_dec_list eoln error {
						ERROR("syntax error: EXPECTING A 'begin'");
	}
*/
;

arg_dec_list :			{ $$ = NULL; }
	| the_list		{ $$ = $1; }
; 

the_list: declaration				{	
							$$ = _NclMakeNewListNode();
							$$->next = NULL;
							$$->node = $1;
						}
	| the_list ',' declaration 		{ 
						/* once again ordering not important as long as it is consistent with function 
							and procedure ordering of argument lists */
							$$ = _NclMakeNewListNode();
							$$->next = $1;
							$$->node = $3;
							  
						}
;

declaration : vname		{ 
					NclSymbol *s;
					int lv = _NclGetCurrentScopeLevel();

					if(($1->type != UNDEF)||($1->level != lv)) {
						s = _NclAddSym($1->name,UNDEF);
					} else {
						s = $1;
					}
					$$ = _NclMakeLocalVarDec(s,NULL,NULL); 
				}
	| vname datatype	{ 
					NclSymbol *s;
					int lv = _NclGetCurrentScopeLevel();

					if(($1->type != UNDEF)||($1->level != lv)) {
						s = _NclAddSym($1->name,UNDEF);
					} else {
						s = $1;
					}
					$$ = _NclMakeLocalVarDec(s,NULL,$2); 
				}
	| vname dim_size_list		{ 
						NclSymbol *s;
						int lv = _NclGetCurrentScopeLevel();
						if(($1->type != UNDEF)||($1->level != lv)) {
							s = _NclAddSym($1->name,UNDEF);
						} else {
							s = $1;
						}

						$$ = _NclMakeLocalVarDec(s,$2,NULL); 
					}
	| vname dim_size_list datatype	{ 
						NclSymbol *s;
						int lv = _NclGetCurrentScopeLevel();
						if(($1->type != UNDEF)||($1->level != lv)) {
							s = _NclAddSym($1->name,UNDEF);
						} else {
							s = $1;
						}

						$$ = _NclMakeLocalVarDec(s,$2,$3); 
					}
	| pfname		{ 
				/* Need to intercept defined names and add them to current scope */
					NclSymbol *s;

					s = _NclAddSym($1->name,UNDEF);
					$$ = _NclMakeLocalVarDec(s,NULL,NULL); 
				}
	| pfname datatype	{ 
					NclSymbol *s;

					s= _NclAddSym($1->name,UNDEF);
					$$ = _NclMakeLocalVarDec(s,NULL,$2); 
				}
	| pfname dim_size_list	{ 
					NclSymbol *s;

					s = _NclAddSym($1->name,UNDEF);
					$$ = _NclMakeLocalVarDec(s,$2,NULL); 
				}
	| pfname dim_size_list datatype	{ 
					NclSymbol *s;

					s = _NclAddSym($1->name,UNDEF);
					$$ = _NclMakeLocalVarDec(s,$2,$3); 
				}
;

pfname : NFUNC		{		
				$$ = $1;
			}
	| EFUNC		{
				$$ = $1;
			}
	| FUNC		{
				$$ = $1;
			}
	| NPROC		{
				$$ = $1;
			}
	| EPROC		{
				$$ = $1;
			}
	| PROC		{
				$$ = $1;
			}
;

datatype : COLON FLOAT		{ $$ = $2; }
	| COLON LONG		{ $$ = $2; }
	| COLON SHORT		{ $$ = $2; }
	| COLON DOUBLE		{ $$ = $2; }
	| COLON CHARACTER	{ $$ = $2; }
	| COLON BYTE		{ $$ = $2; }
	| COLON FILETYPE	{ $$ = $2; }
	| COLON NUMERIC		{ $$ = $2; }
;


dim_size_list : LBK INT RBK		{ 
					/* Dimension size list must be in order */
						$$ = _NclMakeNewListNode();
						$$->next = NULL;
						$$->node = _NclMakeDimSizeNode($2);
						 
					}
	| LBK '*' RBK 			{
						$$ = _NclMakeNewListNode();
						$$->next = NULL;
						$$->node = _NclMakeDimSizeNode(-1);
						 
					}
	| dim_size_list LBK INT RBK 	{   	
						NclSrcListNode *step;
						
						step = $1;
						while(step->next != NULL) 
							step = step->next;
						step->next = _NclMakeNewListNode();
						step->next->next = NULL;
						step->next->node = _NclMakeDimSizeNode($3);
						
					}
	| dim_size_list LBK '*' RBK 	{   
						NclSrcListNode *step;
                                                
                                                step = $1;
                                                while(step->next != NULL) 
                                                        step = step->next;
                                                step->next = _NclMakeNewListNode();
                                                step->next->next = NULL;
                                                step->next->node = _NclMakeDimSizeNode(-1);
						
					}
;

proc_identifier: KEYPROC UNDEF { _NclNewScope(); $$ = $2; }
;
procedure_def : proc_identifier LP arg_dec_list RP opt_eoln LOCAL local_list opt_eoln block   {
								NclSymTableListNode *tmp;
								if(is_error) {
									_NclDeleteNewSymStack();
								}
                                                                tmp = _NclPopScope();
							
								$$ = _NclMakeProcDef(_NclChangeSymbolType($1,NPROC),$3,$9,tmp);
									
							}
	| proc_identifier LP arg_dec_list RP opt_eoln block   {
								NclSymTableListNode *tmp;
								if(is_error) {
									_NclDeleteNewSymStack();
								}
                                                                tmp = _NclPopScope();
								$$ = _NclMakeProcDef(_NclChangeSymbolType($1,NPROC),$3,$6,tmp);
									
							}
	| EXTERNAL proc_identifier LP arg_dec_list RP opt_eoln STRING	{
								NclSymTableListNode *tmp;
								if(is_error) {
									_NclDeleteNewSymStack();
								}
                                                                tmp = _NclPopScope();
								$$ = _NclMakeExternalProcDef(_NclChangeSymbolType($2,EPROC),$4,$7,tmp);
									
							}
	| proc_identifier error {
			is_error += 1;
/*
* Need to call this before new scope is poped so symbols can be found and freed
*/
			_NclDeleteNewSymStack();
/*
* Need to call function to free scope
*/
			(void)_NclPopScope();
	}
;

assignment :  identifier '=' expr		{
						((NclGenericRefNode*)$1)->ref_type = Ncl_WRITEIT;
						$$ = _NclMakeAssignment($1,$3);
						  
					}
;

identifier : vname {
			$$ = _NclMakeVarRef($1,NULL);
		  }
	| vname FVAR 			{
						$$ = _NclMakeFileVarRef($1,&(($2)[2]),NULL,Ncl_FILEVAR);
					}
	| vname FVAR MARKER		{
						$$ = _NclMakeFileVarRef($1,&(($2)[2]),NULL,Ncl_FILEVAR);
					}
	| vname FVAR LP subscript_list RP MARKER {
				
						$$ = _NclMakeFileVarRef($1,&(($2)[2]),$4,Ncl_FILEVAR);
					}
	| vname FVAR LP subscript_list RP	{	
				
						$$ = _NclMakeFileVarRef($1,&(($2)[2]),$4,Ncl_FILEVAR);
					}
	| vname FVAR DIMNUM		{
						$$ = _NclMakeFileVarDimNumRef($1,&(($2)[2]),$3);
					}
	| vname FVAR DIMNAME			{
						$$ = _NclMakeFileVarDimNameRef($1,&(($2)[2]),$3);		
					}
        | vname FVAR ATTNAME			{
						$$ = _NclMakeFileVarAttRef($1,&(($2)[2]),$3,NULL);
					}
        | vname FVAR ATTNAME LP subscript_list RP	{
						$$ = _NclMakeFileVarAttRef($1,&(($2)[2]),$3,$5);
					}
	| vname FVAR COORD			{
						$$ = _NclMakeFileVarCoordRef($1,&(($2)[2]),&(($3)[1]),NULL);
					}
	| vname FVAR COORD LP subscript_list RP{
						$$ = _NclMakeFileVarCoordRef($1,&(($2)[2]),&(($3)[1]),$5);
					}
	| vname DIMNUM			{
						$$ = _NclMakeVarDimNumRef($1,$2);
					}
	| vname DIMNAME			{
						$$ = _NclMakeVarDimNameRef($1,$2);		
					}
        | vname ATTNAME			{
						$$ = _NclMakeVarAttRef($1,$2,NULL);
					}
        | vname ATTNAME LP subscript_list RP	{
						$$ = _NclMakeVarAttRef($1,$2,$4);
					}
	| vname MARKER			{
						$$ = _NclMakeVarRef($1,NULL);
					}
	| vname LP subscript_list RP MARKER     {
						$$ = _NclMakeVarRef($1,$3);
					}
        | vname LP subscript_list RP {
						$$ = _NclMakeVarRef($1,$3);
					}
	| vname COORD			{
						$$ = _NclMakeVarCoordRef($1,&(($2)[1]),NULL);
					}
	| vname COORD LP subscript_list RP{
						$$ = _NclMakeVarCoordRef($1,&(($2)[1]),$4);
					}
;

vname : OBJNAME		{
				$$ = $1;
			}
	| OBJTYPE	{
				$$ = $1;
			}
	| VAR		{
				$$ = $1;
			}
	| UNDEF		{
				$$ = $1;
			}
	| DFILE		{
				$$ = $1;
			}
;
subscript_list :  subscript0 	{
					/* ordering of subscripts must be preserved */
						$$ = _NclMakeNewListNode();
						$$->next = NULL;
						$$->node = $1;
					}
	| LBC subscript1 RBC 		{
					/* ordering of subscripts must be preserved */
                                                $$ = _NclMakeNewListNode();
                                                $$->next = NULL;
                                                $$->node = $2;
					}
	| subscript_list ',' subscript0 {
						NclSrcListNode *step;
                                                
                                                step = $1;
                                                while(step->next != NULL) 
                                                        step = step->next;
                                                step->next = _NclMakeNewListNode();
                                                step->next->next = NULL;
                                                step->next->node = $3;
						
					}
	| subscript_list ',' LBC subscript1 RBC {
						NclSrcListNode *step;
                                         
                                                step = $1;
                                                while(step->next != NULL)
                                                        step = step->next;
                                                step->next = _NclMakeNewListNode();
                                                step->next->next = NULL;
                                                step->next->node = $4;
                                                
					}
;

subscript0:  subexpr 			{  
						$$ = _NclMakeIntSubscript($1,NULL);
						 
					}
	|  DIM subexpr			{ 
						$$ = _NclMakeIntSubscript($2,$1);
						  
					}
;

subscript1:  subexpr	 		{  
						$$ = _NclMakeCoordSubscript($1,NULL);
						 
					}
	|  DIM subexpr			{ 
						$$ = _NclMakeCoordSubscript($2,$1);
						  
					}

;

subexpr: expr				{
						$$ = _NclMakeSingleIndex($1);
					}
	|  COLON 			{
						$$ = _NclMakeRangeIndex(NULL,NULL,NULL);
					}
	| expr COLON expr		{
						$$ = _NclMakeRangeIndex($1,$3,NULL);
					}
	| COLON expr			{
						$$ = _NclMakeRangeIndex(NULL,$2,NULL);
					}
	| expr COLON 			{
						$$ = _NclMakeRangeIndex($1,NULL,NULL);
					}
	| expr COLON expr COLON 	{
						$$ = _NclMakeRangeIndex($1,$3,NULL);
					}
	| COLON expr COLON 		{
						$$ = _NclMakeRangeIndex(NULL,$2,NULL);
					}
	| expr COLON COLON		{
						$$ = _NclMakeRangeIndex($1,NULL,NULL);
					} 
	| expr COLON expr COLON expr	{				
						$$ = _NclMakeRangeIndex($1,$3,$5);
					}
	| expr COLON COLON expr		{				
						$$ = _NclMakeRangeIndex($1,NULL,$4);
					}
	| COLON expr COLON expr		{				
						$$ = _NclMakeRangeIndex(NULL,$2,$4);
					}
	| COLON COLON 			{				
						$$ = _NclMakeRangeIndex(NULL,NULL,NULL);
					}
	| COLON COLON expr		{				
						$$ = _NclMakeRangeIndex(NULL,NULL,$3);
					}
;
expr :  primary				{
						$$ = $1;
					}
	| '-' expr %prec UNOP		{
						$$ = _NclMakeUnaryExpr($2,Ncl_NEGEXPR);
					}
	| NOT expr %prec UNOP		{
						$$ = _NclMakeUnaryExpr($2,Ncl_NOTEXPR);
					}
	| expr '%' expr			{
						$$ = _NclMakeExpr($1,$3,Ncl_MODEXPR);
					}
	| expr OR expr 			{
						$$ = _NclMakeExpr($1,$3,Ncl_OREXPR);
					} 
	| expr AND expr			{			
						$$ = _NclMakeExpr($1,$3,Ncl_ANDEXPR);
					}
	| expr XOR expr			{
						$$ = _NclMakeExpr($1,$3,Ncl_XOREXPR);
					}
	| expr '<' expr			{
						$$ = _NclMakeExpr($1,$3,Ncl_LTSELECTEXPR);
					}
	| expr '>' expr			{
						$$ = _NclMakeExpr($1,$3,Ncl_GTSELECTEXPR);
					}
	| expr '+' expr			{
						$$ = _NclMakeExpr($1,$3,Ncl_PLUSEXPR);
					}
	| expr '-' expr			{
						$$ = _NclMakeExpr($1,$3,Ncl_MINUSEXPR);
					}
	| expr '*' expr			{
						$$ = _NclMakeExpr($1,$3,Ncl_MULEXPR);
					}
	| expr '#' expr			{
						$$ = _NclMakeExpr($1,$3,Ncl_MATMULEXPR);
					}
	| expr '/' expr			{
						$$ = _NclMakeExpr($1,$3,Ncl_DIVEXPR);
					}
	| expr '^' expr			{
						$$ = _NclMakeExpr($1,$3,Ncl_EXPEXPR);
					}
	| expr LE  expr			{
						$$ = _NclMakeExpr($1,$3,Ncl_LEEXPR);
					}
	| expr GE expr			{
						$$ = _NclMakeExpr($1,$3,Ncl_GEEXPR);
					}
	| expr GT expr			{
						$$ = _NclMakeExpr($1,$3,Ncl_GTEXPR);
					}
	| expr LT expr			{
						$$ = _NclMakeExpr($1,$3,Ncl_LTEXPR);
					}
	| expr EQ expr			{
						$$ = _NclMakeExpr($1,$3,Ncl_EQEXPR);
					}
	| expr NE expr			{
						$$ = _NclMakeExpr($1,$3,Ncl_NEEXPR);
					}
;
primary : REAL				{
/*
* Note all of the structures created below the primary rule are special! They
* contain the ref_type field which is used to determine if the item
* is a parameter to a function or a procedure. The LP expr RP is an
* exception
*/
						$$ = _NclMakeIdnExpr(_NclMakeRealExpr($1));
					}
	| INT				{
						$$ = _NclMakeIdnExpr(_NclMakeIntExpr($1));
					}
	| STRING			{
						$$ = _NclMakeIdnExpr(_NclMakeStringExpr($1));
					}
	| function			{	
						$$ = _NclMakeIdnExpr($1);
					}
	| identifier			{
						$$ = _NclMakeIdnExpr($1);
					}
	| array 		 	{
						$$ = _NclMakeIdnExpr($1);
					}
	| LP expr RP			{ 
						$$ = $2;
					}
;
function: FUNC opt_arg_list		{	
						NclSrcListNode *step;
						int count = 0;
					
						step = $2;
						while(step != NULL) {
							count++;
							step = step->next;
						}
						if(count != $1->u.procfunc->nargs) {
							is_error += 1;
							NhlPError(FATAL,E_UNKNOWN,"syntax error: function %s expects %d arguments, got %d",$1->name,$1->u.procfunc->nargs,count);
							$$ = NULL;
						} else {
							$$ = _NclMakeFuncCall($1,$2,Ncl_BUILTINFUNCCALL);
						}
					}
	| EFUNC opt_arg_list		{
						NclSrcListNode *step;
						int count = 0;
					
						step = $2;
						while(step != NULL) {
							count++;
							step = step->next;
						}
						if(count != $1->u.procfunc->nargs) {
							is_error += 1;
							NhlPError(FATAL,E_UNKNOWN,"syntax error: function %s expects %d arguments, got %d",$1->name,$1->u.procfunc->nargs,count);
							$$ = NULL;
						} else {
							$$ = _NclMakeFuncCall($1,$2,Ncl_EXTERNFUNCCALL);
						}
					}
	| NFUNC opt_arg_list		{
						NclSrcListNode *step;
						int count = 0;
					
						step = $2;
						while(step != NULL) {
							count++;
							step = step->next;
						}
						if(count != $1->u.procfunc->nargs) {
							is_error += 1;
							NhlPError(FATAL,E_UNKNOWN,"syntax error: function %s expects %d arguments, got %d",$1->name,$1->u.procfunc->nargs,count);
							$$ = NULL;
						} else {
							$$ = _NclMakeFuncCall($1,$2,Ncl_FUNCCALL);
						}
					}
	| FUNC 				{
						if($1->u.procfunc->nargs != 0) {
							is_error += 1;
							NhlPError(FATAL,E_UNKNOWN,"syntax error: function %s expects %d arguments, got %d",$1->name,$1->u.procfunc->nargs,0);
							$$ = NULL;
						} else {
							$$ = _NclMakeFuncCall($1,NULL,Ncl_BUILTINFUNCCALL);
						}
					}
	| EFUNC 			{
						if($1->u.procfunc->nargs != 0) {
							is_error += 1;
							NhlPError(FATAL,E_UNKNOWN,"syntax error: function %s expects %d arguments, got %d",$1->name,$1->u.procfunc->nargs,0);
							$$ = NULL;
						} else {
							$$ = _NclMakeFuncCall($1,NULL,Ncl_EXTERNFUNCCALL);
						}
					}
	| NFUNC 			{
						if($1->u.procfunc->nargs != 0) {
							is_error += 1;
							NhlPError(FATAL,E_UNKNOWN,"syntax error: function %s expects %d arguments, got %d",$1->name,$1->u.procfunc->nargs,0);
							$$ = NULL;
						} else {
							$$ = _NclMakeFuncCall($1,NULL,Ncl_FUNCCALL);
						}
					}
;
array : LPSLSH expr_list SLSHRP	 { 
							$$ = _NclMakeArrayNode($2);
							 
					}

;
expr_list :  expr				{	
							$$ = _NclMakeRowList();
							$$->list = _NclMakeNewListNode();
							$$->list->next = NULL;
							$$->list->node = $1;
							$$->currentitem= $$->list;
							$$->nelem = 1;
						}
	| expr ',' expr_list   		{ 
						/* pushed on backwards so they can be popped of in correct order*/
                                         
                                                	$3->currentitem->next =  _NclMakeNewListNode();
							$3->currentitem = $3->currentitem->next;
                                                	$3->currentitem->next = NULL;
                                                	$3->currentitem->node = $1;
							$3->nelem++ ;
							$$ = $3;
							 
						}
;
%%
yyerror
#if __STDC__
(char *s)
#else 
(s)
	char *s;
#endif
{
	extern int is_error;
	int i,len;
	char error_buffer[NCL_MAX_STRING];
	

	is_error += 1;

	if(is_error < NCL_MAX_ERROR) {
		if(yytext[0] == '\n') {
			sprintf(error_buffer,"%s\n",cur_line_text);
			len = strlen(error_buffer);
			for(i=0; i<last_line_length-1;i++) sprintf(&(error_buffer[len+i]),"-");
			sprintf(&(error_buffer[len+last_line_length-1]),"^\n");
			if(loading) {
				NhlPError(FATAL,E_UNKNOWN,"%s: line %d in file %s before or near \\n \n%s\n",s,cur_line_number,cur_load_file,error_buffer);
			} else if(cmd_line){
				NhlPError(FATAL,E_UNKNOWN,"%s: line %d before or near \\n \n%s\n",s,cur_line_number-1,error_buffer);
			} else {
				NhlPError(FATAL,E_UNKNOWN,"%s: line %d before or near \\n \n%s\n",s,cur_line_number,error_buffer);
			} 
		} else {
			sprintf(error_buffer,"%s\n",cur_line_text);
			len = strlen(error_buffer);
			for(i=0; i<cur_line_length-1;i++) sprintf(&(error_buffer[len+i]),"-");
			sprintf(&(error_buffer[len+cur_line_length-1]),"^\n");
			if(loading) {
				NhlPError(FATAL,E_UNKNOWN,"%s: line %d in file %s before or near %s \n%s\n",s,cur_line_number,cur_load_file,yytext,error_buffer);
			} else if(cmd_line){
				NhlPError(FATAL,E_UNKNOWN,"%s: line %d before or near %s \n%s\n",s,cur_line_number-1,yytext,error_buffer);
			} else {
				NhlPError(FATAL,E_UNKNOWN,"%s: line %d before or near %s \n%s\n",s,cur_line_number,yytext,error_buffer);
			}
		}
	} else if(is_error == NCL_MAX_ERROR) {
			NhlPError(FATAL,E_UNKNOWN,"Maximum number of errors exceeded, no more will be printed");
	}
	return(0);
}
