%{
#include <stdio.h>
#include <ncarg/hlu/hluP.h>
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
%token <sym> UNDEF VAR WHILE DO QUIT PROC EPROC NPROC UNDEFFILEVAR
%token <sym> BGIN END FUNC EFUNC NFUNC FDIM IF THEN VBLKNAME FILEVAR
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
%type <src_node> subscript0 eoln opt_eoln
%type <src_node> subscript1 subexpr primary function array error
%type <list> the_list dec_list subscript_list opt_arg_list idn_list
%type <list> block_statement_list resource_list dim_size_list vcreate 
%type <list> arg_list do_stmnt resource vset vget get_resource get_resource_list
%type <sym> datatype dfile_or_und var_or_und
%type <sym> func_identifier proc_identifier 
%%

statement_list :  statement eoln			{	
								int strt;

								if(cmd_line)
									fprintf(stdout,"ncl %d> ",yylineno);
								if(($1 != NULL)&&!(is_error)) {
									_NclPrintTree($1,thefptr);
									strt = _NclTranslate($1,theoptr);
									_NclPrintMachine(strt,-1,theoptr);
									_NclFreeTree($1,is_error);
#ifdef MAKEAPI
									return(0);
#endif
								} else {
									_NclFreeTree($1,is_error);
									is_error = 0;
#ifdef MAKEAPI
									return(1);
#endif
								}
								$$ == $2;
							}
	| statement_list statement eoln			{		
								int strt;

								if(cmd_line)
									fprintf(stdout,"ncl %d> ",yylineno);
								if(($2 != NULL) && !(is_error)) {
									_NclPrintTree($2,thefptr);
									strt = _NclTranslate($2,thefptr);
									_NclPrintMachine(strt,-1,theoptr);
									_NclFreeTree($2,is_error);
									$$ = $3;
#ifdef MAKEAPI
									return(0);
#endif 
								} else {
									_NclFreeTree($2,is_error);
									is_error = 0;
									$$ = $3;
#ifdef MAKEAPI
									return(1);
#endif
								}
							}
	| statement_list RECORD STRING eoln		{ 
/*
* These record statments have to occur here so that the record command isn't written out
* by the scanner. The scanner writes each line when an EOLN is scanned.
*/
								char *tmp_string;
								recfp = fopen(_NhlResolvePath($3),"w"); 
								tmp_string = $3;
								if(recfp != NULL){ 
									rec =1;
								} else {
									NhlPError(WARNING,errno,"Could not open record file");
									rec = 0;
								}
								$$ = $4;
#ifdef MAKEAPI
									return(0);
#endif 
							}
	| RECORD STRING eoln				{ 
								char *tmp_string;
								recfp = fopen(_NhlResolvePath($2),"w"); 
								tmp_string = $2;
								if(recfp != NULL){ 
									rec =1;
								} else {
									NhlPError(WARNING,errno,"Could not open record file");
									rec = 0;
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
										top_level_line = cur_line_number;
										cur_line_number = 0;
										yyin = tmp_file;
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
										top_level_line = cur_line_number;
										cur_line_number = 0;
										yyin = tmp_file;
										loading = 1;
										cur_load_file = (char*)NclMalloc(strlen((char*)$2)+1);
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
								if(cmd_line)
									fprintf(stdout,"ncl %d> ",yylineno);
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
								if(cmd_line)
									fprintf(stdout,"ncl %d> ",yylineno);
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
								char *tmp_string;
								recfp = fopen(_NhlResolvePath($3),"w"); 
								tmp_string = $3;
								if(recfp != NULL){ 
									rec =1;
								} else {
									NhlPError(WARNING,errno,"Could not open record file");
									rec = 0;
								}
								$$ = $1;
							}
	| RECORD STRING eoln				{ 
								char *tmp_string;
								recfp = fopen(_NhlResolvePath($2),"w"); 
								tmp_string = $2;
								if(recfp != NULL){ 
									rec =1;
								} else {
									NhlPError(WARNING,errno,"Could not open record file");
									rec = 0;
								}
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
										top_level_line = cur_line_number;
										cur_line_number = 0;
										yyin = tmp_file;
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
										top_level_line = cur_line_number;
										cur_line_number = 0;
										yyin = tmp_file;
										loading = 1;
										cur_load_file = (char*)NclMalloc((unsigned)strlen((char*)$2)+1);
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

opt_eoln : 		{ $$ = NULL; }
	| eoln		{ yyerrok; $$ = $1;}
;

eoln : EOLN 						{ yyerrok; $$ = _NclMakeEoln();}

statement :     					{ $$ = NULL; }
	| 	assignment 				{$$ = $1; }
	|	procedure 				{$$ = $1;}
	|	function_def 				{$$ = $1;}
	|	procedure_def 				{$$ = $1;}
	| 	block 					{$$ = $1;}
	|	do 					{$$ = $1;}
	| 	conditional				{$$ = $1;}
	|	visblk 					{$$ = $1;}
	|	RETURN expr 				{$$ = _NclMakeReturn($2); }
	| 	QUIT 					{ exit(0);}
	| 	error 					{ $$ = NULL ; ERROR("error in statement"); }
	| 	STOP RECORD				{
/*
* this goes here so that rec gets set to one before eoln comes from scanner.
*/
								if(rec ==1 ) {
									fclose(recfp);
								} 
							}
;

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
										
										$$ = _NclMakeDoFromTo($2,$4, $6, $7);
									}
	| DO identifier '=' expr ',' expr ',' expr do_stmnt END DO	 	{ 
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

procedure : PROC opt_arg_list	{ $$ = _NclMakeProcCall($1,$2,Ncl_BUILTINPROCCALL); }
	| EPROC opt_arg_list	{ $$ = _NclMakeProcCall($1,$2,Ncl_EXTERNALPROCCALL); }
	| NPROC opt_arg_list	{ $$ = _NclMakeProcCall($1,$2,Ncl_PROCCALL); }
	| PROC 			{ $$ = _NclMakeProcCall($1,NULL,Ncl_BUILTINPROCCALL); }
	| EPROC 		{ $$ = _NclMakeProcCall($1,NULL,Ncl_EXTERNALPROCCALL); }
	| NPROC 		{ $$ = _NclMakeProcCall($1,NULL,Ncl_PROCCALL); }
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
							$$ = _NclMakeNewListNode();
							$$->next = NULL;
							$$->node = $1;
						}
	| arg_list ',' expr  			{
						/* ordering not important as long as consistent */
							$$ = _NclMakeNewListNode();
							$$->next = $1;
							$$->node = $3;
						}
;
func_identifier: KEYFUNC UNDEF { _NclNewScope(); $$ = $2; }
;

idn_list: identifier			{
                                                        if($1 != NULL) {
                                                                $$ = _NclMakeNewListNode();
                                                                $$->next = NULL;
                                                                $$->node = $1;
                                                        } else {
                                                                $$ = NULL;
                                                        }
					}
	| idn_list ',' identifier	{

                                                        if($1 == NULL) {
                                                                if($3 != NULL) {
                                                                        $$ = _NclMakeNewListNode();
                                                                        $$->next = NULL;
                                                                        $$->node = $3;
                                                                } else {
                                                                        $$ = NULL;
                                                                }
                                                        } else if($3 != NULL) {
                                                                $$ = _NclMakeNewListNode();
                                                                $$->next = $1;
                                                                $$->node = $3;
                                                        } else {
                                                                $$ = $1;
                                                        }
					}
;
function_def :  func_identifier LP dec_list  RP opt_eoln block		
								{  
									NclSymTableListNode *tmp;
	
									tmp = _NclPopScope();	
									$$ = _NclMakeNFunctionDef(_NclChangeSymbolType($1,NFUNC),$3,NULL,$6,tmp);  
								}
	|  func_identifier LP dec_list  RP opt_eoln LOCAL idn_list opt_eoln block		
								{  
									NclSymTableListNode *tmp;
									tmp = _NclPopScope();	
									$$ = _NclMakeNFunctionDef(_NclChangeSymbolType($1,NFUNC),$3,$7,$9,tmp);  
								}
	| EXTERNAL func_identifier LP dec_list  RP opt_eoln STRING 
								{  
									NclSymTableListNode *tmp;
									tmp = _NclPopScope();	
									$$ = _NclMakeEFunctionDef(_NclChangeSymbolType($2,EFUNC),$4,$7,tmp);  
								}

/*---------------------------------------------ERROR HANDLING BELOW THIS LINE-----------------------------------------------------*/
/*
	| func_identifier LP dec_list RP opt_eoln local_dec_list eoln error {
						ERROR("syntax error: EXPECTING A 'begin'");
	}
	| EXTERNAL func_identifier LP dec_list RP opt_eoln local_dec_list eoln error {
						ERROR("syntax error: EXPECTING A 'begin'");
	}
*/
;

dec_list :			{ $$ = NULL; }
	| the_list		{ $$ = $1; }
; 

the_list: declaration				{	
							$$ = _NclMakeNewListNode();
							$$->next = NULL;
							$$->node = $1;
						}
	| dec_list ',' declaration 		{ 
						/* once again ordering not important as long as it is consistent with function 
							and procedure ordering of argument lists */
							$$ = _NclMakeNewListNode();
							$$->next = $1;
							$$->node = $3;
							  
						}
;

declaration : UNDEF		{ $$ = _NclMakeLocalVarDec($1,NULL,NULL); }
	| UNDEF datatype		{ $$ = _NclMakeLocalVarDec($1,NULL,$2); }
	| UNDEF dim_size_list		{ $$ = _NclMakeLocalVarDec($1,$2,NULL); }
	| UNDEF dim_size_list datatype		{ $$ = _NclMakeLocalVarDec($1,$2,$3); }
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
procedure_def : proc_identifier LP dec_list RP opt_eoln LOCAL idn_list opt_eoln block   {
								NclSymTableListNode *tmp;
                                                                tmp = _NclPopScope();
							
								$$ = _NclMakeProcDef(_NclChangeSymbolType($1,NPROC),$3,$7,$9,tmp);
									
							}
	| proc_identifier LP dec_list RP opt_eoln block   {
								NclSymTableListNode *tmp;
                                                                tmp = _NclPopScope();
								$$ = _NclMakeProcDef(_NclChangeSymbolType($1,NPROC),$3,NULL,$6,tmp);
									
							}
	| EXTERNAL proc_identifier LP dec_list RP opt_eoln STRING	{
								NclSymTableListNode *tmp;
                                                                tmp = _NclPopScope();
								$$ = _NclMakeExternalProcDef(_NclChangeSymbolType($2,EPROC),$4,$7,tmp);
									
							}
;

assignment :  identifier '=' expr		{
						$$ = _NclMakeAssignment($1,$3);
						  
					}
;

identifier : DFILE				{
						$$ = _NclMakeFileRef($1);
					}
	| dfile_or_und FVAR 			{
						NclSymbol *s;
						NclFileInfo *test;
						s = $1;
						test = s->u.file;
					
						s = _NclLookUpInScope($1->u.file->filescope,&(($2)[2]));
						if(s == NULL) {
							s = _NclAddInScope($1->u.file->filescope,&(($2)[2]),UNDEFFILEVAR);
						}
						$$ = _NclMakeFileVarRef($1,s,NULL,Ncl_FILEVAR);
					}
	| dfile_or_und FVAR MARKER		{
						NclSymbol *s;
						NclFileInfo *test;
						s = $1;
						test = s->u.file;
						
						s = _NclLookUpInScope($1->u.file->filescope,&(($2)[2]));
						if(s == NULL) {
							s = _NclAddInScope($1->u.file->filescope,&(($2)[2]),UNDEFFILEVAR);
						}
						$$ = _NclMakeFileVarRef($1,s,NULL,Ncl_FILEVAR);
					}
	| dfile_or_und FVAR LP subscript_list RP MARKER {
						NclSymbol *s;
				
						s = _NclLookUpInScope($1->u.file->filescope,&(($2)[2]));
						if(s == NULL) {
							s = _NclAddInScope($1->u.file->filescope,&(($2)[2]),UNDEFFILEVAR);
						} 
						$$ = _NclMakeFileVarRef($1,s,$4,Ncl_FILEVAR);
					}
	| dfile_or_und FVAR LP subscript_list RP	{	
						NclSymbol *s;
				
						s = _NclLookUpInScope($1->u.file->filescope,&(($2)[2]));
						if(s == NULL) {
							s = _NclAddInScope($1->u.file->filescope,&(($2)[2]),UNDEFFILEVAR);
						}
						$$ = _NclMakeFileVarRef($1,s,$4,Ncl_FILEVAR);
					}
	| DFILE DIMNUM			{
						$$ = _NclMakeFileDimNumRef($1,$2);
					}
	| DFILE DIMNAME			{
						$$ = _NclMakeFileDimNameRef($1,$2);
					}
        | DFILE ATTNAME			{
						$$ = _NclMakeFileAttRef($1,$2,NULL); 
					}
        | DFILE ATTNAME LP subscript_list RP	{
						$$ = _NclMakeFileAttRef($1,$2,$4);
					}
        | var_or_und			{
						$$ = _NclMakeVarRef($1,NULL);
					}
	| var_or_und MARKER			{
						$$ = _NclMakeVarRef($1,NULL);
					}
	| var_or_und LP subscript_list RP MARKER     {
						$$ = _NclMakeVarRef($1,$3);
					}
        | var_or_und LP subscript_list RP {
						$$ = _NclMakeVarRef($1,$3);
					}
	| var_or_und DIMNUM			{
						$$ = _NclMakeVarDimNumRef($1,$2);
					}
        | var_or_und DIMNAME			{
						$$ = _NclMakeVarDimNameRef($1,$2);
					}
	| var_or_und ATTNAME 			{
						$$ = _NclMakeVarAttRef($1,$2,NULL);
					}
	| var_or_und ATTNAME LP subscript_list RP {
						$$ = _NclMakeVarAttRef($1,$2,$4);
					}
	| var_or_und COORD			{
						$$ = _NclMakeVarCoordRef($1,$2,NULL);
					}
	| var_or_und COORD LP subscript_list RP{
						$$ = _NclMakeVarCoordRef($1,$2,$4);
					}
	| OBJNAME			{
						$$ = _NclMakeVarRef($1,NULL);
					}
	| OBJTYPE			{
						$$ = _NclMakeVarRef($1,NULL);
					}
;

var_or_und : UNDEF		{
					$$ = $1;
				}
	| VAR			{
					$$ = $1;
				}
;

dfile_or_und : UNDEF		{
					$$ = $1;
				}
	| DFILE			{		
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
						$$ = _NclMakeIdnExpr(_NclMakeRealExpr($1));
					}
	| INT				{
						$$ = _NclMakeIdnExpr(_NclMakeIntExpr($1));
					}
	| STRING			{
						$$ = _NclMakeIdnExpr(_NclMakeStringExpr($1));
					}
	| identifier				{
						$$ = _NclMakeIdnExpr($1);
					}
	| function			{	
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
						$$ = _NclMakeFuncCall($1,$2,Ncl_BUILTINFUNCCALL);
					}
	| EFUNC opt_arg_list		{
						$$ = _NclMakeFuncCall($1,$2,Ncl_EXTERNFUNCCALL);
					}
	| NFUNC opt_arg_list		{
						$$ = _NclMakeFuncCall($1,$2,Ncl_FUNCCALL);
					}
	| FUNC 				{
						$$ = _NclMakeFuncCall($1,NULL,Ncl_BUILTINFUNCCALL);
					}
	| EFUNC 			{
						$$ = _NclMakeFuncCall($1,NULL,Ncl_EXTERNFUNCCALL);
					}
	| NFUNC 			{
						$$ = _NclMakeFuncCall($1,NULL,Ncl_FUNCCALL);
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
	| expr_list ',' expr 		{ 
						/* array column ordering must be preserved */
                                         
                                                	$1->currentitem->next =  _NclMakeNewListNode();
							$1->currentitem = $1->currentitem->next;
                                                	$1->currentitem->next = NULL;
                                                	$1->currentitem->node = $3;
							$1->nelem++ ;
							$$ = $1;
							 
						}
;
%%
yyerror(s)
	char *s;
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
			NhlPError(FATAL,E_UNKNOWN,"%s: near \\n \n%s\n",s,error_buffer);
		} else {
			sprintf(error_buffer,"%s\n",cur_line_text);
			len = strlen(error_buffer);
			for(i=0; i<cur_line_length-1;i++) sprintf(&(error_buffer[len+i]),"-");
			sprintf(&(error_buffer[len+cur_line_length-1]),"^\n");
			NhlPError(FATAL,E_UNKNOWN,"%s: near %s \n%s\n",s,yytext,error_buffer);
		}
	} else if(is_error == NCL_MAX_ERROR) {
			NhlPError(FATAL,E_UNKNOWN,"Maximum number of errors exceeded, no more will be printed");
	}
	return(0);
}
