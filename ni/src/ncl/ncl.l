%{
/*#include <stdio.h>*/
#include <string.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include "defs.h"
#include "NclData.h"
#include "Symbol.h"
#include "parser.h"
#ifdef DEBUGER
extern void printtoken(
#ifdef NhlNeedProto
	int /* type */,
	char* /* type */
#endif
);
#else
#define printtoken(n,m)
#endif
#define MAXTMPSPACE 512
char *cur_line_text = NULL;
char *cur_line_text_pos = NULL;
int cur_line_length = 0;
int cur_line_number= 1;
int last_line_length = 0;
int ok_to_start_vsblk = 0;
int cur_line_maxsize = 0;
extern int loading;
#include <ctype.h>
#ifdef MAKEAPI
#undef input
#undef unput
char *the_input_buffer = NULL;
char *the_input_buffer_ptr = NULL;
int the_input_buffer_size = 0;
#define input() (( (int)the_input_buffer_ptr >=the_input_buffer_size + (int)the_input_buffer)? (char)0177 : (((int)*the_input_buffer_ptr == 10) ? yylineno++,*the_input_buffer_ptr++:*the_input_buffer_ptr++))

#define unput(c) { yytchar = (c); if(yytchar=='\n')yylineno--;--the_input_buffer_ptr; }
#endif /* MAKEAPI */

int rec = 0;
FILE *recfp;


void CopyIt
#if NhlNeedProto
(char **cltp,int *cll,int ylg,char* ytxt)
#else
(cltp,cll,ylg,ytxt)
char **cltp;
int *cll;
int ylg;
char* ytxt;
#endif
{
	if(*cll + yyleng > cur_line_maxsize -1) {
		cur_line_text = (char*)NclRealloc(cur_line_text,2*cur_line_maxsize);
		*cltp = &(cur_line_text[*cll]);
		cur_line_maxsize = 2 * cur_line_maxsize;
	}
	*cll += yyleng;
        strcpy(*cltp,ytxt);
        (*cltp) += ylg;
	return;
}

%}
%a 4000
%o 4000
let	[_a-zA-Z]
alnum   [_a-zA-Z0-9]
white	[\t ]
digit	[0-9]
%START AA
%%
<AA>[^\n^\177] {
	cur_line_number++;
	BEGIN 0;
	yyless(yyleng-1);
}
<AA>[\177] {
	cur_line_number++;
	BEGIN 0;
}
<AA>[\n] {
	cur_line_number++;
	printtoken(yytext[0],(char*)NULL); 
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	if(rec == 1) {
		fprintf(recfp,"%s",cur_line_text);
	}
	last_line_length = cur_line_length;
	cur_line_length = 0;
	*cur_line_text_pos = '\0';
	cur_line_text_pos = &(cur_line_text[0]);
	return EOLN;
}
[\\][ \t]*[\n] {
	BEGIN AA;
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	if(rec == 1) {
		fprintf(recfp,"%s",cur_line_text);
	}
	last_line_length = cur_line_length;
	cur_line_length = 0;
	cur_line_length = 0;
	*cur_line_text_pos = '\0';
	cur_line_text_pos = &(cur_line_text[0]);
}
\;[^\n]*[\n] { 
	printtoken(yytext[0],(char*)NULL); 
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	if(rec == 1) {
		fprintf(recfp,"%s",cur_line_text);
	}
	last_line_length = cur_line_length;
	cur_line_length = 0;
	*cur_line_text_pos = '\0';
	cur_line_text_pos = &(cur_line_text[0]);
	cur_line_number++;
	return EOLN;
}
[\n] {
	printtoken(EOLN,(char*)NULL);
	cur_line_length += yyleng;
	if(rec == 1) {
		fprintf(recfp,"%s\n",cur_line_text);
	}
	last_line_length = cur_line_length;
	cur_line_length = 0;
	*cur_line_text_pos = '\0';
	cur_line_text_pos = &(cur_line_text[0]);
	BEGIN AA;
	return EOLN;
}
[ \t] { 
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	
}
{digit}+\.|{digit}*\.{digit}+ {
		sscanf(yytext,"%lf",&(yylval.real));
		printtoken(REAL,yytext);
		CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
		return REAL;
}
{digit}\.?{digit}*[eE][+-]?{digit}+|{digit}?\.{digit}+[eE][+-]?{digit}+ {
	sscanf(yytext,"%le",&(yylval.real));
	printtoken(REAL,yytext);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return REAL;
}
{digit}+ {
	sscanf(yytext,"%d",&(yylval.integer));
	printtoken(INT,yytext);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return INT;
}
{digit}+(".gt."|".GT.") {
	yyless(yyleng - strlen(".gt."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
        return INT;
}
{digit}+(".lt."|".LT.") {
	yyless(yyleng - strlen(".lt."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
        return INT;
}
{digit}+(".le."|".LE.") {
	yyless(yyleng - strlen(".le."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
        return INT;
}
{digit}+(".eq."|".EQ.") {
	yyless(yyleng - strlen(".eq."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
        return INT;
}
{digit}+(".ge."|".GE.") {
	yyless(yyleng - strlen(".ge."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
        return INT;
}
{digit}+(".ne."|".NE.") {
	yyless(yyleng - strlen(".ne."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
        return INT;
}
{digit}+(".and."|".AND.") {
	yyless(yyleng - strlen(".and."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
        return INT;
}
{digit}+(".or."|".OR.") {
	yyless(yyleng - strlen(".or."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
        return INT;
}
{digit}+(".xor."|".XOR.") {
	yyless(yyleng - strlen(".xor."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
        return INT;
}
{digit}+(".not."|".NOT.") {
	yyless(yyleng - strlen(".not."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
        return INT;
}
{digit}+(".xor."|".XOR.") {
	yyless(yyleng - strlen(".xor."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
        return INT;
}
".gt."|".GT." {
	printtoken(GT,(char*)NULL);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return GT;
}
".lt."|".LT." {
	printtoken(LT,(char*)NULL);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return LT;
} 
".le."|".LE." { 
	printtoken(LE,(char*)NULL);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return LE;
}
".eq."|".EQ." { 
	printtoken(EQ,(char*)NULL);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return EQ;
}
".ge."|".GE." {
	printtoken(GE,(char*)NULL);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return GE;
}
".not."|".not." {
	printtoken(NOT,(char*)NULL);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return NOT;
}
".ne."|".NE." {
	printtoken(NE,(char*)NULL);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return NE;
}
".and."|".AND." {
	printtoken(AND,(char*)NULL);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return AND;
}
".or."|".OR." {
	printtoken(OR,(char*)NULL);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return OR;
}
".xor."|".XOR." {
	printtoken(XOR,(char*)NULL);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return XOR;
}
"->"{alnum}+ {

	strcpy(yylval.str,yytext);
	printtoken(FVAR,yylval.str);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return FVAR;
}
"$" {
	strcpy(yylval.str,yytext);
	printtoken(FVAR,yylval.str);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return EFSTRING;
}
"->$" {
	strcpy(yylval.str,yytext);
	printtoken(FVAR,yylval.str);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return FSTRING;
}
{let}{alnum}* {
        NclSymbol *s;

	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
        if(( s = _NclLookUp(yytext)) == NULL) 
		s = _NclAddSym(yytext,UNDEF);
       	yylval.sym = s;
       	printtoken(s->type,yytext);
	if((rec == 1)&&(s->type == QUIT)) {
		fclose(recfp);
	}
       	return s->type;
}
[!] {
	strcpy(yylval.str,&(yytext[1]));
	printtoken(DIM_MARKER,yylval.str);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return DIM_MARKER;
}
[@]{let}{alnum}* {
	char *tmp;

	tmp = (char*)yytext + 1;
	strcpy(yylval.str,tmp);
	printtoken(ATTNAME,yylval.str);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return ATTNAME;

}
{let}{alnum}*[ \t]*[\|] {
	char *tmp;

	tmp = strchr(yytext,' ');
	if (tmp != NULL) {
		*tmp = '\0';
	} else {
	   tmp = strchr(yytext,'\t');
	   if (tmp != NULL) {
		*tmp = '\0';
	   } else {
	   	tmp = strchr(yytext,'|');;
	   	if (tmp != NULL) {
			*tmp = '\0';
	   	} 
	   }
	}
	strcpy(yylval.str,yytext);
	printtoken(DIM,yylval.str);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return DIM;
}
[&]{let}{alnum}* {

	strcpy(yylval.str,yytext);
	printtoken(COORDV,yylval.str);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return COORDV;
/* NOTREACHED */
}
["][^"\n]*["] {
	char *tmp;

	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	tmp = strrchr(yytext,'\"');
	*tmp = '\0';
	tmp = (char*)&(yytext[0]) + 1;
	strncpy(yylval.str,tmp,strlen(tmp)+1);
	printtoken(STRING,yylval.str);
	return STRING;
}
\: { 
	printtoken(yytext[0],(char*)NULL); 
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return(COLON); 
}
\{ { 
	printtoken(yytext[0],(char*)NULL); 
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	printtoken(yytext[0],(char*)NULL); 
	return(LBC); 
}
\} { 
	printtoken(yytext[0],(char*)NULL); 
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return(RBC); 
}
\( { 
	printtoken(yytext[0],(char*)NULL); 
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return(LP); 
}
"(\/" { 
	printtoken(yytext[0],(char*)NULL); 
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return(LPSLSH); 
}
\) { 
	printtoken(yytext[0],(char*)NULL); 
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return(RP); 
}
"\/)" { 
	printtoken(yytext[0],(char*)NULL); 
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return(SLSHRP); 
}
\[ { 
	printtoken(yytext[0],(char*)NULL); 
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return(LBK); 
}
\] { 
	printtoken(yytext[0],(char*)NULL); 
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return(RBK); 
}
\' {
	printtoken(yytext[0],(char*)NULL);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return(MARKER);
}
[\177] {
	return(EOFF);
}
. {  
	printtoken(yytext[0],(char*)NULL);
	CopyIt(&cur_line_text_pos,&cur_line_length,yyleng,yytext);
	return(yytext[0]);
}
