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
#ifdef YYLMAX
#define MAXTMPSPACE YYLMAX
#else
#define MAXTMPSPACE 512
#endif
char cur_line_text[MAXTMPSPACE];
char *cur_line_text_pos = &(cur_line_text[0]);
int cur_line_length = 0;
int cur_line_number= 1;
int last_line_length = 0;
int ok_to_start_vsblk = 0;
extern int loading;
#include <ctype.h>
#ifdef MAKEAPI
#undef input
#undef unput
char *the_input_buffer = NULL;
char *the_input_buffer_ptr = NULL;
int the_input_buffer_size = 0;
#define input() (( (int)the_input_buffer_ptr >=the_input_buffer_size + (int)the_input_buffer)? (char)0177 : (((int)*the_input_buffer_ptr == 10) ? yylineno++,*the_input_buffer_ptr++:*the_input_buffer_ptr++))

#define unput(c) { yytchar = (c); if(yytchar=='\n')yylineno--;*(--the_input_buffer_ptr) = yytchar; }
#endif /* MAKEAPI */

int rec = 0;
FILE *recfp;
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
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
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
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
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
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
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
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	cur_line_length += yyleng;
	
}
{digit}+\.|{digit}*\.{digit}+ {
		sscanf(yytext,"%lf",&(yylval.real));
		printtoken(REAL,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
		return REAL;
}
{digit}\.?{digit}*[eE][+-]?{digit}+|{digit}?\.{digit}+[eE][+-]?{digit}+ {
	sscanf(yytext,"%le",&(yylval.real));
	printtoken(REAL,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return REAL;
}
{digit}+ {
	sscanf(yytext,"%d",&(yylval.integer));
	printtoken(INT,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return INT;
}
{digit}+(".gt."|".GT.") {
	yyless(yyleng - strlen(".gt."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
        return INT;
}
{digit}+(".lt."|".LT.") {
	yyless(yyleng - strlen(".lt."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
        return INT;
}
{digit}+(".le."|".LE.") {
	yyless(yyleng - strlen(".le."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
        return INT;
}
{digit}+(".eq."|".EQ.") {
	yyless(yyleng - strlen(".eq."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
        return INT;
}
{digit}+(".ge."|".GE.") {
	yyless(yyleng - strlen(".ge."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
        return INT;
}
{digit}+(".ne."|".NE.") {
	yyless(yyleng - strlen(".ne."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
        return INT;
}
{digit}+(".and."|".AND.") {
	yyless(yyleng - strlen(".and."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
        return INT;
}
{digit}+(".or."|".OR.") {
	yyless(yyleng - strlen(".or."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
        return INT;
}
{digit}+(".xor."|".XOR.") {
	yyless(yyleng - strlen(".xor."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
        return INT;
}
{digit}+(".not."|".NOT.") {
	yyless(yyleng - strlen(".not."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
        return INT;
}
{digit}+(".xor."|".XOR.") {
	yyless(yyleng - strlen(".xor."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
        return INT;
}
".gt."|".GT." {
	printtoken(GT,(char*)NULL);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return GT;
}
".lt."|".LT." {
	printtoken(LT,(char*)NULL);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return LT;
} 
".le."|".LE." { 
	printtoken(LE,(char*)NULL);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return LE;
}
".eq."|".EQ." { 
	printtoken(EQ,(char*)NULL);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return EQ;
}
".ge."|".GE." {
	printtoken(GE,(char*)NULL);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return GE;
}
".not."|".not." {
	printtoken(NOT,(char*)NULL);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return NOT;
}
".ne."|".NE." {
	printtoken(NE,(char*)NULL);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return NE;
}
".and."|".AND." {
	printtoken(AND,(char*)NULL);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return AND;
}
".or."|".OR." {
	printtoken(OR,(char*)NULL);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return OR;
}
".xor."|".XOR." {
	printtoken(XOR,(char*)NULL);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return XOR;
}
"->"{alnum}+ {

	strcpy(yylval.str,yytext);
	printtoken(FVAR,yylval.str);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return FVAR;
}
{let}{alnum}* {
        NclSymbol *s;

	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
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
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return DIM_MARKER;
}
[@]{let}{alnum}* {
	char *tmp;

	tmp = (char*)yytext + 1;
	strcpy(yylval.str,tmp);
	printtoken(ATTNAME,yylval.str);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
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
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return DIM;
}
[&]{let}{alnum}* {

	strcpy(yylval.str,yytext);
	printtoken(COORDV,yylval.str);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return COORDV;
/* NOTREACHED */
}
["][^"\n]*["] {
	char *tmp;

	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	tmp = strrchr(yytext,'\"');
	*tmp = '\0';
	tmp = (char*)&(yytext[0]) + 1;
	strncpy(yylval.str,tmp,strlen(tmp)+1);
	printtoken(STRING,yylval.str);
	return STRING;
}
\: { 
	printtoken(yytext[0],(char*)NULL); 
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return(COLON); 
}
\{ { 
	printtoken(yytext[0],(char*)NULL); 
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	printtoken(yytext[0],(char*)NULL); 
	return(LBC); 
}
\} { 
	printtoken(yytext[0],(char*)NULL); 
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return(RBC); 
}
\( { 
	printtoken(yytext[0],(char*)NULL); 
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return(LP); 
}
"(\/" { 
	printtoken(yytext[0],(char*)NULL); 
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return(LPSLSH); 
}
\) { 
	printtoken(yytext[0],(char*)NULL); 
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return(RP); 
}
"\/)" { 
	printtoken(yytext[0],(char*)NULL); 
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return(SLSHRP); 
}
\[ { 
	printtoken(yytext[0],(char*)NULL); 
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return(LBK); 
}
\] { 
	printtoken(yytext[0],(char*)NULL); 
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return(RBK); 
}
\' {
	printtoken(yytext[0],(char*)NULL);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return(MARKER);
}
[\177] {
	return(EOFF);
}
. {  
	printtoken(yytext[0],(char*)NULL);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return(yytext[0]);
}
