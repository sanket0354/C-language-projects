/*********************************************************************************

File Name :		parser.h
Compiler :		MS VISUAL STUDIO 2013
Author :		Sanket Patel (040809189) & Yasser Noor (040815665)
Course :		CST8152
Assignment :	4
Date :			08 - December - 2016
Professor :		Svillen Ranev
Purpose :		Header for parser.c
Function List : 

**********************************************************************************/

#ifndef PARSER_H_
#define PARSER_H_

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef TOKEN_H_
#include "token.h"
#endif


#ifndef STABLE_H_
#include "stable.h"
#endif


/*
* Token attributes defines
*/
#define NO_ATTR -1

#define ELSE 0		
#define IF 1
#define INPUT 2
#define OUTPUT 3
#define PLATYPUS 4
#define REPEAT 5
#define THEN 6
#define USING 7

#define AND 0
#define OR 1

#define EQ 0
#define NE 1
#define GT 2
#define LT 3

#define PLUS 0
#define MINUS 1 
#define MULT 2 
#define DIV 3

#define SCC_OP_T 7

/* lookahead: store the next token */
static Token lookahead;
/* sc_buf: scanner buffer */
static Buffer* sc_buf;

/* synerrno: error counter */
int synerrno;
/* str_LTBL: string literal table*/
extern Buffer * str_LTBL; /*String literal table */

/* line: line numbers */
extern int line;
/* sym_table: symbol table */
extern STD sym_table;

extern Token mlwpar_next_token(Buffer*);		/* find the next token*/
/* kw_table: keyword table */
extern char * kw_table[];

/* Function prototypes for parser.c */
void parser(Buffer*);
void match(int, int );
void syn_eh(int);
void syn_printe();
void gen_incode(char*);
void program(void);
void opt_statements(void);
void statements(void);
void statement(void);
void statements_p(void);
void assignment_statement(void);
void assignment_expression(void);
void selection_statement(void);
void iteration_statement(void);
void input_statement(void);
void output_statement(void);
void variable_list(void);
void variable_list_p(void);
void variable_identifier(void);
void conditional_expression(void);
void logical_OR_expression(void);
void logical_OR_expression_p(void);
void logical_AND_expression(void);
void logical_AND_expression_p(void);
void relational_expression(void);
void primary_a_relational_expression(void);
void primary_a_relational_expression_p(void);
void primary_s_relational_expression(void);
void primary_s_relational_expression_p(void);
void arithmetic_expression(void);
void unary_arithmetic_expression(void);
void additive_arithmetic_expression(void);
void primary_aithmetic_expression(void);
void additive_arithmetic_expression_p(void);
void multiplicative_arithmetic_expression(void);
void multiplicative_arithmetic_expression_p(void);
void string_expression(void);
void primary_string_expression(void);
void string_expression_p(void);
void output_list(void);
#endif
