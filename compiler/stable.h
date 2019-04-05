/************************************************************************
File Name :		stable.h
Compiler :		MS Visual Studio 2013
Author :		Sanket Patel (040-809-189) & Yasser Noor (040-815-665)
Course :		CST 8152 – Compilers, Lab Section: 011
Assignment :	3
Date :			24 November 2016
Professor :		Sv. Ranev
Purpose :		header for stable.c
Function List :
**************************************************************************/
#ifndef STABLE_H_
#define STABLE_H_

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#define DEFAULT_STATUS_FIELD 0xFFF8			/* 1111 1111 1111 1000 */
#define CHK_LSB     0x0001					/* 0000 0000 0000 0001 */
#define RESET_TYPE_INDICATOR 0xFFF9			/* 1111 1111 1111 1001 */
#define SET_INT 0x0004		  				/* 0000 0000 0000 0100 */
#define SET_FLOAT 0x0002					/* 0000 0000 0000 0010 */
#define UPDATE_FLAG 0x0001					/* 0000 0000 0000 0001 */ 
#define SET_STRING 0x0006					/* 0000 0000 0000 0110 */ 
//#define CHK_STRING 0x0007					/* 0000 0000 0000 0111 */

#define ST_FAIL -1							/* Failure flag for symbol table */
#define ADDITIVE_MODE 'a'					/* additive mode constant */
#define INCREMENT_FACTOR 10					/* increment factor for character array buffer */
#define SYMBOL_TABLE_BUFFER_SIZE 1000		/* initial size for character array buffer */
#define EOS '\0'							/* End of string */
#define INTEGER_CONST 'I'					/* constant for Integer type VID */
#define FLOAT_CONST 'F'						/* constant for float type VID */
#define	STRING_CONST 'S'					/* constant for String type VID */
#define DEFAULT_INT_VALUE 0					/* default value for int VID */
#define DEFAULT_FLOAT_VALUE 0.0				/* default value for float VID */
#define DEFAULT_STRING_VALUE -1				/* default value for string VID */


typedef union InitialValue{
	int int_val;							/* integer variable initial value */
	float fpl_val;							/* floating-point variable initial value */
	int str_offset;							/* string variable initial value */
}InitialValue;

typedef struct SymbolTableVidRecord {
	unsigned short status_field;			/* variable record status field */
	char * plex;							/* pointer to lexeme (VID name) in character array buffer */
	int o_line;								/* line of first occurrence of VID */
	InitialValue i_value;					/* variable initial value */
	size_t reserved;					    /*reserved for future use*/
}STVR;

typedef struct SymbolTableDescriptor {
	STVR *pstvr;							/* pointer to array of STVR */
	int st_size;							/* size in number of STVR elements */
	int st_offset;							/*offset in number of STVR elements */
	Buffer *plsBD;							/* pointer to the lexeme storage buffer descriptor */
} STD;



STD st_create(int st_size);													/* create the symbol table */
int st_install(STD sym_table, char *lexeme, char type, int line);			/* install new entry in symbol table*/
int st_lookup(STD sym_table, char *lexeme);									/* lookup for entry in symbol table*/
int st_update_type(STD sym_table, int vid_offset, char v_type);				/* update the type for VID */
int st_update_value(STD sym_table, int vid_offset, InitialValue i_value);	/* update the value of the VID */
char st_get_type(STD sym_table, int vid_offset);							/* get the type of VID */
void st_destroy(STD sym_table);												/* destroy the symbol table, free all the memory */
int st_print(STD sym_table);												/* print the symbol table */
int st_sort(STD sym_table, char s_order);									/* sort the symbol table */
int st_store(STD sym_table);												/* store the symbol table if there in failure while installing the new entry*/

#endif
