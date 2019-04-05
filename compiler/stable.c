/************************************************************************
File Name :		stable.c
Compiler :		MS Visual Studio 2013
Author :		Sanket Patel (040-809-189) & Yasser Noor (040-815-665)
Course :		CST 8152 – Compilers, Lab Section: 011
Assignment :	3
Date :			24 November 2016
Professor :		Sv. Ranev
Purpose :		To store the VID entries
Function List : st_incoffset(), st_setsize(), st_re_arrange_plex(),
st_create(), st_install(), st_lookup(),
st_update_type(), st_get_type(), st_update_value(),
st_destroy(), st_print(), st_store(), st_sort()
**************************************************************************/
#include "stable.h"
#include <string.h>

/* C4001: single line comment in some c library file */
/* C4127: because a constant is used to run the loop */
/* C4100 Warning : we havent done the bonus so the parameters are not used in st_sort */

extern STD sym_table;							/* symbol table */

static void st_incoffset(void);					/* to increment the global symbol table st_offset */
static void st_setsize(void);					/* to set the global symbol size to zero*/
static void st_re_arrange_plex(STD sym_table);	/* to re-arrange the plex pointers when memory is reallocated for char array buffer */

/**********************************************************************

Purpose :			This function is used to create a new symbol table
Author :			Yasser Noor
History/Version :	1.0
Called functions :	malloc(), b_create()
parameters :		st_size: int, the size of the symbol table
Return value :		STD, returns the symbol table descriptor
Algorithm :			if the size is zero or less than zero (out of range)
then, assign teh size of STD to zero and return the
STD.
else, allocate memory for symbol table and character
array to store the token, set offset and size and
retirn the STD

**********************************************************************/
STD st_create(int st_size){
	STD symbol_table_descriptor;
	/* if the size is negetive set the st_size in STD to 0 and
	return the STD*/
	if (st_size <= 0){
		symbol_table_descriptor.st_size = 0;
		return symbol_table_descriptor;
	}
	/* alloc memory for symbol table descriptor */
	if ((symbol_table_descriptor.pstvr = (STVR*)malloc(sizeof(STVR)* st_size)) == NULL){
		symbol_table_descriptor.st_size = 0;
		return symbol_table_descriptor;
	}

	/* create the character array buffer */
	if ((symbol_table_descriptor.plsBD = b_create(SYMBOL_TABLE_BUFFER_SIZE, INCREMENT_FACTOR, ADDITIVE_MODE)) == NULL){
		free(sym_table.pstvr);
		symbol_table_descriptor.st_size = 0;
		return symbol_table_descriptor;
	}

	symbol_table_descriptor.st_offset = 0;

	symbol_table_descriptor.st_size = st_size;

	return symbol_table_descriptor;
}

/****************************************************************************

Purpose :			This function is use to enter a new entry in symbol table
Author :			Sanket Patel & Yasser Noor
History/Version :	1.0
Called functions :	strlen(), st_lookup(), b_addc(), b_rflag(), st_re_arrange_plex(),
st_incoffset()
parameters :		sym_table: STD, the copy of the symbol table created above
lexeme: char*, the lexeme name that would be added in symbol table
type: char, the type of the VID(integer,string or float)
line: int, the line number where the lexeme was found

Return value :		int, returns the st_offset on success and -1 on failure

Algorithm :			check for bad parameters, then lookup if the value exist
in symbol table, if yes return the index otherwise
add the entry to symbol table, made plex point to
the right address in character array,
then set the bits in status field, increment the offset and
return the offset

**********************************************************************/
int st_install(STD sym_table, char *lexeme, char type, int line){
	unsigned int i;
	int look_up_value;;
	unsigned int lexeme_length = strlen(lexeme);
	/* check if the size is zero */
	if (sym_table.st_size == 0)
		return ST_FAIL;


	/* lookup if VID exist in symbol table */
	look_up_value = st_lookup(sym_table, lexeme);

	//printf("Looking up : %s, lookup Value : %d\n", lexeme, look_up_value);

	if (look_up_value != ST_FAIL)
		return look_up_value;


	/*
	* check to make sure, elements are not added
	* more than the size of symbol table
	*/
	if (sym_table.st_offset >= sym_table.st_size)
		return ST_FAIL;

	/*
	* Set the plex to point at the start of lexeme
	* in the character arrray
	*/
	sym_table.pstvr[sym_table.st_offset].plex = b_cbhead(sym_table.plsBD) + b_size(sym_table.plsBD);

	/*
	* copy the lexeme to the character array buffer
	* created one by one, check for re-allocation flag
	* and re-arrange the plex pointers if the memory is
	* re-allocated
	*/
	for (i = 0; i < lexeme_length; i++){

		if (b_addc(sym_table.plsBD, lexeme[i]) == NULL)
			return ST_FAIL;

		if (b_rflag(sym_table.plsBD) == 1)
			st_re_arrange_plex(sym_table);
	}

	b_addc(sym_table.plsBD, EOS);

	/* check again for memory re-allocation in character array buffer */
	if (b_rflag(sym_table.plsBD) == 1)
		st_re_arrange_plex(sym_table);

	/* set the line number for lexeme in symbol table */
	sym_table.pstvr[sym_table.st_offset].o_line = line;

	/* Set the status field to default value */
	sym_table.pstvr[sym_table.st_offset].status_field = DEFAULT_STATUS_FIELD;

	/*
	* Depending on the type of lexeme, give bit field a default type value
	* 01 for float, 10 for int and 11 for string, and set the update flag in
	* LSB to 0 for float and int, 1 for string( as they can be updated once)
	*/
	if (type == INTEGER_CONST){
		sym_table.pstvr[sym_table.st_offset].status_field |= SET_INT;
		sym_table.pstvr[sym_table.st_offset].i_value.int_val = DEFAULT_INT_VALUE;
	}
	else if (type == FLOAT_CONST){
		sym_table.pstvr[sym_table.st_offset].status_field |= SET_FLOAT;
		sym_table.pstvr[sym_table.st_offset].i_value.fpl_val = DEFAULT_FLOAT_VALUE;
	}
	else if (type == STRING_CONST){
		sym_table.pstvr[sym_table.st_offset].status_field |= SET_STRING;
		sym_table.pstvr[sym_table.st_offset].status_field |= UPDATE_FLAG;
		sym_table.pstvr[sym_table.st_offset].i_value.str_offset = DEFAULT_STRING_VALUE;
	}

	st_incoffset();
	return sym_table.st_offset;
}
/**********************************************************************

Purpose :			This function is used to re-arrage the plex pointers
whenever the memory is re-allocated
Author :			Sanket Patel
History/Version :	1.0
Called functions :	B_cbhead(), strlen()
parameters :		sym_table: STD, pass the copy of symbol table
Return value :		none
Algorithm :			get the new cb_head pointing to new memory,
					and make plex pointer of first element to point to
					the cb_head, then follow the last plex pointer to
					assign the new one
**********************************************************************/
static void st_re_arrange_plex(STD sym_table){
	sym_table.pstvr[0].plex = b_cbhead(sym_table.plsBD);
	for (int j = 1; j <= sym_table.st_offset; j++)
		sym_table.pstvr[j].plex = sym_table.pstvr[j - 1].plex + (strlen(sym_table.pstvr[j - 1].plex) + 1);

}

/**********************************************************************

Purpose :			This function is used look-up the lexeme in symbol table
Author :			Yasser Noor
History/Version :	1.0
Called functions :	strcmp()
parameters :		sym_table: STD, pass the copy of symbol table
lexeme: char*, pass the lexeme to look up for
Return value :		int, on success the lexeme index, on failure -1
Algorithm :			look for the lexeme starting from backward, until
the start, if lexeme is found return the index else
return -1

**********************************************************************/
int st_lookup(STD sym_table, char *lexeme){
	int counter = sym_table.st_offset - 1;

	if (sym_table.st_size == 0 || counter == ST_FAIL)
		return ST_FAIL;

	while (counter >= 0 && strcmp(lexeme, sym_table.pstvr[counter].plex) != 0)
		counter--;
	return counter;
}
/**********************************************************************

Purpose :			This function is used to update the type of VID
Author :			Sanket Patel
History/Version :	1.0
Called functions :
parameters :		sym_table: STD, pass the copy of symbol table
vid_offset: int, the index in the symbol table to change the VID for
v_type: char, to know the type of VID passed
Return value :		int, return the vid_offset
Algorithm :			change the type for VID, if its int or float,
but if its string the type cannot be changed and return
-1
**********************************************************************/
int st_update_type(STD sym_table, int vid_offset, char v_type){
	/*
	* if the & of status field and CHK_LSB is not equal to zero
	* then  it is sure that the LSB is 1, which shows that the
	* field has already been updated, so return -1
	*/

	if (sym_table.st_size == 0)
		return ST_FAIL;

	if ((sym_table.pstvr[vid_offset].status_field & CHK_LSB) != 0)
		return ST_FAIL;
	/*
	* Reset the two bits that represent the type indicator to zero
	*/
	sym_table.pstvr[vid_offset].status_field &= RESET_TYPE_INDICATOR;

	/*
	* Depending on what is the type of the variable,
	* set the two field of status field to 01 if float
	* or 10 if int
	*/
	if (v_type == FLOAT_CONST)
		sym_table.pstvr[vid_offset].status_field |= SET_FLOAT;
	else if (v_type == INTEGER_CONST)
		sym_table.pstvr[vid_offset].status_field |= SET_INT;

	/*
	* Finally update the flag bit (LSB) to 1, so as to know the
	* the update has occured once and prevent it again
	*/
	sym_table.pstvr[vid_offset].status_field |= UPDATE_FLAG;
	return vid_offset;
}
/**********************************************************************

Purpose :			used to update the value for VID
Author :			Yasser Noor
History/Version :	1.0
Called functions :
parameters :		sym_table: STD, pass the copy of symbol table
vid_offset: int, the index in the symbol table to update the VID for
i_value: InitialValue, the value to be updated
Return value :		int, return the vid_offset
Algorithm :
**********************************************************************/
int st_update_value(STD sym_table, int vid_offset, InitialValue i_value){

	if (sym_table.st_size == 0)
		return ST_FAIL;

	sym_table.pstvr[vid_offset].i_value = i_value;

	return vid_offset;

}
/**********************************************************************

Purpose :			used to get the type for VID
Author :			Yasser Noor
History/Version :	1.0
Called functions :
parameters :		sym_table: STD, pass the copy of symbol table
vid_offset: int, the index in the symbol table to look the type for
Return value :		char, return the type of VID
Algorithm :			check the bits the in the status field and return the
type of variable accordingly
**********************************************************************/
char st_get_type(STD sym_table, int vid_offset){
	unsigned short get_type;

	if (sym_table.st_size == 0)
		return ST_FAIL;

	/*
	* use bit field operation to check if its String, int or float
	* and then return the type correspondind to the result
	*/
	get_type = sym_table.pstvr[vid_offset].status_field & SET_STRING;
	if (get_type == 6)
		return STRING_CONST;

	get_type = sym_table.pstvr[vid_offset].status_field & SET_FLOAT;
	if (get_type == 2)
		return FLOAT_CONST;

	get_type = sym_table.pstvr[vid_offset].status_field & SET_INT;
	if (get_type == 4)
		return INTEGER_CONST;

	return ST_FAIL;

}
/**********************************************************************

Purpose :			free all the memory allocated
Author :			Sanket Patel
History/Version :	1.0
Called functions :	free(), b_free(), st_setsize()
parameters :		sym_table: STD, pass the copy of symbol table
Return value :
Algorithm :
**********************************************************************/
void st_destroy(STD sym_table){

	if (sym_table.st_size == 0)
		return;

	b_free(sym_table.plsBD);
	free(sym_table.pstvr);
	st_setsize();
}

/**********************************************************************

Purpose :			used to print the VID an its line number where first found
Author :			Yasser Noor
History/Version :	1.0
Called functions :	printf()
parameters :		sym_table: STD, pass the copy of symbol table
Return value :		int, return the st_offset
Algorithm :			loop throufh teh symbol table and print the entry in symbol
table one by one
**********************************************************************/
int st_print(STD sym_table){
	int i;

	if (sym_table.st_size == 0)
		return ST_FAIL;

	if (sym_table.st_offset == 0)
		return ST_FAIL;

	printf("\nSymbol Table\n");
	printf("____________\n\n");
	printf("Line Number Variable Identifier\n");

	for (i = 0; i < sym_table.st_offset; i++)
		printf("%2d%9s %s\n", sym_table.pstvr[i].o_line, "", sym_table.pstvr[i].plex);

	return sym_table.st_offset;
}

/**********************************************************************

Purpose :			used to store the VID in file, if at all failure occurs
in between when the program is run
Author :			Sanket Patel
History/Version :	1.0
Called functions :	fopen(), fclose(), fprintf(), st_get_type()
strlen(), printf()
parameters :		sym_table: STD, pass the copy of symbol table
Return value :		int, return the st_offset
Algorithm :			open a file, loop throgh the symbol table and store
values in file, in the manner
first write the size of symbol table, then
for each lexeme :
-status field
-length of lexeme
-lexeme name
-line number
-initial value
**********************************************************************/
int st_store(STD sym_table){
	FILE * fp;
	int i;
	char type;

	if (sym_table.st_size == 0)
		return ST_FAIL;

	if ((fp = fopen("$stable.ste", "w+")) == NULL){
		fclose(fp);
		return ST_FAIL;
	}

	fprintf(fp, "%d", sym_table.st_size);

	for (i = 0; i < sym_table.st_offset; i++){
		fprintf(fp, " %X %d %s %d ", sym_table.pstvr[i].status_field, strlen(sym_table.pstvr[i].plex), sym_table.pstvr[i].plex, sym_table.pstvr[i].o_line);

		type = st_get_type(sym_table, i);

		if (type == FLOAT_CONST)
			fprintf(fp, "%.2f", sym_table.pstvr[i].i_value.fpl_val);
		else if (type == INTEGER_CONST)
			fprintf(fp, "%d", sym_table.pstvr[i].i_value.int_val);
		if (type == STRING_CONST){
			fprintf(fp, "%d", sym_table.pstvr[i].i_value.str_offset);
		}

	}

	printf("Symbol Table stored.\n");
	fclose(fp);
	return sym_table.st_offset;
}

/**********************************************************************

Purpose :			to sort the lexeme
Author :			Yasser Noor
History/Version :	1.0
Called functions :
parameters :		sym_table: STD, pass the copy of symbol table
s_order: char, specifies to sort in ascending or descending
Return value :
Algorithm :
**********************************************************************/
/* C4100 Warning : we havent done the bonus so the parameters are not used*/
int st_sort(STD sym_table, char s_order) {
	return 0;
}

/**********************************************************************

Purpose :			incremet the global st_offset
Author :			Sanket Patel
History/Version :	1.0
Called functions :
parameters :
Return value :
Algorithm :
**********************************************************************/
static void st_incoffset(void){
	sym_table.st_offset++;
}


/**********************************************************************

Purpose :			set the global symbol table size to zero
Author :			Sanket Patel
History/Version :	1.0
Called functions :
parameters :
Return value :
Algorithm :
**********************************************************************/
static void st_setsize(void){
	sym_table.st_size = 0;
}