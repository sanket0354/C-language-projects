/*********************************************************************************

File Name :			buffer.c
Compiler :			MS Visual Studio 2013
Author :			Sanket Patel
Course :			CST 8152 - Compilers, Lab Section: 011
Assignment :		1
Date :				September 29 2016
Professor :			Sv. Ranev
Purpose :			To create a buffer which can be of three types :
fixed : once the size assigned it cannot be changed
additive : The size could be increased everytime its needed
by adding a certain amount until buffer is full
multiplicative :  The size could be increased everytime its
needed by multiplying a certain amount
until buffer is full
Function List :		b_create(), b_addc(), b_reset(), b_free(), b_isfull(),
b_size(), b_capacity(), b_setmark(), b_mark(), b_mode(),
b_incfactor(), b_load(), b_isempty(), b_eob(), b_getc(),
b_print(), b_pack(), b_rflag(), b_retract(),
b_retract_to_mark(), b_getcoffset(), b_cbhead()

**********************************************************************************/

#include "buffer.h" /* include the buffer header file */

/***********************************************************************************

Purpose :			Create a buffer by allocating dynamic memory on head based on
parameters passed
Author :			Sanket Patel
History/Version :	1.0
Called functions :	calloc(), malloc(), free()
parameters :		int_capacity:short, should be in between SHRT_MAX and SHRT_MIN
and also cannot be less than zero
inc_factor:char, should be between 0 to 255
o_mode:char, should be 'f', 'm', or 'a'
Return value :		Buffer*
Algorithm :			First calloc memory for single buffer and then malloc memory to store
characters based on initial capacity

if o_mode is 'f' then, assign inc_factor and o_mode both to 0 ( FIXED as defined in macros )
if o_mode is 'a' then, assign inc_factor to value passed in to function
and o_mode to 1 ( ADDITIVE as defined in macros )
if o_mode is 'm' then, assign inc_factor to value passed in to function
and o_mode to -1 ( MULTIPLICATIVE as defined in macros )
if its other than above three then  free the buffer, and return NULL


***********************************************************************************/
Buffer* b_create(short int_capacity, char inc_factor, char o_mode){
	/*
	* temp_buffer : pointer of type buffer , used to point to memory block of size Buffer
	*/
	Buffer* temp_buffer = NULL;

	if (int_capacity < 0){
		return NULL;
	}
	/*
	* An edge case for fixed mode : if the initial capacity is zero and the mode is fixed, then there could be
	* be no increment in size of buffer, and buffer wont be created as the int_capacity
	* is zero, so i just return NULL
	*/
	if (int_capacity == 0 && o_mode == 'f'){
		return NULL;
	}

	if ((temp_buffer = (Buffer*)calloc(1, sizeof(Buffer))) == NULL)
		return NULL;

	if ((temp_buffer->cb_head = (char*)malloc(sizeof(char)* int_capacity)) == NULL){
		free(temp_buffer);
		return NULL;
	}


	if (o_mode == 'f' || (unsigned char)inc_factor == 0){
		temp_buffer->mode = FIXED;
		temp_buffer->inc_factor = 0;
	}
	else if (o_mode == 'f' && (unsigned char)inc_factor != 0){
		temp_buffer->mode = FIXED;
		temp_buffer->inc_factor = 0;
	}
	else if (o_mode == 'a' && (unsigned char)inc_factor > 0){
		temp_buffer->mode = ADDITIVE;
		temp_buffer->inc_factor = (unsigned char)inc_factor;
	}
	else if (o_mode == 'm' && inc_factor >= 1 && inc_factor <= 100) {
		temp_buffer->mode = MULTIPLICATIVE;
		temp_buffer->inc_factor = (unsigned char)inc_factor;
	}
	else{
		/* free the buffer created above if none of the mode matches */
		free(temp_buffer->cb_head);
		free(temp_buffer);
		return NULL;
	}

	temp_buffer->capacity = int_capacity;

	return temp_buffer;
}

/***********************************************************************************

Purpose :			adds the charater to the buffer, and also depending on buffer mode
it increases the size of buffer when needed and realloc dynamic memory
then add the character to it
Author :			Sanket Patel
History/Version :	1.0
Called functions :	realloc(), b_capacity(), b_mode()
parameters :		pBD:pBuffer, it cannot be NULL
symbol:char
Return value :		pBuffer
Algorithm :			First, if there is space in buffer the character is added in buffer
if not, the buffer mode is checked,

now,
if buffer mode is FIXED, then NULL is returned
if buffer mode is ADDITIVE, then the size is increased by increment factor
if buffer mode is MULTIPLICATIVE, the size is increased based on the formula and
increment factor

then, memory is realloced and new character is added

***********************************************************************************/
pBuffer b_addc(pBuffer const pBD, char symbol){
	/*
	* available_space : store the calculated space left in buffer
	*					subtracting the used space from total size (SHRT_MAX)
	* new_increment : stores the calculated space , that would be needed to be incremented
	*				  so that the left over charaters could be included in buffer
	* new_capacity : stores the new capcity by adding new_increment to the
	*				 old capacity of buffer
	*/
	short available_space = 0, new_increment = 0, new_capacity = 0;
	char* temp_cb_head_add;
	if (pBD == NULL)
		return NULL;

	if (pBD->addc_offset == SHRT_MAX)
		return NULL;

	pBD->r_flag = 0;

	temp_cb_head_add = pBD->cb_head;

	/*
	* If the buffer is not full and still has enough capacity to store characters,
	* then just store the characters
	*/
	if ((pBD->addc_offset*sizeof(char)) < (unsigned)pBD->capacity){
		pBD->cb_head[pBD->addc_offset] = symbol;
		//printf("Char in buffer at adding : %c\n", symbol);
		pBD->addc_offset++;
		return pBD;
	}
	/*
	* if the buffer is fixed size we cannot increase the buffer size and we just return NULL
	*/
	if (b_mode(pBD) == FIXED)
		return NULL;

	/*
	* if buffer is of type additive then we calculate the new_capacity,
	* and if the new capacity is less than zero ( that is it should not go beyond the
	* maximum buffer limit which is SHRT_MAX ) , then I return NULL as the size of
	* buffer cannot be increased
	*/
	else if (b_mode(pBD) == ADDITIVE){
		/* check if the new_capacity formed is in the range of Buffer or not*/
		if ((new_capacity = pBD->capacity + (unsigned char)pBD->inc_factor) < 0)
			return NULL;

		//printf("Char in buffer after buffer increased : %c\n", symbol);
		//printf("new capacity after additive : %d\n", new_capacity);

	}

	/*
	* if the buffer is of type multiplicative,
	* then we calculate the new_capacity of the buffer.
	*/
	else if (b_mode(pBD) == MULTIPLICATIVE){
		/*
		* so if the capacity was assigned SHRT_MAX
		* then the capacity could not be increased and hence,
		* I return NULL
		*/
		if (pBD->capacity == SHRT_MAX)
			return NULL;

		available_space = SHRT_MAX - pBD->capacity;
		new_increment = available_space * pBD->inc_factor / 100;
		new_capacity = b_capacity(pBD) + new_increment;

		/*
		* now, this would be an edge case
		* so if there is still some space available and still the new_increment
		* is zero for some edge case calculations, such as
		*
		* so if available space = 6, inc_factor = 15
		* then, according to above formula, new_increment = 0.9
		*
		* but as new_increment is short , it will take zero, which is wrong
		* because there is still space left, so that is why i assign SHRT_MAX as
		* new_capacity
		*/
		if (new_increment == 0)
			new_capacity = SHRT_MAX;
	}

	if ((pBD->cb_head = (char*)realloc(pBD->cb_head, sizeof(char)* new_capacity)) == NULL)
		return NULL;

	pBD->cb_head[pBD->addc_offset] = symbol;
	pBD->capacity = new_capacity;


	if (temp_cb_head_add != pBD->cb_head){
		pBD->r_flag = SET_R_FLAG;
	}

	if (pBD->addc_offset < b_capacity(pBD))
		pBD->addc_offset++;

	return pBD;
}

/***********************************************************************************

Purpose :			reset the buffer, but do not free the memory
Author :			Sanket Patel
History/Version :	1.0
Called functions :
parameters :		pBD:Buffer*, cannot be null
Return value :		int: on success return 1(SUCCESS)
on failure return -1(R_FAIL1)
Algorithm :

***********************************************************************************/
int b_reset(Buffer* const pBD){
	if (pBD == NULL)
		return R_FAIL1;

	pBD->eob = 0;
	pBD->addc_offset = 0;
	pBD->getc_offset = 0;
	pBD->mark_offset = 0;
	pBD->r_flag = 0;

	return SUCCESS;
}


/***********************************************************************************

Purpose :			free cb_head and buffer , if they are not NULL
Author :			Sanket Patel
History/Version :	1.0
Called functions :	exit(), free()
parameters :		pBD:Buffer*
Return value :
Algorithm :

***********************************************************************************/
void b_free(Buffer* const pBD){
	if (pBD == NULL)
		return;

	if (pBD->cb_head != NULL)
		free(pBD->cb_head);

	free(pBD);
}

/***********************************************************************************

Purpose :			check if buffer is full or not
Author :			Sanket Patel
History/Version :	1.0
Called functions :
parameters :
Return value :		int: on failure -1
on success 1 (SUCCESS)
Algorithm :

***********************************************************************************/
int b_isfull(Buffer* const pBD){
	if (pBD == NULL)
		return R_FAIL1;

	if ((signed)(pBD->addc_offset * sizeof(char)) == pBD->capacity)
		return SUCCESS;

	return FAILURE;
}

/***********************************************************************************

Purpose :			to return size of buffer
Author :			Sanket Patel
History/Version :	1.0
Called functions :
parameters :		pBD:Buffer*, it cannot be null
Return value :		short: on failure -1 (R_FAIL1)
on success addc_offset
Algorithm :

***********************************************************************************/
short b_size(Buffer* const pBD){
	if (pBD == NULL)
		return R_FAIL1;

	return  pBD->addc_offset;
}

/***********************************************************************************

Purpose :			It returns capacity of buffer
Author :			Sanket Patel
History/Version :	1.0
Called functions :
parameters :		pBD:Buffer*, it cannot be null
Return value :		short: on failure -1 (R_FAIL1)
on success capacity
Algorithm :

***********************************************************************************/
short b_capacity(Buffer* const pBD){
	if (pBD == NULL)
		return R_FAIL1;

	return pBD->capacity;
}


/***********************************************************************************

Purpose :			sets the mark_offset
Author :			Sanket Patel
History/Version :	1.0
Called functions :
parameters :		pBD:Buffer*, it cannot be NULL
mark:short, it cannot be less than zero and
it would be between size of buffer
Return value :		short :on failure -1 (R_FAIL1)
on success mark_offset
Algorithm :

***********************************************************************************/
short b_setmark(Buffer* const pBD, short mark){
	if (pBD == NULL)
		return R_FAIL1;

	if (mark < 0 || mark > pBD->addc_offset)
		return R_FAIL1;

	if (mark >= 0 && mark <= pBD->addc_offset)
		pBD->mark_offset = mark;

	return pBD->mark_offset;
}

/***********************************************************************************

Purpose :
Author :			Sanket Patel
History/Version :	1.0
Called functions :
parameters :		pBD:Buffer*, it cannot be NULL
Return value :		short: on failure -1 (R_FAIL1)
on success mark_offset
Algorithm :

***********************************************************************************/
short b_mark(Buffer* const pBD){
	if (pBD == NULL)
		return R_FAIL1;

	return pBD->mark_offset;
}

/***********************************************************************************

Purpose :			return the current mode
Author :			Sanket Patel
History/Version :	1.0
Called functions :
parameters :		pBD:Buffer*, it cannot be NULL
Return value :		int: on failure -1 (R_FAIL1)
on success pBD->mode
Algorithm :

***********************************************************************************/
int b_mode(Buffer* const pBD){
	if (pBD == NULL)
		return R_FAIL2;

	return pBD->mode;
}

/***********************************************************************************

Purpose :			return the increment factor
Author :			Sanket Patel
History/Version :	1.0
Called functions :
parameters :		pBD:Buffer*, it cannot be NULL
Return value :		size_t: on failure 256
on success inc_factor
Algorithm :

***********************************************************************************/
size_t b_incfactor(Buffer* const pBD){
	if (pBD == NULL)
		return 256;

	return (unsigned char)pBD->inc_factor;
}

/***********************************************************************************

Purpose :			load each character from file to buffer
Author :			Sanket Patel
History/Version :	1.0
Called functions :	feof(), fgetc(), b_addc()
parameters :		fi:FILE*, it cannot be NULL
pBD:Buffer*, it cannot be NULL
Return value :		int: on failure -1(R_FAIL1)
on success, no of letter loaded
Algorithm :			load one character and check if its the End of file or not,
add it to buffer if its not the EOF
keep doing it until EOF is encountered or buffer becomes full

***********************************************************************************/
int b_load(FILE* const fi, Buffer* const pBD){
	/*
	* no_of_letters: stores number of characters loaded from the file
	* temp_letter: used to store character just loaded from file
	*/
	int no_of_letters = 0;
	char temp_letter;
	if (fi == NULL)
		return R_FAIL1;

	if (pBD == NULL)
		return R_FAIL1;

	/*
	* check for end of file, load the character from file,
	* and add the character to buffer
	*/
	for (;;){
		temp_letter = (char)fgetc(fi);
		if (feof(fi))
			break;
		if ((b_addc(pBD, temp_letter)) == NULL)
			return LOAD_FAIL;
		no_of_letters++;
	}
	return no_of_letters;
}

/***********************************************************************************

Purpose :			check if the buffer is empty
Author :			Sanket Patel
History/Version :	1.0
Called functions :
parameters :		pBD:Buffer*,it cannot be NULL
Return value :		short: on failure -1(R_FAIL1)
on success 1(SUCCESS)
Algorithm :

***********************************************************************************/
int b_isempty(Buffer* const pBD){
	if (pBD == NULL)
		return R_FAIL1;

	if (pBD->addc_offset == 0)
		return SUCCESS;

	return FAILURE;
}

/***********************************************************************************

Purpose :			return the end of buffer flag
Author :			Sanket Patel
History/Version :	1.0
Called functions :
parameters :		pBD:Buffer*,it cannot be NULL
Return value :		int: on failure -1(R_FAIL1)
on success pBD->eob
Algorithm :

***********************************************************************************/
int b_eob(Buffer* const pBD){
	if (pBD == NULL)
		return R_FAIL1;

	return pBD->eob;
}

/***********************************************************************************

Purpose :			returns the character at getc_offset
also set the EOB depending on what it was before
Author :			Sanket Patel
History/Version :	1.0
Called functions :
parameters :		pBD:Buffer*, it cannot be NULL
Return value :		char: on failure -2(R_FAIL2)
on success char at getc_offset
Algorithm :

***********************************************************************************/
char b_getc(Buffer* const pBD){
	/*
	* letter : use to store characte at getc_offset and return it to the method
	*		   calling this method
	*/
	char letter;
	if (pBD == NULL)
		return R_FAIL2;

	if (pBD->getc_offset == pBD->addc_offset){
		pBD->eob = 1;
		return R_FAIL1;
	}

	pBD->eob = 0;
	letter = pBD->cb_head[pBD->getc_offset];
	pBD->getc_offset++;
	//printf("getcoffset getc buffer.c : %d\n", pBD->getc_offset);

	return letter;
}

/***********************************************************************************

Purpose :			print all the characters one by one stored in Buffer
Author :			Sanket Patel
History/Version :	1.0
Called functions :	b_isempty(), b_getc(), b_eob()
parameters :		pBD:Buffer*, it cannot be NULL
Return value :		int: on failure -1(R_FAIL1)
on success number of charaters printed
Algorithm :			load the character from buffer,
check if the loaded character is not end of buffer
if its not eob then print the character

***********************************************************************************/
int b_print(Buffer* const pBD){
	/*
	* temp_getc_offset : temperory variable to store getc_offset
	* no_of_letters : counts number of letters
	* char_to_print : stores the charater that would be printed
	*/
	short temp_getc_offset;
	int num_of_letters = 0;
	char char_to_print;

	if (pBD == NULL){
		printf("The buffer is empty.\n");
		return R_FAIL1;
	}

	if (b_isempty(pBD)){
		printf("The buffer is empty.\n");
		return R_FAIL1;
	}

	temp_getc_offset = pBD->getc_offset;
	pBD->getc_offset = 0;


	do{
		char_to_print = b_getc(pBD);
		if (b_eob(pBD) != 1)
			printf("%c", char_to_print);

		num_of_letters++;

	} while (b_eob(pBD) != 1);

	pBD->getc_offset = temp_getc_offset;

	printf("\n");

	return num_of_letters;
}

/***********************************************************************************

Purpose :			resizes the buffer adding one byte to it
Author :			Sanket Patel
History/Version :	1.0
Called functions :	realloc(),
parameters :		pBD:Buffer*, it cannot be NULL
Return value :		Buffer* : on failure NULL
on Success pBD
Algorithm :			increase the new_capacity by 1 and realloc the memory

***********************************************************************************/
Buffer* b_pack(Buffer* const pBD){
	/*
	* new_capacity : stores the new capacity calculated which would be capacity + 1
	* temp_cb_head : points to the new re-allocated memory which was allocated with new_capacity
	*/
	short new_capacity;
	char* temp_cb_head;
	int* temp_cb_head_add;

	if (pBD == NULL)
		return NULL;

	new_capacity = pBD->addc_offset + 1;

	if (new_capacity < 0)
		return NULL;

	temp_cb_head_add = (int*)pBD->cb_head;
	pBD->r_flag = 0;

	if ((temp_cb_head = (char*)realloc(pBD->cb_head, sizeof(char)* new_capacity)) == NULL)
		return NULL;

	pBD->cb_head = temp_cb_head;
	pBD->capacity = new_capacity;

	if (temp_cb_head_add != (int*)pBD->cb_head)
		pBD->r_flag = 1;

	return pBD;


}

/***********************************************************************************

Purpose :			return reallocation flag
Author :			Sanket Patel
History/Version :	1.0
Called functions :
parameters :		pBD:Buffer*,it cannot be NULL
Return value :		short: on failure -1(R_FAIL1)
on success pBD->r_flag
Algorithm :

***********************************************************************************/
char b_rflag(Buffer* const pBD){
	if (pBD == NULL)
		return R_FAIL1;

	return pBD->r_flag;
}

/***********************************************************************************

Purpose :			Decrement getc_offset
Author :			Sanket Patel
History/Version :	1.0
Called functions :
parameters :		pBD:Buffer*,it cannot be NULL
Return value :		short: on failure -1(R_FAIL1)
on success pBD->getc_offset
Algorithm :

***********************************************************************************/
short b_retract(Buffer* const pBD){
	if (pBD == NULL || pBD->getc_offset < 0)
		return R_FAIL1;

	pBD->getc_offset--;
	return pBD->getc_offset;
}

/***********************************************************************************

Purpose :			sets the getc_offset to mark_offset
Author :			Sanket Patel
History/Version :	1.0
Called functions :
parameters :		pBD:Buffer*,it cannot be NULL
Return value :		short: on failure -1(R_FAIL1)
on success pBD->getc_offset
Algorithm :

***********************************************************************************/
short b_retract_to_mark(Buffer* const pBD){
	if (pBD == NULL)
		return R_FAIL1;

	return pBD->getc_offset = pBD->mark_offset;
}

/***********************************************************************************

Purpose :			return the location of get-character
Author :			Sanket Patel
History/Version :	1.0
Called functions :
parameters :		pBD:Buffer*,it cannot be NULL
Return value :		short: on failure -1(R_FAIL1)
on success pBD->getc_offset
Algorithm :

***********************************************************************************/
short b_getcoffset(Buffer* const pBD){
	if (pBD == NULL)
		return R_FAIL1;

	return pBD->getc_offset;
}

/***********************************************************************************

Purpose :			return the head of character array
Author :			Sanket Patel
History/Version :	1.0
Called functions :
parameters :		pBD:Buffer*,it cannot be NULL
Return value :		char*: on failure NULL
on success pBD->cb_head
Algorithm :

***********************************************************************************/
char* b_cbhead(Buffer* const pBD){
	if (pBD == NULL)
		return NULL;

	return pBD->cb_head;
}