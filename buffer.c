/*****************************************************************************
File Name: buffer.c
Compiler: Visual Studio 2013 Compiler
Author: Brandon Keohane
Course: Computer Engineering Technology Lab: 012
Assignment: Assignment 01 - The Buffer
Date: 28th September, 2016
Professor: Svillen Ranev
Purpose: Represents the buffer program of a compiler
Function List:
	-> b_create();
	-> b_addc();
	-> b_reset();
	-> b_free();
	-> b_isfull();
	-> b_size();
	-> b_capacity();
	-> b_setmark();
	-> b_mark();
	-> b_mode();
	-> b_incfactor();
	-> b_load();
	-> b_isempty();
	-> b_eob();
	-> b_getc();
	-> b_print();
	-> b_pack();
	-> b_rflag();
	-> b_retract();
	-> b_retract_to_mark();
	-> b_getcoffset();
	-> b_cbhead();
*****************************************************************************/

#include "buffer.h"

/*****************************************************************************
Purpose: Creates and allocates for a new buffer in memory.
Author: Brandon Keohane
History/Version: v1.0
Called Functions: None
Parameters: Pointer to the buffer structure and character to add
Return Value: Return pointer is assigned to cb_head.
Algorithm: 
		-> Checks parameter for errors, 
		-> If o-mode is not a correct letter than returned error, 
		-> Else set all variables and allocate memory for initial capacity 
		-> Return buffer.  
*****************************************************************************/
Buffer* b_create(short init_capacity, char inc_factor, char o_mode){
	/* Pointer to new buffer structure to be returned once created */
	Buffer* pBuffer;

	/* Unsigned increment factor */
	unsigned char inc = inc_factor;

	/* If initial capacity is greater than short for this os can hold */
	if ((unsigned short)init_capacity > SHRT_MAX){
		return NULL;
	}

	/* If initial capacity is less than 0 */
	if (init_capacity < 0){
		return NULL;
	}

	/* If fixed and initial capacity is 0 */
	if (init_capacity <= 0 && o_mode == 'f'){
		return NULL;
	}

	/* If inc_factorrement factor is less than 0 */
	if (inc < 0){
		return NULL;
	}

	/* if 0 the mode is always going to be fixed *** mode 0 *** inc_factor_factor = 0 */
	if (inc_factor == 0){
		
		if ((pBuffer = (Buffer*)calloc(1, sizeof(Buffer))) == NULL){
			return NULL;
		}

		if ((pBuffer->cb_head = (char*)malloc(init_capacity * sizeof(char))) == NULL){
			/* If error occured there's not enough memory so you should free previous struct */
			free(pBuffer);
			return NULL;
		}

		pBuffer->mode = 0;
		pBuffer->inc_factor = 0;
		pBuffer->capacity = init_capacity;

		return pBuffer;
	}

	switch (o_mode){

		
		/* O-Mode: Fixed */
		case 'f':

			/* mode = 0 inc_factor != 0 */
			if ((pBuffer = (Buffer*)calloc(1, sizeof(Buffer))) == NULL){
				return NULL;
			}

			if ((pBuffer->cb_head = (char*)malloc(init_capacity * sizeof(char))) == NULL){
				/* If error occured there's not enough memory so you should free previous struct */
				free(pBuffer);
				return NULL;
			}

			pBuffer->mode = 0;
			pBuffer->inc_factor = 0;
			pBuffer->capacity = init_capacity;

			return pBuffer;
		break;

		/* O-Mode: Additive self-incrementing */
		case 'a':
			
			/* mode = 1 inc_factor = inc_factor */
			if (inc >= 1 && inc <= 255){

				if ((pBuffer = (Buffer*)calloc(1, sizeof(Buffer))) == NULL){
					return NULL;
				}

				if ((pBuffer->cb_head = (char*)malloc(init_capacity * sizeof(char))) == NULL){
					/* If error occured there's not enough memory so you should free previous struct */
					free(pBuffer);
					return NULL;
				}

				pBuffer->mode = 1;
				pBuffer->inc_factor = inc_factor;
				pBuffer->capacity = init_capacity;

				return pBuffer;
			}
			else {
				return NULL;
			}
		break;

		/* O-Mode: Multiplicative self-incrementing */
		case 'm':

			/* mode = -1 inc_factor = inc_factor */
			if (inc_factor >= 1 && inc_factor <= 100){

				if ((pBuffer = (Buffer*)calloc(1, sizeof(Buffer))) == NULL){
					return NULL;
				}

				if ((pBuffer->cb_head = (char*)malloc(init_capacity * sizeof(char))) == NULL){
					/* If error occured there's not enough memory so you should free previous struct */
					free(pBuffer);
					return NULL;
				}

				pBuffer->mode = -1;
				pBuffer->inc_factor = inc_factor;
				pBuffer->capacity = init_capacity;

				return pBuffer;
			}
			else {
				return NULL;
			}
		break;

	default:
		/* If the wrong operational mode is passed this will be reached */
		return NULL;
	}

}

/*****************************************************************************
Purpose: Function adds character to buffer and reallocates if it needs 
		 more space.
Author: Brandon Keohane
History/Version: v1.0
Called Functions: None
Parameters: Pointer to the buffer structure and character to add
Return Value: Return pointer to buffer or NULL if error.
Algorithm: 
		-> Checks parameter for errors
		-> checks if capacity needs to increase and if it does it reallocates 
		   memory for capacity plus inc factor.
		   -> If memory location changes from original location after realloc
			  throw r flag
		   -> Adds symbol and returns
		-> Else adds symbol and increments addc offset
*****************************************************************************/
pBuffer b_addc(pBuffer const pBD, char symbol){

	/* New capacity - calcuated in additive and multiplicative mode */
	unsigned short nCapacity = 0;

	/* 
	Pointer to the memory location of head before realloc
	Used to check if pointer head location changed after re-allocating 
	*/
	char* memPosition;

	/* Used to prevent reallocation from overriding head and creating dangling pointer */
	char* pTemp;

	/* Reset r_flag to 0 */
	pBD->r_flag = 0;

	/* Enough space in buffer */
	if (!b_isfull(pBD)){

		/* Add symbol */
		pBD->cb_head[pBD->addc_offset] = symbol;
		
		/* Increment offset */
		pBD->addc_offset += 1;

		return pBD;
	}
	else {
		
		/* Not enough space */

		switch (pBD->mode){
			/* Mode: Fixed mode */
			case 0:
				/* Cannot increase size because its fixed, just return null*/
				return NULL;

			/* Mode: Additive self-incrementing */
			case 1:
				nCapacity = pBD->capacity + (unsigned char)pBD->inc_factor;

				/* The buffer can be increased by increment factor */
				if (nCapacity <= SHRT_MAX){

					/* Hold temp memory location of cb_head to check if location moves during realloc */
					memPosition = pBD->cb_head;

					/* Increase the capacity by using realloc */
					if ((pTemp = (char*)realloc(pBD->cb_head, nCapacity)) == NULL){
						return NULL;
					}

					/* Point head at new reallocated pointer */
					if (pTemp != NULL){
						pBD->cb_head = pTemp;
					} 

					/* Check if the memory location has moved for the cb_head */
					if (pBD->cb_head != memPosition){
						pBD->r_flag = SET_R_FLAG;
					}

					/* No dangling pointers */
					memPosition = NULL;

					/* Add new character to the newly allocated buffer */
					pBD->cb_head[pBD->addc_offset] = symbol;

					/* Increment addc */
					pBD->addc_offset += 1;

					/* Set new capacity value */
					pBD->capacity = nCapacity;

					return pBD;
				}
				else {
					return NULL;
				}
				break;

			/* Mode: Multiplicative self-incrementing */
			case -1: 
				/* At max capacity already .. cannot add symbol */
				if (pBD->capacity == SHRT_MAX){
					return NULL;
				}

				/* Calculate new capacity */
				nCapacity = pBD->capacity + (short) ( ((SHRT_MAX - pBD->capacity) * (unsigned char)pBD->inc_factor) / 100);

				/* Hold temp memory location of cb_head to check if location moves during realloc */
				memPosition = pBD->cb_head;

				/* The buffer can be increased by increment factor */
				if ((nCapacity > 0 && nCapacity <= SHRT_MAX) && nCapacity != pBD->capacity){

					/* Increase the capacity by using realloc */
					if ((pTemp = (char*)realloc(pBD->cb_head, nCapacity)) == NULL){
						memPosition = NULL;
						return NULL;
					}

					/* Point head at new reallocated pointer */
					if (pTemp != NULL){
						pBD->cb_head = pTemp;
					}

					/* Set new capacity value */
					pBD->capacity = nCapacity;
				}
				else {
					/* The buffer can not be increased by increment factor, increase the buffer to the max */

					/* Increase the capacity to max by using realloc */
					if ((pTemp = (char*)realloc(pBD->cb_head, SHRT_MAX)) == NULL){
						memPosition = NULL;
						return NULL;
					}

					/* Point head at new reallocated pointer */
					if (pTemp != NULL){
						pBD->cb_head = pTemp;
					}

					/* Set new capacity value */
					pBD->capacity = SHRT_MAX;
				}

				/* Check if the memory location has moved for the cb_head */
				if (pBD->cb_head != memPosition){
					pBD->r_flag = SET_R_FLAG;
				}

				/* No dangling pointers */
				memPosition = NULL;

				/* Add new character to the newly allocated buffer */
				pBD->cb_head[pBD->addc_offset] = symbol;

				/* Increment addc */
				pBD->addc_offset += 1;

				return pBD;
			default: /* Incorrect Mode */
				return NULL;
		}

	} /* Increase capacity else end */
}


/*****************************************************************************
Purpose: Function reinitializes all appropriate data members and sets the 
		 getc_offset back to beggining to make it look like buffer is empty.
Author: Brandon Keohane
History/Version: v1.0
Called Functions: None
Parameters: Pointer to the buffer structure
Return Value: -1 if error, 0 if reset was successful.
Algorithm: 
		-> Checks parameter for errors
		-> Resets getc, addc, mark, rFlag
		-> returns 0
*****************************************************************************/
int b_reset(Buffer* const pBD){

	/* Runtime error */
	if (pBD == NULL) return R_FAIL1;

	pBD->getc_offset = 0;
	pBD->addc_offset = 0;
	pBD->mark_offset = 0;
	pBD->r_flag = 0;
	pBD->eob = 0; /* FIXED */
	
	return 0;
}

/*****************************************************************************
Purpose: Function frees the character buffer and the Buffer structure.
Author: Brandon Keohane
History/Version: v1.0
Called Functions: None
Parameters: Pointer to the buffer structure
Return Value: Void
Algorithm: 
		-> Checks parameter for errors
		-> if not NULL frees character buffer and the buffer structure 
*****************************************************************************/
void b_free(Buffer* const pBD){

	/* Runtime error */
	if (pBD == NULL) return;
	
	/* Free Character Buffer */
	if (pBD->cb_head != NULL)
		free(pBD->cb_head);

	pBD->cb_head = NULL;
	/* Free Buffer Structure */
	free(pBD);
}

/*****************************************************************************
Purpose: Function notifies if the buffer is full or not.
Author: Brandon Keohane
History/Version: v1.0
Called Functions: None
Parameters: Pointer to the buffer structure
Return Value: Returns 1 FULL, 0 NOT FULL, or -1 ERROR:
Algorithm: 
*****************************************************************************/
int b_isfull(Buffer* const pBD){
	return pBD == NULL ? R_FAIL1 : pBD->capacity == pBD->addc_offset ? 1 : 0;
}

/*****************************************************************************
Purpose: Function returns the size used in the buffer so far.
Author: Brandon Keohane
History/Version: v1.0
Called Functions: None
Parameters: Pointer to the buffer structure
Return Value: Returns size or -1 if runtime error occurs
Algorithm: 
*****************************************************************************/
short b_size(Buffer* const pBD){
	/* If structure is NULL */
	if (pBD == NULL) return R_FAIL1;
	/* If addc is out of bounds */
	if (pBD->addc_offset < 0) return R_FAIL1;
	
	return pBD->addc_offset;
}

/*****************************************************************************
Purpose: Function returns the capacity of the character buffer.
Author: Brandon Keohane
History/Version: v1.0
Called Functions: None
Parameters: Pointer to the buffer structure
Return Value: Returns capacity or -1 if runtime error occurs
Algorithm: 
*****************************************************************************/
short b_capacity(Buffer* const pBD){
	/* If structure is NULL */
	if (pBD == NULL) return R_FAIL1;
	/* If capacity is out of bounds */
	if (pBD->capacity < 0) return R_FAIL1;

	return pBD->capacity;
}


/*****************************************************************************
Purpose: Function sets the mark_offset with mark parameter if within range
		 of the size used so far.
Author: Brandon Keohane
History/Version: v1.0
Called Functions: None
Parameters: Pointer to the buffer structure and new mark
Return Value: Returns mark_offset or -1 if runtime error occurs.
Algorithm: 
		-> Checks parameter for errors
		-> Checks if mark is in range
		-> Sets mark and returns the mark value
*****************************************************************************/
short b_setmark(Buffer* const pBD, short mark){
	/* If structure is NULL */
	if (pBD == NULL) return R_FAIL1;
	/* If mark is out of bounds */
	if (mark < 0 || mark > pBD->addc_offset) return R_FAIL1;

	/* Set mark */
	pBD->mark_offset = mark;

	return pBD->mark_offset;
}

/*****************************************************************************
Purpose: Function returns the mark_offset from buffer.
Author: Brandon Keohane
History/Version: v1.0
Called Functions: None
Parameters: Pointer to the buffer structure
Return Value: Returns mark_offset from buffer pointer or -1 if runtime 
			  error occurs.
Algorithm: 
*****************************************************************************/
short b_mark(Buffer* const pBD){
	/* If structure is NULL */
	if (pBD == NULL) return R_FAIL1;
	/* If out of bounds */
	if (pBD->mark_offset < 0 && pBD->mark_offset > pBD->addc_offset) return R_FAIL1;
	
	return pBD->mark_offset;
}

/*****************************************************************************
Purpose: Function returns the mode used by buffer.
Author: Brandon Keohane
History/Version: v1.0
Called Functions: None
Parameters: Pointer to the buffer structure
Return Value: Returns mode value or -2 if runtime error occurs.
Algorithm: 
*****************************************************************************/
int b_mode(Buffer* const pBD){
	/* If NULL returns error, else return mode */
	return pBD == NULL ? R_FAIL2 : pBD->mode;
}

/*****************************************************************************
Purpose: Function returns the inc_factor value of the buffer.
Author: Brandon Keohane
History/Version: v1.0
Called Functions: None
Parameters: Pointer to the buffer structure
Return Value: Returns inc_factor or 256 if there is a runtime error.
Algorithm: 
*****************************************************************************/
size_t b_incfactor(Buffer* const pBD){
	return pBD == NULL ? R_FAIL3 : (unsigned char)pBD->inc_factor;
}

/*****************************************************************************
Purpose: Function loads all characters from the file passed and fills buffer.
Author: Brandon Keohane
History/Version: v1.0
Called Functions: b_addc, feof, getc.
Parameters: Pointer to the buffer structure, and file pointer
Return Value: Returns the number of characters added to the buffer, 
			  -2 if the load failed.
Algorithm: 
		-> Checks parameters for errors
		-> Loads file character by character until empty and adds each to the 
		   character buffer.
		   -> If char is EOF then break
		   -> If could not add character to buffer return LOAD_FAIL
		-> Return number of character read
*****************************************************************************/
int b_load(FILE* const fi, Buffer* const pBD){
	/* Symbol from file to add to buffer */
	char c;
	/* Return value of how many symbols were read from file */
	int iCount = 0;

	/* Runtime error */
	if (pBD == NULL) return LOAD_FAIL;
	if (fi == NULL) return LOAD_FAIL;
	
	/* Loop while the file still has characters */
	while (feof(fi) == 0){

		/* Get next character */
		c = (char)fgetc(fi);

		/* If character is end of file character */
		if (c == EOF) break;

		/* If failed to load the next character */
		if (b_addc(pBD, c) == NULL){
			return LOAD_FAIL;
		}

		/* Add to character read iCount */
		++iCount;
	}

	return iCount;
}

/*****************************************************************************
Purpose: Function returns if the buffer is empty or not
Author: Brandon Keohane
History/Version: v1.0
Called Functions: None
Parameters: Pointer to the buffer structure
Return Value: Returns 1 if empty, 0 if not, -1 if runtime error occurs.
Algorithm: 
*****************************************************************************/
int b_isempty(Buffer* const pBD){
	return pBD == NULL ? R_FAIL1 : pBD->addc_offset == 0 ? 1 : 0;
}

/*****************************************************************************
Purpose: Function returns eob flag for buffer.
Author: Brandon Keohane
History/Version: v1.0
Called Functions: None
Parameters: Pointer to the buffer structure
Return Value: Returns eob or -1 if runtime error occurs.
Algorithm: 
*****************************************************************************/
int b_eob(Buffer* const pBD){
	return pBD == NULL ? R_FAIL1 : pBD->eob == 0 || pBD->eob == 1 ? pBD->eob : R_FAIL1;
}

/*****************************************************************************
Purpose: Gets the character at the position at the getc offset.
Author: Brandon Keohane
History/Version: v1.0
Called Functions: None
Parameters: Pointer to the buffer structure
Return Value: Returns the character at the getc offset position.
Algorithm: 
		-> Checks parameter for errors
		-> if at the end of the buffer set eob to 1 and return R_FAIL1
		-> else get character from position and increment by 1
*****************************************************************************/
char b_getc(Buffer* const pBD){
	/* Symbol retrieved from buffer */
	char c;

	/* Reset flag */
	pBD->eob = 0;

	/* Runtime error */
	if (pBD == NULL) return R_FAIL2;

	if (pBD->getc_offset == pBD->addc_offset){
		/* End of buffer */
		pBD->eob = 1;
		return R_FAIL1;
	}
	else {
		
		/* Get next character from buffer */
		c = pBD->cb_head[pBD->getc_offset];

		/* Increment get offset by 1 */
		pBD->getc_offset += 1;
		
		return c;
	}
}

/*****************************************************************************
Purpose: Function prints the buffer or notifies that the buffer is empty.
Author: Brandon Keohane
History/Version: v1.0
Called Functions: None
Parameters: Pointer to the buffer structure
Return Value: Returns iCount of characters printed or -1 if failure.
Algorithm: 
		-> Checks parameter for errors
		-> If buffer is empty print empty statement and returns
		-> Loop and print all characters to standard out
		-> Return count of characters read
*****************************************************************************/
int b_print(Buffer* const pBD){
	/* Return value of how many characters were printed to standard out */
	int iCount = 0;
	/* Temporary value to hold the position of get character offset */
	short iPos = 0;
	/* Symbol retrieved from buffer to print */
	char c;
	
	/* Runtime error */
	if (pBD == NULL) return R_FAIL1;

	if (b_isempty(pBD)){
		printf("The buffer is empty.\n");
		return 0;
	}

	/* Hold original position */
	iPos = pBD->getc_offset;

	/* Set offset to 0 */
	pBD->getc_offset = 0;

	/* While not the end of the buffer */
	while (b_eob(pBD) != 1){
		
		/* Get character from buffer */
		c = b_getc(pBD);
		/* If get character returned error return with error */
		if (c == R_FAIL2){
			return R_FAIL1;
		}
		/* If flag was not thrown in b_getc */
		if (pBD->eob != 1){
			printf("%c", c);
			++iCount;
		}
	}

	/* Print new line */
	printf("\n");

	/* Restore original offset for getc */
	pBD->getc_offset = iPos;

	return iCount;
}

/*****************************************************************************
Purpose: Function compresses or expands the buffer by
reallocating to the size of offset + 1.
Author: Brandon Keohane
History/Version: v1.0
Called Functions: None
Parameters: Pointer to the buffer structure
Return Value: Returns new buffer or NULL if there is a runtime error.
Algorithm: 
		-> Checks parameter for errors 
		-> Checks for addc + 1 being less than SHRT MAX 
			-> Realloc for addc + 1
		-> Else its at max so just allocate for addc
		-> If memory location changed set r flag
		-> Return buffer pointer
*****************************************************************************/
Buffer* b_pack(Buffer* const pBD){
	/* Temp variable to check if location moved from realloc */
	char* pHead;
	/* Temp to hold new location if reallocation gets incorrect value */
	char* pTemp;

	/* Runtime error */
	if (pBD == NULL || pBD->cb_head == NULL) return NULL;

	/* Original memory location */
	pHead = pBD->cb_head;

	/* Reallocate memory for addc_offset + 1 */
	if (pBD->addc_offset + 1 <= SHRT_MAX){
		/* Allocate memory for addc_offset + 1 */
		if ((pTemp = (char*)realloc(pBD->cb_head, pBD->addc_offset + 1)) == NULL){
			return NULL;
		}

		/* Realloc added one to capacity */
		pBD->capacity = pBD->addc_offset + 1;
	}
	else {
		/* Allocate memory for MAX */
		if ((pTemp = (char*)realloc(pBD->cb_head, pBD->addc_offset)) == NULL){
			return NULL;
		}

		/* Realloc added one to capacity */
		pBD->capacity = pBD->addc_offset;
	}

	/* Point head to new memory */
	if (pTemp != NULL){
		pBD->cb_head = pTemp;
	}

	/* No dangling pointers */
	pTemp = NULL;

	/* Reset eob */
	pBD->eob = 0;

	/* Check if the memory location changed during reallocation memory */
	if (pBD->cb_head != pHead){
		pBD->r_flag = SET_R_FLAG;
	}

	/* No dangling pointers */
	pHead = NULL;

	return pBD;
}

/*****************************************************************************
Purpose: Function returns r_flag value from Buffer parameter.
Author: Brandon Keohane
History/Version: v1.0
Called Functions: None
Parameters: Pointer to the buffer structure
Return Value: Returns r_flag value or -1 if runtime error occurs.
Algorithm: 
*****************************************************************************/
char b_rflag(Buffer* const pBD){
	/* If structure is NULL */
	if (pBD == NULL) return R_FAIL1;
	/* If flag is not a valid value */
	if (pBD->r_flag != 0 && pBD->r_flag != 1) return R_FAIL1;

	return pBD->r_flag;
}

/*****************************************************************************
Purpose: Function decrements the get character offset.
Author: Brandon Keohane
History/Version: v1.0
Called Functions: None
Parameters: Pointer to the buffer structure
Return Value: Returns getc_offset or -1 if runtime error occurs.
Algorithm: 
		-> Checks parameter for errors 
		-> Decrements getc offset by 1 if not at 0 already
		-> Returns getc offset
*****************************************************************************/
short b_retract(Buffer* const pBD){

	/* If structure is NULL */
	if (pBD == NULL) return R_FAIL1;
	/* If cannot retract */
	if (pBD->getc_offset <= 0) return R_FAIL1;

	/* Retract offset */
	pBD->getc_offset -= 1;

	return pBD->getc_offset;
}

/*****************************************************************************
Purpose: Function sets the Buffer getc_offset to the mark_offset.
Author: Brandon Keohane
History/Version: v1.0
Called Functions: None
Parameters: Pointer to the buffer structure
Return Value: Returns getc_offset or -1 if runtime error occurs.
Algorithm: 
		-> Checks parameter for errors
		-> Sets retracts getc to mark if mark is valid
		-> Returns getc offset
*****************************************************************************/
short b_retract_to_mark(Buffer* const pBD){

	/* If structure is NULL */
	if (pBD == NULL) return R_FAIL1;
	/* If the mark is not set */
	if (pBD->mark_offset < 0) return R_FAIL1;

	/* Retract to mark */
	pBD->getc_offset = pBD->mark_offset;

	return pBD->getc_offset;
}

/*****************************************************************************
Purpose: Function returns the getc_offset value.
Author: Brandon Keohane
History/Version: v1.0
Called Functions: None
Parameters: Pointer to the buffer structure
Return Value: Returns getc_offset value or -1 if runtime error occurs.
Algorithm: 
*****************************************************************************/
short b_getcoffset(Buffer* const pBD){
	/* If structure is NULL */
	if (pBD == NULL) return R_FAIL1;
	/* If offset is invalid value */
	if (pBD->getc_offset < 0) return R_FAIL1;

	return pBD->getc_offset;
}

/*****************************************************************************
Purpose: Function returns the character buffer.
Author: Brandon Keohane
History/Version: v1.0
Called Functions: None
Parameters: Pointer to the buffer structure
Return Value: Return cb_head or NULL if runtime error occurs.
Algorithm: 
*****************************************************************************/
char* b_cbhead(Buffer* const pBD){
	return pBD == NULL ? NULL : pBD->cb_head;
}
