TITLE cs240

INCLUDE Irvine16.inc

.8086

SFLAG = 7
ZFLAG = 6

.code
; load the bx register with all zeroes except
; the bit # cx set
; IN: cx, 0 <= cx <= 15
; OUT: bx, as described
auth_onebit16 PROC
	pushf
	push cx

	mov bx,1
	jcxz done

top:
	shl bx,1
	loop top

done:
	pop cx
	popf
	ret
auth_onebit16 ENDP

; set flag specfied by cx
; IN: cx, 0 <= cx <= 15
auth_setflag PROC
	push ax
	push bx
	pushf

	call auth_onebit16
	pop ax		; pop flags into ax
	or ax,bx	; and flags with bx
	push ax		; push flags back into position

	popf
	pop bx
	pop ax
	ret

auth_setflag ENDP

; clear flag specfied by cx
; IN: cx, 0 <= cx <= 15
auth_clearflag PROC
	push ax
	push bx
	pushf

	call auth_onebit16
	not bx				; gives bx as 1...101...1
	pop ax				; pop flags
	and ax,bx			; and flags with bx
	push ax				; push "flags" back into position

	popf
	pop bx
	pop ax
	ret
auth_clearflag ENDP


.data
orig_flags WORD ?

.code
; Compares two strings lexicographically, according to the ASCII values
; of their characters
; IN: BX, offset to first string
; IN: DX, offset to second string
; OUT: first string comes before second string: S flag set, Z flag clear
; OUT: second string comes before first string: S flag clear, Z flag clear
; OUT: strings are equal: S flag clear, Z flag set
strcmp proc
	push ax
	push bx
	push cx
	push di

	pushf				; get flags into memory for
	pop orig_flags		; modification later

	mov di,dx			; mov dx to an index register

top:
	cmp byte ptr [bx],0	; done with first string?
	jz jfirst
	cmp byte ptr [di],0	; done with second string?
	jz f2

	; Compare characters
	mov al,[bx]
	cmp al,[di]
	jz cont				; chars the same
	jb f1				; first string comes first
	jmp f2				; second string comes first

cont:
	inc bx				; move on to
	inc di				; next characters
	jmp top

jfirst:
	cmp byte ptr [di],0	; done with fist string and second string
						; at the same time?
	jz feq

f1:
	push orig_flags		; first string comes before
	popf				; second string
	mov cx, SFLAG
	call auth_setflag	; S flag set
	mov cx, ZFLAG
	call auth_clearflag ; Z flag clear
	jmp coda

f2:
	push orig_flags		; second string comes before
	popf				; first string
	mov cx,SFLAG
	call auth_clearflag	; S flag clear
	mov cx,ZFLAG
	call auth_clearflag	; Z flag clear
	jmp coda

feq:
	push orig_flags		; strings are equal
	popf
	mov cx,SFLAG
	call auth_clearflag	; S flag clear
	mov cx,ZFLAG
	call auth_setflag	; Z flag set

coda:
	pop di
	pop cx
	pop bx
	pop ax
	ret
strcmp endp

; IN: DX, offset to a string for the filename
; OUT: AX, file handle
; filehandle: is a temporary reference number that an OS assigns to a file
; requested by a user to be opened. The system calls, accesses and iteracts with
; the file through that reference number throughout the session unitl the user
; terminates the file or the system session
; CF clear if successful
; CF set on error, and AX will be an error code.
; error code: 02h = file not found
OpenInputFile240 PROC
    mov ah,3Dh  ; open file function
    mov al,0    ; access mode 0 = read only
    int 21h

    ret
OpenInputFile240 ENDP

.data
readbuf byte "    "

.code
; read a signle char from a filehandle
; IN: ax, filehandle
; OUT; dx, char, -1 if end of file
ReadFileChar240 PROC
        push ax
        push bx
        push cx

        ; 3Fh: bx, file handle
        ;      cx, number of bytes to read
        ;      dx, buffer for data
        ; return:
        ;      CF clear if successful -->
        ;      ax, number of bytes actually read (o if at end of file
        ;          before call)
        ;      CF set on error -->
        ;      ax, error code
        mov bx,ax		        ; put filehandle in bx
        mov ah,3Fh		        ; read function
        mov dx, OFFSET readbuf  ; store in memory here
        mov cx,1		        ; read only 1 char
        int 21h

        jc coda		            ; error, exit

        cmp ax,0
        jz EndOfFile

        mov bx,OFFSET readbuf
        mov dh,0                ; zero out high bits of dx
        mov dl,[bx]
        jmp coda

EndOfFile:
        mov dx,-1

coda:   pop cx
        pop bx
        pop ax
        ret
ReadFileChar240 ENDP

; IN: ax, file handle
CloseFile240 PROC
	push bx
	mov bx,ax
	mov ah,3Eh     ; file handle in bx.
	int 21h
	pop bx
	ret
CloseFile240 ENDP

; clear screen
clrscr240 PROC
	pushf
	push ax

	mov ah,0
	mov al,3
	int 10h

	pop ax
	popf
	ret
clrscr240 ENDP

; IN: dh, row number
; IN: dl, column number
JumpCursor proc
	pushf
	push ax
	push bx

	mov ah,02h     ; set cursor position
	mov bh,0       ; set page number (0..7)
	int 10h        ; note: int 10h instead of int 21h

	pop bx
	pop ax
	popf
	ret
JumpCursor endp

.data
hexdigs BYTE "0123456789ABCDEF"

.code
; writes the hex code.
; IN: dl, in the binary: 0000 ????, i.e. a nybble.
_writehex_nybble PROC
	pushf
	push ax
	push dx
	push si

	mov dh,0	           ; clear the high bits of dx
	mov si,dx	           ; use an index register
	mov al,hexdigs[si]     ; indexing!!
	call WriteChar240

	pop si
	pop dx
	pop ax
	popf
	ret
_writehex_nybble ENDP

; writes a single character to the console window
; IN: al, character to write (in ascii)
WriteChar240 PROC
	pushf
	push ax
	push dx

	mov ah,02h	; DOS write-character function code
	mov dl,al	; DOS expects the char in dl
	int 21h		; DOS Interrupt

	pop dx
	pop ax
	popf
	ret
WriteChar240 ENDP

; writes the 4 hexadecimal nybbles of AX
; IN: ax, word to write
_writehex_word PROC
        pushf
	    push ax
	    push cx
	    push dx

        mov cx, 4	; do the following 4 times {
top:	mov dl,ah	; move high bits of ah into low bits of dl

	    push cx		; save the outer counter
	    mov cx, 4	; do the following 4 times {
inn:	shr dl,1    ;      shift dl right one position
	    loop inn    ; }
        pop cx		;   restore outer counter

	    call _writehex_nybble
        push cx
	    mov cx,4
inn2:	shl ax,1
	    loop inn2
	    pop cx
	    loop top

	    pop dx
	    pop cx
	    pop ax
	    popf
	    ret
_writehex_word ENDP

; IN: ax, a value
; IN: dx, offset to a string
; IN: cx, # of spaces
; Writes 2 spaces, then the String, then the value in hex, then CX spaces
_drhelp PROC
	pushf
	push ax
	push cx

	push ax
	mov al,' '
	call WriteChar240
	call WriteChar240
	pop ax

	call WriteString240
	call _writehex_word
	mov al,' '
splp:
    call WriteChar240
	loop splp

	pop cx
	pop ax
	popf
	ret
_drhelp ENDP

; determines whether a given flag is set or clear.
; IN: cx, flag # (bit position)
; OUT: ax, that flag (0 if clear, 1 if set)
; ACCESSES: DS:FLAGS, where the flags have been stored away
_getflag PROC
	pushf
	push bx
	push cx

	mov ax,FLAGS
	mov bx,1       ; start with 1 in position 0
top:
    cmp cx,0
	jz done
	shl bx,1
	dec cx
	jmp top

	; now mask is in bx
done:
    and ax,bx
	jz finis	; if ax != 0
	mov ax,1	;  set ax to 1

finis:
    pop cx
	pop bx
	popf
	ret
_getflag ENDP

.data
AXEQ BYTE "AX=",0
BXEQ BYTE "BX=",0
CXEQ BYTE "CX=",0
DXEQ BYTE "DX=",0
FLEQ BYTE "FL=",0
SPEQ BYTE "SP=",0
IPEQ BYTE "IP=",0
SIEQ BYTE "SI=",0
DIEQ BYTE "DI=",0
BPEQ BYTE "BP=",0
FHEAD BYTE "  FLAGS  O D I S Z A P C",0
SPACES1 BYTE "           =",0

FLAGNUMS WORD 11,10,9,7,6,4,2,0     ; bit positions for standard flags

AXVAL WORD ?
BXVAL WORD ?
CXVAL WORD ?
DXVAL WORD ?
FLAGS WORD ?
IPVAL WORD ?
SPVAL WORD ?
SIVAL WORD ?
DIVAL WORD ?
BPVAL WORD ?

.code
DumpRegs240 PROC
	mov AXVAL,ax       ; save registers now
	mov BXVAL,bx
	mov CXVAL,cx
	mov DXVAL,dx
	mov SIVAL,si
	mov DIVAL,di
	mov BPVAL,bp

	pushf		       ; move flags
	pop ax		       ; to ax
	mov FLAGS,ax	   ; and then to memory

	push sp		       ; mov stack pointer
	pop ax		       ; to ax
	add ax,2	       ; add 2 to get the SP before the call
	mov SPVAL,ax       ; to this funcion

	mov bp,sp
	mov ax,[bp]	       ; get caller's IP into ax
	mov IPVAL,ax	   ; save

	mov cx,2	       ; 2 spaces after each output for now

	mov ax,AXVAL
	mov dx,OFFSET AXEQ
	call _drhelp

	mov ax,BXVAL
	mov dx,OFFSET BXEQ
	call _drhelp

	mov ax,CXVAL
	mov dx,OFFSET CXEQ
	call _drhelp

	mov ax,DXVAL
	mov dx,OFFSET DXEQ
	call _drhelp

	mov ax,SIVAL
	mov dx,OFFSET SIEQ
	call _drhelp

	mov ax,DIVAL
	mov dx,OFFSET DIEQ
	call _drhelp

	call Newline

	mov ax,BPVAL
	mov dx,OFFSET BPEQ
	call _drhelp

	mov ax,SPVAL
	mov dx,OFFSET SPEQ
	call _drhelp

	mov dx,OFFSET FHEAD
	call WriteString240

	call Newline

	mov ax,IPVAL
	mov dx,OFFSET IPEQ
	call _drhelp

	mov ax,FLAGS
	mov dx,OFFSET SPACES1
	call _drhelp

	; output individual flag bits
	mov bx,offset FLAGNUMS
	mov cx,8		; loop 8 times {
flp:
    push cx			; save the count
	mov cx,[bx]		; get the next flag #
	call _getflag   ; and determine its value
	add al,48		; write 0 or 1 for the flag
	call WriteChar240
	mov al, ' '		; and a space
	call WriteChar240
	pop cx			; restore the count
	inc bx			; move to next word
	inc bx			; by incrementing pointer twice
                    ; since word = 2 byte
	loop flp		; }

	call Newline
	call Newline

	mov ax,FLAGS ; restore flags
	push ax
	popf

	mov dx,DXVAL ; restore registers
	mov cx,CXVAL
	mov bx,BXVAL
	mov ax,AXVAL
	mov bp,BPVAL
	ret
DumpRegs240 ENDP

; writes a null-terminated string to the console window
; IN: dx, string offset
WriteString240 PROC
	pushf
	push ax
	push bx

	mov bx,dx
top:
    mov al,[bx]
	cmp al,0
	jz done
	call WriteChar240
	inc bx
	jmp top
done:
	pop bx
	pop ax
	popf
	ret
WriteString240 ENDP

; writes carriage return and linefeed to the console window
; line feed means moving one line forward \n
; carriage return means moving the cursor to the beginning of the line \r
NewLine PROC
	pushf
	push ax

	mov al,0dh
	call WriteChar240
	mov al,0ah
	call WriteChar240

	pop ax
	popf
	ret
NewLine ENDP

; advance the cursor using BIOS interrupts
; buggy
BiosCursorAdv PROC
	pushf
	push ax
	push bx
	push cx
	push dx

	; get cursor position and size
	; OUT: DH, row number
	; 	   DL, column number
	mov ah,03h
	int 10h

	inc dl
	; check if cursor is on last column
	cmp dl,80
	jnz good

	; cursor is on the last column
	; move to the next row
	inc dh
	; start cursor position at column 0
	mov dl,0
	; check if cursor is on last row
	cmp dh,25
	jnz good

	; cursor is on the last row
	mov ah,06h		; scroll the window
	mov al,01h		; scroll up by 1 line

	; set the new page to start from row 0 and column 0
	mov cx,0
	mov dh,24
	mov dl,79

	mov bh,0 		; set the attribute of next line to write to not blinking
	int 10h

	mov dh,24
	mov dl,0

good:
	; set cursor position
	mov bh,0
	mov ah,02h
	int 10h

	pop dx
	pop cx
	pop bx
	pop ax
	popf
	ret
BiosCursorAdv ENDP

; writes a character using BIOS interrupts
; IN: al, character
; IN: ah, attribute
BiosWriteChar proc
	pushf
	push ax
	push bx
	push cx

	mov bl,ah
	mov bh,0
	mov cx,1	; number of times to wrtie character
	mov ah,09h
	int 10h
	call bioscursoradv

	pop cx
	pop bx
	pop ax
	popf
	ret
BiosWriteChar endp

; Write dx using random video attributes
; IN: dx, offset of a null terminated string
BiosFunWrite proc
	pushf
	push ax
	push bx
	push di

	mov di,dx
top:
	mov al,[di]
	cmp al,0
	jz done

	push ax
	mov ax,256
	call RandomRange
	mov bx,ax
	pop ax
	mov ah,bl
	call BiosWriteChar
	inc di
	jmp top

done:
	pop di
	pop bx
	pop ax
	popf
	ret
BiosFunWrite endp

.data
buffer BYTE "?????",0
ten WORD 10				;constant 10.
.code

; IN: ax, an unsigned number to write in base 10.
WriteDec240 PROC
	pushf
	push ax
	push bx
	push dx

	; special case ax = 0
	cmp ax,0
	jnz reg
	mov al,'0'
	call WriteChar240	; just write a 0 and be done.
	jmp quit

reg:
	mov bx,OFFSET buffer + 5

	; while ax != 0, divide ax by 10.
    	; convert remainder to a char and store it at
		; the end of the buffer.
top:
	cmp ax,0
	jz done
	mov dx,0	; zero out the high-16 bits of the dividend
	div ten		; divide dx:ax by 10, quotient in ax, remainder in dx
	add dl,48	; convert dl (remainder) to character
	dec bx
	mov [bx],dl	; store dl in the buffer, using bx as a pointer
	jmp top

done:
	mov dx,bx
	call WriteString240

quit:
	pop dx
	pop bx
	pop ax
	popf
	ret
WriteDec240 ENDP

; The ReadChar procedure reads a single character from the keyboard
; and returns the character in the AL register. The character is not
; echoed in the console window. Enhancement: If a keystroke yields two
; bytes (00 followed by something else), then AH is set to 1, otherwise
; AH is set to 0. (Thus this procedure does not preserve ah anymore.)
ReadChar240 PROC
	pushf

	mov ah,08
	int 21h
	cmp al,0
	jnz single

double:
	int 21h		; read the second character
	mov ah,1
	jmp done

single:
	mov ah,0

done:
	popf
	ret
ReadChar240 ENDP

; IN: di, points to buffer
;     bx, is di's distance from the null terminator
;         at the end of the string
; Precondition: di points to a non-null character in the string being
; read.
; OUT: di is unchanged, all characters in the string after di have
;      been shifted back, effectively erasing the character
; 	   therefore, bx is decremented.
_delete_char PROC
	pushf
	push ax
	push cx
	push dx
	push si
	push di

	push di			; save di
	mov si,di
	inc si
top:
	mov al,[si]
	mov [di],al		; move a character backward
	cmp al,0		; was it the terminator?
	jz done			; if so, stop

	inc di
	inc si
	jmp top

done:
	pop di					; restore di

	mov al,08h				; write a single backspace to the screen
	call WriteChar240

	mov dx,di
	call WriteString240		; write the string from di forward
	mov al, ' '				; also write an extra space
	call WriteChar240

	mov cx,bx				; and write bx backspaces!
bslp:
	mov al,08h
	call WriteChar240
	loop bslp

	dec bx					; align bx properly

	pop di
	pop si
	pop dx
	pop cx
	pop ax
	popf
	ret
_delete_char ENDP

; IN: di, points to a char in a string being read. Could be
;         the terminator when inserting at the end.
;     bx, is the distance of DI from the terminator
;     al, is a character to be inserted
; Precondition: there is room in the buffer for AL to be inserted
; OUT: The character is inserted at DI. Other character shift to the right.
;	   bx is unchanged. Because an insertion has occurred, DI is incremented
_insert_char PROC
	pushf
	push ax
	push cx
	push dx
	push si

	push di
	mov si,di		; move si to the end
	add si,bx
	mov di,si		; mov di to one after that
	inc di
	mov cx,bx
	inc cx			; shift bx+1 times

shftlp:
	mov dl,[si]
	mov [di],dl
	dec si
	dec di
	loop shftlp
	pop di


	mov [di],al			; insert al at position di
	mov dx,di
	call WriteString240	; write the buffer
	mov al,' '			; and an extra space
	call WriteChar240

	mov cx,bx
	inc cx			; and backspace bx + 1 times
	mov al,08h
bslp:
	call WriteChar240
	loop bslp

	inc di			; reflects the insertion

	pop si
	pop dx
	pop cx
	pop ax
	popf
	ret
_insert_char ENDP

; The ReadString240 procedure reads a sequence of characters,
; echoing them as they are typed. The characters are stored
; in a buffer, and null-terminated
; IN: cx, size of the buffer
; IN: dx, offset of buffer
; OUT: ax, string length
ReadString240 PROC
	pushf
	push bx
	push cx
	push di

	mov di,dx			; put dx in an index reg.
	mov byte ptr [di],0	; null terminate the buffer from the beginning
						; we will maintain this property
	dec cx				; cx is now the number of remaing chars we
						; can type

	mov bx,0			; bx: distance of di from the end of the string
						; normally 0 unless editing in the middle

top:
	call ReadChar240
	cmp ax,0dh			; see if it was the return key
	jz done				; if so, all finished

	cmp ax,14bh			; left arrow key!
	jnz c1

	; code to move back
	cmp dx,di
	jz top				; ignore if we'e at the beginning
	mov al,08h			; else
	call WriteChar240	; write a backspace
	dec di				; focus back
	inc bx				; further from the end
	jmp top

c1:	cmp ax,14dh		; right arrow key!
	jnz c2

	;code to move forward
	mov al,[di]			; ignore if we're at the end
	cmp al,0
	jz top
	call WriteChar240	; write the current character
	inc di				; focus forward
	dec bx				; closer to the end
	jmp top

c2:	cmp ax,08h		; see if it's delete
	jnz c3
	cmp di,dx		; are we at the beginning?
	jz top			; if so, ignore the keystroke

	; code to handle delete key
	dec di				; virtually move back one space
	inc bx
	call _delete_char	; deletes the char at di
	                    ; fixes the screen using bx
	inc cx				; update the remaining chars counter
	jmp top				; and then just keep reading


c3:	cmp ax,153h			; see if it's the other delete
	jnz reglar
	cmp byte ptr [di],0	; ignore if we're at the end
	jz top
	mov al,[di]
	call WriteChar240
	call _delete_char

	inc cx
	jmp top

reglar:
	jcxz top			; ignore a regular keystroke if
						; no room to insert

	call _insert_char	; takes di, inserts al at di, and increments
                        ; di. fixes the screen using bx

	dec cx				; one fewer characters available
	jmp top

done:
	mov ax,di		; compute the length
	add ax,bx		; in ax
	sub ax,dx
	call newline	; one more newline before returning

	pop di
	pop cx
	pop bx
	popf
	ret
ReadString240 ENDP
END
