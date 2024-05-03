INCLUDE Irvine16.inc
INCLUDE stuff.inc

.8086

.data
badr BYTE "Bad Read",0

.code
; IN: ax, file handle
; IN: cx, number of chars to read
; IN: dx, buffer to write
; OUT: cx, number of chars actually read
ReadFileN proc
	pushf
	push ax
	push bx
	push dx

	mov bx,ax
	mov ah,3Fh	; read from file
	int 21h
	jc bad

	mov cx,ax
	jmp coda

bad:
	mov dx, offset badr
	call writestring240
	call newline

coda:
	pop dx
	pop bx
	pop ax
	popf
	ret
ReadFileN endp

; IN: bx, string1 OFFSET
; IN: dx, string2 OFFSET
; OUT: boolean in AX
strequal proc
	pushf

	call strcmp
	jz yep

nope:
    mov ax,0
	jmp coda

yep:
    mov ax,1

coda:
    popf
	ret
strequal endp

.data
wordfile BYTE "WORDS.TXT",0
wordbuf BYTE "             ",0

.code
; IN: dx, offset to some word
; OUT: ax, 1 if dx is a word, 0 if dx is not a word
isword proc
	pushf
	push bx
	push cx
	push dx

	mov bx,dx	; save offset of word
	mov dx, offset wordfile
	call OpenInputFile240

	; ax is file handle
top:
	mov cx,6
	mov dx,offset wordbuf
	call ReadFileN
	cmp cx,6
	jnz endofi

	mov byte ptr [wordbuf + 5],0 ; null terminate buffer with word

	push ax			; save filehandle
	call strequal
	cmp ax,1		; see if it's equal
	pop ax			; restore file handle
	jz found		; word is found

	jmp top

endofi:
	call CloseFile240
	mov ax,0
	jmp coda

found:
	call CloseFile240
	mov ax,1

coda:
	pop dx
	pop cx
	pop bx
	popf
	ret
isword endp

; IN: dx, offset to buffer for the command line tail
; The command tail is the string of characters typed on the command line
; following the command/program name. It is located in the Program Segmenet
; Prefix (PSP)
; remenber to skip the space prior to cmd tail
getcmdtail proc
	pushf
	push ax
	push cx
	push si
	push di

	mov di,dx

	mov ch,0
	mov cl,es:[80h]		; extra segment has start of program segment prefix
						; needs to specifi es since by default data segment is
						; used
						; cl contains length of command line
	jcxz done
	mov si,81h			; point to first byte of cmd tail
						; in PSP

top:
	mov al,es:[si]		; get a byte from cmd tail in PSP
	mov [di],al			; store into our data segment string
	inc di				; move along both
	inc si				; strings
	loop top

done:
	mov byte ptr [di],0	; null terminate our buffer
	pop di
	pop si
	pop cx
	pop ax
	popf
	ret
getcmdtail endp

; IN: al, a character
; IN: dx, offset to a string
; IN: cx, an index in DX.
; Precondition: cx < the length of dx
; OUT: al, a hint
getHint PROC
	pushf
	push cx

	call strfind
	cmp ax,65535
	jz no

	cmp ax,cx
	jz yes

	mov al,'?'
	jmp done

yes:
    mov al,'*'
	jmp done

no:
    mov al,'.'

done:
    pop cx
	popf
	ret
getHint ENDP

; Finds the index of the first occurence of a character in a string
; IN: dx, offset of a string
; IN: al, character to search for
; OUT: ax, index of AL in DX, or -1 if AL does not occur
strfind PROC
	pushf
	push bx
	push cx
	push dx
	push si

	mov bx,dx	; use bx as pointer
	mov si,0	; si to keep track of location in string

	push ax
	call strlen	; compute how long to run the loop
	mov cx,ax
	pop ax

top:
    cmp si,cx	; loop when string is not fully searched
	jz notfound
	mov dl,[bx]	; move the current char to dx
	cmp dl,al	; compare char with target
	jz found
	inc si		; increase location by 1
	inc bx		; point pointer to next char
	jmp top

found:
    mov ax,si
	jmp done

notfound:
    mov ax,-1

done:
    pop si
	pop dx
	pop cx
	pop bx
	popf
	ret
strfind ENDP

; Determines the length of a string
; IN: dx, offset of string
; OUT: ax, string length
strlen PROC
	pushf
	push bx
	push cx
	push dx

	mov cx,0
	mov bx,dx	; use bx as pointer
	mov al,[bx]	; store first char in al

lp:	cmp al,0
	jz done
	inc cx		; increase str len by 1
	inc bx		; set pointer to next char
	mov al,[bx]
	jmp lp

done:
    mov ax,cx

	pop dx
	pop cx
	pop bx
	popf
	ret
strlen ENDP

; Writes a string with Wordle hints in color
; IN: dx, offset to a guess
; IN: bx offset to a string of corresponding hints
WriteWordle PROC
	pushf
	push ax
	push bx
	push cx
	push di

	mov di,dx
	mov cx,5
top:
    mov al,[bx]
	cmp al,'.'
	jnz lab1
	mov ah,00000111b   ; dull white on black
	jmp doit

lab1:
    cmp al,'?'
	jnz lab2
	mov ah,00011110b   ; yellow on blue
	jmp doit

lab2:
    mov ah,00101111b    ; bright white on green

doit:
    mov al,[di]
	call BiosWriteChar
	inc bx
	inc di

	loop top

	call Newline
	pop di
	pop cx
	pop bx
	pop ax
	popf
	ret
WriteWordle ENDP

; copy the content of a 5 byte buffer to another 5 byte buffer
; IN: dx, offset of string to copy from
; IN: bx, offset of string to copy to
_copy_word PROC
	pushf
	push ax
	push bx
	push cx
	push dx
	push si

	mov si,dx
	mov cx,5
top:
	mov al,[si]
	mov [bx],al
	inc bx
	inc si
	loop top

	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	popf
	ret
_copy_word ENDP

.data
placeholder BYTE "-----",0

.code
; construct the wordle hint in guess buffer
; IN: dx, offset of answer
; IN: bx, offset of guess
ConstuctHint PROC
    pushf
    push ax
    push bx
    push cx
    push dx
    push si
    push di

	push bx
	mov bx,OFFSET placeholder
	call _copy_word
	pop bx

	mov si,OFFSET placeholder
    mov di,dx
    mov cx,5
	mov ax,si

	; write * in hint buffer if the guess char matches answer
	push bx
	push si
	push di
c_check:
	push di
	mov dx,si
	pop di
	push ax
    mov al,[di]
    cmp al,[bx]
	pop ax
    jnz not_match

    push ax
    mov al,'*'
    mov [si],al
	mov [bx],al
    pop ax
not_match:
	inc di
	inc bx
    inc si
    loop c_check

	pop di
	pop si
	pop bx

	push bx
	push si
	push di

	; write ? in hint buffer if the guess char exsist in answer
	mov cx,5
d_check:
	push ax
	mov al,'*'
	cmp [bx],al
	pop ax
	jz not_d

	; ensures construct hint behaves when there are duplicate chars in guess
	; but single char in answer
	push si
	mov si,ax
	push cx
	mov cx,5
s_check:
	push ax
	mov al,[si]
	cmp [bx],al
	pop ax
	jnz bad

	push ax
	mov al,'?'
	mov [bx],al
	mov [si],al
	pop ax
	jmp good

bad:
	inc si
	loop s_check

good:
	pop cx
	pop si

not_d:
	inc bx
	inc si
	inc di
	loop d_check

	pop di
	pop si
	pop bx

	mov cx,5
final:
	mov al,'?'
	cmp [bx],al
	jz almost
	mov al,'*'
	cmp [bx],al
	jz almost
	mov al,'.'
	mov [bx],al

almost:
	inc bx
	loop final

    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    popf
    ret
ConstuctHint ENDP

; Write dx using black and white text
; IN: dx, offset of a null terminated string
BiosWrite proc
	pushf
	push ax
	push bx
	push di

	mov di,dx
top:
	mov al,[di]
	cmp al,0
	jz done

	mov ah,00000111b
	call BiosWriteChar
	inc di
	jmp top

done:
	pop di
	pop bx
	pop ax
	popf
	ret
BiosWrite endp

; The ReadString240 procedure reads a sequence of characters,
; echoing them as they are typed. The characters are stored
; in a buffer, and null-terminated
; IN: cx, size of the buffer
; IN: dx, offset of buffer
; OUT: ax, string length
; Diff from ReadString240: clear char echoed on terminal
ReadStringWordle PROC
   pushf
   push bx
   push cx
   push di

   mov di,dx			; put dx in an index reg.
   mov byte ptr [di],0	; null terminate the buffer from the beginning
					    ; we will maintain this property
   dec cx				; cx is now the number of remaing chars we
					    ; can type

   mov bx,0			    ; bx: distance of di from the end of the string
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

c1:	cmp ax,14dh			; right arrow key!
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
   jcxz top				; ignore a regular keystroke if
					    ; no room to insert

   call _insert_char	; takes di, inserts al at di, and increments
					    ; di. fixes the screen using bx

   dec cx				; one fewer characters available
   jmp top

done:
   mov ax,di		; compute the length
   add ax,bx		; in ax
   sub ax,dx
   ; call newline	; one more newline before returning

   ; clear the echo on terminal
   push ax
   mov cx,ax
   mov al,08h

clear:
   call WriteChar240
   loop clear

   pop ax

   pop di
   pop cx
   pop bx
   popf
   ret
ReadStringWordle ENDP

.data
D1 BYTE "Pick a 5 letter word: ",0
D2 BYTE "Please enter a 5 letter word!",0
D3 BYTE "WORDLE!",0
D4 BYTE "You won!",0
D5 BYTE  "                        Invalid Word",0
D6 BYTE  "                        Valid Word  ",0
D10 BYTE "                        Follow hint ",0
D7 BYTE "You lost! :( ",0
D8 BYTE "The answer is: ",0
guess BYTE "-----",0
answer BYTE "-----",0
previous BYTE "-----",0
guess_copy BYTE ".....",0

.code
; two player wordle game as described in assignment
TwoPlayerWordle PROC
	pushf
	push ax
	push bx
	push cx
	push dx
	push si
	push di

	call clrscr240
	mov dx,OFFSET D1
	call BiosWrite

	mov cx,6
	mov dx,OFFSET answer

t1:
	call ReadString240
	cmp ax,5
	jz n1
	mov dx,OFFSET D2
	call BiosWrite
	call newline
	mov dx,OFFSET D1
	call BiosWrite
	jmp t1

n1:
	call clrscr240
	mov dx,OFFSET D3
	call BiosWrite
	call newline

	mov si,OFFSET guess
	mov di,OFFSET guess_copy
	mov cx,0

t2:
	cmp cx,6
	jz loser

read:
	mov dx,si
	push cx
	mov cx,6
	call ReadStringWordle
	pop cx

	call isword
	cmp ax,0
	jnz yesWord
	mov dx,OFFSET D5
	call BiosWrite

	push ax
	push cx
	mov cx,79
top:
	mov al,08h
	call WriteChar240
	loop top
	pop cx
	pop ax
	jmp read

yesWord:
	push dx
	mov dx, OFFSET D6
	call BiosWrite
	pop dx

	push ax
	push cx
	mov cx,79
top1:
	mov al,08h
	call WriteChar240
	loop top1
	pop cx
	pop ax

	push bx
	mov bx,di
	call _copy_word
	pop bx

	mov dx,OFFSET answer
	mov bx,di
	call ConstuctHint

	mov dx,si
	mov bx,di
	call WriteWordle

	push bx
	push dx
	mov bx,OFFSET guess
	mov dx,OFFSET answer
	call strcmp
	jz winner
	pop dx
	pop bx

	inc cx
	jmp t2

loser:
	mov dx,OFFSET D7
	call BiosWrite
	mov dx,OFFSET D8
	call BiosWrite
	mov dx,OFFSET answer
	call BiosWrite
	jmp done
	jmp done

winner:
	pop dx
	pop bx
	call NewLine
	mov dx,OFFSET D4
	call BiosWrite

done:
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	popf
	ret
TwoPlayerWordle ENDP

.data
filename BYTE "WORDS.TXT",0
randomword BYTE "-----",0

.code
; generates a random word from dictionary and writes it in random word buffer
RandomGen PROC
	pushf
	push ax
	push bx
	push cx
	push dx


	mov ax,8494
	call Randomize		;re-seed generator
	call RandomRange
	inc ax

	mov cx,ax
	call WriteDec240
	call newline

	mov dx,OFFSET filename
	call OpenInputFile240
	mov dx,OFFSET randomword
top:
	push cx
	push dx
	mov cx,6
	call ReadFileN
	pop dx
	pop cx
	loop top

	pop dx
	pop cx
	pop bx
	pop ax
	popf
	ret
RandomGen ENDP

; signle player wordle game as described in asssigment
signlePlayerwordle PROC
	pushf
	push ax
	push bx
	push cx
	push dx
	push si
	push di

	call clrscr240

	call RandomGen
	mov dx,OFFSET randomword
	mov bx,OFFSET answer
	call _copy_word
	mov dx,OFFSET answer

n1:
	call clrscr240
	mov dx,OFFSET D3
	call BiosWrite
	call newline

	mov si,OFFSET guess
	mov di,OFFSET guess_copy
	mov cx,0

t2:
	cmp cx,6
	jz loser

read:
	mov dx,si
	push cx
	mov cx,6
	call ReadStringWordle
	pop cx

	call isword
	cmp ax,0
	jnz yesWord
	mov dx,OFFSET D5
	call BiosWrite

	push ax
	push cx
	mov cx,79
top:
	mov al,08h
	call WriteChar240
	loop top
	pop cx
	pop ax
	jmp read

yesWord:
	push dx
	mov dx, OFFSET D6
	call BiosWrite
	pop dx

	push ax
	push cx
	mov cx,79
top1:
	mov al,08h
	call WriteChar240
	loop top1
	pop cx
	pop ax

	push bx
	mov bx,di
	call _copy_word
	pop bx

	mov dx,OFFSET answer
	mov bx,di
	call ConstuctHint

	mov dx,si
	mov bx,di
	call WriteWordle

	push bx
	push dx
	mov bx,OFFSET guess
	mov dx,OFFSET answer
	call strcmp
	jz winner
	pop dx
	pop bx

	inc cx
	jmp t2

loser:
	mov dx,OFFSET D7
	call BiosWrite
	mov dx,OFFSET D8
	call BiosWrite
	mov dx,OFFSET answer
	call BiosWrite
	jmp done

winner:
	pop dx
	pop bx
	call NewLine
	mov dx,OFFSET D4
	call BiosWrite


done:
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	popf
	ret
signlePlayerwordle ENDP

.data
last_guess BYTE "-----",0

.code
; hard wordle game where each subsequent guess must follow hint
HardWordle PROC
	pushf
	push ax
	push bx
	push cx
	push dx
	push si
	push di

	call clrscr240

	call RandomGen
	mov dx,OFFSET randomword
	mov bx,OFFSET answer
	call _copy_word
	mov dx,OFFSET answer

n1:
	call clrscr240
	mov dx,OFFSET D3
	call BiosWrite
	call newline

	mov si,OFFSET guess
	mov di,OFFSET guess_copy
	mov cx,0

t2:
	cmp cx,6
	jz loser

read:
	mov dx,si
	push cx
	mov cx,6
	call ReadStringWordle
	pop cx

	push ax
	push bx
	push di
	push si
	mov bx,OFFSET guess_copy
	mov di,OFFSET previous
	mov si,OFFSET guess
	call HardCheck
	pop si
	pop di
	pop bx

	cmp ax,0
	pop ax
	jnz yesWord
	mov dx,OFFSET D10
	call BiosWrite

	push ax
	push cx
	mov cx,79
top:
	mov al,08h
	call WriteChar240
	loop top
	pop cx
	pop ax



	jmp read

yesWord:
	push bx
	push dx
	mov dx,OFFSET guess
	mov bx,OFFSET previous
	call _copy_word
	pop dx
	pop bx

	push dx
	mov dx, OFFSET D6
	call BiosWrite
	pop dx

	push ax
	push cx
	mov cx,79
top1:
	mov al,08h
	call WriteChar240
	loop top1
	pop cx
	pop ax

	push bx
	mov bx,di
	call _copy_word
	pop bx

	mov dx,OFFSET answer
	mov bx,di
	call ConstuctHint

	mov dx,si
	mov bx,di
	call WriteWordle

	push bx
	push dx
	mov bx,OFFSET guess
	mov dx,OFFSET answer
	call strcmp
	jz winner
	pop dx
	pop bx

	inc cx
	jmp t2

loser:
	mov dx,OFFSET D7
	call BiosWrite
	mov dx,OFFSET D8
	call BiosWrite
	mov dx,OFFSET answer
	call BiosWrite
	jmp done

winner:
	pop dx
	pop bx
	call NewLine
	mov dx,OFFSET D4
	call BiosWrite


done:
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	popf
	ret
HardWordle ENDP

.code
; IN: bx, offset of previous hints
; IN: di, offset of guess
; IN: si, offset of new guess
; determin whether the guess followed the hint, 1 if hint is followed
; 0 otherwise
HardCheck PROC
	pushf
	push bx
	push cx
	push dx
	push si
	push di

	mov dx,si
	mov cx,5
top:
	mov al,'*'
	cmp [bx],al
	jnz process

	push ax
	mov al,[si]
	cmp [di],al
	pop ax
	jnz false

process:
	mov al,'?'
	cmp [bx],al
	jnz almost

	mov al,[di]
	; call WriteChar240

	call strfind
	cmp ax,65535
	jz false

almost:
	inc bx
	inc di
	inc si
	loop top

true:
	mov ax,1
	jmp done

false:
	mov ax,0
	jmp done

done:
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	popf
	ret
HardCheck ENDP

.data
test1 BYTE "Welcome to my WORDLE!",0
test2 BYTE "Pick a mode: ",0
test3 BYTE "1. two player wordle",0
test4 BYTE "2. signle player wordle",0
test5 BYTE "3. hard wordle, where the subsequent guess must follow hint",0


.code
main PROC
	mov ax,@data
	mov ds,ax

	; creates the menu system
	call clrscr240
	mov dx,OFFSET test1
	call BiosWrite
	call NewLine
	mov dx,OFFSET test2
	call BiosWrite
	call NewLine
	mov dx,OFFSET test3
	call BiosWrite
	call NewLine
	mov dx,OFFSET test4
	call BiosWrite
	call NewLine
	mov dx,OFFSET test5
	call BiosWrite
	call newline
	call readchar240


	cmp al,'1'
	jnz next
	call TwoPlayerWordle
	jmp done
next:
	cmp al,'2'
	jnz next2
	call signlePlayerwordle
	jmp done
next2:
	call HardWordle

done:
	mov ax,4C00h
	int 21h
main ENDP
end main
