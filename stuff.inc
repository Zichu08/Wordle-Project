; Public
StrCmp	proto
OpenInputFile240 proto
ReadFileChar240 proto
CloseFile240 proto
clrscr240 proto
JumpCursor proto        ; dh: 0 <= row <= 24, 0 <=  dl <= 79
WriteDec240 proto
WriteChar240 proto
WriteString240 proto
NewLine proto
DumpRegs240 proto
ReadChar240 proto
ReadString240 proto
BiosWriteChar proto
BiosFunWrite proto
_delete_char proto
_insert_char proto

; Private
_writehex proto
_writehex_nybble proto
_drhelp proto
