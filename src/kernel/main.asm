org 0x0
bits 16

%define ENDL 0x0D,0x0A



start:

	;print msg
	mov si,msg_welcome
	call puts
.halt:
	cli
	hlt
	


;prints str onto screen
;params -ds:si points to the str
puts:
	;saves registers
	push si
	push ax
	
.get_char:
	lodsb    ;loads next char in al
	or al,al  ; check if next char is null
	jz .done
	

	mov ah,0x0e ;call bios input interrupt
	mov bh,0
	int 0x10 
	jmp .get_char ;if we didnt finish 
	
.done:
	pop ax
	pop si
	ret


msg_welcome: db 'Welcome to MielOs from kernel :P',ENDL,0




