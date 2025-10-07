org 0x7C00
bits 16

%define ENDL 0x0D,0x0A


;----- FAT12 HEADERS

jmp short main
nop

bdb_oem: db 'MS_OS4.1' ;8 bytes the OEM'S ID

bdb_bytes_per_sector: dw 512 

bdb_sectors_per_cluster: db 1

bdb_reserved_sectors: dw 1 ;where does the first FAT begin

bdb_fat_count: db 2 ;how many FATS tables exist

bdb_dir_entries_count: dw 0x0E0
bdb_total_sectors: dw 2880 ;2880 *512 = total megabytes needed
bdb_media_discriptor_type: db 0F0h ;  F0= 3.5 inch floppy disk
bdb_sectors_per_fat: dw 9  ;nine sectors per fat
bdb_sectors_per_track: dw 18
bdb_heads: dw 2
bdb_hidden_sectors: dd 0
bdb_large_sector_count: dd 0


;----- Extended boot record (EBR)

ebr_drive_number: db 0  ;0x00 for floppy disk, 0x80  for hdd, useless
				  db 0 ;reserved

ebr_signature: 	  db 29h
ebr_volume_id:	  db 12h, 34h, 56h, 78h  ;serial numbers that dont matter during os boot
ebr_volume_label: db 'MIELDISK OS' ;11 bytes padded with spaces
ebr_system_id: db 'FAT12   '




main:
	;set up the data segments, using register 
	;since u cant write to ds/ex directly
	mov ax,0
	mov ds,ax
	mov es,ax

	;setup stack
	mov ss,ax
	mov sp,0x7C00 ;go to the stating point of our os

	;makign sure we are in the expected star location
	push es 
	push word .after
	retf
.after:

	; read something from floppy disk
	; BIOS SHOULD SET DL TO DRIVE NUM
	mov [ebr_drive_number],dl



	;print loading msg
	mov si,msg_loading
	call puts

	;read drive parameters:sectors per track & head count (instead of relying on data from disk)
	push es
	mov ah, 08h
	int 13h
	jc floppy_err
	pop es


	and cl, 0x3F ;remove the top 2 bits
	xor ch,ch
	mov [bdb_sectors_per_track] ,cx ;sector count
	inc dh
	mov[bdb_heads],dh;head count

	;getting lba(reserved+fats+sectors_per_fat)
	mov ax,[bdb_sectors_per_fat] 
	mov bl,[bdb_fat_count]
	xor bh,bh
	imul bx ;ax=fats* sectors per fat
	add ax, [bdb_reserved_sectors]
	push ax

	;calc size of dir=> (32*num_of_enteries)/bytes_per_sector
	mov ax, [bdb_dir_entries_count]
	shl ax, 5 ;num*=32
	xor dx, dx
	div word [bdb_bytes_per_sector]

	test dx,dx
	jz .root_dir_after
	inc ax ; add 1 to res if div remainder !=0 , which means this sector is partially filled with entries

.root_dir_after:
	;read root dir
	mov cl,al; cl->num of sectors to read=size of root dir
	pop ax; ax-> lba of root dir
	mov dl,[ebr_drive_number] ;dl->get drive number (saved it previously)
	mov bx,buffer	;es:bx -> buffer
	call disk_read

	;search for kernel.bin
	xor bx,bx
	mov di, buffer
.search_kernel:
	mov si, file_kernel_bin
	mov cx,11 ;cmp 11 chars
	push di
	repe cmpsb ;cmp two bytes from mem as long as they are equal and cx>0
	pop di
	je .found_kernel
	add di,32
	inc bx;increment dir entry count
	cmp bx, [bdb_dir_entries_count]
	jl .search_kernel

	;kernel not found
	jmp kernel_not_found_error

.found_kernel:
	;save the first cluster val, di should have the addr to the entry
	mov ax,[di+26] ;1st logical cluster field
	mov [kernel_cluster],ax

	;load FAT from disk into mem
	mov ax, [bdb_reserved_sectors]
	mov bx, buffer
	mov cl , [bdb_sectors_per_fat]
	mov dl, [ebr_drive_number]
	call disk_read

	;read kernel +process fat chain
	mov bx, KERNEL_LOAD_SEGMENT
	mov es,bx
	mov bx, KERNEL_LOAD_OFFSET

.load_kernel_loop:
	;Read next cluster
	mov ax, [kernel_cluster] ;first cluster ->(kernel_cluster-2[reserved ones]* sectors_per_cluster+start sector)

	; The first data sector (cluster 2) starts after:
	;   * 1 reserved sector (the boot sector),
	;   * 18 FAT sectors (2 FATs × 9 sectors each),
	;   * 14 root directory sectors (224 entries × 32 bytes / 512 bytes per sector = 14).
	; So: 1 + 18 + 14 = 33 → the first usable data sector is LBA 33. (NOTE: CHANGE HARDCODED VAL LATER)

	add ax, 31

	mov cx, 1
	mov dl,[ebr_drive_number]
	call disk_read	
	add bx,[bdb_bytes_per_sector]

	;computer location of next cluster
	mov ax, [kernel_cluster]
	mov cx, 3
	mul cx
	mov cx, 2
	div cx;ax-> index of FAT entry, dx->cluster %2
	mov si, buffer
	add si, ax
	mov ax,[ds:si]

	or dx,dx
	jz .even

.odd:
	shr ax, 4
	jmp .next_cluster_after
.even:
	and ax, 0X0FFF

.next_cluster_after:
	cmp ax, 0x0FF8
	jae .read_done
	mov [kernel_cluster],ax
	jmp .load_kernel_loop

.read_done:
	;jmp 2 kernel
	mov dl, [ebr_drive_number] ;boot device in dl
	;set segment register
	mov ax, KERNEL_LOAD_SEGMENT
	mov ds, ax;setup data regs
	mov es, ax
	jmp KERNEL_LOAD_SEGMENT:KERNEL_LOAD_OFFSET

	jmp wait_key_and_reboot ;SHOULD NEVER HAPPEN

	cli
	hlt;disable interrupts so cpu cant get out of "halt" state


;Handle errors:

floppy_err:	
	mov si,read_failed
	call puts
	jmp wait_key_and_reboot

kernel_not_found_error:
	mov si, error_kernel_not_found
	call puts
	jmp wait_key_and_reboot

wait_key_and_reboot:
	mov ah,0
	int 16 ;wait for keypress
	jmp 0xFFFF:0 ;jmp to beginning
	


.halt:
	cli ;disable interrupts, so the cpu cant get out of the "halt" state 
	hlt


;
; Prints a string to the screen
; Params:
;   - ds:si points to string
;
puts:
    ; save registers we will modify
    push si
    push ax
    push bx

.loop:
    lodsb               ; loads next character in al
    or al, al           ; verify if next character is null?
    jz .done

    mov ah, 0x0E        ; call bios interrupt
    mov bh, 0           ; set page number to 0
    int 0x10

    jmp .loop

.done:
    pop bx
    pop ax
    pop si    
    ret
    
;Disk routine


;This fuction converts an LBA(logical block addressing- 1 num) to CHS (cylinder head sector- 3 numbers for reaching in the disk)
;Paramaters :  ax register-> LBA ADDR
;Returns: 
; 		cs[0-5]: sector number
; 		cs[6-15]: cylinder
;       dh: head
lba_to_chs:

	push ax
	push dx


	xor dx,dx ;clear dx
	div word [bdb_sectors_per_track] ;ax= LBA / sectorsPerTrack  dx=LBA % sectorsPerTrack

	inc dx
	mov cx,dx  ;cx is the sector

	xor dx,dx
	div word[bdb_heads] ;ax = (LBA/sectorsPerTrack) / heads =cylinder
						;dx = (LBA/sectorsPerTrack) % heads = head 

	mov dh,dl  ;dh = head
	mov ch,al  ;ch = lower 8 bits of cylinder
	shl ah,6   ;shift cylinder high bits into 6 7 pos 
	or cl,ah   ;merge with sector num cl

	pop ax 
	mov dl,al ;restore dl
	pop ax 
	ret 



;Reads from disk
;Paramaters :  
;			-ax register-> LBA ADDR
;			-cl -> num of sectors to read (up to 128)
;			-dl -> drive num
;			-es:bx -> mem addr where to store read data

disk_read:
	push ax ;save regs that we will modify
	push bx 
	push cx
	push dx
	push di

	push cx	; save cl temporarly for num of sectors
	call lba_to_chs
	pop ax  ;get num of sectors to read (al)

	mov ah,02h
	mov di,3 ;retry count

.retry_bios:
	pusha 		;save all registers
	stc	  		;set carry flag, because some BIOS's dont set it
	int 13h 	;carry flag cleared == success	
	jnc .done	;jump if carry not set 
	popa

	;if read failed
	popa 
	call disk_reset

	dec di
	test di,di
	jnz .retry_bios

.fail:
	;after all failed attempts
	jmp floppy_err

.done:
	popa
	;restore regs that were modified
	pop di
	pop dx
	pop cx
	pop bx 
	pop ax 
	ret
	
;reset Disk
;Paramaters :  dl->drive num
disk_reset:
	pusha
	int 13h
	mov ah,0
	stc
	int 13
	jc floppy_err
	popa
	ret



msg_loading: db 'Loading...',ENDL,0
read_failed: db 'Read from disk failed',ENDL,0
error_kernel_not_found: db 'KERNEL.BIN not found!', ENDL, 0
file_kernel_bin:        db 'KERNEL  BIN'
kernel_cluster:         dw 0


KERNEL_LOAD_SEGMENT equ 0x2000
KERNEL_LOAD_OFFSET	equ 0


times 510-($-$$) db 0
dw 0AA55h


buffer: 
