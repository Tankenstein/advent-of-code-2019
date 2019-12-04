.intel_syntax noprefix
.data

file_name:
    .asciz "input"
read_flag:
    .asciz "r"
scan_digit_format:
    .asciz "%d"
print_digit_format:
    .asciz "%d\n"

.text
.global _main
_main:
    // allocate 16 bytes
    enter 16, 0
    // locations:
    // pointer to input file at -8
    // total sum of mass at -12
    // current input at -16

    // read input file into -8
    lea rdi, [rip + file_name] 
    lea rsi, [rip + read_flag] 
    call _fopen 
    mov qword ptr [rbp - 8], rax 

    // initialize sum at -12
    mov dword ptr [rbp - 12], 0 

read_loop: 
    // fscanf from input file into -16
    mov rdi, qword ptr [rbp - 8]
    lea rsi, [rip + scan_digit_format]
    lea rdx, [rbp - 16]
    call _fscanf

    // jump to after loop if we didn't read anything
    cmp eax, 1
    jne after_loop

    // -16 slot / 3 - 2, add it to sum at -12
    mov eax, dword ptr [rbp - 16]
    cdq
    mov ecx, 3
    idiv ecx
    sub eax, 2
    add eax, dword ptr [rbp - 12]
    mov dword ptr [rbp - 12], eax

    // rinse and repeat
    jmp read_loop

after_loop:
    // close the file
    mov rdi, qword ptr [rbp - 8]
    call _fclose

    // print the sum
    lea rdi, [rip + print_digit_format]
    mov esi, dword ptr [rbp - 12]
    call _printf

    // buzz off 
    leave
    ret

