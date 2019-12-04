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
    // allocate 32 bytes
    enter 32, 0
    // locations:
    // pointer to input file at -8
    // total sum of mass at -12
    // current input at -16
    // current fuel value at -20

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

    // -16 slot / 3 - 2, add it to sum at -12, move to new fuel slot at -20
    mov eax, dword ptr [rbp - 16]
    cdq
    mov ecx, 3
    idiv ecx
    sub eax, 2
    mov dword ptr [rbp - 20], eax

fuel_loop:
    // if fuel so far smaller than 0, go read next input.
    cmp dword ptr [rbp - 20], 0
    jle read_loop

    // add current fuel value to the total sum, store as new total sum.
    mov eax, dword ptr [rbp - 20]
    add eax, dword ptr [rbp - 12]
    mov dword ptr [rbp - 12], eax

    // run /3 - 2 on current fuel value
    mov eax, dword ptr [rbp - 20]
    cdq
    mov ecx, 3
    idiv ecx
    sub eax, 2
    mov dword ptr [rbp - 20], eax

    jmp fuel_loop

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

