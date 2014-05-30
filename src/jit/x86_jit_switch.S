.text

#.globl _jit_switch
_jit_switch:
    push %r15
    push %rdi
    push %rsi
    push %rax
    
    #movabsq __neart_jit_compile, %r15
    callq __neart_jit_compile
    mov %rax, %r15

    pop %rax
    pop %rsi
    pop %rdi

    callq *%r15

    pop %r15
    ret
