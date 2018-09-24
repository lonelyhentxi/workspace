# asm diff

## introduction

**from "brennan's fuide to inline assembly**

this is meant to be an introduction to inline assembly under DJGPP. DJGPP is based on GCC, using AT&T/UNIX synatx.

## the syntax

**left(front) is AT&T/UNIX, right(latter) is MS/Intel**

### - register naming:

prefixed with "%":

```x86asm
%eax,eax
```

### - source/destination ordering

```x86asm
move %eax, %ebx ; op src,des
move ebx,eax  ; op des,src
```

### - constant value/immediate value format:

prefixed with "$":

```x86asm
; load eax with the address of the "C" variable booga, which is static
movl $_booga,%eax
mov eax,_booga

; load ebx with 0xd00d
movl $0xd00d, %ebx
mov ebx,d00dh
```

### - operator size specification

**must** suffix the instruction with one of `b`,`w`,`l` to specify the width of the destination register as a `byte`,`word`,`longword`. If not, GNU assembler will attempt to guess.

```x86asm 
movw %ax,%bx
mov bx,ax
```

ps: The equivalent forms for Intel is `byte ptr`,`word ptr`,`dword ptr`, but that is for when you are...

### - Referencing memory

DJGPP use 386-protected mode, so "need forget all that real-mode addressing junk, including the restrictions on which register on which default segment, which registers can be base or index pointers"

ps: is use ebp, be sure to restore it ourselves or compile with `-formit-frame-pointer`

calculate method: immed32(least-or) + basepointer(least-or) + indexpointer * indexscale (must add the size suffix to the operator)

```x86asm
immed32(basepointer,indexpointer,indexscale)
[basepointer+indexpointer*indexscale+immed32]
```

#### - addressing a particular C variable

```x86asm
; the underscore _ is how to get a static(global) variables from assembler. 
; otherwise, can use extended asm to have variables preloaded into registers
_booga
[_booga]
```

### - addressing what a register points to:

```x86asm
(%eax)
[eax]
```

### - addressing a variable offset by a value in a register:

```x86asm
_variable(%eax)
[eax+_variable]
```

### - addressing a value in an array of integers(scalling up by 4):

```x86asm
_array(,%eax,4)
[eax*4+ array]
```

### - do offsets with the immediate value:

```x86asm
; c code: *(p+1) p is a char *
1(%eax)
[eax+1]
```

### - do some simple math on the immediate value:

```x86asm
_struct_pointer+8
```

### - addressing a particular char in an array of 8-character records:

```x86asm
; eax holds number of the record desired
; ebx has the wanted char's offset within the record

_array(%ebx,%eax,8)
[ebx+eax*8+_array]
```

## basic inline assembly

```c
asm("statements")
// or 
__asm__("statements")
// multiline
asm("pushl %eax\n\t"
    "movl $0,%eax\n\t"
    "popl %eax")
```

## extended inline assembly

```c
// basic format
asm("statements":ouput_registers:input_registers:clobbered_registers);
// nifty example
asm("cld\n\t"
"rep\n\t"
"stosl"
: /* no output registers */
: "c"(count),"a"(fill_value),"D"(dest)
: "%ecx","%edi"
)
```
|symbol|description|
|------|------|
|a | eax |
|b | ebx |
|c | ecx |
|d | edx |
|S | esi |
|D | edi |
|I | constant value |
|q,r | dunmiacally allocated register |
|g |eax, ebx, ecx, edx or variable in memory |
|A | eax and edx into a 64-bit integer(use long longs) |

```c++
// @TODO: if need, add more info
```
