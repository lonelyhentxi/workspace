#include <inc/mmu.h>

; 启动 CPU，切换到 32 位保护模式，并且跳转到 C 代码中；
; BIOS 从硬盘的第一个扇区加载这段代码到物理内存的 0x7c00 区域
; 并且开始在 %cs=0 %ip=7c00 处开始执行实模式

.set PROT_MODE_CSEG, 0x8 ; 核代码段选择器
.set PROT_MODE_DSEG, 0x10 ; 核数据段选择器
.set CR0_PE_ON, 0x1 ; 保护模式允许标志

.globl start
start:
    .code16    ; 16位模式汇编
    cli        ; 关中断
    cld        ; reset DF

    ; 设置数据段、附加段、堆栈段寄存器
    xorw    %ax,%ax
    movw    %ax,%ds
    movw    %ax,%es
    movw    %ax,%ss

    ; A20 允许：
    ; 为后向兼容，物理地址线只有20根，所以高位默认被0填充，这些代码取消这个设定

seta20.1:
    inb     $0x64,%al   ; 等待直到不忙的时候
    testb   $0x2,%al
    jnz     seta20.1
    
    movb    $0xd1,%al   ;  向 0x64 端口传递 0xd1
    outb    %al,$0x64

seta20.2:
    inb     $0x64,%al   ; 等待直到不忙
    testb   $0x2,%al
    jnz     seta20.2

    movb    $0xdf,%al,  ; 向 0x60 端口传递 0xdf
    outb    %al,$0x60

    ; 从实模式切换到保护模式，使用引导 GDT 和段翻译以使得虚拟地址匹配它们的物理地址；
    ; 来保证有效的内存映射不在切换期间被改变

    lgdt    gdtdesc  ; lgdt 加载全局描述符
    movl    %cr0,%eax
    orl     $CR0_PE_ON, %eax
    movl    %eax, %cr0

    ; 跳转到下条指令，但是在32位代码段中
    ; 切换处理器到 32 位模式
    ljmp    $PROT_MODE_CSEG,$protcseg

    .code32     ; 32位模式汇编
protcseg:
    ; 设置保护模式寄存器
    movw    %PROT_MODE_CSEG,%ax
    movw    %ax,%ds
    movw    %ax,%es
    movw    %ax,%fs
    movw    %ax,%gs
    movw    %ax,%ss

    ; 设置栈指针来呼叫 C 代码
    movl    $start,%esp
    call bootmain

    ; 如果出现了不应出现的从 bootmain 的返回，循环
spin:
    jmp spin

; 引导 GDT
.p2align 2
gdt:
    SEG_NULL ; null seg
    SEG(STA_X|STA_R,0x0,0xffffffff) ; 代码段
    SEG(STA_W,0x0,0xffffffff) ; 数据段

gdtdesc:
    .word 0x17 # sizeof(gdt) - 1
    .long gdt  # address gdt
