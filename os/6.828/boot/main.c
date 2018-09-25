#include <inc/x86.h>
#include <inc/elf.h>

// 等待磁盘加载完毕

#define SECTSIZE 512
#define ELFHDR ((struct ELF *) 0x10000)

void readsect(void*,uint32_t);
void readseg(uint32_t, uint32_t,uint32_t);

waitdisk();

outb(0x1F2, 1);
outb(0x1F3, offset);
outb(0x1F4, offset >> 8);
outb(0x1F5, offset >> 16);
outb(0x1F6, (offset >> 24) | 0xE0);
outb(0x1F7, 0x20);

waitdisk();

insl(0x1F0, dst, SECTSIZE/4);