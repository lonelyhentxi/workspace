/*
 * extmem.c
 * Zhaonian Zou
 * Harbin Institute of Technology
 * Jun 22, 2011
 */

#define _CRT_SECURE_NO_WARNINGS

#include <stdlib.h>
#include <stdio.h>
#include "extmem.h"
#include <memory.h>
#include <string.h>
#include <errno.h>

	// device name, should not bigger than 10
	Buffer* initBuffer(const char* name, size_t bufSize, size_t blkSize, Buffer* buf)
	{
		buf->numIO = 0;
		buf->bufSize = bufSize;
		buf->blkSize = blkSize;
		buf->numAllBlk = bufSize / (blkSize + 1);
		buf->numFreeBlk = buf->numAllBlk;
		buf->data = (unsigned char*)malloc(bufSize * sizeof(unsigned char));
		buf->name = _strdup(name);
		if (!buf->data)
		{
			// errno has been set
			perror("Buffer Initialization Failed!\n");
			return NULL;
		}
		memset(buf->data, 0, bufSize * sizeof(unsigned char));
		return buf;
	}

	void freeBuffer(Buffer* buf)
	{
		if(buf!=NULL)
		{
			if (buf->data != NULL) {
				//free(buf->data);
				buf->data = NULL;
			}
		}
	}

	unsigned char* getNewBlockInBuffer(Buffer* buf)
	{
		if (buf->numFreeBlk == 0)
		{
			errno = ENOBUFS;
			perror("No buffer space available");
			return NULL;
		}

		unsigned char* blkPtr = buf->data;

		while (blkPtr < buf->data + (buf->blkSize + 1) * buf->numAllBlk)
		{
			if (*blkPtr == BLOCK_AVAILABLE)
				break;
			else
				blkPtr += buf->blkSize + 1;
		}
		*blkPtr = BLOCK_UNAVAILABLE;
		buf->numFreeBlk--;
		return blkPtr + 1;
	}

	void freeBlockInBuffer(unsigned char* blk, Buffer * buf)
	{
		*(blk - 1) = BLOCK_AVAILABLE;
		buf->numFreeBlk++;
	}

	int dropBlockOnDisk(unsigned int addr, Buffer * buf)
	{
		char filename[50];
		sprintf(filename, "data/%s/%d.blk", buf->name, addr);
		if (remove(filename) == -1)
		{
			errno = EAGAIN;
			perror("Dropping Block Fails!\n");
			return -1;
		}

		return 0;
	}

	unsigned char* readBlockFromDisk(unsigned char* blkPtr, unsigned int addr, Buffer * buf)
	{
		char filename[50];
		unsigned char* bytePtr;
		char ch;

		if (buf->numFreeBlk == 0)
		{
			errno = ENOBUFS;
			perror("Buffer Overflows!\n");
			return NULL;
		}

		sprintf(filename, "data/%s/%d.blk", buf->name, addr);
		FILE* fp = fopen(filename, "r");

		if (!fp)
		{
			fp = fopen(filename, "w");
			if (!fp)
			{
				errno = EAGAIN;
				perror("open file failed.");
				return NULL;
			}
		}
		*blkPtr = BLOCK_UNAVAILABLE;
		blkPtr++;
		bytePtr = blkPtr;

		while (bytePtr < blkPtr + buf->blkSize)
		{
			ch = fgetc(fp);
			*bytePtr = ch;
			bytePtr++;
		}

		fclose(fp);
		buf->numFreeBlk--;
		buf->numIO++;
		return blkPtr;
	}

	int writeBlockToDisk(unsigned char* blkPtr, unsigned int addr, Buffer * buf)
	{
		char filename[50];
		unsigned char* bytePtr;

		sprintf(filename, "data/%s/%d.blk", buf->name, addr);
		FILE* fp = fopen(filename, "w");

		if (!fp)
		{
			errno = EAGAIN;
			perror("Writing Block Failed!\n");
			return -1;
		}

		for (bytePtr = blkPtr; bytePtr < blkPtr + buf->blkSize; bytePtr++)
			fputc((int)(*bytePtr), fp);

		fclose(fp);
		*(blkPtr - 1) = BLOCK_AVAILABLE;
		buf->numFreeBlk++;
		buf->numIO++;
		return 0;
	}