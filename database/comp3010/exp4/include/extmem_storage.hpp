#ifndef TINY_DB_ENGINE_EXTMEM_STORAGE_HPP
#define TINY_DB_ENGINE_EXTMEM_STORAGE_HPP

#include "vfs.hpp"
#include "pager.hpp"

extern "C" typedef struct tagBuffer {
    unsigned long numIO;
    size_t bufSize;
    size_t blkSize;
    size_t numAllBlk;
    size_t numFreeBlk;
    unsigned char *data;
} Buffer;

extern "C" Buffer *initBuffer(size_t bufSize, size_t blkSize, Buffer *buf);
extern "C" void freeBuffer(Buffer *buf);
extern "C" unsigned char *getNewBlockInBuffer(Buffer *buf);
extern "C" void freeBlockInBuffer(unsigned char *blk, Buffer *buf);
extern "C" int dropBlockOnDisk(unsigned int addr);
extern "C" unsigned char *readBlockFromDisk(unsigned int addr, Buffer *buf);
extern "C" int writeBlockToDisk(unsigned char *blkPtr, unsigned int addr, Buffer *buf);

namespace tinydb::extmem {

    constexpr size_t extmem_storage_buffer_bytes = 520;
    constexpr size_t extmem_storage_block_bytes = 64;
    constexpr size_t extmem_storage_page_bytes = extmem_storage_block_bytes;
    constexpr size_t extmem_storage_block_tail_bytes = 4;
    constexpr size_t extmem_storage_page_head_bytes = 1;

    using vfs::block_storage_interface;
    using pager::page_storage_interface;

    class extmem_storage : block_storage_interface, page_storage_interface {
    private:
        Buffer buffer_;
        size_t block_header_bytes_;
        size_t page_header_bytes_;
    public:
        extmem_storage(const extmem_storage &) = delete;

        extmem_storage(extmem_storage &&rhs) = delete;

        extmem_storage &operator=(const extmem_storage &) = delete;

        extmem_storage &operator=(extmem_storage &&rhs) = delete;

        static extmem_storage &initialize() {
            static extmem_storage storage_;
            return storage_;
        }

        size_t block_content_size() const override {
            return buffer_.blkSize - block_header_bytes_;
        }

        size_t block_header_size() const override {
            return block_header_bytes_;
        }

        size_t block_size() const override {
            return buffer_.blkSize;
        }

        size_t page_size() const override {
            return buffer_.blkSize;
        }

        size_t page_content_size() const override {
            return buffer_.blkSize;
        }

        size_t page_header_size() const override {
            return page_header_size_;
        }

        size_t buffer_pages() const override {

        }

    protected:
        extmem_storage() : extmem_storage{extmem_storage_buffer_bytes,extmem_storage_block_bytes} {
        }

        explicit extmem_storage(uint32_t buf_size, uint32_t block_size) : buffer_{} {
            initBuffer(buf_size, block_size, &buffer_);
        }

        ~extmem_storage() override {
            freeBuffer(&buffer_);
        }
    };
}

#endif