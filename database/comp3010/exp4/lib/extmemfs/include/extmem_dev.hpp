#ifndef TINY_DB_ENGINE_EXTMEM_DEVICE_HPP
#define TINY_DB_ENGINE_EXTMEM_DEVICE_HPP

#include "fs_error.hpp"
#include "dev.hpp"
#include <optional>
#include <valarray>
#include <string>
#include <memory>

extern "C" typedef struct tagBuffer {
	unsigned long numIO;
	size_t bufSize;
	size_t blkSize;
	size_t numAllBlk;
	size_t numFreeBlk;
	unsigned char* data;
	char* name;
} Buffer;

extern "C" Buffer* initBuffer(const char* name, size_t bufSize, size_t blkSize, Buffer* buf);
extern "C" void freeBuffer(Buffer* buf);
extern "C" unsigned char* getNewBlockInBuffer(Buffer* buf);
extern "C" void freeBlockInBuffer(unsigned char* blk, Buffer* buf);
extern "C" int dropBlockOnDisk(unsigned int addr, Buffer* buf);
extern "C" unsigned char* readBlockFromDisk(unsigned int addr, Buffer* buf);
extern "C" int writeBlockToDisk(unsigned char* blkPtr, unsigned int addr, Buffer* buf);

namespace tinydb::extmem
{
	using std::valarray;
	using std::slice;
	using std::byte;
	using std::optional;
	using std::string;
	using std::unique_ptr;
	using tinydb::filesystem::block_stream_device;
	using tinydb::filesystem::fs_error;
	
	class extmem_device:block_stream_device {
	private:
		Buffer buf_;
		size_t block_num_;
		friend class extmem_device_manager;
		extmem_device(const size_t block_num, Buffer buf) : buf_{buf}, block_num_{block_num}
		{}
	public:
		extmem_device(const extmem_device& rhs) = delete;
		extmem_device(extmem_device&& rhs) = delete;
		extmem_de
		bool read_at_b(size_t offset, valarray<byte>& buf, slice& s) override
		{

		}
		bool write_at_b(size_t offset, const valarray<byte>& buf, const slice& s) override
		{

		}
		~extmem_device() override = default;
	};

	class extmem_device_manager
	{
	public:
		optional<extmem_device> make(const string& name, size_t block_size,size_t buffer_size)
		{
			Buffer buf{};
			auto res = initBuffer(name.c_str(), buffer_size, block_size, &buf);
			if(res==nullptr)
			{
				return {};
			} else
			{
				return extmem_device{  }
			}
		}
	};
}

#endif // TINY_DB_ENGINE_EXTMEM_DEVICE_HPP