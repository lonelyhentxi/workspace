#ifndef TINY_DB_ENGINE_EXTMEM_DEVICE_HPP
#define TINY_DB_ENGINE_EXTMEM_DEVICE_HPP

#include "../basic/fs_error.hpp"
#include "../basic/dev.hpp"
#include "util.hpp"
#include <optional>
#include <valarray>
#include <string>
#include <memory>
#include <cmath>
#include <boost/multi_index_container.hpp>
#include <boost/multi_index/hashed_index.hpp>
#include <boost/multi_index/ordered_index.hpp>
#include <boost/multi_index/member.hpp>
#include <boost/filesystem.hpp>
#include <cassert>
#include <list>

extern "C" {
typedef struct tagBuffer {
    unsigned long numIO; /* Number of IO's*/
    size_t bufSize; /* Buffer size*/
    size_t blkSize; /* Block size */
    size_t numAllBlk; /* Number of blocks that can be kept in the buffer */
    size_t numFreeBlk; /* Number of available blocks in the buffer */
    unsigned char *data; /* Starting address of the buffer */
    char* name;
} Buffer;

Buffer *initBuffer(const char* name,size_t bufSize, size_t blkSize, Buffer *buf);
void freeBuffer(Buffer *buf);
unsigned char *getNewBlockInBuffer(Buffer *buf);
void freeBlockInBuffer(unsigned char *blk, Buffer *buf);
int dropBlockOnDisk(unsigned int addr, Buffer *buf);
unsigned char *readBlockFromDisk(unsigned char* blkPtr, unsigned int addr, Buffer *buf);
int writeBlockToDisk(unsigned char *blkPtr, unsigned int addr, Buffer *buf);

}

namespace tinydb::extmem {
    using std::valarray;
    using std::slice;
    using std::byte;
    using std::optional;
    using std::string;
    using std::unique_ptr;
    using std::list;
    using tinydb::filesystem::util::sys_time_spec;
    using tinydb::filesystem::block_stream_device;
    using tinydb::filesystem::fs_error;
    using tinydb::filesystem::util::current_sys_time_spec;

    class extmem_device final : public block_stream_device {
    private:
        using extmem_cache_id_t = size_t;
        using extmem_block_id_t = size_t;
        struct extmem_device_index {
            extmem_block_id_t block_id;
            extmem_cache_id_t cache_id;
            sys_time_spec time;
        };
        using extmem_cache_index_t = typename boost::multi_index::multi_index_container<extmem_device_index,
                boost::multi_index::indexed_by<
                        boost::multi_index::hashed_non_unique<boost::multi_index::member<
                                extmem_device_index, extmem_block_id_t,
                                &extmem_device_index::block_id>>,
                        boost::multi_index::hashed_unique<boost::multi_index::member<extmem_device_index, extmem_cache_id_t,
                                &extmem_device_index::cache_id>>,
                        boost::multi_index::ordered_non_unique<boost::multi_index::member<extmem_device_index, sys_time_spec,
                                &extmem_device_index::time>>
                >>;
        Buffer *buf_;
        size_t block_num_;
        extmem_cache_index_t cache_index_;

        friend class extmem_device_manager;

        extmem_device(const size_t block_num, Buffer *buf) :
                buf_{buf}, block_num_{block_num},
                cache_index_{} {
            for (size_t i = 0; i < cache_num(); i++) {
                cache_index_.insert(extmem_device_index{0, i, current_sys_time_spec()});
            }
        }

        inline size_t cal_cache_offset(const size_t idx) const {
            return idx * cache_size();
        }

        inline void valarray_to_cache(const valarray<byte> &source, const slice &s, size_t target) {
            // assert(s.size() >= block_size());
            // skip flag byte
            target = target + sizeof(unsigned char);
            for (auto i = s.start(); i < s.size() * s.stride(); i += s.stride(), target += sizeof(unsigned char)) {
                buf_->data[target] = static_cast<unsigned char>(source[i]);
            }
        }

        inline void cache_to_valarray(valarray<byte> &target, const slice &s, size_t source) const {
            // assert(s.size() >= block_size());
            // skip flag byte
            source = source + sizeof(unsigned char);
            for (auto i = s.start(); i < s.size() * s.stride(); i += s.stride(), source += sizeof(unsigned char)) {
                target[i] = static_cast<byte>(buf_->data[source]);
            }
        }

    public:
        extmem_device(const extmem_device &rhs) = delete;

        extmem_device(extmem_device &&rhs) noexcept :
                buf_(rhs.buf_), block_num_{rhs.block_num_},
                cache_index_{std::move(rhs.cache_index_)} {
            rhs.block_num_ = 0;
            rhs.buf_ = nullptr;
            rhs.cache_index_ = {};
        }

        extmem_device &operator=(const extmem_device &rhs) = delete;

        extmem_device &operator=(extmem_device &&rhs) noexcept {
            std::swap(block_num_, rhs.block_num_);
            std::swap(buf_, rhs.buf_);
            std::swap(cache_index_, rhs.cache_index_);
            return *this;
        }

        inline size_t block_num() const {
            return block_num_;
        }

        inline size_t block_size() const override {
            return buf_->blkSize;
        }

        inline size_t cache_count() const {
			return cache_num() - buf_->numFreeBlk;
        }

        inline size_t cache_header_size() const {
            return 1;
        }

        inline size_t cache_size() const {
            return block_size() + cache_header_size();
        }

        inline size_t cache_num() const {
            return buf_->numAllBlk;
        }

        inline bool cache_empty() const {
			return cache_count() == 0;
        }

        inline bool cache_full() const {
            return cache_count() == cache_num();
        }

        ~extmem_device() override {
            if(buf_!= nullptr) {
                freeBuffer(buf_);
            }
        }

		bool read_at_b(const size_t block_id, valarray<byte>& buf, const slice& s) override;

		bool write_at_b(size_t block_id, const valarray<byte>& buf, const slice& s) override;
    };

    class extmem_device_manager {
    public:
		static optional<extmem_device> make(const string& name, const size_t block_num, const size_t block_size,
			const size_t buffer_size);
    };
}

#endif // TINY_DB_ENGINE_EXTMEM_DEVICE_HPP
