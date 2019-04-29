#ifndef TINY_DB_ENGINE_EXTMEM_DEVICE_HPP
#define TINY_DB_ENGINE_EXTMEM_DEVICE_HPP

#include "fs_error.hpp"
#include "dev.hpp"
#include "util.hpp"
#include <optional>
#include <boost/compute/container/valarray.hpp>
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
    using boost::compute::valarray;
    using s::slice;
    using std::byte;
    using std::optional;
    using std::string;
    using std::unique_ptr;
    using std::list;
    using tinydb::filesystem::util::sys_time_spec;
    using tinydb::filesystem::block_stream_device;
    using tinydb::filesystem::fs_error;
    using tinydb::filesystem::util::current_sys_time_spec;

    class extmem_device final : block_stream_device {
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
        size_t head_;
        size_t tail_;
        size_t count_;
        extmem_cache_index_t cache_index_;

        friend class extmem_device_manager;

        extmem_device(const size_t block_num, Buffer *buf) :
                buf_{buf}, block_num_{block_num}, head_{0},
                tail_{0}, count_{0},
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
                head_{rhs.head_}, tail_{rhs.tail_}, count_{rhs.count_},
                cache_index_{std::move(rhs.cache_index_)} {
            rhs.block_num_ = 0;
            rhs.buf_ = nullptr;
            rhs.head_ = 0;
            rhs.tail_ = 0;
            rhs.count_ = 0;
            rhs.cache_index_ = {};
        }

        extmem_device &operator=(const extmem_device &rhs) = delete;

        extmem_device &operator=(extmem_device &&rhs) noexcept {
            std::swap(block_num_, rhs.block_num_);
            std::swap(buf_, rhs.buf_);
            std::swap(head_, rhs.head_);
            std::swap(tail_, rhs.tail_);
            std::swap(count_, rhs.count_);
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
            return count_;
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
            return head_ == tail_;
        }

        inline bool cache_full() const {
            return cache_count() == cache_num();
        }

        ~extmem_device() override {
            if(buf_!= nullptr) {
                freeBuffer(buf_);
            }
        }

        bool read_at_b(const size_t block_id, valarray<byte> &buf, const slice &s) override {
            if (block_id == 0 || block_id > block_num() || s.size()<=block_size()|| buf.size()<=block_size()) {
                return false;
            }
            auto &block_id_indices = cache_index_.get<0>();
            auto &cache_id_indices = cache_index_.get<1>();
            auto &timespec_indices = cache_index_.get<2>();
            const auto found = block_id_indices.find(block_id);
            extmem_device_index index{};
            if (found == block_id_indices.end()) {
                if (cache_full()) {
                    index = *timespec_indices.begin(); // find earliest time
                    freeBlockInBuffer(buf_->data + cal_cache_offset(index.cache_id), buf_);
                } else {
                    index = *block_id_indices.find(0); // find empty cache
                }
                if (readBlockFromDisk(buf_->data + cal_cache_offset(index.cache_id),
                                      static_cast<unsigned int>(block_id), buf_) == nullptr) {
                    return false;
                }
                index.block_id = block_id;
            } else {
                index = *found;
            }
            const auto cache_id = index.cache_id;
            const auto target = cache_id_indices.find(cache_id);
            index.time = current_sys_time_spec();
            cache_id_indices.replace(target, index);
            cache_to_valarray(buf, s, cal_cache_offset(cache_id));
            return true;
        }

        bool write_at_b(size_t block_id, const valarray<byte> &buf, const slice &s) override {
            if (block_id == 0 || block_id > block_num() || s.size()<=block_size()|| buf.size()<=block_size()) {
                return false;
            }
            auto &block_id_indices = cache_index_.get<0>();
            auto &cache_id_indices = cache_index_.get<1>();
            auto &timespec_indices = cache_index_.get<2>();
            const auto found = block_id_indices.find(block_id);
            extmem_device_index index{};
            if (found == block_id_indices.end()) {
                if (cache_full()) {
                    index = *timespec_indices.begin(); // find earliest time
                    freeBlockInBuffer(buf_->data + cal_cache_offset(index.cache_id), buf_);
                } else {
                    index = *block_id_indices.find(0); // find empty cache
                }
            } else {
                index = *found;
            }
            const auto cache_id = index.cache_id;
            const auto target = cache_id_indices.find(cache_id);
            valarray_to_cache(buf, s, cal_cache_offset(cache_id));
            if (writeBlockToDisk(buf_->data + cal_cache_offset(cache_id), static_cast<unsigned int>(block_id), buf_) <
                0) {
                return false;
            }
            index.time = current_sys_time_spec();
            index.block_id = block_id;
            cache_id_indices.replace(target, index);
            return true;
        }
    };

    class extmem_device_manager {
    public:
        static optional<extmem_device> make(const string &name, const size_t block_num, const size_t block_size,
                                            const size_t buffer_size) {
            Buffer buf{};
            const auto target_path = boost::filesystem::path{"./data"}.append(name);
            if(boost::filesystem::is_directory(target_path)) {
                boost::filesystem::remove_all(target_path);
            }
            if(!boost::filesystem::exists(target_path)) {
                boost::filesystem::create_directory(target_path);
            }
            const auto res = initBuffer(name.c_str(), buffer_size, block_size, &buf);
            if (res == nullptr) {
                return {};
            } else {
                return extmem_device{block_num, &buf};
            }
        }
    };
}

#endif // TINY_DB_ENGINE_EXTMEM_DEVICE_HPP
