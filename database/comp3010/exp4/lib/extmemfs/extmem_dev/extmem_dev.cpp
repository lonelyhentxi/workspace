#include "extmem_dev.hpp"
#include "util.hpp"
#include <filesystem>

namespace tinydb::extmem
{
	bool extmem_device::read_at_b(const size_t block_id, valarray<byte>& buf, const slice& s)  {
		if (block_id == 0 || block_id > block_num() + 1 || s.size() < block_size() || buf.size() < block_size()) {
			return false;
		}
		auto& block_id_indices = cache_index_.get<0>();
		auto& cache_id_indices = cache_index_.get<1>();
		auto& timespec_indices = cache_index_.get<2>();
		const auto found = block_id_indices.find(block_id);
		extmem_device_index index;
		if (found == block_id_indices.end()) {
			auto& zero = block_id_indices.find(0);
			if (cache_full()||zero== block_id_indices.end()) {
				auto &tmp = timespec_indices.begin(); // find earliest time
				index.block_id = tmp->block_id;
				index.cache_id = tmp->cache_id;
				index.time = tmp->time;
				freeBlockInBuffer(buf_->data + cal_cache_offset(index.cache_id), buf_);
			}
			else {
				index.block_id = zero->block_id;
				index.cache_id = zero->cache_id;
				index.time = zero->time;
			}
			
			if (readBlockFromDisk(buf_->data + cal_cache_offset(index.cache_id),
				static_cast<unsigned int>(block_id), buf_) == nullptr) {
				return false;
			}
			index.block_id = block_id;
		}
		else {
			auto &tmp = found;
			index.block_id = tmp->block_id;
			index.cache_id = tmp->cache_id;
			index.time = tmp->time;
		}
		const auto cache_id = index.cache_id;
		const auto target = cache_id_indices.find(cache_id);
		index.time = filesystem::util::current_sys_time_spec();
		cache_id_indices.replace(target, index);
		cache_to_valarray(buf, s, cal_cache_offset(cache_id));
		return true;
	}

	bool extmem_device::write_at_b(size_t block_id, const valarray<byte>& buf, const slice& s)  {
		if (block_id == 0 || block_id > block_num() + 1 || s.size() < block_size() || buf.size() < block_size()) {
			return false;
		}
		auto& block_id_indices = cache_index_.get<0>();
		auto& cache_id_indices = cache_index_.get<1>();
		auto& timespec_indices = cache_index_.get<2>();
		const auto found = block_id_indices.find(block_id);
		extmem_device_index index;
		if (found == block_id_indices.end()) {
			auto& zero = block_id_indices.find(0);
			if (cache_full()||zero==block_id_indices.end()) {
				auto& tmp = timespec_indices.begin(); // find earliest time
				index.block_id = tmp->block_id;
				index.cache_id = tmp->cache_id;
				index.time = tmp->time;
				freeBlockInBuffer(buf_->data + cal_cache_offset(index.cache_id), buf_);
			}
			else {
				 // find empty cache
				index.block_id = zero->block_id;
				index.cache_id = zero->cache_id;
				index.time = zero->time;
			}
		}
		else {
			index.cache_id = found->cache_id;
			index.block_id = found->block_id;
			index.time = found->time;
		}
		const auto cache_id = index.cache_id;
		const auto target = cache_id_indices.find(cache_id);
		valarray_to_cache(buf, s, cal_cache_offset(cache_id));
		if (writeBlockToDisk(buf_->data + cal_cache_offset(cache_id), static_cast<unsigned int>(block_id), buf_) < 0) {
			return false;
		}
		index.time = filesystem::util::current_sys_time_spec();
		index.block_id = block_id;
		cache_id_indices.replace(target, index);
		return true;
	}

	optional<extmem_device> extmem_device_manager::make(const string& name, const size_t block_num, const size_t block_size,
		const size_t buffer_size) {
		auto buf = new Buffer{};
		const auto target_path = boost::filesystem::path{ "./data" }.append(name);
		if (!boost::filesystem::is_directory(target_path)) {
			boost::filesystem::remove_all(target_path);
		}
		if (!boost::filesystem::exists(target_path)) {
			boost::filesystem::create_directory(target_path);
		}
		const auto res = initBuffer(name.c_str(), buffer_size, block_size, buf);
		if (res == nullptr) {
			return {};
		}
		else {
			return extmem_device{ block_num, buf };
		}
	}
}
