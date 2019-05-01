#ifndef TINY_DB_ENGINE_FILESYSTEM_VFS_HPP
#define TINY_DB_ENGINE_FILESYSTEM_VFS_HPP

#include <valarray>
#include <string>
#include <memory>
#include <cstdint>
#include <any>
#include <optional>
#include <type_traits>
#include <vector>
#include <variant>
#include <filesystem>
#include "fs_error.hpp"
#include "result.hpp"
#include "util.hpp"
#include "fs_config.hpp"

namespace tinydb::filesystem
{
	using tinydb::filesystem::util::valarray_to_container;
	template<typename T>
	using fs_result = tinydb::util::result<T,tinydb::filesystem::fs_error>;
	using tinydb::filesystem::util::sys_time_spec;
	using std::filesystem::path;
	using std::valarray;
	using std::byte;
	using std::valarray;
	using std::string;
	using std::shared_ptr;
	using std::any;
	using std::weak_ptr;
	using std::optional;
	using std::vector;
	using std::monostate;
	using std::exception;
	using std::slice;

	struct poll_status;
	struct fs_info;
	struct inode;

	enum struct file_type: uint16_t
	{
		file = 0,
		dir,
		sym_link,
		char_device,
		block_device,
		named_pipe,
		socket,
	};
	struct metadata
	{
	public:
		size_t dev;
		size_t inode;
		size_t size;
		size_t blk_size;
		size_t blocks;
		sys_time_spec atime;
		sys_time_spec mtime;
		sys_time_spec ctime;
		file_type type;
		uint16_t mode;
		size_t links;
		size_t uid;
		size_t gid;
	};
	struct file_system
	{
	public:
		virtual fs_result<monostate> sync() = 0;
		virtual shared_ptr<inode> root_inode() const = 0;
		virtual fs_info info() const = 0;
		virtual ~file_system() = default;
	};

	struct inode
	{
		virtual fs_result<size_t> read_at(const size_t offset, valarray<byte>& buf, const slice& s) = 0;
		virtual fs_result<size_t> write_at(const size_t offset, const valarray<byte>& buf, const slice& s) = 0;
		virtual fs_result<poll_status> poll() = 0;
		virtual fs_result<metadata> metadata() = 0;
		virtual fs_result<monostate> metadata(struct metadata&) = 0;
		virtual fs_result<monostate> sync_all() = 0;
		virtual fs_result<monostate> sync_data() = 0;
		virtual fs_result<monostate> resize(size_t len) = 0;
		virtual fs_result<shared_ptr<inode>> create(const path &name, file_type type, uint32_t mode) = 0;
		virtual fs_result<monostate> link(const path &name, shared_ptr<inode>& other) = 0;
		virtual fs_result<monostate> unlink(path name) = 0;
		virtual fs_result<monostate> move(const path &old_name, shared_ptr<inode> target, path new_name) = 0;
		virtual fs_result<shared_ptr<inode>> find(const path& name) = 0;
		virtual fs_result<path> entry(size_t id) = 0;
		virtual fs_result<monostate> io_control(uint32_t cmd, size_t data) = 0;
		virtual shared_ptr<file_system> filesystem() = 0;
		virtual ~inode() = default;

		template <typename T>
		std::enable_if_t<std::is_base_of<inode, T>::value, optional<T&>> downcast_ref()
		{
			try
			{
				return &*dynamic_cast<T*>(this);
			}
			catch (std::bad_cast e)
			{
				return {};
			}
		}

		fs_result<vector<path>> list()
		{
			UNWRAP(metadata(),info,const);
			if (info.type != file_type::dir)
			{
				ERR(fs_error::not_dir);
			}
			auto res = vector<path>{};
			for (size_t i = 0; i < info.size; i++)
			{
				UNWRAP(entry(i), item);
				res.push_back(item);
			}
			OK(res);
		}

		fs_result<shared_ptr<inode>> lookup(const path& pth)
		{
			return lookup_follow(pth, 0);
		}

		fs_result<shared_ptr<inode>> lookup_follow(path pth, size_t follow_times)
		{
			UNWRAP(metadata(),info,const)
			if (info.type != file_type::dir)
			{
				ERR(fs_error::not_dir);
			}
			UNWRAP(find("."),current);
			pth = pth.generic_string();
			for (auto p_pth = pth.begin();p_pth!=pth.end();++p_pth)
			{
				auto entry = *p_pth;
				if (entry.empty())
				{
					break;
				}
				UNWRAP(current->metadata(), current_metadata, const);
				if (current_metadata.type != file_type::dir)
				{
					ERR(fs_error::not_dir);
				}
				if (entry.has_root_path())
				{
					current = filesystem()->root_inode();
					continue;
				}
				UNWRAP(current->find(entry),node,const);
				UNWRAP(node->metadata(), node_metadata, const);
				if (node_metadata.type == file_type::sym_link && follow_times > 0)
				{
					follow_times--;
					auto content = valarray<byte>(config::max_file_name);
					const auto s = slice(0, config::max_file_name, 1);
					UNWRAP(node->read_at(0, content,s), size_t,const);
					auto link_string = string{};
					valarray_to_container(link_string, content);
					++p_pth;
					for(auto other = p_pth;p_pth!=pth.end();++other)
					{
						link_string += p_pth->string();
					}
					return lookup_follow(link_string, follow_times);
				}
				else
				{
					current = node;
				}
				
			}
			OK(current);
		}
	};

	struct poll_status
	{
	public:
		bool read;
		bool write;
		bool error;
	};

	struct fs_info
	{
	public:
		size_t block_size;
		size_t fr_size;
		size_t block_num;
		size_t block_free_num;
		size_t block_avaiable_num;
		size_t file_num;
		size_t file_free;
		size_t max_name;
	};

}

#endif //TINY_DB_ENGINE_FILESYSTEM_VFS_HPP
