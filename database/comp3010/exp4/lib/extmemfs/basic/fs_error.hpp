#ifndef TINY_DB_ENGINE_FILESYSTEM_ERROR_HPP
#define TINY_DB_ENGINE_FILESYSTEM_ERROR_HPP

namespace tinydb::filesystem {
	enum class fs_error {
		not_supported,  //E_UNIMP, or E_INVAL
		not_file,       //E_ISDIR
		is_dir,         //E_ISDIR, used only in link
		not_dir,        //E_NOTDIR
		entry_not_found, //E_NOENT
		entry_exist,    //E_EXIST
		not_same_fs,     //E_XDEV
		invalid_param,  //E_INVAL
		no_device_space, //E_NOSPC, but is defined and not used in the original ucore, which uses E_NO_MEM
		dir_removed,    //E_NOENT, when the current dir was remove by a previous unlink
		dir_not_empty,   //E_NOTEMPTY
		wrong_fs,       //E_INVAL, when we find the content on disk is wrong when opening the device
		device_error,
	};
}

#endif //TINY_DB_ENGINE_FILESYSTEM_ERROR_HPP
