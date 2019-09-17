#ifndef TINY_DB_ENGINE_RESULT_HPP
#define TINY_DB_ENGINE_RESULT_HPP

#include <variant>
#include <exception>

namespace tinydb::util {
    template<typename T, typename Err = std::exception>
    using result = std::variant<T, Err>;

#define OK(x) \
return x

#define ERR(x) \
return x

#define UNWRAP(x,name,...) \
	std::variant_alternative_t<0,decltype( x )> __unwrap_##name##__; \
	try { \
	  __unwrap_##name##__ = std::get<0>( x );\
	} catch(std::bad_variant_access& ){ \
		return std::get<1>( x ); \
	}  \
	__VA_ARGS__ auto &name = __unwrap_##name##__;

#define UNWRAP_E(x,name,err,...) \
	std::variant_alternative_t<0, decltype(x)> __unwrap_##name##__; \
	try { \
	  __unwrap_##name##__ = std::get<0>( x );\
	} catch(std::bad_variant_access& ){ \
		return err; \
	}  \
	__VA_ARGS__ auto &name = __unwrap_##name##__;
}

#endif //TINY_DB_ENGINE_RESULT_HPP
