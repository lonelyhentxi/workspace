#ifndef TINY_DB_ENGINE_FACTORY_HPP
#define TINY_DB_ENGINE_FACTORY_HPP

#include <unordered_map>
#include <memory>
#include <optional>
#include <string>
#include "type_util.hpp"

namespace tinydb::util {
    using std::unique_ptr;
    using std::unordered_map;
    using std::forward;
    using std::make_unique;
    using std::pair;
    using std::make_pair;
    using std::optional;
    using std::string;

    template<typename Base, typename... Args>
    class sorter {
    public:
        template<class T>
        struct registrar : Base {
            friend T;

            static bool register_t() {
                const auto name = type_name<T>();
                sorter::types()[name] = make_pair<ctor_type, matcher_type>(
                        [](Args... args) -> unique_ptr<Base> {
                            return make_unique<T>(forward<Args>(args)...);
                        },
                        [](const Args &... args) -> bool {
                            return T::match(args...);
                        });
                return true;
            }

            static bool registered;

        private:
            registrar() : Base(key{}) { (void) registered; };
        };

        template<typename ... T>
        static optional<unique_ptr<Base>> create(T &&... args) {
            const auto iter = std::find_if(types().cbegin(), types().cend(),
                                           [&](const auto &kv) -> bool {
                                               const auto &matcher = kv.second.second;
                                               return matcher(args...);
                                           });
            if (iter == types().cend()) {
                if (Base::match(args...)) {
                    return {make_unique<Base>(forward<T>(args)...)};
                } else {
                    return {};
                }

            }
            return {iter->second.first(forward<T>(args)...)};
        }

        friend Base;
    private:
        using ctor_type = unique_ptr<Base>(*)(Args...);
        using matcher_type = bool (*)(const Args &...);

        sorter() = default;

        static auto &types() {
            static unordered_map<string, pair<ctor_type, matcher_type>> types_;
            return types_;
        }

        class key {
            key() = default;

            template<class T> friend
            struct registrar;
        };
    };

    template<typename Base, typename ... Args>
    template<typename T>
    bool sorter<Base, Args...>::registrar<T>::registered =
            sorter<Base, Args...>::registrar<T>::register_t();

    template<typename Base, typename... Args>
    class factory {
    public:
        template<class T>
        struct registrar : Base {
            friend T;

            static bool register_t() {
                const auto name = type_name<T>();
                factory::types()[name] =
                        [](Args... args) -> unique_ptr<Base> {
                            return make_unique<T>(forward<Args>(args)...);
                        };
                return true;
            }

            static bool registered;
        private:
            registrar() : Base(key{}) { (void) registered; };
        };

        template<typename ... T>
        static unique_ptr<Base> create(const string &s, T &&... args) {
            return types().at(s)(forward<T>(args)...);
        }

        friend Base;
    private:
        using func_type = unique_ptr<Base>(*)(Args...);

        factory() = default;

        static auto &types() {
            static unordered_map<string, func_type> types_;
            return types_;
        }

        class key {
            key() = default;

            template<class T> friend
            struct registrar;
        };
    };

    template<typename Base, typename ... Args>
    template<typename T>
    bool factory<Base, Args...>::registrar<T>::registered =
            factory<Base, Args...>::registrar<T>::register_t();
}

#endif