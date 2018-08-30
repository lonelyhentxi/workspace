#include <iostream>
#include <boost/type_index.hpp>

namespace runtime_output {
    const int theAnswer = 42;
    auto x = theAnswer;
    auto &y = theAnswer;

    /*
    template<typename T>
    class TD;
    TD<decltype(x)> xType; // x:int
    TD<decltype(y)> yType; // x: const int &
     */

    void show_output() {
        std::cout<< typeid(x).name()<<std::endl; // i
        std::cout<< typeid(y).name()<<std::endl; // i rather than PKi ?
    }
}

template<typename T>
void f(const T& param) {
    using std::cout;
    using boost::typeindex::type_id_with_cvr;
    cout << "T = "
    << type_id_with_cvr<T>().pretty_name()
    << std::endl;

    cout << "param = "
    << type_id_with_cvr<decltype(param)>().pretty_name()
    << '\n';
}

int main() {
    runtime_output::show_output();
    f(runtime_output::x);
}