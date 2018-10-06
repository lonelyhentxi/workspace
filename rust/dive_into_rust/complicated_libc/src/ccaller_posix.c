#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>

struct RustLogMessage {
    int id;
    char *msg;
};

typedef void (*func_addr)(struct RustLogMessage msg);

int main() {
    void *rust_log_lib = dlopen("rust_log.so",RTLD_LAZY);
    func_addr rust_log_fn;
    if(rust_log_lib!=NULL)
    {
        rust_log_fn = dlsym(rust_log_lib,"rust_log");
    }
    else
    {
        fprintf(stderr, "load dy library failed.\n");
        return -1;
    }
    for(int i=0;i<10;i++)
    {
        struct RustLogMessage msg;
        msg.id = i;
        msg.msg = "string in C\n";
        rust_log_fn(msg);
    }
    if(rust_log_fn!=NULL)
    {
        dlclose(rust_log_lib);
        return 0;
    }
}