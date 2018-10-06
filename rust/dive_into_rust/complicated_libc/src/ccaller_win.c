#include <stdio.h>
#include <stdlib.h>
#include <windows.h>

struct RustLogMessage {
    int id;
    char *msg;
};

typedef void (*FUNCADDR)(struct RustLogMessage msg);

int main() {
	HINSTANCE rust_log_lib = LoadLibrary("rust_log.dll"); //
	FUNCADDR rust_log_fn;
	if(rust_log_lib!=NULL)
	{
		rust_log_fn = (FUNCADDR)GetProcAddress(rust_log_lib, "rust_log");
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
		FreeLibrary(rust_log_lib);
		return 0;
	}
}