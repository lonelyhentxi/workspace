#include <iostream>
#include <memory>
#include "i_application.hpp"

namespace enfw
{
	namespace ge_demo
	{
		extern std::unique_ptr<i_application> pg_app;
	}
}

int main(int argc, char ** argv) 
{
	using enfw::ge_demo::pg_app;
	int ret;
	if((ret = pg_app -> initialize())!=0)
	{
		std::cout<<"App Initialize failed, will exit now."<<std::endl;
		return ret;
	}

	while(!pg_app->is_quit())
	{
		pg_app->tick();
	}
	pg_app->finalize();
	return 0;
}