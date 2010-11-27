#include <runtime.h>
#include "llvm/Support/PluginLoader.h"
#include <iostream>

using namespace llvm;
using namespace std;

BABEL_EXPORT void tensorlang_load_library (char *library_name)
{
	static PluginLoader pluginLoader;
	pluginLoader = library_name;
}

