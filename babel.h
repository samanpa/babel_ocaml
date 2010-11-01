#ifndef BABEL_H
#define BABEL_H

#ifdef WIN32
/* we should perhaps do a dllimport too but that might cause some 
   trouble when compiling with mingw*/
#define BABEL_EXPORT __declspec(dllexport)
#else
#define BABEL_EXPORT
#endif


#endif
