/* */
#pragma once
typedef void *va_list;
#define va_start(args, fmt)
#define va_end(args)

#define vprintf(...) 0
#define vfprintf(...) 0
#define vdprintf(...) 0
#define vsprintf(...) 0
#define vsnprintf(...) 0

