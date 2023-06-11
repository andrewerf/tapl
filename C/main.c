#include "Lambda2/Lambda2_stub.h"
#include <stdio.h>

int main()
{
    hs_init( 0, NULL );
    printf(processStringExported("\\x:Int.x"));
    hs_exit();
    return 0;
}
