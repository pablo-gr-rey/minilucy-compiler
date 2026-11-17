// file auto-generated from Lustre file short.lus
#include <stdio.h>
#include "short.h"

int main(void) {
    test_if_merge_mem__69 mem;
    test_if_merge_reset__72(&mem);
    test_if_merge_ret__70 ret;
    bool input_a__8;
    char inputtmp_a__8;
    printf("#inputs \"a\":bool\n#outputs \"s1\":int, \"s2\":int, \"s3\":int\n\n");
    
    
    for (int step = 0; ; step++) {
        printf("#step %d\n", step);
        if (scanf(" %c", &inputtmp_a__8) != 1) {
            printf("Failed to read input; exiting\n");
            return 1;
        }
        bool bool_OK = true;
        if (inputtmp_a__8 == '1' || inputtmp_a__8 == 'T' || inputtmp_a__8 == 't') input_a__8 = true;
        else {
            input_a__8 = false;
            if (inputtmp_a__8 != '0' && inputtmp_a__8 != 'F' && inputtmp_a__8 != 'f') bool_OK = false;
        }
        if (!bool_OK) {
            printf("Failed to read input; exiting\n");
            return 1;
        }
        
        ret = test_if_merge_step__73(input_a__8, &mem);
        
        printf("%d #outs %d, %d, %d\n\n", input_a__8, ret.s1__9, ret.s2__10, ret.s3__11);
    }
}
