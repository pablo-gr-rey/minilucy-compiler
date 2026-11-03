// file auto-generated from Lustre file test.lus
#include <stdio.h>
#include "test.h"

int main(void) {
    mod_count_mem__121 mem;
    mod_count_reset__124(&mem);
    mod_count_ret__122 ret;
    int input_m__14;
    printf("#inputs \"m\":int\n#outputs \"out\":int\n\n");
    
    
    for (int step = 0; ; step++) {
        printf("#step %d\n", step);
        if (scanf("%d", &input_m__14) != 1) {
            printf("Failed to read input; exiting\n");
            return 1;
        };
        
        ret = mod_count_step__125(input_m__14, &mem);
        
        printf("%d #outs %d\n\n", input_m__14, ret.out__15);
    }
}
