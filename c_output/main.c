// file auto-generated from Lustre file short.lus
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "short.h"
#include "lv6/short_proc0.h"

int main(void) {
    short_proc0_ctx_type* ctx = short_proc0_ctx_new_ctx();
    proc0_mem__109 mem;
    proc0_reset__112(&mem);
    proc0_ret__110 ret;
    _integer output_pc__30;
    _boolean output_asks_cs0__31;
    bool input_cpu__29;
    
    printf("testing node proc0 (file short.lus) on 10000 steps...\n");
    
    for (int step = 0; step < 10000; step++) {
        input_cpu__29 = (bool)(rand() & 1);
        ret = proc0_step__113(input_cpu__29, &mem);
        short_proc0_step(input_cpu__29, &output_pc__30, &output_asks_cs0__31, ctx);
        if (output_pc__30 != ret.pc__30) {
            printf("ERROR at step %d for node proc0__33, field pc__30: got %d, expected %d\n", step, ret.pc__30, output_pc__30);
            return 1;
        }
        if (output_asks_cs0__31 != ret.asks_cs0__31) {
            printf("ERROR at step %d for node proc0__33, field asks_cs0__31: got %d, expected %d\n", step, ret.asks_cs0__31, output_asks_cs0__31);
            return 1;
        }
        
    }
    printf("Ran for 10000 steps, got the same output!\n");
    
}
