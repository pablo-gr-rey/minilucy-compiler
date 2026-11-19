// file auto-generated from Lustre file short.lus
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "short.h"
#include "lv6/short_test_real.h"

int main(void) {
    short_test_real_ctx_type* ctx = short_test_real_ctx_new_ctx();
    test_real_ret__95 ret;
    double output_c__27;
    double input_a__25;
    double input_b__26;
    
    printf("testing node test_real (file short.lus) on 10000 steps...\n");
    
    for (int step = 0; step < 10000; step++) {
        input_a__25 = (double)rand() / (double)RAND_MAX * 100.0;
        input_b__26 = (double)rand() / (double)RAND_MAX * 100.0;
        ret = test_real_step__97(input_a__25, input_b__26);
        short_test_real_step(input_a__25, input_b__26, &output_c__27, ctx);
        if (fabs(output_c__27 - ret.c__27) > 1e-5) {
            printf("ERROR at step %d for node test_real__28, field c__27: got %lf, expected %lf\n", step, ret.c__27, output_c__27);
            return 1;
        }
        
    }
    printf("Ran for 10000 steps, got the same output!\n");
    
}
