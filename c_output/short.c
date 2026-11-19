// file auto-generated from Lustre file short.lus
#include "short.h"

void count_reset__72(count_mem__69* self) {
    self->temp34_tmp__56 = true;
    self->temp29_tmp__55 = 0;
}

count_ret__70 count_step__73(count_mem__69* self) {
    bool temp34__34;
    int temp32__32;
    int temp31__31;
    int temp29__29;
    count_ret__70 count_result__71;
    
    temp31__31 = 0;
    temp29__29 = self->temp29_tmp__55;
    temp32__32 = (1) + (temp29__29);
    temp34__34 = self->temp34_tmp__56;
    if (temp34__34) {
        count_result__71.a__3 = temp31__31;
    }
    else {
        count_result__71.a__3 = temp32__32;
    }
    self->temp34_tmp__56 = false;
    self->temp29_tmp__55 = count_result__71.a__3;
    
    return count_result__71;
}
idiot_ret__74 idiot_step__76() {
    idiot_ret__74 idiot_result__75;
    
    idiot_result__75.a__5 = 3;
    
    return idiot_result__75;
}
void count_tick_reset__80(count_tick_mem__77* self) {
    self->temp40_tmp__58 = true;
    self->temp35_tmp__57 = 0;
}

count_tick_ret__78 count_tick_step__81(bool tick__7, count_tick_mem__77* self) {
    bool temp40__40;
    int temp38__38;
    int temp37__37;
    int temp35__35;
    count_tick_ret__78 count_tick_result__79;
    
    temp37__37 = 0;
    temp35__35 = self->temp35_tmp__57;
    temp38__38 = (1) + (temp35__35);
    temp40__40 = self->temp40_tmp__58;
    if (temp40__40) {
        count_tick_result__79.a__8 = temp37__37;
    }
    else {
        count_tick_result__79.a__8 = temp38__38;
    }
    self->temp40_tmp__58 = false;
    self->temp35_tmp__57 = count_tick_result__79.a__8;
    
    return count_tick_result__79;
}
nomem_ret__82 nomem_step__84(int a__10, int b__11) {
    nomem_ret__82 nomem_result__83;
    
    nomem_result__83.c__12 = (a__10) + (b__11);
    
    return nomem_result__83;
}
void test_reset__88(test_mem__85* self) {
    count_tick_reset__80(&self->count_tick_64__64);
    count_tick_reset__80(&self->count_tick_63__63);
    count_reset__72(&self->count_62__62);
    count_reset__72(&self->count_61__61);
    count_reset__72(&self->count_60__60);
    count_reset__72(&self->count_59__59);
}

test_ret__86 test_step__89(bool a__14, test_mem__85* self) {
    int temp50__50;
    int temp49__49;
    int temp48__48;
    int temp47__47;
    int temp42__42;
    int temp41__41;
    test_ret__86 test_result__87;
    count_ret__70 count_result__103;
    count_ret__70 count_result__102;
    count_ret__70 count_result__101;
    count_ret__70 count_result__100;
    count_tick_ret__78 count_tick_result__99;
    count_tick_ret__78 count_tick_result__98;
    
    if (a__14) {
        count_tick_result__99 = count_tick_step__81(true, &self->count_tick_63__63);
        temp50__50 = count_tick_result__99.a__8;
        test_result__87.s3__17 = temp50__50;
    }
    else {
        count_tick_result__98 = count_tick_step__81(true, &self->count_tick_64__64);
        temp49__49 = count_tick_result__98.a__8;
        test_result__87.s3__17 = temp49__49;
    }
    count_result__100 = count_step__73(&self->count_62__62);temp47__47 = count_result__100.a__3;
    count_result__101 = count_step__73(&self->count_61__61);temp48__48 = count_result__101.a__3;
    if (a__14) {
        test_result__87.s2__16 = temp48__48;
    }
    else {
        test_result__87.s2__16 = temp47__47;
    }
    count_result__102 = count_step__73(&self->count_60__60);temp41__41 = count_result__102.a__3;
    count_result__103 = count_step__73(&self->count_59__59);temp42__42 = count_result__103.a__3;
    if (a__14) {
        test_result__87.s1__15 = temp42__42;
    }
    else {
        test_result__87.s1__15 = temp41__41;
    }
    
    return test_result__87;
}
void test2_reset__93(test2_mem__90* self) {
    count_reset__72(&self->count_68__68);
    count_reset__72(&self->count_67__67);
    count_reset__72(&self->count_66__66);
    count_reset__72(&self->count_65__65);
}

test2_ret__91 test2_step__94(bool a__19, test2_mem__90* self) {
    int temp54__54;
    int temp53__53;
    int temp52__52;
    int temp51__51;
    int tmp2__23;
    int tmp1__22;
    test2_ret__91 test2_result__92;
    count_ret__70 count_result__107;
    count_ret__70 count_result__106;
    count_ret__70 count_result__105;
    count_ret__70 count_result__104;
    
    count_result__104 = count_step__73(&self->count_68__68);temp51__51 = count_result__104.a__3;
    if (a__19) {
        tmp1__22 = temp51__51;
    }
    else {
        
    }
    count_result__105 = count_step__73(&self->count_67__67);temp52__52 = count_result__105.a__3;
    if (a__19) {
        test2_result__92.s1__20 = tmp1__22;
    }
    else {
        tmp2__23 = temp52__52;
        test2_result__92.s1__20 = tmp2__23;
    }
    count_result__106 = count_step__73(&self->count_66__66);temp53__53 = count_result__106.a__3;
    count_result__107 = count_step__73(&self->count_65__65);temp54__54 = count_result__107.a__3;
    if (a__19) {
        test2_result__92.s2__21 = temp54__54;
    }
    else {
        test2_result__92.s2__21 = temp53__53;
    }
    
    return test2_result__92;
}
test_real_ret__95 test_real_step__97(double a__25, double b__26) {
    test_real_ret__95 test_real_result__96;
    
    test_real_result__96.c__27 = ((a__25) + ((b__26) * (a__25))) + (b__26);
    
    return test_real_result__96;
}

