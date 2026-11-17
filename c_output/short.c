// file auto-generated from Lustre file short.lus
#include "short.h"

void count_reset__62(count_mem__59* self) {
    self->temp24_tmp__46 = true;
    self->temp19_tmp__45 = 0;
}

count_ret__60 count_step__63(count_mem__59* self) {
    bool temp24__24;
    int temp22__22;
    int temp21__21;
    int temp19__19;
    count_ret__60 count_result__61;
    
    temp21__21 = 0;
    temp19__19 = self->temp19_tmp__45;
    temp22__22 = (1) + (temp19__19);
    temp24__24 = self->temp24_tmp__46;
    if (temp24__24) {
        count_result__61.a__3 = temp21__21;
    }
    else {
        count_result__61.a__3 = temp22__22;
    }
    self->temp24_tmp__46 = false;
    self->temp19_tmp__45 = count_result__61.a__3;
    
    return count_result__61;
}
void count_tick_reset__67(count_tick_mem__64* self) {
    self->temp30_tmp__48 = true;
    self->temp25_tmp__47 = 0;
}

count_tick_ret__65 count_tick_step__68(bool tick__5, count_tick_mem__64* self) {
    bool temp30__30;
    int temp28__28;
    int temp27__27;
    int temp25__25;
    count_tick_ret__65 count_tick_result__66;
    
    temp27__27 = 0;
    temp25__25 = self->temp25_tmp__47;
    temp28__28 = (1) + (temp25__25);
    temp30__30 = self->temp30_tmp__48;
    if (temp30__30) {
        count_tick_result__66.a__6 = temp27__27;
    }
    else {
        count_tick_result__66.a__6 = temp28__28;
    }
    self->temp30_tmp__48 = false;
    self->temp25_tmp__47 = count_tick_result__66.a__6;
    
    return count_tick_result__66;
}
void test_if_merge_reset__72(test_if_merge_mem__69* self) {
    count_tick_reset__67(&self->count_tick_54__54);
    count_tick_reset__67(&self->count_tick_53__53);
    count_reset__62(&self->count_52__52);
    count_reset__62(&self->count_51__51);
    count_reset__62(&self->count_50__50);
    count_reset__62(&self->count_49__49);
}

test_if_merge_ret__70 test_if_merge_step__73(bool a__8, test_if_merge_mem__69* self) {
    int temp40__40;
    int temp39__39;
    int temp38__38;
    int temp37__37;
    int temp32__32;
    int temp31__31;
    test_if_merge_ret__70 test_if_merge_result__71;
    count_ret__60 count_result__84;
    count_ret__60 count_result__83;
    count_ret__60 count_result__82;
    count_ret__60 count_result__81;
    count_tick_ret__65 count_tick_result__80;
    count_tick_ret__65 count_tick_result__79;
    
    if (a__8) {
        count_tick_result__80 = count_tick_step__68(true, &self->count_tick_53__53);
        temp40__40 = count_tick_result__80.a__6;
        test_if_merge_result__71.s3__11 = temp40__40;
    }
    else {
        count_tick_result__79 = count_tick_step__68(true, &self->count_tick_54__54);
        temp39__39 = count_tick_result__79.a__6;
        test_if_merge_result__71.s3__11 = temp39__39;
    }
    count_result__81 = count_step__63(&self->count_52__52);temp37__37 = count_result__81.a__3;
    count_result__82 = count_step__63(&self->count_51__51);temp38__38 = count_result__82.a__3;
    if (a__8) {
        test_if_merge_result__71.s2__10 = temp38__38;
    }
    else {
        test_if_merge_result__71.s2__10 = temp37__37;
    }
    count_result__83 = count_step__63(&self->count_50__50);temp31__31 = count_result__83.a__3;
    count_result__84 = count_step__63(&self->count_49__49);temp32__32 = count_result__84.a__3;
    if (a__8) {
        test_if_merge_result__71.s1__9 = temp32__32;
    }
    else {
        test_if_merge_result__71.s1__9 = temp31__31;
    }
    
    return test_if_merge_result__71;
}
void test2_reset__77(test2_mem__74* self) {
    count_reset__62(&self->count_58__58);
    count_reset__62(&self->count_57__57);
    count_reset__62(&self->count_56__56);
    count_reset__62(&self->count_55__55);
}

test2_ret__75 test2_step__78(bool a__13, test2_mem__74* self) {
    int temp44__44;
    int temp43__43;
    int temp42__42;
    int temp41__41;
    int tmp2__17;
    int tmp1__16;
    test2_ret__75 test2_result__76;
    count_ret__60 count_result__88;
    count_ret__60 count_result__87;
    count_ret__60 count_result__86;
    count_ret__60 count_result__85;
    
    count_result__85 = count_step__63(&self->count_58__58);temp41__41 = count_result__85.a__3;
    if (a__13) {
        tmp1__16 = temp41__41;
    }
    else {
        
    }
    count_result__86 = count_step__63(&self->count_57__57);temp42__42 = count_result__86.a__3;
    if (a__13) {
        test2_result__76.s1__14 = tmp1__16;
    }
    else {
        tmp2__17 = temp42__42;
        test2_result__76.s1__14 = tmp2__17;
    }
    count_result__87 = count_step__63(&self->count_56__56);temp43__43 = count_result__87.a__3;
    count_result__88 = count_step__63(&self->count_55__55);temp44__44 = count_result__88.a__3;
    if (a__13) {
        test2_result__76.s2__15 = temp44__44;
    }
    else {
        test2_result__76.s2__15 = temp43__43;
    }
    
    return test2_result__76;
}

