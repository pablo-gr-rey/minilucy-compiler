// file auto-generated from Lustre file test.lus
#include "test.h"

void counting_reset__109(counting_mem__106* self) {
    self->temp51_tmp__84 = true;
    self->temp48_tmp__83 = 0;
}

counting_ret__107 counting_step__110(bool tick__3, bool top__4, counting_mem__106* self) {
    int temp54__54;
    bool temp51__51;
    int temp49__49;
    int temp48__48;
    counting_ret__107 counting_result__108;
    
    if (top__4) {
        temp54__54 = 1;
    }
    else {
        temp54__54 = 0;
    }
    temp48__48 = self->temp48_tmp__83;
    temp51__51 = self->temp51_tmp__84;
    if (temp51__51) {
        temp49__49 = 0;
    }
    else {
        temp49__49 = (temp48__48) + (temp54__54);
    }
    if (tick__3) {
        counting_result__108.o__5 = temp54__54;
    }
    else {
        counting_result__108.o__5 = temp49__49;
    }
    self->temp51_tmp__84 = false;
    self->temp48_tmp__83 = counting_result__108.o__5;
    
    return counting_result__108;
}
void h_reset__114(h_mem__111* self) {
    self->temp56_tmp__86 = false;
    self->temp57_tmp__85 = true;
}

h_ret__112 h_step__115(bool b__8, h_mem__111* self) {
    bool temp57__57;
    bool temp56__56;
    h_ret__112 h_result__113;
    
    temp57__57 = self->temp57_tmp__85;
    temp56__56 = self->temp56_tmp__86;
    if (temp57__57) {
        h_result__113.e__9 = !(b__8);
    }
    else {
        h_result__113.e__9 = temp56__56;
    }
    self->temp56_tmp__86 = b__8;
    self->temp57_tmp__85 = false;
    
    return h_result__113;
}
void redge_reset__119(redge_mem__116* self) {
    self->temp63_tmp__89 = true;
    self->temp60_tmp__88 = false;
    self->temp59_tmp__87 = false;
}

redge_ret__117 redge_step__120(bool b__11, redge_mem__116* self) {
    bool temp63__63;
    bool temp60__60;
    bool temp59__59;
    redge_ret__117 redge_result__118;
    
    temp59__59 = self->temp59_tmp__87;
    temp60__60 = self->temp60_tmp__88;
    temp63__63 = self->temp63_tmp__89;
    if (temp63__63) {
        redge_result__118.e__12 = false;
    }
    else {
        redge_result__118.e__12 = ((b__11) && (!(temp60__60))) ||
                                  ((!(b__11)) && (temp59__59));
    }
    self->temp63_tmp__89 = false;
    self->temp60_tmp__88 = b__11;
    self->temp59_tmp__87 = b__11;
    
    return redge_result__118;
}
void mod_count_reset__124(mod_count_mem__121* self) {
    self->temp67_tmp__91 = true;
    self->temp64_tmp__90 = 0;
}

mod_count_ret__122 mod_count_step__125(int m__14, mod_count_mem__121* self) {
    bool temp67__67;
    int temp64__64;
    mod_count_ret__122 mod_count_result__123;
    
    temp64__64 = self->temp64_tmp__90;
    temp67__67 = self->temp67_tmp__91;
    if (temp67__67) {
        mod_count_result__123.out__15 = 0;
    }
    else {
        mod_count_result__123.out__15 = ((1) + (temp64__64)) % (m__14);
    }
    self->temp67_tmp__91 = false;
    self->temp64_tmp__90 = mod_count_result__123.out__15;
    
    return mod_count_result__123;
}
void test3_reset__129(test3_mem__126* self) {
    mod_count_reset__124(&self->mod_count_101__101);
    mod_count_reset__124(&self->mod_count_100__100);
}

test3_ret__127 test3_step__130(test3_mem__126* self) {
    test3_ret__127 test3_result__128;
    mod_count_ret__122 mod_count_result__165;
    mod_count_ret__122 mod_count_result__164;
    
    mod_count_result__164 = mod_count_step__125(4, &self->mod_count_101__101);
    test3_result__128.b__18 = mod_count_result__164.out__15;
    mod_count_result__165 = mod_count_step__125(3, &self->mod_count_100__100);
    test3_result__128.a__17 = mod_count_result__165.out__15;
    
    return test3_result__128;
}
add_nomem_ret__131 add_nomem_step__133(int a__20, int b__21) {
    add_nomem_ret__131 add_nomem_result__132;
    
    add_nomem_result__132.c__22 = ((a__20) + (b__21)) + (1);
    
    return add_nomem_result__132;
}
void add_mem_reset__137(add_mem_mem__134* self) {
    self->temp71_tmp__93 = 0;
    self->temp70_tmp__92 = 0;
}

add_mem_ret__135 add_mem_step__138(int a__24, int b__25, add_mem_mem__134* self) {
    int temp71__71;
    int temp70__70;
    add_mem_ret__135 add_mem_result__136;
    
    temp71__71 = self->temp71_tmp__93;
    temp70__70 = self->temp70_tmp__92;
    self->temp71_tmp__93 = (1) + (temp70__70);
    self->temp70_tmp__92 = temp71__71;
    add_mem_result__136.c__26 = ((a__24) + (b__25)) + (temp71__71);
    
    return add_mem_result__136;
}
void test1_reset__142(test1_mem__139* self) {
    test3_reset__129(&self->test3_102__102);
}

test1_ret__140 test1_step__143(test1_mem__139* self) {
    int temp73__73;
    int temp72__72;
    test1_ret__140 test1_result__141;
    test3_ret__127 test3_result__166;
    
    test3_result__166 = test3_step__130(&self->test3_102__102);temp72__72 = test3_result__166.a__17;
    temp73__73 = test3_result__166.b__18;
    test1_result__141.a__29 = (temp72__72) + (temp73__73);
    
    return test1_result__141;
}
void test4mem_reset__147(test4mem_mem__144* self) {
    self->y_tmp__95 = 0;
    add_mem_reset__137(&self->add_mem_103__103);
    self->x_tmp__94 = 0;
}

test4mem_ret__145 test4mem_step__148(test4mem_mem__144* self) {
    test4mem_ret__145 test4mem_result__146;
    add_mem_ret__135 add_mem_result__167;
    
    test4mem_result__146.y__34 = self->y_tmp__95;
    test4mem_result__146.x__33 = self->x_tmp__94;
    self->y_tmp__95 = test4mem_result__146.x__33;
    add_mem_result__167 = add_mem_step__138(test4mem_result__146.y__34, 1, &self->add_mem_103__103);
    test4mem_result__146.z__35 = add_mem_result__167.c__26;
    self->x_tmp__94 = test4mem_result__146.z__35;
    
    return test4mem_result__146;
}
void test4nomem_reset__152(test4nomem_mem__149* self) {
    self->y_tmp__97 = 0;
    
    self->x_tmp__96 = 0;
}

test4nomem_ret__150 test4nomem_step__153(test4nomem_mem__149* self) {
    test4nomem_ret__150 test4nomem_result__151;
    add_nomem_ret__131 add_nomem_result__168;
    
    test4nomem_result__151.y__38 = self->y_tmp__97;
    test4nomem_result__151.x__37 = self->x_tmp__96;
    self->y_tmp__97 = test4nomem_result__151.x__37;
    add_nomem_result__168 = add_nomem_step__133(test4nomem_result__151.y__38, 1);
    test4nomem_result__151.z__39 = add_nomem_result__168.c__22;
    self->x_tmp__96 = test4nomem_result__151.z__39;
    
    return test4nomem_result__151;
}
void count_reset__157(count_mem__154* self) {
    self->temp80_tmp__98 = 0;
}

count_ret__155 count_step__158(int i__41, count_mem__154* self) {
    int temp80__80;
    count_ret__155 count_result__156;
    
    temp80__80 = self->temp80_tmp__98;
    count_result__156.o__42 = (temp80__80) + (i__41);
    self->temp80_tmp__98 = count_result__156.o__42;
    
    return count_result__156;
}
void condact__reset__162(condact__mem__159* self) {
    count_reset__157(&self->count_105__105);
    self->temp81_tmp__99 = 0;
}

condact__ret__160 condact__step__163(bool c__44, int i__45, condact__mem__159* self) {
    int temp82__82;
    int temp81__81;
    condact__ret__160 condact__result__161;
    count_ret__155 count_result__169;
    
    temp81__81 = self->temp81_tmp__99;
    if (c__44) {
        count_result__169 = count_step__158(i__45, &self->count_105__105);
        temp82__82 = count_result__169.o__42;
        condact__result__161.o__46 = temp82__82;
    }
    else {
        condact__result__161.o__46 = temp81__81;
    }
    self->temp81_tmp__99 = condact__result__161.o__46;
    
    return condact__result__161;
}

