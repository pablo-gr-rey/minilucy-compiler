// file auto-generated from Lustre file ex1.lus
#include "ex1.h"

void test_fby_reset__144(test_fby_mem__141* self) {
    self->b_tmp__120 = 1;
    self->a_tmp__119 = 0;
}

test_fby_ret__142 test_fby_step__145(test_fby_mem__141* self) {
    test_fby_ret__142 test_fby_result__143;
    
    test_fby_result__143.b__4 = self->b_tmp__120;
    test_fby_result__143.a__3 = self->a_tmp__119;
    self->b_tmp__120 = test_fby_result__143.a__3;
    self->a_tmp__119 = test_fby_result__143.b__4;
    
    return test_fby_result__143;
}
test2_ret__146 test2_step__148(int a__6, int b__7) {
    bool temp71__71;
    bool temp67__67;
    test2_ret__146 test2_result__147;
    
    temp67__67 = (a__6) < (b__7);
    if (temp67__67) {
        test2_result__147.d__9 = b__7;
    }
    else {
        test2_result__147.d__9 = a__6;
    }
    temp71__71 = (a__6) < (b__7);
    if (temp71__71) {
        test2_result__147.c__8 = a__6;
    }
    else {
        test2_result__147.c__8 = b__7;
    }
    
    return test2_result__147;
}
prop_ret__149 prop_step__151(int a__11) {
    prop_ret__149 prop_result__150;
    
    prop_result__150.b__12 = (((1) + ((2) * (3))) + (4)) * (a__11);
    
    return prop_result__150;
}
example_ret__152 example_step__154(int x__14, bool c__15) {
    example_ret__152 example_result__153;
    
    example_result__153.y__16 = x__14;
    
    return example_result__153;
}
void test0_reset__158(test0_mem__155* self) {
    self->temp75_tmp__122 = 0;
    self->temp74_tmp__121 = 0;
    
}

test0_ret__156 test0_step__159(int a__18, int b__19, test0_mem__155* self) {
    int temp75__75;
    int temp74__74;
    test0_ret__156 test0_result__157;
    test2_ret__146 test2_result__198;
    
    temp75__75 = self->temp75_tmp__122;
    self->temp75_tmp__122 = b__19;
    temp74__74 = self->temp74_tmp__121;
    self->temp74_tmp__121 = a__18;
    test2_result__198 = test2_step__148(temp74__74, temp75__75);test0_result__157.c__20 = test2_result__198.c__8;
    test0_result__157.d__21 = test2_result__198.d__9;
    
    return test0_result__157;
}
void testA_reset__163(testA_mem__160* self) {
    self->temp76_tmp__126 = 0;
    self->temp77_tmp__125 = true;
    self->temp79_tmp__124 = 0;
    self->temp80_tmp__123 = true;
}

testA_ret__161 testA_step__164(int a__23, int b__24, testA_mem__160* self) {
    bool temp80__80;
    int temp79__79;
    bool temp77__77;
    int temp76__76;
    testA_ret__161 testA_result__162;
    
    temp76__76 = self->temp76_tmp__126;
    self->temp76_tmp__126 = b__24;
    temp77__77 = self->temp77_tmp__125;
    self->temp77_tmp__125 = false;
    if (temp77__77) {
        testA_result__162.d__26 = b__24;
    }
    else {
        testA_result__162.d__26 = temp76__76;
    }
    temp79__79 = self->temp79_tmp__124;
    self->temp79_tmp__124 = a__23;
    temp80__80 = self->temp80_tmp__123;
    self->temp80_tmp__123 = false;
    if (temp80__80) {
        testA_result__162.c__25 = a__23;
    }
    else {
        testA_result__162.c__25 = temp79__79;
    }
    
    return testA_result__162;
}
void alt_reset__168(alt_mem__165* self) {
    self->a_tmp__128 = false;
    self->temp82_tmp__127 = false;
}

alt_ret__166 alt_step__169(alt_mem__165* self) {
    bool temp82__82;
    alt_ret__166 alt_result__167;
    
    alt_result__167.a__28 = self->a_tmp__128;
    temp82__82 = self->temp82_tmp__127;
    self->a_tmp__128 = !(temp82__82);
    self->temp82_tmp__127 = alt_result__167.a__28;
    
    return alt_result__167;
}
bad_ret__170 bad_step__172() {
    bool f__32;
    bool e__31;
    bad_ret__170 bad_result__171;
    
    bad_result__171.a__30 = true;
    e__31 = true;
    if (e__31) {
        f__32 = false;
    }
    else {
        
    }
    
    return bad_result__171;
}
void testB_reset__176(testB_mem__173* self) {
    alt_reset__168(&self->alt_134__134);
    alt_reset__168(&self->alt_133__133);
}

testB_ret__174 testB_step__177(bool a__34, bool b__35, testB_mem__173* self) {
    bool temp93__93;
    int temp91__91;
    int temp90__90;
    bool temp88__88;
    bool temp86__86;
    bool temp85__85;
    int f__42;
    bool g__41;
    bool e__40;
    testB_ret__174 testB_result__175;
    alt_ret__166 alt_result__200;
    alt_ret__166 alt_result__199;
    
    if (a__34) {
        testB_result__175.x__38 = 2;
        g__41 = false;
        if (g__41) {
            e__40 = true;
        }
        else {
            e__40 = false;
        }
    }
    else {
        f__42 = 4;
        testB_result__175.x__38 = f__42;
    }
    alt_result__199 = alt_step__169(&self->alt_134__134);temp93__93 = alt_result__199.a__28;
    temp90__90 = 4;
    temp91__91 = 1;
    if (temp93__93) {
        testB_result__175.y__39 = temp90__90;
    }
    else {
        testB_result__175.y__39 = temp91__91;
    }
    alt_result__200 = alt_step__169(&self->alt_133__133);temp88__88 = alt_result__200.a__28;
    temp85__85 = false;
    temp86__86 = true;
    if (temp88__88) {
        testB_result__175.d__37 = temp85__85;
    }
    else {
        testB_result__175.d__37 = temp86__86;
    }
    testB_result__175.c__36 = (a__34) && (b__35);
    
    return testB_result__175;
}
void count_reset__181(count_mem__178* self) {
    self->temp103_tmp__130 = true;
    self->temp98_tmp__129 = 0;
}

count_ret__179 count_step__182(bool a__44, count_mem__178* self) {
    bool temp103__103;
    int temp100__100;
    int temp99__99;
    int temp98__98;
    int temp96__96;
    int temp95__95;
    int temp94__94;
    count_ret__179 count_result__180;
    
    temp103__103 = self->temp103_tmp__130;
    self->temp103_tmp__130 = false;
    temp100__100 = 0;
    temp98__98 = self->temp98_tmp__129;
    if (temp103__103) {
        temp99__99 = temp100__100;
    }
    else {
        temp99__99 = temp98__98;
    }
    temp95__95 = 1;
    temp96__96 = 0;
    if (a__44) {
        temp94__94 = temp95__95;
    }
    else {
        temp94__94 = temp96__96;
    }
    count_result__180.s__45 = (temp99__99) + (temp94__94);
    self->temp98_tmp__129 = count_result__180.s__45;
    
    return count_result__180;
}
void count_2_reset__186(count_2_mem__183* self) {
    count_reset__181(&self->count_136__136);
    count_reset__181(&self->count_135__135);
}

count_2_ret__184 count_2_step__187(bool a__47, count_2_mem__183* self) {
    count_2_ret__184 count_2_result__185;
    count_ret__179 count_result__202;
    count_ret__179 count_result__201;
    
    count_result__201 = count_step__182(!(a__47), &self->count_136__136);
    count_2_result__185.s2__49 = count_result__201.s__45;
    count_result__202 = count_step__182(a__47, &self->count_135__135);
    count_2_result__185.s1__48 = count_result__202.s__45;
    
    return count_2_result__185;
}
void test_if_merge_reset__191(test_if_merge_mem__188* self) {
    count_reset__181(&self->count_140__140);
    count_reset__181(&self->count_139__139);
    count_reset__181(&self->count_138__138);
    count_reset__181(&self->count_137__137);
}

test_if_merge_ret__189 test_if_merge_step__192(bool a__51, test_if_merge_mem__188* self) {
    int temp113__113;
    int temp112__112;
    int temp107__107;
    int temp106__106;
    test_if_merge_ret__189 test_if_merge_result__190;
    count_ret__179 count_result__206;
    count_ret__179 count_result__205;
    count_ret__179 count_result__204;
    count_ret__179 count_result__203;
    
    count_result__203 = count_step__182(true, &self->count_140__140);
    temp113__113 = count_result__203.s__45;
    count_result__204 = count_step__182(true, &self->count_139__139);
    temp112__112 = count_result__204.s__45;
    if (a__51) {
        test_if_merge_result__190.s2__53 = temp113__113;
    }
    else {
        test_if_merge_result__190.s2__53 = temp112__112;
    }
    count_result__205 = count_step__182(true, &self->count_138__138);
    temp107__107 = count_result__205.s__45;
    count_result__206 = count_step__182(true, &self->count_137__137);
    temp106__106 = count_result__206.s__45;
    if (a__51) {
        test_if_merge_result__190.s1__52 = temp107__107;
    }
    else {
        test_if_merge_result__190.s1__52 = temp106__106;
    }
    
    return test_if_merge_result__190;
}
void regulator_reset__196(regulator_mem__193* self) {
    self->temp114_tmp__131 = 0.000000;
}

regulator_ret__194 regulator_step__197(double cruise_speed__55, double speed__56, bool rst__57, regulator_mem__193* self) {
    double temp117__117;
    double temp115__115;
    double temp114__114;
    double delta__59;
    regulator_ret__194 regulator_result__195;
    
    delta__59 = (cruise_speed__55) - (speed__56);
    temp114__114 = self->temp114_tmp__131;
    temp117__117 = (delta__59) + (temp114__114);
    if (rst__57) {
        temp115__115 = delta__59;
    }
    else {
        temp115__115 = temp117__117;
    }
    self->temp114_tmp__131 = temp115__115;
    regulator_result__195.throttle__58 = ((delta__59) * (10.000000)) +
                                         ((temp115__115) * (0.200000));
    
    return regulator_result__195;
}

