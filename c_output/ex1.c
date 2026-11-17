// file auto-generated from Lustre file ex1.lus
#include "ex1.h"

test2_ret__134 test2_step__136(int a__3, int b__4) {
    bool temp66__66;
    bool temp62__62;
    test2_ret__134 test2_result__135;
    
    temp66__66 = (a__3) < (b__4);
    if (temp66__66) {
        test2_result__135.c__5 = a__3;
    }
    else {
        test2_result__135.c__5 = b__4;
    }
    temp62__62 = (a__3) < (b__4);
    if (temp62__62) {
        test2_result__135.d__6 = b__4;
    }
    else {
        test2_result__135.d__6 = a__3;
    }
    
    return test2_result__135;
}
prop_ret__137 prop_step__139(int a__8) {
    prop_ret__137 prop_result__138;
    
    prop_result__138.b__9 = (((1) + ((2) * (3))) + (4)) * (a__8);
    
    return prop_result__138;
}
example_ret__140 example_step__142(int x__11, bool c__12) {
    example_ret__140 example_result__141;
    
    example_result__141.y__13 = x__11;
    
    return example_result__141;
}
void test0_reset__146(test0_mem__143* self) {
    
    self->temp70_tmp__115 = 0;
    self->temp69_tmp__114 = 0;
}

test0_ret__144 test0_step__147(int a__15, int b__16, test0_mem__143* self) {
    int temp70__70;
    int temp69__69;
    test0_ret__144 test0_result__145;
    test2_ret__134 test2_result__186;
    
    temp69__69 = self->temp69_tmp__114;
    temp70__70 = self->temp70_tmp__115;
    test2_result__186 = test2_step__136(temp69__69, temp70__70);test0_result__145.c__17 = test2_result__186.c__5;
    test0_result__145.d__18 = test2_result__186.d__6;
    self->temp70_tmp__115 = b__16;
    self->temp69_tmp__114 = a__15;
    
    return test0_result__145;
}
void testA_reset__151(testA_mem__148* self) {
    self->temp74_tmp__119 = 0;
    self->temp75_tmp__118 = true;
    self->temp71_tmp__117 = 0;
    self->temp72_tmp__116 = true;
}

testA_ret__149 testA_step__152(int a__20, int b__21, testA_mem__148* self) {
    bool temp75__75;
    int temp74__74;
    bool temp72__72;
    int temp71__71;
    testA_ret__149 testA_result__150;
    
    temp75__75 = self->temp75_tmp__118;
    temp74__74 = self->temp74_tmp__119;
    if (temp75__75) {
        testA_result__150.c__22 = a__20;
    }
    else {
        testA_result__150.c__22 = temp74__74;
    }
    self->temp74_tmp__119 = a__20;
    self->temp75_tmp__118 = false;
    temp72__72 = self->temp72_tmp__116;
    temp71__71 = self->temp71_tmp__117;
    if (temp72__72) {
        testA_result__150.d__23 = b__21;
    }
    else {
        testA_result__150.d__23 = temp71__71;
    }
    self->temp71_tmp__117 = b__21;
    self->temp72_tmp__116 = false;
    
    return testA_result__150;
}
void alt_reset__156(alt_mem__153* self) {
    self->a_tmp__121 = false;
    self->temp77_tmp__120 = false;
}

alt_ret__154 alt_step__157(alt_mem__153* self) {
    bool temp77__77;
    alt_ret__154 alt_result__155;
    
    alt_result__155.a__25 = self->a_tmp__121;
    temp77__77 = self->temp77_tmp__120;
    self->a_tmp__121 = !(temp77__77);
    self->temp77_tmp__120 = alt_result__155.a__25;
    
    return alt_result__155;
}
bad_ret__158 bad_step__160() {
    bool f__29;
    bool e__28;
    bad_ret__158 bad_result__159;
    
    bad_result__159.a__27 = true;
    e__28 = true;
    if (e__28) {
        f__29 = false;
    }
    else {
        
    }
    
    return bad_result__159;
}
void testB_reset__164(testB_mem__161* self) {
    alt_reset__156(&self->alt_127__127);
    alt_reset__156(&self->alt_126__126);
}

testB_ret__162 testB_step__165(bool a__31, bool b__32, testB_mem__161* self) {
    bool temp88__88;
    int temp86__86;
    int temp85__85;
    bool temp83__83;
    bool temp81__81;
    bool temp80__80;
    int f__39;
    bool g__38;
    bool e__37;
    testB_ret__162 testB_result__163;
    alt_ret__154 alt_result__188;
    alt_ret__154 alt_result__187;
    
    if (a__31) {
        testB_result__163.x__35 = 2;
        g__38 = false;
        if (g__38) {
            e__37 = true;
        }
        else {
            e__37 = false;
        }
    }
    else {
        f__39 = 4;
        testB_result__163.x__35 = f__39;
    }
    temp85__85 = 4;
    temp86__86 = 1;
    alt_result__187 = alt_step__157(&self->alt_127__127);temp88__88 = alt_result__187.a__25;
    if (temp88__88) {
        testB_result__163.y__36 = temp85__85;
    }
    else {
        testB_result__163.y__36 = temp86__86;
    }
    temp80__80 = false;
    temp81__81 = true;
    alt_result__188 = alt_step__157(&self->alt_126__126);temp83__83 = alt_result__188.a__25;
    if (temp83__83) {
        testB_result__163.d__34 = temp80__80;
    }
    else {
        testB_result__163.d__34 = temp81__81;
    }
    testB_result__163.c__33 = (a__31) && (b__32);
    
    return testB_result__163;
}
void count_reset__169(count_mem__166* self) {
    self->temp98_tmp__123 = true;
    self->temp93_tmp__122 = 0;
}

count_ret__167 count_step__170(bool a__41, count_mem__166* self) {
    bool temp98__98;
    int temp95__95;
    int temp94__94;
    int temp93__93;
    int temp91__91;
    int temp90__90;
    int temp89__89;
    count_ret__167 count_result__168;
    
    temp91__91 = 0;
    temp90__90 = 1;
    if (a__41) {
        temp89__89 = temp90__90;
    }
    else {
        temp89__89 = temp91__91;
    }
    temp93__93 = self->temp93_tmp__122;
    temp98__98 = self->temp98_tmp__123;
    temp95__95 = 0;
    if (temp98__98) {
        temp94__94 = temp95__95;
    }
    else {
        temp94__94 = temp93__93;
    }
    count_result__168.s__42 = (temp94__94) + (temp89__89);
    self->temp98_tmp__123 = false;
    self->temp93_tmp__122 = count_result__168.s__42;
    
    return count_result__168;
}
void count_2_reset__174(count_2_mem__171* self) {
    count_reset__169(&self->count_129__129);
    count_reset__169(&self->count_128__128);
}

count_2_ret__172 count_2_step__175(bool a__44, count_2_mem__171* self) {
    count_2_ret__172 count_2_result__173;
    count_ret__167 count_result__190;
    count_ret__167 count_result__189;
    
    count_result__189 = count_step__170(!(a__44), &self->count_129__129);
    count_2_result__173.s2__46 = count_result__189.s__42;
    count_result__190 = count_step__170(a__44, &self->count_128__128);
    count_2_result__173.s1__45 = count_result__190.s__42;
    
    return count_2_result__173;
}
void test_if_merge_reset__179(test_if_merge_mem__176* self) {
    count_reset__169(&self->count_133__133);
    count_reset__169(&self->count_132__132);
    count_reset__169(&self->count_131__131);
    count_reset__169(&self->count_130__130);
}

test_if_merge_ret__177 test_if_merge_step__180(bool a__48, test_if_merge_mem__176* self) {
    int temp108__108;
    int temp107__107;
    int temp102__102;
    int temp101__101;
    test_if_merge_ret__177 test_if_merge_result__178;
    count_ret__167 count_result__194;
    count_ret__167 count_result__193;
    count_ret__167 count_result__192;
    count_ret__167 count_result__191;
    
    count_result__191 = count_step__170(true, &self->count_133__133);
    temp107__107 = count_result__191.s__42;
    count_result__192 = count_step__170(true, &self->count_132__132);
    temp108__108 = count_result__192.s__42;
    if (a__48) {
        test_if_merge_result__178.s2__50 = temp108__108;
    }
    else {
        test_if_merge_result__178.s2__50 = temp107__107;
    }
    count_result__193 = count_step__170(true, &self->count_131__131);
    temp101__101 = count_result__193.s__42;
    count_result__194 = count_step__170(true, &self->count_130__130);
    temp102__102 = count_result__194.s__42;
    if (a__48) {
        test_if_merge_result__178.s1__49 = temp102__102;
    }
    else {
        test_if_merge_result__178.s1__49 = temp101__101;
    }
    
    return test_if_merge_result__178;
}
void regulator_reset__184(regulator_mem__181* self) {
    self->temp109_tmp__124 = 0.000000;
}

regulator_ret__182 regulator_step__185(float cruise_speed__52, float speed__53, bool rst__54, regulator_mem__181* self) {
    float temp112__112;
    float temp110__110;
    float temp109__109;
    float delta__56;
    regulator_ret__182 regulator_result__183;
    
    delta__56 = (cruise_speed__52) - (speed__53);
    temp109__109 = self->temp109_tmp__124;
    temp112__112 = (delta__56) + (temp109__109);
    if (rst__54) {
        temp110__110 = delta__56;
    }
    else {
        temp110__110 = temp112__112;
    }
    self->temp109_tmp__124 = temp110__110;
    regulator_result__183.throttle__55 = ((delta__56) * (10.000000)) +
                                         ((temp110__110) * (0.200000));
    
    return regulator_result__183;
}

