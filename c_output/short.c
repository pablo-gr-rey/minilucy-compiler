// file auto-generated from Lustre file short.lus
#include "short.h"

void count_reset__83(count_mem__80* self) {
    self->temp39_tmp__66 = true;
    self->temp34_tmp__65 = 0;
}

count_ret__81 count_step__84(count_mem__80* self) {
    bool temp39__39;
    int temp37__37;
    int temp36__36;
    int temp34__34;
    count_ret__81 count_result__82;
    
    temp39__39 = self->temp39_tmp__66;
    self->temp39_tmp__66 = false;
    temp36__36 = 0;
    temp34__34 = self->temp34_tmp__65;
    temp37__37 = (1) + (temp34__34);
    if (temp39__39) {
        count_result__82.a__3 = temp36__36;
    }
    else {
        count_result__82.a__3 = temp37__37;
    }
    self->temp34_tmp__65 = count_result__82.a__3;
    
    return count_result__82;
}
idiot_ret__85 idiot_step__87() {
    idiot_ret__85 idiot_result__86;
    
    idiot_result__86.a__5 = 3;
    
    return idiot_result__86;
}
void count_tick_reset__91(count_tick_mem__88* self) {
    self->temp45_tmp__68 = true;
    self->temp40_tmp__67 = 0;
}

count_tick_ret__89 count_tick_step__92(bool tick__7, count_tick_mem__88* self) {
    bool temp45__45;
    int temp43__43;
    int temp42__42;
    int temp40__40;
    count_tick_ret__89 count_tick_result__90;
    
    temp45__45 = self->temp45_tmp__68;
    self->temp45_tmp__68 = false;
    temp42__42 = 0;
    temp40__40 = self->temp40_tmp__67;
    temp43__43 = (1) + (temp40__40);
    if (temp45__45) {
        count_tick_result__90.a__8 = temp42__42;
    }
    else {
        count_tick_result__90.a__8 = temp43__43;
    }
    self->temp40_tmp__67 = count_tick_result__90.a__8;
    
    return count_tick_result__90;
}
nomem_ret__93 nomem_step__95(int a__10, int b__11) {
    nomem_ret__93 nomem_result__94;
    
    nomem_result__94.c__12 = (a__10) + (b__11);
    
    return nomem_result__94;
}
void test_reset__99(test_mem__96* self) {
    count_tick_reset__91(&self->count_tick_75__75);
    count_tick_reset__91(&self->count_tick_74__74);
    count_reset__83(&self->count_73__73);
    count_reset__83(&self->count_72__72);
    count_reset__83(&self->count_71__71);
    count_reset__83(&self->count_70__70);
}

test_ret__97 test_step__100(bool a__14, test_mem__96* self) {
    int temp55__55;
    int temp54__54;
    int temp53__53;
    int temp52__52;
    int temp47__47;
    int temp46__46;
    test_ret__97 test_result__98;
    count_ret__81 count_result__119;
    count_ret__81 count_result__118;
    count_ret__81 count_result__117;
    count_ret__81 count_result__116;
    count_tick_ret__89 count_tick_result__115;
    count_tick_ret__89 count_tick_result__114;
    
    if (a__14) {
        count_tick_result__114 = count_tick_step__92(true, &self->count_tick_75__75);
        temp55__55 = count_tick_result__114.a__8;
        test_result__98.s3__17 = temp55__55;
    }
    else {
        count_tick_result__115 = count_tick_step__92(true, &self->count_tick_74__74);
        temp54__54 = count_tick_result__115.a__8;
        test_result__98.s3__17 = temp54__54;
    }
    count_result__116 = count_step__84(&self->count_73__73);temp53__53 = count_result__116.a__3;
    count_result__117 = count_step__84(&self->count_72__72);temp52__52 = count_result__117.a__3;
    if (a__14) {
        test_result__98.s2__16 = temp53__53;
    }
    else {
        test_result__98.s2__16 = temp52__52;
    }
    count_result__118 = count_step__84(&self->count_71__71);temp47__47 = count_result__118.a__3;
    count_result__119 = count_step__84(&self->count_70__70);temp46__46 = count_result__119.a__3;
    if (a__14) {
        test_result__98.s1__15 = temp47__47;
    }
    else {
        test_result__98.s1__15 = temp46__46;
    }
    
    return test_result__98;
}
void test2_reset__104(test2_mem__101* self) {
    count_reset__83(&self->count_79__79);
    count_reset__83(&self->count_78__78);
    count_reset__83(&self->count_77__77);
    count_reset__83(&self->count_76__76);
}

test2_ret__102 test2_step__105(bool a__19, test2_mem__101* self) {
    int temp59__59;
    int temp58__58;
    int temp57__57;
    int temp56__56;
    int tmp2__23;
    int tmp1__22;
    test2_ret__102 test2_result__103;
    count_ret__81 count_result__123;
    count_ret__81 count_result__122;
    count_ret__81 count_result__121;
    count_ret__81 count_result__120;
    
    count_result__120 = count_step__84(&self->count_79__79);temp56__56 = count_result__120.a__3;
    if (a__19) {
        tmp1__22 = temp56__56;
    }
    else {
        
    }
    count_result__121 = count_step__84(&self->count_78__78);temp57__57 = count_result__121.a__3;
    if (a__19) {
        test2_result__103.s1__20 = tmp1__22;
    }
    else {
        tmp2__23 = temp57__57;
        test2_result__103.s1__20 = tmp2__23;
    }
    count_result__122 = count_step__84(&self->count_77__77);temp58__58 = count_result__122.a__3;
    count_result__123 = count_step__84(&self->count_76__76);temp59__59 = count_result__123.a__3;
    if (a__19) {
        test2_result__103.s2__21 = temp59__59;
    }
    else {
        test2_result__103.s2__21 = temp58__58;
    }
    
    return test2_result__103;
}
test_real_ret__106 test_real_step__108(double a__25, double b__26) {
    test_real_ret__106 test_real_result__107;
    
    test_real_result__107.c__27 = ((a__25) + ((b__26) * (a__25))) + (b__26);
    
    return test_real_result__107;
}
void proc0_reset__112(proc0_mem__109* self) {
    self->asks_cs0_tmp__69 = false;
}

proc0_ret__110 proc0_step__113(bool cpu__29, proc0_mem__109* self) {
    int temp62__62;
    int temp61__61;
    proc0_ret__110 proc0_result__111;
    
    proc0_result__111.asks_cs0__31 = self->asks_cs0_tmp__69;
    temp61__61 = 3;
    temp62__62 = 10;
    if (cpu__29) {
        proc0_result__111.pc__30 = temp61__61;
    }
    else {
        proc0_result__111.pc__30 = temp62__62;
    }
    self->asks_cs0_tmp__69 = ((1) <= (proc0_result__111.pc__30)) &&
                             ((proc0_result__111.pc__30) < (6));
    
    return proc0_result__111;
}

