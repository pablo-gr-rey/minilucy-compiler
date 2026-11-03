// file auto-generated from Lustre file test.lus
#include <stdbool.h>

// ------------------ NODE counting ------------------

typedef struct {
    bool temp51_tmp__84;
    int temp48_tmp__83;} counting_mem__106;

void counting_reset__109(counting_mem__106* self);

typedef struct {
    int o__5;} counting_ret__107;

counting_ret__107 counting_step__110(bool tick__3, bool top__4, counting_mem__106* self);

// ------------------ NODE h ------------------

typedef struct {
    bool temp56_tmp__86;
    bool temp57_tmp__85;} h_mem__111;

void h_reset__114(h_mem__111* self);

typedef struct {
    bool e__9;} h_ret__112;

h_ret__112 h_step__115(bool b__8, h_mem__111* self);

// ------------------ NODE redge ------------------

typedef struct {
    bool temp63_tmp__89;
    bool temp60_tmp__88;
    bool temp59_tmp__87;} redge_mem__116;

void redge_reset__119(redge_mem__116* self);

typedef struct {
    bool e__12;} redge_ret__117;

redge_ret__117 redge_step__120(bool b__11, redge_mem__116* self);

// ------------------ NODE mod_count ------------------

typedef struct {
    bool temp67_tmp__91;
    int temp64_tmp__90;} mod_count_mem__121;

void mod_count_reset__124(mod_count_mem__121* self);

typedef struct {
    int out__15;} mod_count_ret__122;

mod_count_ret__122 mod_count_step__125(int m__14, mod_count_mem__121* self);

// ------------------ NODE test3 ------------------

typedef struct {
    mod_count_mem__121 mod_count_101__101;
    mod_count_mem__121 mod_count_100__100;} test3_mem__126;

void test3_reset__129(test3_mem__126* self);

typedef struct {
    int a__17;
    int b__18;} test3_ret__127;

test3_ret__127 test3_step__130(test3_mem__126* self);

// ------------------ NODE add_nomem ------------------

typedef struct {
    int c__22;} add_nomem_ret__131;

add_nomem_ret__131 add_nomem_step__133(int a__20, int b__21);

// ------------------ NODE add_mem ------------------

typedef struct {
    int temp71_tmp__93;
    int temp70_tmp__92;} add_mem_mem__134;

void add_mem_reset__137(add_mem_mem__134* self);

typedef struct {
    int c__26;} add_mem_ret__135;

add_mem_ret__135 add_mem_step__138(int a__24, int b__25, add_mem_mem__134* self);

// ------------------ NODE test1 ------------------

typedef struct {
    test3_mem__126 test3_102__102;} test1_mem__139;

void test1_reset__142(test1_mem__139* self);

typedef struct {
    int a__29;} test1_ret__140;

test1_ret__140 test1_step__143(test1_mem__139* self);

// ------------------ NODE test4mem ------------------

typedef struct {
    int y_tmp__95;
    int x_tmp__94;
    add_mem_mem__134 add_mem_103__103;} test4mem_mem__144;

void test4mem_reset__147(test4mem_mem__144* self);

typedef struct {
    int x__33;
    int y__34;
    int z__35;} test4mem_ret__145;

test4mem_ret__145 test4mem_step__148(test4mem_mem__144* self);

// ------------------ NODE test4nomem ------------------

typedef struct {
    int y_tmp__97;
    int x_tmp__96;} test4nomem_mem__149;

void test4nomem_reset__152(test4nomem_mem__149* self);

typedef struct {
    int x__37;
    int y__38;
    int z__39;} test4nomem_ret__150;

test4nomem_ret__150 test4nomem_step__153(test4nomem_mem__149* self);

// ------------------ NODE count ------------------

typedef struct {
    int temp80_tmp__98;} count_mem__154;

void count_reset__157(count_mem__154* self);

typedef struct {
    int o__42;} count_ret__155;

count_ret__155 count_step__158(int i__41, count_mem__154* self);

// ------------------ NODE condact_ ------------------

typedef struct {
    int temp81_tmp__99;
    count_mem__154 count_105__105;} condact__mem__159;

void condact__reset__162(condact__mem__159* self);

typedef struct {
    int o__46;} condact__ret__160;

condact__ret__160 condact__step__163(bool c__44, int i__45, condact__mem__159* self);

