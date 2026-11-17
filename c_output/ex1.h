// file auto-generated from Lustre file ex1.lus
#include <stdbool.h>

// ------------------ NODE test2 ------------------

typedef struct {
    int c__5;
    int d__6;} test2_ret__134;

test2_ret__134 test2_step__136(int a__3, int b__4);

// ------------------ NODE prop ------------------

typedef struct {
    int b__9;} prop_ret__137;

prop_ret__137 prop_step__139(int a__8);

// ------------------ NODE example ------------------

typedef struct {
    int y__13;} example_ret__140;

example_ret__140 example_step__142(int x__11, bool c__12);

// ------------------ NODE test0 ------------------

typedef struct {
    int temp70_tmp__115;
    int temp69_tmp__114;} test0_mem__143;

void test0_reset__146(test0_mem__143* self);

typedef struct {
    int c__17;
    int d__18;} test0_ret__144;

test0_ret__144 test0_step__147(int a__15, int b__16, test0_mem__143* self);

// ------------------ NODE testA ------------------

typedef struct {
    int temp74_tmp__119;
    bool temp75_tmp__118;
    int temp71_tmp__117;
    bool temp72_tmp__116;} testA_mem__148;

void testA_reset__151(testA_mem__148* self);

typedef struct {
    int c__22;
    int d__23;} testA_ret__149;

testA_ret__149 testA_step__152(int a__20, int b__21, testA_mem__148* self);

// ------------------ NODE alt ------------------

typedef struct {
    bool a_tmp__121;
    bool temp77_tmp__120;} alt_mem__153;

void alt_reset__156(alt_mem__153* self);

typedef struct {
    bool a__25;} alt_ret__154;

alt_ret__154 alt_step__157(alt_mem__153* self);

// ------------------ NODE bad ------------------

typedef struct {
    bool a__27;} bad_ret__158;

bad_ret__158 bad_step__160();

// ------------------ NODE testB ------------------

typedef struct {
    alt_mem__153 alt_127__127;
    alt_mem__153 alt_126__126;} testB_mem__161;

void testB_reset__164(testB_mem__161* self);

typedef struct {
    bool c__33;
    bool d__34;
    int x__35;
    int y__36;} testB_ret__162;

testB_ret__162 testB_step__165(bool a__31, bool b__32, testB_mem__161* self);

// ------------------ NODE count ------------------

typedef struct {
    bool temp98_tmp__123;
    int temp93_tmp__122;} count_mem__166;

void count_reset__169(count_mem__166* self);

typedef struct {
    int s__42;} count_ret__167;

count_ret__167 count_step__170(bool a__41, count_mem__166* self);

// ------------------ NODE count_2 ------------------

typedef struct {
    count_mem__166 count_129__129;
    count_mem__166 count_128__128;} count_2_mem__171;

void count_2_reset__174(count_2_mem__171* self);

typedef struct {
    int s1__45;
    int s2__46;} count_2_ret__172;

count_2_ret__172 count_2_step__175(bool a__44, count_2_mem__171* self);

// ------------------ NODE test_if_merge ------------------

typedef struct {
    count_mem__166 count_133__133;
    count_mem__166 count_132__132;
    count_mem__166 count_131__131;
    count_mem__166 count_130__130;} test_if_merge_mem__176;

void test_if_merge_reset__179(test_if_merge_mem__176* self);

typedef struct {
    int s1__49;
    int s2__50;} test_if_merge_ret__177;

test_if_merge_ret__177 test_if_merge_step__180(bool a__48, test_if_merge_mem__176* self);

// ------------------ NODE regulator ------------------

typedef struct {
    float temp109_tmp__124;} regulator_mem__181;

void regulator_reset__184(regulator_mem__181* self);

typedef struct {
    float throttle__55;} regulator_ret__182;

regulator_ret__182 regulator_step__185(float cruise_speed__52, float speed__53, bool rst__54, regulator_mem__181* self);

