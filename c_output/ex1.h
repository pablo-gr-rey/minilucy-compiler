// file auto-generated from Lustre file ex1.lus
#include <stdbool.h>

// ------------------ NODE test_fby ------------------

typedef struct {
    int b_tmp__120;
    int a_tmp__119;} test_fby_mem__141;

void test_fby_reset__144(test_fby_mem__141* self);

typedef struct {
    int a__3;
    int b__4;} test_fby_ret__142;

test_fby_ret__142 test_fby_step__145(test_fby_mem__141* self);

// ------------------ NODE test2 ------------------

typedef struct {
    int c__8;
    int d__9;} test2_ret__146;

test2_ret__146 test2_step__148(int a__6, int b__7);

// ------------------ NODE prop ------------------

typedef struct {
    int b__12;} prop_ret__149;

prop_ret__149 prop_step__151(int a__11);

// ------------------ NODE example ------------------

typedef struct {
    int y__16;} example_ret__152;

example_ret__152 example_step__154(int x__14, bool c__15);

// ------------------ NODE test0 ------------------

typedef struct {
    int temp75_tmp__122;
    int temp74_tmp__121;} test0_mem__155;

void test0_reset__158(test0_mem__155* self);

typedef struct {
    int c__20;
    int d__21;} test0_ret__156;

test0_ret__156 test0_step__159(int a__18, int b__19, test0_mem__155* self);

// ------------------ NODE testA ------------------

typedef struct {
    int temp76_tmp__126;
    bool temp77_tmp__125;
    int temp79_tmp__124;
    bool temp80_tmp__123;} testA_mem__160;

void testA_reset__163(testA_mem__160* self);

typedef struct {
    int c__25;
    int d__26;} testA_ret__161;

testA_ret__161 testA_step__164(int a__23, int b__24, testA_mem__160* self);

// ------------------ NODE alt ------------------

typedef struct {
    bool a_tmp__128;
    bool temp82_tmp__127;} alt_mem__165;

void alt_reset__168(alt_mem__165* self);

typedef struct {
    bool a__28;} alt_ret__166;

alt_ret__166 alt_step__169(alt_mem__165* self);

// ------------------ NODE bad ------------------

typedef struct {
    bool a__30;} bad_ret__170;

bad_ret__170 bad_step__172();

// ------------------ NODE testB ------------------

typedef struct {
    alt_mem__165 alt_134__134;
    alt_mem__165 alt_133__133;} testB_mem__173;

void testB_reset__176(testB_mem__173* self);

typedef struct {
    bool c__36;
    bool d__37;
    int x__38;
    int y__39;} testB_ret__174;

testB_ret__174 testB_step__177(bool a__34, bool b__35, testB_mem__173* self);

// ------------------ NODE count ------------------

typedef struct {
    bool temp103_tmp__130;
    int temp98_tmp__129;} count_mem__178;

void count_reset__181(count_mem__178* self);

typedef struct {
    int s__45;} count_ret__179;

count_ret__179 count_step__182(bool a__44, count_mem__178* self);

// ------------------ NODE count_2 ------------------

typedef struct {
    count_mem__178 count_136__136;
    count_mem__178 count_135__135;} count_2_mem__183;

void count_2_reset__186(count_2_mem__183* self);

typedef struct {
    int s1__48;
    int s2__49;} count_2_ret__184;

count_2_ret__184 count_2_step__187(bool a__47, count_2_mem__183* self);

// ------------------ NODE test_if_merge ------------------

typedef struct {
    count_mem__178 count_140__140;
    count_mem__178 count_139__139;
    count_mem__178 count_138__138;
    count_mem__178 count_137__137;} test_if_merge_mem__188;

void test_if_merge_reset__191(test_if_merge_mem__188* self);

typedef struct {
    int s1__52;
    int s2__53;} test_if_merge_ret__189;

test_if_merge_ret__189 test_if_merge_step__192(bool a__51, test_if_merge_mem__188* self);

// ------------------ NODE regulator ------------------

typedef struct {
    double temp114_tmp__131;} regulator_mem__193;

void regulator_reset__196(regulator_mem__193* self);

typedef struct {
    double throttle__58;} regulator_ret__194;

regulator_ret__194 regulator_step__197(double cruise_speed__55, double speed__56, bool rst__57, regulator_mem__193* self);

