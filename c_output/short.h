// file auto-generated from Lustre file short.lus
#include <stdbool.h>

// ------------------ NODE count ------------------

typedef struct {
    bool temp34_tmp__56;
    int temp29_tmp__55;} count_mem__69;

void count_reset__72(count_mem__69* self);

typedef struct {
    int a__3;} count_ret__70;

count_ret__70 count_step__73(count_mem__69* self);

// ------------------ NODE idiot ------------------

typedef struct {
    int a__5;} idiot_ret__74;

idiot_ret__74 idiot_step__76();

// ------------------ NODE count_tick ------------------

typedef struct {
    bool temp40_tmp__58;
    int temp35_tmp__57;} count_tick_mem__77;

void count_tick_reset__80(count_tick_mem__77* self);

typedef struct {
    int a__8;} count_tick_ret__78;

count_tick_ret__78 count_tick_step__81(bool tick__7, count_tick_mem__77* self);

// ------------------ NODE nomem ------------------

typedef struct {
    int c__12;} nomem_ret__82;

nomem_ret__82 nomem_step__84(int a__10, int b__11);

// ------------------ NODE test ------------------

typedef struct {
    count_tick_mem__77 count_tick_64__64;
    count_tick_mem__77 count_tick_63__63;
    count_mem__69 count_62__62;
    count_mem__69 count_61__61;
    count_mem__69 count_60__60;
    count_mem__69 count_59__59;} test_mem__85;

void test_reset__88(test_mem__85* self);

typedef struct {
    int s1__15;
    int s2__16;
    int s3__17;} test_ret__86;

test_ret__86 test_step__89(bool a__14, test_mem__85* self);

// ------------------ NODE test2 ------------------

typedef struct {
    count_mem__69 count_68__68;
    count_mem__69 count_67__67;
    count_mem__69 count_66__66;
    count_mem__69 count_65__65;} test2_mem__90;

void test2_reset__93(test2_mem__90* self);

typedef struct {
    int s1__20;
    int s2__21;} test2_ret__91;

test2_ret__91 test2_step__94(bool a__19, test2_mem__90* self);

// ------------------ NODE test_real ------------------

typedef struct {
    double c__27;} test_real_ret__95;

test_real_ret__95 test_real_step__97(double a__25, double b__26);

