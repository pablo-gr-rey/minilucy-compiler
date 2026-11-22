// file auto-generated from Lustre file short.lus
#include <stdbool.h>

// ------------------ NODE count ------------------

typedef struct {
    bool temp39_tmp__66;
    int temp34_tmp__65;} count_mem__80;

void count_reset__83(count_mem__80* self);

typedef struct {
    int a__3;} count_ret__81;

count_ret__81 count_step__84(count_mem__80* self);

// ------------------ NODE idiot ------------------

typedef struct {
    int a__5;} idiot_ret__85;

idiot_ret__85 idiot_step__87();

// ------------------ NODE count_tick ------------------

typedef struct {
    bool temp45_tmp__68;
    int temp40_tmp__67;} count_tick_mem__88;

void count_tick_reset__91(count_tick_mem__88* self);

typedef struct {
    int a__8;} count_tick_ret__89;

count_tick_ret__89 count_tick_step__92(bool tick__7, count_tick_mem__88* self);

// ------------------ NODE nomem ------------------

typedef struct {
    int c__12;} nomem_ret__93;

nomem_ret__93 nomem_step__95(int a__10, int b__11);

// ------------------ NODE test ------------------

typedef struct {
    count_tick_mem__88 count_tick_75__75;
    count_tick_mem__88 count_tick_74__74;
    count_mem__80 count_73__73;
    count_mem__80 count_72__72;
    count_mem__80 count_71__71;
    count_mem__80 count_70__70;} test_mem__96;

void test_reset__99(test_mem__96* self);

typedef struct {
    int s1__15;
    int s2__16;
    int s3__17;} test_ret__97;

test_ret__97 test_step__100(bool a__14, test_mem__96* self);

// ------------------ NODE test2 ------------------

typedef struct {
    count_mem__80 count_79__79;
    count_mem__80 count_78__78;
    count_mem__80 count_77__77;
    count_mem__80 count_76__76;} test2_mem__101;

void test2_reset__104(test2_mem__101* self);

typedef struct {
    int s1__20;
    int s2__21;} test2_ret__102;

test2_ret__102 test2_step__105(bool a__19, test2_mem__101* self);

// ------------------ NODE test_real ------------------

typedef struct {
    double c__27;} test_real_ret__106;

test_real_ret__106 test_real_step__108(double a__25, double b__26);

// ------------------ NODE proc0 ------------------

typedef struct {
    bool asks_cs0_tmp__69;} proc0_mem__109;

void proc0_reset__112(proc0_mem__109* self);

typedef struct {
    int pc__30;
    bool asks_cs0__31;} proc0_ret__110;

proc0_ret__110 proc0_step__113(bool cpu__29, proc0_mem__109* self);

