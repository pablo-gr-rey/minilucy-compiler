// file auto-generated from Lustre file short.lus
#include <stdbool.h>

// ------------------ NODE count ------------------

typedef struct {
    bool temp24_tmp__46;
    int temp19_tmp__45;} count_mem__59;

void count_reset__62(count_mem__59* self);

typedef struct {
    int a__3;} count_ret__60;

count_ret__60 count_step__63(count_mem__59* self);

// ------------------ NODE count_tick ------------------

typedef struct {
    bool temp30_tmp__48;
    int temp25_tmp__47;} count_tick_mem__64;

void count_tick_reset__67(count_tick_mem__64* self);

typedef struct {
    int a__6;} count_tick_ret__65;

count_tick_ret__65 count_tick_step__68(bool tick__5, count_tick_mem__64* self);

// ------------------ NODE test_if_merge ------------------

typedef struct {
    count_tick_mem__64 count_tick_54__54;
    count_tick_mem__64 count_tick_53__53;
    count_mem__59 count_52__52;
    count_mem__59 count_51__51;
    count_mem__59 count_50__50;
    count_mem__59 count_49__49;} test_if_merge_mem__69;

void test_if_merge_reset__72(test_if_merge_mem__69* self);

typedef struct {
    int s1__9;
    int s2__10;
    int s3__11;} test_if_merge_ret__70;

test_if_merge_ret__70 test_if_merge_step__73(bool a__8, test_if_merge_mem__69* self);

// ------------------ NODE test2 ------------------

typedef struct {
    count_mem__59 count_58__58;
    count_mem__59 count_57__57;
    count_mem__59 count_56__56;
    count_mem__59 count_55__55;} test2_mem__74;

void test2_reset__77(test2_mem__74* self);

typedef struct {
    int s1__14;
    int s2__15;} test2_ret__75;

test2_ret__75 test2_step__78(bool a__13, test2_mem__74* self);

