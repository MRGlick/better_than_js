
#pragma once

#define def_result(T) typedef struct {int err; T val; } Result_##T
#define errors(...) enum {_ = 0, __VA_ARGS__}

#define Result(T) Result_##T

#define Ok(res_type, v) (Result(res_type)){.val = v}
#define Err(res_type, e) (Result(res_type)){.err = e}