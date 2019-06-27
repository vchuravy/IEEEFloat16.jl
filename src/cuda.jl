import .CUDAnative

# __CUDA_FP16_DECL__ __half hexp(const __half a) {
#     __half val;
#     asm("{.reg.b32         f, C;           \n"
#         " .reg.b16         h,r;            \n"
#         "  mov.b16         h,%1;           \n"
#         "  cvt.f32.f16     f,h;            \n"
#         "  mov.b32         C, 0x3fb8aa3b;  \n"
#         "  mul.f32         f,f,C;          \n"
#         "  ex2.approx.f32      f,f;        \n"
#         "  cvt.rn.f16.f32      r,f;        \n"
#         __SPEC_CASE(h, r, 0X1F79, 0x9400)
#         __SPEC_CASE(h, r, 0X25CF, 0x9400)
#         __SPEC_CASE(h, r, 0XC13B, 0x0400)
#         __SPEC_CASE(h, r, 0XC1EF, 0x0200)
#         "  mov.b16         %0,r;           \n"
#         "}": "=h"(__HALF_TO_US(val)) : "h"(__HALF_TO_CUS(a)));
#     return val;
# }
#

ex2approx(x) = ccall("llvm.nvvm.ex2.approx.f", llvmcall, Float32, (Float32,), x)
function CUDAnative.exp(a::Float16)
    f = convert(Float32, a)
    f = f * log2(Float32(ℯ))
    f = ex2approx(f)
    h = convert(Float16, f)
end

# Generate a `llvmcall` statement calling an intrinsic specified as follows:
#
#     intrinsic(arg::arg_type, arg::arg_type, ... arg::arg_type)::return_type [attr]
#
# The argument types should be valid LLVM type identifiers (eg. i32, float, double).
# Conversions to the corresponding Julia type are automatically generated; make sure the
# actual arguments are of the same type to make these conversions no-ops. The optional
# argument `attr` indicates which LLVM function attributes (such as `readnone` or `nounwind`)
# to add to the intrinsic declaration.

# For example, the following call:
#     `@wrap __some_intrinsic(x::float, y::double)::float`
#
# will yield the following `llvmcall`:
# ```
#     Base.llvmcall(("declare float @__somme__intr(float, double)",
#                    "%3 = call float @__somme__intr(float %0, double %1)
#                     ret float %3"),
#                   Float32, Tuple{Float32,Float64},
#                   convert(Float32,x), convert(Float64,y))
# ```
# macro wrap(call, attrs="")
#     intrinsic, args, argtypes, rettype = CUDAnative.decode_call(call)
#     @assert rettype === :half
# 
#     llvm_ret_typ = "i16"
# 
#     # decide on intrinsic return type
#     if isa(rettype, Symbol)
#         # only LLVM return type specified, match against known LLVM/Julia type combinations
#         llvm_ret_typ = rettype
#         julia_ret_typ = jltypes[rettype]
#     else
#         # both specified (for when there is a mismatch, eg. i32 -> UInt32)
#         llvm_ret_typ = rettype[1]
#         julia_ret_typ = rettype[2]
#     end
# 
#     llvm_args = String["%$i" for i in 0:length(argtypes)]
#     if llvm_ret_typ == :void
#         llvm_ret_asgn = ""
#         llvm_ret = "void"
#     else
#         llvm_ret_var = "%$(length(argtypes)+1)"
#         llvm_ret_asgn = "$llvm_ret_var = "
#         llvm_ret = "$llvm_ret_typ $llvm_ret_var"
#     end
#     llvm_declargs = join(argtypes, ", ")
#     llvm_defargs = join(("$t $arg" for (t,arg) in zip(argtypes, llvm_args)), ", ")
# 
#     julia_argtypes = (jltypes[t] for t in argtypes)
#     julia_args = (:(convert($argtype, $(esc(arg)))) for (arg, argtype) in zip(args, julia_argtypes))
# 
#     dest = ("""declare $llvm_ret_typ @$intrinsic($llvm_declargs)""",
#             """$llvm_ret_asgn call $llvm_ret_typ @$intrinsic($llvm_defargs)
#                 ret $llvm_ret""")
#     return quote
#         Base.llvmcall($dest, $julia_ret_typ, Tuple{$(julia_argtypes...)}, $(julia_args...))
#     end
# end

