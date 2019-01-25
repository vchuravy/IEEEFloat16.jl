module IEEEFloat16

using LLVM
using LLVM.Interop

primitive type Float16 <: AbstractFloat 16 end

import Core.Intrinsics.bitcast
import Core.Intrinsics.llvmcall

## floatfuncs
Base.signbit(x::Float16) = signbit(bitcast(Int16, x))
Base.maxintfloat(::Type{Float16}) = Float16(2048f0)
# TODO: fma

## hashing2
# TODO

## fastmath
# TODO

## complex
# TODO

## irrationals
# TODO

## float
const IEEEInf16 = bitcast(Float16, 0x7c00)
const IEEENaN16 = bitcast(Float16, 0x7e00)

Base.convert(::Type{Float16}, x::Base.Float16) = bitcast(Float16, x)
Base.convert(::Type{Base.Float16}, x::Float16) = bitcast(Base.Float16, x)

@generated function Base.convert(::Type{Float16}, x::T) where T<:Union{Float32, Float64}
    T_float = convert(LLVMType, T)
    T_i16 = convert(LLVMType, Int16)
    T_f16 = LLVM.HalfType(JuliaContext())

    f, ft = create_function(T_i16, [T_float])

    Builder(JuliaContext()) do builder
        entry = BasicBlock(f, "entry", JuliaContext())
        position!(builder, entry)

        val = fptrunc!(builder, parameters(f)[1], T_f16)
        retval = bitcast!(builder, val, T_i16)
        ret!(builder, retval)
    end

    call_function(f, Float16, Tuple{T}, :((x,)))
end

@generated function Base.convert(::Type{T}, x::Float16) where T<:Union{Float32, Float64}
    T_float = convert(LLVMType, T)
    T_i16 = convert(LLVMType, Int16)
    T_f16 = LLVM.HalfType(JuliaContext())

    f, ft = create_function(T_float, [T_i16])

    Builder(JuliaContext()) do builder
        entry = BasicBlock(f, "entry", JuliaContext())
        position!(builder, entry)
        
        val = bitcast!(builder, parameters(f)[1], T_f16)
        retval = fpext!(builder, val, T_float)
        ret!(builder, retval)
    end

    call_function(f, T, Tuple{Float16}, :((x,)))
end

@generated function Base.convert(::Type{T}, x::Float16) where T<:Unsigned
    T_uint = convert(LLVMType, T)
    T_i16 = convert(LLVMType, Int16)
    T_f16 = LLVM.HalfType(JuliaContext())

    f, ft = create_function(T_uint, [T_i16])
    Builder(JuliaContext()) do builder
        entry = BasicBlock(f, "entry", JuliaContext())
        position!(builder, entry)
        
        val = bitcast!(builder, parameters(f)[1], T_f16)
        retval = fptoui!(builder, val, T_uint)
        ret!(builder, retval)
    end

    call_function(f, T, Tuple{Float16}, :((x,)))
end

@generated function Base.convert(::Type{T}, x::Float16) where T<:Signed
    T_int = convert(LLVMType, T)
    T_i16 = convert(LLVMType, Int16)
    T_f16 = LLVM.HalfType(JuliaContext())

    f, ft = create_function(T_int, [T_i16])
    Builder(JuliaContext()) do builder
        entry = BasicBlock(f, "entry", JuliaContext())
        position!(builder, entry)
        
        val = bitcast!(builder, parameters(f)[1], T_f16)
        retval = fptosi!(builder, val, T_int)
        ret!(builder, retval)
    end

    call_function(f, T, Tuple{Float16}, :((x,)))
end

for (op, llvmop!) in ((:+, fadd!), (:-, fsub!), (:*, fmul!), (:/, fdiv!))
    @eval @generated function Base.$op(a::Float16, b::Float16)
        T_i16 = convert(LLVMType, Int16)
        T_f16 = LLVM.HalfType(JuliaContext())

        f, ft = create_function(T_i16, [T_i16, T_i16])
        Builder(JuliaContext()) do builder
            entry = BasicBlock(f, "entry", JuliaContext())
            position!(builder, entry)

            a = bitcast!(builder, parameters(f)[1], T_f16)
            b = bitcast!(builder, parameters(f)[2], T_f16)

            val = $llvmop!(builder, a, b)
            retval = bitcast!(builder, val, T_i16)
            ret!(builder, retval)
        end

        call_function(f, Float16, Tuple{Float16, Float16}, :((a,b)))
    end
end

@generated function Base.:-(x::Float16)
    T_i16 = convert(LLVMType, Int16)
    T_f16 = LLVM.HalfType(JuliaContext())

    f, ft = create_function(T_i16, [T_i16])
    Builder(JuliaContext()) do builder
        entry = BasicBlock(f, "entry", JuliaContext())
        position!(builder, entry)
        
        x = bitcast!(builder, parameters(f)[1], T_f16)
        val = fneg!(builder, x)
        retval = bitcast!(builder, val, T_i16)

        ret!(builder, retval)
    end

    call_function(f, Float16, Tuple{Float16}, :((x,)))
end

# TODO check that power_by_sqauring is efficent
@inline Base.literal_pow(::typeof(^), x::Float16, ::Val{0}) = one(x)
@inline Base.literal_pow(::typeof(^), x::Float16, ::Val{1}) = x
@inline Base.literal_pow(::typeof(^), x::Float16, ::Val{2}) = x*x
@inline Base.literal_pow(::typeof(^), x::Float16, ::Val{3}) = x*x*x

# TODO promote rules
# TODO

## math
# TODO

## int
Base.flipsign(x::Signed, y::Float16) = flipsign(x, bitcast(Int16, y))
Base.copysign(x::Signed, y::Float16) = copysign(x, bitcast(Int16, y))

## iostream
# TODO

## gmp
# TODO

## io
# TODO

## parse
# TODO

## grisu
# The printing in Grisu._show is not extendible
Base.print(io::IO, x::Float16) = Base.print(io, reinterpret(Base.Float16, x))
Base.show(io::IO, x::Float16) = Base.print(io, reinterpret(Base.Float16, x))

## range
# TODO

## intfuncs
# TODO

## mpfr
# TODO

## iobuffer
# TODO

## essentials
Base.reinterpret(::Type{Unsigned}, x::Float16) = reinterpret(UInt16,x)
Base.reinterpret(::Type{Signed}, x::Float16) = reinterpret(Int16,x)

## printf
# TODO

## twiceprecision
# TODO

end
