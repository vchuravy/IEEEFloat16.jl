using Test
import IEEEFloat16: Float16

@test 1.0f0 == convert(Float32, convert(Float16, 1.0f0))
