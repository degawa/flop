!>2次元Staggered格子上で定義される，ユーザ定義の2項演算子を
!>一括して`use`するためのモジュール
module grid_uniform_stg_op_binary
    use :: grid_uniform_stg_op_binary_times_central
    use :: grid_uniform_stg_op_binary_dot
    use :: grid_uniform_stg_op_binary_times_interpolatedHadamard
    public
end module grid_uniform_stg_op_binary
