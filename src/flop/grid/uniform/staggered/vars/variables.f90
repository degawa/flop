!>2次元Staggered格子上で定義される量に関する型を
!>一括して`use`するためのモジュール
module grid_uniform_staggered_variables
    use :: grid_uniform_staggered_2d
    use :: grid_uniform_staggered_vars_scalar_2d
    use :: grid_uniform_staggered_vars_vector_2d
    use :: grid_uniform_staggered_vars_tensor_2d
    use :: grid_uniform_staggered_vars_vector_2d_bc
    use :: grid_uniform_staggered_vars_scalar_2d_bc
    public ! 参照しているモジュール内を全て公開
end module grid_uniform_staggered_variables
