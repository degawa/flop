!>2次元Staggered格子上で定義される物理量に対する境界条件を
!>一括して取り扱うために`use`するモジュール
module grid_uniform_staggered_op_custom_bc
    use :: grid_uniform_staggered_op_custom_bc_vars_type
    use :: grid_uniform_staggered_op_custom_bc_vars_position

    use :: grid_uniform_staggered_op_custom_bc_vars_vector_value_Dirichlet
    use :: grid_uniform_staggered_op_custom_bc_vars_vector_value_on_bnd

    use :: grid_uniform_staggered_op_custom_bc_vars_scalar_value_Dirichlet
    use :: grid_uniform_staggered_op_custom_bc_vars_scalar_value_on_bnd
    use :: grid_uniform_staggered_op_custom_bc_vars_scalar_grad_Neumann
    use :: grid_uniform_staggered_op_custom_bc_vars_scalar_grad_on_bnd

    use :: grid_uniform_staggered_op_custom_bc_on
    use :: grid_uniform_staggered_op_custom_bc_set
    use :: grid_uniform_staggered_op_custom_bc_impose
    public
end module grid_uniform_staggered_op_custom_bc
