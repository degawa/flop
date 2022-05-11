!>2次元Staggered格子上で構築される連立方程式を
!>一括して取り扱うために`use`するモジュール
module grid_uniform_staggered_op_custom_solver
    use :: grid_uniform_staggered_op_custom_solver_Ax_adt
    use :: grid_uniform_staggered_op_custom_solver_Ax_laplacian
    use :: grid_uniform_staggered_op_custom_solver_AxEqB
    use :: grid_uniform_staggered_op_custom_solver_results
    use :: grid_uniform_staggered_op_custom_solver_inverse
    use :: grid_uniform_staggered_op_custom_solver_until
    use :: grid_uniform_staggered_op_custom_solver_with
    public
end module grid_uniform_staggered_op_custom_solver
