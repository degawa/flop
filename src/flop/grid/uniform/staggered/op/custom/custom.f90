!| Navier-Stokes方程式の移流項の演算に関わる型と演算子を
!一括して`use`するためのモジュール
module grid_uniform_staggered_op_custom
    use :: grid_uniform_staggered_op_custom_nabla
    use :: grid_uniform_staggered_op_custom_uGrad
    use :: grid_uniform_staggered_op_custom_dot
    public
end module grid_uniform_staggered_op_custom