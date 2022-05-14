!>連立方程式\(\boldsymbol{Ax}=\boldsymbol{b}\)
!>の境界条件に関する手続を提供する．
!>
!>手続には，Laplace-Poisson方程式から作られる連立方程式の
!>左辺を取り扱う型[[Ax_laplacian_with_BC]]に境界条件を
!>渡すための手続が含まれる．
!>
!>また，`laplacian(p) .with. BC_p`などと
!>表現することを実現するためのユーザ定義演算子`.with.`を
!>定義するインタフェースも含まれる．
!>
!>その他の連立方程式を取り扱う場合は，専用の手続を追加し，
!>interfaceを用いて適切な手続が自動で選択されるようにする．
!>
module grid_uniform_staggered_op_custom_solver_with
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_staggered_vars_scalar_2d_bc
    use :: grid_uniform_staggered_op_custom_solver_vars_Ax_adt
    use :: grid_uniform_staggered_op_custom_solver_Ax_vars_laplacian
    implicit none
    private
    public :: operator(.with.)

    !>ユーザ定義演算子`.with.`を定義するインタフェース
    interface operator(.with.)
        procedure :: Ax_laplacian_with_BC
    end interface

contains
    !>Laplace-Poisson方程式から作られる連立方程式の
    !>左辺を取り扱う型`Ax_laplacian_type`に境界条件を渡し，
    !>それらを反映した`Ax_laplacian_type`を返す．
    function Ax_laplacian_with_BC(Ax, scr_bc) result(new_ax)
        implicit none
        !&<
        type(Ax_laplacian_type)                 , intent(in) :: Ax
            !! 連立方程式の左辺
        class(scalar_boundary_condition_type)   , intent(in) :: scr_bc
            !! 未知数xに対する境界条件
        !&>
        type(Ax_laplacian_type) :: new_Ax
            !! 境界条件を反映した左辺

        new_Ax%x = Ax%x
        new_Ax%err_tol = Ax%err_tol
        new_Ax%BC = scr_bc
        new_Ax%accel = Ax%accel
    end function Ax_laplacian_with_BC

end module grid_uniform_staggered_op_custom_solver_with
