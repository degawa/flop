!>連立方程式\(\boldsymbol{Ax}=\boldsymbol{b}\)
!>の取り扱いに関する派生型を提供する．
!>
!>派生型には，連立方程式を求解するソルバを表す抽象データ型が含まれる．
!>
module grid_uniform_stg_op_custom_linEqs_vars_solver_adt
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_stg_vars_scalar_2d
    use :: grid_uniform_stg_vars_scalar_2d_bc
    implicit none
    private

    !>連立方程式のソルバを表す抽象データ型．
    type, public, abstract :: solver_atype
    contains
        procedure(ISolve_iterative), public, pass, deferred :: solve_using_iterative_method
        !* 連立方程式を解いて未知数`x`を更新
        generic :: solve => solve_using_iterative_method
    end type solver_atype

    abstract interface
        !>連立方程式を解いて未知数を更新する手続のインタフェース．
        subroutine ISolve_iterative(this, x, b, BC, err_tol)
            use, intrinsic :: iso_fortran_env
            import solver_atype
            import scalar_2d_type
            import scalar_boundary_condition_type
            class(solver_atype), intent(in) :: this
            class(scalar_2d_type), intent(inout) :: x
            class(scalar_2d_type), intent(in) :: b
            class(scalar_boundary_condition_type), intent(in) :: BC
            real(real64), intent(in) :: err_tol
        end subroutine ISolve_iterative
    end interface
end module grid_uniform_stg_op_custom_linEqs_vars_solver_adt
