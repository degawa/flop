!>連立方程式\(\boldsymbol{Ax}=\boldsymbol{b}\)
!>の取り扱いに関する派生型を提供する．
!>
!>派生型には，連立方程式の左辺\(\boldsymbol{Ax}\)を表す
!>抽象データ型が含まれる．
!>
module grid_uniform_staggered_op_custom_solver_vars_Ax_adt
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_staggered_vars_scalar_2d
    use :: grid_uniform_staggered_vars_scalar_2d_bc
    implicit none
    private

    !>連立方程式の左辺\(\boldsymbol{Ax}\)を表す抽象データ型．
    type, public, abstract :: Ax_atype
        type(scalar_2d_type) :: x
            !! 未知数
        type(scalar_boundary_condition_type) :: BC
            !! 未知数`x`に対する境界条件
        real(real64) :: err_tol = 1d-3
            !! 連立方程式を反復法で解く場合の許容誤差
    contains
        procedure(ISolve), public, pass, deferred :: solve
            !! 連立方程式を解いて未知数`x`を更新
        procedure(IEval), public, pass, deferred :: eval
            !! \(\boldsymbol{Ax}\)を計算した結果を返却
    end type Ax_atype

    abstract interface
        !>連立方程式を解いて未知数を更新する手続のインタフェース．
        subroutine ISolve(this, x, b)
            import Ax_atype
            import scalar_2d_type
            class(Ax_atype), intent(in) :: this
            class(scalar_2d_type), intent(inout) :: x
            class(scalar_2d_type), intent(in) :: b
        end subroutine ISolve

        !>連立方程式の左辺を計算する手続のインタフェース．
        function IEval(this) result(new_b)
            import Ax_atype
            import scalar_2d_type
            class(Ax_atype), intent(in) :: this
            type(scalar_2d_type) :: new_b
        end function IEval
    end interface
end module grid_uniform_staggered_op_custom_solver_vars_Ax_adt
