!>連立方程式\(\boldsymbol{Ax}=\boldsymbol{b}\)
!>の取り扱いに関する派生型を提供する．
!>
!>派生型には，連立方程式の左辺\(\boldsymbol{Ax}\)を表す
!>抽象データ型が含まれる．
!>
module grid_uniform_stg_op_custom_linEqs_vars_Ax_adt
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_stg_vars_scalar_2d
    use :: grid_uniform_stg_vars_scalar_2d_bc
    use :: grid_uniform_stg_op_custom_linEqs_vars_solver_adt
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
        class(solver_atype), allocatable :: solver
            !! 連立方程式を解くソルバ
    contains
        procedure, public, pass :: solve
        !* 連立方程式を解いて未知数`x`を更新
        procedure(IEval), public, pass, deferred :: eval
        !* \(\boldsymbol{Ax}\)を計算した結果を返却
        procedure(IConstruct_solver), public, pass, deferred :: construct_solver
        !* 連立方程式の解法を割付・設定
        procedure, public, pass :: destruct_solver
        !* 連立方程式の解法を破棄
    end type Ax_atype

    abstract interface
        !>連立方程式の左辺を計算する手続のインタフェース．
        function IEval(this) result(new_b)
            import Ax_atype
            import scalar_2d_type
            class(Ax_atype), intent(in) :: this
            type(scalar_2d_type) :: new_b
        end function IEval
        !>連立方程式のソルバを構築する手続のインタフェース．
        subroutine IConstruct_solver(this, solver_spec)
            use :: grid_uniform_stg_op_custom_linEqs_vars_solver_spec_adt
            import Ax_atype
            implicit none
            class(Ax_atype), intent(inout) :: this
            class(solver_spec_atype), intent(in) :: solver_spec
        end subroutine IConstruct_solver
    end interface

contains
    !>連立方程式\(Ax=b\)を解いて\(x\)を更新する．
    subroutine solve(this, x, b)
        implicit none
        class(Ax_atype), intent(in) :: this
            !! 当該実体仮引数
        class(scalar_2d_type), intent(inout) :: x
            !! 未知数
        class(scalar_2d_type), intent(in) :: b
            !! 右辺ベクトル

        call this%solver%solve(x, b, this%BC, this%err_tol)
    end subroutine solve

    !>連立方程式の解法`solver`を破棄する．
    subroutine destruct_solver(this)
        implicit none
        class(Ax_atype), intent(inout) :: this
            !! 当該実体仮引数

        if (allocated(this%solver)) deallocate (this%solver)
    end subroutine destruct_solver
end module grid_uniform_stg_op_custom_linEqs_vars_Ax_adt
