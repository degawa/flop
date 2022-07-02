!>連立方程式\(\boldsymbol{Ax}=\boldsymbol{b}\)
!>の取り扱いに関する手続を提供する．
!>
!>手続には，連立方程式の左辺を取り扱う型[[Ax_laplacian_with_BC]]に
!>反復解法の許容誤差を渡すための手続が含まれる．
!>
!>また，`.inverse.((laplacian(p) .results. b).until. 1d-9)`などと
!>表現することを実現するためのユーザ定義演算子`.until.`を
!>定義するインタフェースも含まれる．
!>
!>@note
!>`.until.`は`.inverse.`よりも先に適用される必要があるため，
!>必ず`.inverse.`に括弧を付け，その中に記述する．
!>
!>つまり，`.inverse.( Ax_eq_b .until. error_tolerance )`と書く必要がある．
!>違和感を感じるが，改行を入れると違和感は幾分軽減される．
!>
!>```Fortran
!>.inverse.( &
!>          (Ax_eq_b)&
!>         .until. error_tolerance)
!>```
!>
!>@endnote
!>
module grid_uniform_staggered_op_custom_solver_until
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_staggered_vars_scalar_2d
    use :: grid_uniform_staggered_op_custom_solver_vars_Ax_adt
    use :: grid_uniform_staggered_op_custom_solver_vars_AxEqB
    implicit none
    private
    public :: operator(.until.)

    !>ユーザ定義演算子`.until.`を定義するインタフェース
    interface operator(.until.)
        procedure :: set_error_tolerance
    end interface

contains
    !>連立方程式を取り扱う型`Ax_eq_b_type`に反復法の許容誤差を渡し，
    !>それらを反映した`Ax_eq_b_type`を返す．
    function set_error_tolerance(Ax_eq_b, error_tolerance) result(new_ax_eq_b)
        implicit none
        !&<
        type(Ax_eq_b_type)  , intent(in) :: Ax_eq_b
            !! 連立方程式
        real(real64)        , intent(in) :: error_tolerance
            !! 許容誤差
        !&>
        type(Ax_eq_b_type) :: new_Ax_eq_b
            !! 許容誤差を反映した連立方程式

        new_Ax_eq_b = Ax_eq_b
        new_Ax_eq_b%Ax%err_tol = error_tolerance
    end function set_error_tolerance
end module grid_uniform_staggered_op_custom_solver_until
