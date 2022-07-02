!>連立方程式\(\boldsymbol{Ax}=\boldsymbol{b}\)
!>の取り扱いに関する手続を提供する．
!>
!>手続には，連立方程式を取り扱う型[[Ax_eq_b_type]]に右辺値を
!>渡すための手続が含まれる．
!>
!>また，`laplacian(p) .results. b`などと
!>表現することを実現するためのユーザ定義演算子`.results.`を
!>定義するインタフェースも含まれる．
!>
!>@note
!>連立方程式をより数式表現に近づけることを目的として，`.eq.`や`==`を
!>オーバーロードし，`laplacian(p) == b`などと書くことも考えられる．
!>しかし，プログラミングの原則として，`.eq.`や`==`が等値性の比較結果
!>以外を返すことは受け入れられない．
!>
!>そのため，`.eq.`や`==`のオーバーロードは採用しなかった．
!>
!>@endnote
!>
module grid_uniform_staggered_op_custom_solver_results
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_staggered_vars_scalar_2d
    use :: grid_uniform_staggered_op_custom_solver_vars_Ax_adt
    use :: grid_uniform_staggered_op_custom_solver_vars_AxEqB
    implicit none
    private
    public :: operator(.results.)

    !>ユーザ定義演算子`.results.`を定義するインタフェース
    interface operator(.results.)
        procedure :: results_Ax_eq_b
    end interface

contains
    !>連立方程式を取り扱う型`Ax_eq_b_type`に左辺値と右辺値を渡し，
    !>それらを反映した`Ax_eq_b_type`を返す．
    function results_Ax_eq_b(Ax, b) result(new_ax_eq_b)
        implicit none
        !&<
        class(Ax_atype)         , intent(in) :: Ax
            !! 連立方程式の左辺
        type(scalar_2d_type)    , intent(in) :: b
            !! 連立方程式の右辺
        !&>
        type(Ax_eq_b_type) :: new_Ax_eq_b
            !! 左辺と右辺を反映した連立方程式

        ! `new_Ax_eq_b%Ax`は`class,allocatable`のため，引数のAxをクローン
        allocate (new_Ax_eq_b%Ax, source=Ax)
        ! 右辺は`type`のため，そのまま代入
        new_Ax_eq_b%b = b
    end function results_Ax_eq_b
end module grid_uniform_staggered_op_custom_solver_results
