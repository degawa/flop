!| 連立方程式\(\boldsymbol{Ax}=\boldsymbol{b}\)
!の取り扱いに関する手続を提供する．
!
!手続には，連立方程式を取り扱う型[[Ax_eq_b_type]]を利用して，
!連立方程式を求解するための手続が含まれる．
!
!また，`.inverse.(laplacian(p) .results. b)`などと
!表現することを実現するためのユーザ定義演算子`.inverse.`を
!定義するインタフェースも含まれる．
!
module grid_uniform_staggered_op_custom_solver_inverse
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_staggered_vars_scalar_2d
    use :: grid_uniform_staggered_op_custom_solver_Ax_adt
    use :: grid_uniform_staggered_op_custom_solver_AxEqB
    implicit none
    private
    public :: operator(.inverse.)

    !| ユーザ定義演算子`.inverse.`を定義するインタフェース
    interface operator(.inverse.)
        procedure :: inverse_Ax_eq_b
    end interface

contains
    !| 連立方程式を取り扱う型`Ax_eq_b_type`の手続`inverse`を呼び出して，
    !連立方程式を解き，得られた未知数`x`を返す．
    !
    !この手続は`.inverse.`演算子として利用することを前提としている．
    !演算子を用いると被演算子を更新できないため，
    !左辺`Ax`の成分`x`は，コピーするが直接更新はしない．
    function inverse_Ax_eq_b(Ax_eq_b) result(x)
        implicit none
        !&<
        class(Ax_eq_b_type), intent(in) :: Ax_eq_b
            !! 解かれる連立方程式
        !&>
        type(scalar_2d_type) :: x
            !! 連立方程式の解

        ! 演算子は`Ax_eq_b%Ax%x`を直接更新できないため，解に一度複製
        x = Ax_eq_b%Ax%x

        ! 手続`inverse`を呼び出して連立方程式を解く
        call Ax_eq_b%inverse(x, Ax_eq_b%b)
    end function inverse_Ax_eq_b
end module grid_uniform_staggered_op_custom_solver_inverse
