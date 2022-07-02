!>Navier-Stokes方程式の移流項\((\boldsymbol{u}\cdot\nabla)\boldsymbol{u}\)
!>に関する手続を提供する．
!>
!>手続には，Navier-Stokes方程式の非線形演算\((\boldsymbol{u}\cdot\nabla)\)
!>のうち，左括弧を表現する手続が含まれる．
!>
!>また，移流項の計算を`.l.(u.dot.nabla).r. u`と表現することを実現するための
!>bracket演算子（ユーザ定義演算子）`.l.`を定義するインタフェースも含まれる．
!>
!>@note
!>`.l.`演算子は，実際には何も行わず，非線形演算子を適用する`.r.`の
!>対として用意されている．
!>
!>`.l.(u.dot.nabla).r. u`の動作としては，(u.dot.nabla)が処理されて
!>`u_grad_type`の一時変数が返ってくるので，次に単項演算
!>`.l.(u.dot.nabla)`が処理される．
!>
!>そこで，`.l.`は`u_grad_type`をそのまま返すだけの手続とした．
!>ただし，他の言語のように，`return`で戻り値を指定できないので，
!>コピーが発生している．
!>
!>@endnote
!>
module grid_uniform_stg_op_cust_bracket_l
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_stg_op_cust_binary_vars_uGrad
    implicit none
    private
    public :: operator(.l.)

    !>bracket演算子`.l.`を定義するインタフェース
    interface operator(.l.)
        procedure :: l
    end interface

contains

    !>`u_grad_type`を受け取り，そのまま返す．
    !>
    !>Navier-Stokes方程式の移流項を`.l.(u.dot.nabla).r. u`
    !>と表現するため，`.r.`の対として導入された．
    function l(u_grad) result(new_u_grad)
        implicit none

        type(u_grad_type), intent(in) :: u_grad
            !! 非線形演算子\((\boldsymbol{u}\cdot\nabla)\)

        type(u_grad_type) :: new_u_grad
            !! 非線形演算子\((\boldsymbol{u}\cdot\nabla)\)<br>
            !! 引数がそのまま返る

        new_u_grad = u_grad
    end function l
end module grid_uniform_stg_op_cust_bracket_l
