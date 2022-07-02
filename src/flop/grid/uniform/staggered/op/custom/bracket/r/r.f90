!>Navier-Stokes方程式の移流項\((\boldsymbol{u}\cdot\nabla)\boldsymbol{u}\)
!>に関する手続を提供する．
!>
!>手続には，Navier-Stokes方程式の非線形演算\((\boldsymbol{u}\cdot\nabla)\)
!>のうち，右括弧を表現する手続が含まれる．
!>
!>また，移流項の計算を`.l.(u.dot.nabla).r. u`と表現することを実現するための
!>bracket演算子（ユーザ定義演算子）`.r.`を定義するインタフェースも含まれる．
!>
!>@note
!>`.r.`演算子は，移流項の非線形演算を表現する`u_grad_type`に
!>保存量の速度を渡し，`u_grad_type`の手続`compute`を呼び出すための
!>演算子である．
!>
!>`.l.(u.dot.nabla).r. u`の動作としては，(u.dot.nabla)が処理されて
!>`u_grad_type`の一時変数が返ってきた後，単項演算`.l.(u.dot.nabla)`
!>で．`u_grad_type`がそのまま返ってくる．
!>
!>最後に，`.r.`が演算子右側の`u`を引数として`u_grad_type`の手続`compute`を
!>呼び出すことで移流項の演算が行われ，結果としてベクトル量が返される．
!>
!>@endnote
!>
module grid_uniform_stg_op_cust_bracket_r
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_stg_vars_vector_2d
    use :: grid_uniform_stg_op_cust_binary_vars_uGrad
    implicit none
    private
    public :: operator(.r.)

    !>bracket演算子`.l.`を定義するインタフェース
    interface operator(.r.)
        procedure :: r
    end interface

contains
    !>移流項を計算し，結果をベクトル量で返す．
    !>
    !>`u`を引数として`u_grad_type`の手続`compute`を
    !>呼び出すことで移流項の演算を実行する．
    function r(u_grad, u) result(new_vec)
        implicit none
        !&<
        class(u_grad_type)      , intent(in) :: u_grad
            !! 非線形演算子\((\boldsymbol{u}\cdot\nabla)\)
        class(vector_2d_type)   , intent(in) :: u
            !! 速度（保存量）
        !&>
        type(vector_2d_type) :: new_vec
            !! 移流項の計算結果

        call new_vec%construct(u%get_base_grid())
        new_vec = u_grad%compute(u)
    end function r
end module grid_uniform_stg_op_cust_bracket_r
