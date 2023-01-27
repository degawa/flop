!>Navier-Stokes方程式の移流項\((\boldsymbol{u}\cdot\nabla)\boldsymbol{u}\)
!>に関する派生型を提供する．
!>
!>派生型には，ナブラ演算子\(\nabla\)を模擬した派生型が含まれる．
!>ただし，演算子としての機能は有しておらず，
!>移流項を`u.dot.nabla`と表現するための擬似的な型である．
!>
!>@note
!>メインルーチンの中では明確に型宣言されない．
!>演算に使われる他の型の成分として宣言，演算子に渡す値として生成，
!>演算の結果として中間的に生成される．
!>@endnote
!>
module grid_uniform_stg_op_cust_binary_vars_nabla
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_stg_op_cust_binary_vars_uGrad
    implicit none
    private

    !>ナブラ演算子\(\nabla\)を模擬した派生型．
    !>
    !>演算子としての機能は有さない擬似的な空の型．
    type, public :: nabla_type
    end type nabla_type

    type(nabla_type), public, parameter :: nabla = nabla_type()
        !! ナブラ演算子\(\nabla\)
end module grid_uniform_stg_op_cust_binary_vars_nabla
