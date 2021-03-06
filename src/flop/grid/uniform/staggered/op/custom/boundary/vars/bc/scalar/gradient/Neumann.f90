!>スカラ量に対するNeumann境界条件に関係した型や手続を提供する．
!>
!>型には，スカラ量の外向き法線方向勾配を取り扱う型が含まれる．
!>
!>@note
!>メインルーチンの中では明確に型宣言されない．
!>演算に使われる他の型の成分として宣言，演算子に渡す値として生成，
!>演算の結果として中間的に生成される．
!>@endnote
!>
!>手続には，スカラ量の外向き法線方向勾配を取り扱う型を構築する
!>コンストラクタが含まれる．
!>
module grid_uniform_stg_op_cust_bc_vars_scalar_grad_Neumann
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: Neumann

    !>スカラ量の外向き法線方向勾配を取り扱う派生型．
    type, public :: Neumann_scalar_gradient_type
        real(real64) :: gradient
            !! スカラ量の外向き法線方向勾配
    end type Neumann_scalar_gradient_type

    !>スカラ量の外向き法線方向勾配を`Neumann(grad)`
    !>で作成できるようにするためのインタフェース
    interface Neumann
        procedure :: Neumann_scalar
    end interface

contains
    !>スカラ量の外向き法線方向勾配を扱う型を構築するためのコンストラクタ．
    function Neumann_scalar(grad) result(new_grad)
        implicit none

        real(real64), intent(in) :: grad
            !! スカラ量の外向き法線方向勾配
        type(Neumann_scalar_gradient_type) :: new_grad
            !! 派生型にまとめられたスカラ量の外向き法線方向勾配

        new_grad%gradient = grad
    end function Neumann_scalar
end module grid_uniform_stg_op_cust_bc_vars_scalar_grad_Neumann
