!>ある境界における，スカラ量に対する
!>Neumann境界条件に関係した型を提供する．
!>
!>型には，境界におけるスカラ量の外向き法線方向勾配を取り扱う型が含まれる．
!>
!>@note
!>メインルーチンの中では明確に型宣言されない．
!>演算に使われる他の型の成分として宣言，演算子に渡す値として生成，
!>演算の結果として中間的に生成される．
!>@endnote
!>
module grid_uniform_stg_op_cust_bc_vars_scalar_grad_on_bnd
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_stg_op_cust_bc_vars_scalar_grad_Neumann
    implicit none
    private

    !>ある境界におけるスカラ量の外向き法線方向勾配を取り扱うための派生型．
    !>スカラ量の外向き法線方向勾配の値と，境界の位置を示すインデックスを成分に持つ．
    type, public :: scalar_gradient_on_boundary_type
        type(Neumann_scalar_gradient_type) :: scalar_gradient
            !! 境界におけるスカラ量の外向き法線方向勾配
        integer(int32) :: boundary_index
            !! 境界の位置を示すインデックス
    end type scalar_gradient_on_boundary_type

end module grid_uniform_stg_op_cust_bc_vars_scalar_grad_on_bnd
