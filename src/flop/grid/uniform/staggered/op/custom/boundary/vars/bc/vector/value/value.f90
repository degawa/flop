!>ある境界における，ベクトル量に対する
!>Dirichlet境界条件に関係した型を提供する．
!>
!>型には，境界におけるベクトル量の値を取り扱う型が含まれる．
!>
!>@note
!>メインルーチンの中では明確に型宣言されない．
!>演算に使われる他の型の成分として宣言，演算子に渡す値として生成，
!>演算の結果として中間的に生成される．
!>@endnote
!>
module grid_uniform_stg_op_cust_bc_vars_vector_value_on_bnd
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_stg_op_cust_bc_vars_vector_value_Dirichlet
    implicit none
    private

    !>ある境界におけるベクトル量を取り扱うための派生型．
    !>ベクトル量の値と，境界の位置を示すインデックスを成分に持つ．
    type, public :: vector_value_on_boundary_type
        type(Dirichlet_vector_value_type) :: vector_value
            !! 境界におけるベクトル量の値
        integer(int32) :: boundary_index
            !! 境界の位置を示すインデックス
    end type vector_value_on_boundary_type

end module grid_uniform_stg_op_cust_bc_vars_vector_value_on_bnd
