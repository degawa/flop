!>ある境界における，ベクトル量に対する
!>Dirichlet境界条件に関係した型を提供する．
!>
!>型には，境界におけるベクトル量の値を取り扱う型が含まれる．
!>
module grid_uniform_staggered_op_custom_bc_vector_value_on_bnd
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_staggered_op_custom_bc_vector_value_Dirichlet
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

end module grid_uniform_staggered_op_custom_bc_vector_value_on_bnd
