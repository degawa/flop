!>ある境界における，スカラ量に対する
!>Dirichlet境界条件に関係した型を提供する．
!>
!>型には，境界におけるスカラ量の値を取り扱う型が含まれる．
!>
module grid_uniform_staggered_op_custom_bc_scalar_value_on_bnd
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_staggered_op_custom_bc_scalar_value_Dirichlet
    implicit none
    private

    !>ある境界におけるスカラ量を取り扱うための派生型．
    !>スカラ量の値と，境界の位置を示すインデックスを成分に持つ．
    type, public :: scalar_value_on_boundary_type
        type(Dirichlet_scalar_value_type) :: scalar_value
            !! 境界におけるスカラ量の値
        integer(int32) :: boundary_index
            !! 境界の位置を示すインデックス
    end type scalar_value_on_boundary_type

end module grid_uniform_staggered_op_custom_bc_scalar_value_on_bnd
