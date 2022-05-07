!| 境界の位置に関係する型や定数を提供する．
!
!型には，境界位置を表現するための派生型が含まれる．
!
!定数は，2次元空間の4辺を番号付けし，それを示す定数が含まれる．
!
!境界は下記の様に番号付けられ，その位置を示す定数
!\(B_1 \sim B_4\)が定義される．
!
!```console
!         B4
!     +---------+
!     |         |
!  B1 |         | B2
! y   |         |
! ^   +---------+
! +->x      B3
!```
!
module grid_uniform_staggered_op_custom_bc_position
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding
    implicit none
    private
    public :: boundary_left_index, &
              boundary_right_index, &
              boundary_bottom_index, &
              boundary_top_index

    integer(int32), public, parameter :: number_of_boundaries = 4
        !! 境界の数

    enum, bind(c)
        enumerator :: boundary_left_index = 1
            !! 左側（\(-x\)方向）境界の番号
        enumerator :: boundary_right_index
            !! 右側（\(+x\)方向）境界の番号
        enumerator :: boundary_bottom_index
            !! 下側（\(-y\)方向）境界の番号
        enumerator :: boundary_top_index
            !! 上側（\(+y\)方向）境界の番号
    end enum

    !| 境界位置を表現するための派生型．
    !
    !変数として宣言することは想定しておらず，
    !境界`B1`~`B4`を表現した定数を定義するために用いる．
    type, public :: boundary_position_type
        integer(int32) :: position
            !! 境界の位置を示すインデックス
    end type boundary_position_type

    type(boundary_position_type), public, parameter :: &
        B1 = boundary_position_type(position=boundary_left_index)
        !! 左側（\(-x\)方向）境界\(B_1\)を表現する定数

    type(boundary_position_type), public, parameter :: &
        B2 = boundary_position_type(position=boundary_right_index)
        !! 右側（\(+x\)方向）境界\(B_2\)を表現する定数

    type(boundary_position_type), public, parameter :: &
        B3 = boundary_position_type(position=boundary_bottom_index)
        !! 下側（\(-y\)方向）境界\(B_3\)を表現する定数

    type(boundary_position_type), public, parameter :: &
        B4 = boundary_position_type(position=boundary_top_index)
        !! 上側（\(+y\)方向）境界\(B_4\)を表現する定数

end module grid_uniform_staggered_op_custom_bc_position
