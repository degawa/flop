!>時空間離散化に関する派生型，手続を提供する．
!>
!>派生型には，空間離散化に用いる空間情報を持つ型，
!>および時間離散化に用いる時間情報を持つ型が含まれる．
!>
!>手続には，空間離散化に用いる空間情報を持つ型
!>および時間離散化に用いる時間情報を持つ型のコンストラクタが含まれる．
!>
module discretization_op_divide
    use, intrinsic :: iso_fortran_env
    use :: space_vars_Cartesian
    use :: time_vars_axis
    implicit none
    private
    public :: operator(.divide.)

    !>空間離散化に用いる空間情報を持つ型．
    type, public :: space_container_type
        type(Cartesian_2d_type) :: space
            !! 離散化する空間座標系
    end type space_container_type

    !>時間離散化に用いる時間軸情報を持つ型．
    type, public :: time_container_type
        type(time_axis_type) :: time
            !! 離散化する時間軸
    end type time_container_type

    !>ユーザ定義演算子`.divide.`を定義するインタフェース
    interface operator(.divide.)
        procedure :: construct_space_container
        procedure :: construct_time_container
    end interface

contains
    !>離散化に用いる空間情報を持つ型を構築して返す．
    function construct_space_container(space) result(space_container)
        implicit none

        type(Cartesian_2d_type), intent(in) :: space
            !!

        type(space_container_type) :: space_container
            !!

        space_container%space = space
    end function construct_space_container

    !>離散化に用いる時間軸情報を持つ型を構築して返す．
    function construct_time_container(time) result(time_container)
        implicit none

        type(time_axis_type), intent(in) :: time
            !!

        type(time_container_type) :: time_container
            !!

        time_container%time = time
    end function construct_time_container
end module discretization_op_divide
