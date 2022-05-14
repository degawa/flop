!>時空間離散化に関する手続を提供する．
!>
!>手続には，空間情報を用いて格子に離散化する手続や，
!>時間情報を用いて時間離散化を行う手続が含まれる．
!>
module discretization_op_into
    use, intrinsic :: iso_fortran_env
    use :: discretization_op_divide
    use :: grid_uniform_staggered_2d
    use :: discreteTime_vars_discreteTime
    implicit none
    private
    public :: operator(.into.)

    !>ユーザ定義演算子`.into.`を定義するインタフェース
    interface operator(.into.)
        procedure :: construct_grid
        procedure :: construct_discrete_time
    end interface

contains
    !>空間情報を用いて空間を離散化し，格子を返す．
    function construct_grid(space_contaier, num_cells) result(new_grid)
        implicit none
        !&<
        type(space_container_type)  , intent(in) :: space_contaier
            !! 離散化する空間情報
        integer(int32)              , intent(in) :: num_cells(:)
            !! 各軸方向のセル数
        !&>
        type(staggered_uniform_grid_2d_type) :: new_grid
            !! 構築されるスタガード格子

        integer(int32), allocatable :: num_points(:)
        num_points = num_cells + 1

        call new_grid%construct(space_contaier%space, num_points)
    end function construct_grid

    !>時間情報を用いて時間を離散化し，離散化された時間情報を返す．
    function construct_discrete_time(time_container, interval) result(new_delta)
        implicit none
        !&<
        type(time_container_type)   , intent(in) :: time_container
            !! 離散化する時間情報
        real(real64)                , intent(in) :: interval
            !! 計算時間間隔
        !&>
        type(discrete_time_type) :: new_delta
            !! 離散化された時間

        call new_delta%construct(time_container%time, interval)
    end function construct_discrete_time
end module discretization_op_into
