!>時空間離散化に関する手続を提供する．
!>
!>手続には，空間情報を用いて格子に離散化する手続や，
!>時間情報を用いて時間離散化を行う手続が含まれる．
!>
module discretization_op_divide
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_staggered_2d
    use :: discreteTime_vars_discreteTime
    use :: discretization_op_vars_space_intoCells
    use :: discretization_op_vars_time_intoIntervals
    implicit none
    private
    public :: operator(.divide.)

    !>ユーザ定義演算子`.divide.`を定義するインタフェース
    interface operator(.divide.)
        procedure :: divide_space_into_cells
        procedure :: divide_time_into_intervals
    end interface

contains
    !>空間情報を用いて空間を離散化し，格子を返す．
    function divide_space_into_cells(space_into_cells) result(new_grid)
        implicit none
        !&<
        type(space_into_cells_type), intent(in) :: space_into_cells
            !! 離散化する空間情報
        !&>
        type(staggered_uniform_grid_2d_type) :: new_grid
            !! 構築されるスタガード格子

        integer(int32), allocatable :: num_points(:)
        num_points = space_into_cells%cells%num_cells + 1

        call new_grid%construct(space_into_cells%space, num_points)
    end function divide_space_into_cells

    !>時間情報を用いて時間を離散化し，離散化された時間情報を返す．
    function divide_time_into_intervals(time_into_interval) result(new_delta)
        implicit none
        type(time_into_intervals_type), intent(in) :: time_into_interval
            !! 離散化する時間情報
        type(discrete_time_type) :: new_delta
            !! 離散化された時間

        call new_delta%construct_by_time_interval(time_into_interval%time, &
                                                  time_into_interval%intervals%intervals(1))
    end function divide_time_into_intervals
end module discretization_op_divide
