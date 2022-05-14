!>時空間離散化に関する手続を提供する．
!>
!>手続には，空間座標系と各軸方向のセル数をまとめる手続，
!>および時間軸と時間方向の微小区間の幅をまとめる手続が含まれる．
!>
module discretization_op_into
    use :: space_vars_Cartesian
    use :: time_vars_axis
    use :: discretization_vars_cells
    use :: discretization_vars_intervals
    use :: discretization_op_vars_space_intoCells
    use :: discretization_op_vars_time_intoIntervals
    implicit none
    private
    public :: operator(.into.)

    !>ユーザ定義演算子`.into.`を定義するインタフェース
    interface operator(.into.)
        procedure :: construct_space_into_cells
        procedure :: construct_time_into_intervals
    end interface

contains
    !>空間情報とセル数をまとめた型を返す．
    function construct_space_into_cells(space, cells) result(new_space_into_cells)
        implicit none
        !&<
        type(Cartesian_2d_type) , intent(in) :: space
            !! 離散化する空間座標系
        type(cells_type)        , intent(in) :: cells
            !! 各軸方向のセル数
        !&>
        type(space_into_cells_type) :: new_space_into_cells
            !! 空間情報とセル数をまとめた型

        new_space_into_cells%space = space
        allocate (new_space_into_cells%cells%num_cells, source=cells%num_cells)
    end function construct_space_into_cells

    !>時間軸と微小区間の幅をまとめた型を返す．
    function construct_time_into_intervals(time, intervals) result(new_time_into_interval)
        implicit none
        !&<
        type(time_axis_type), intent(in) :: time
            !! 離散化する時間軸
        type(intervals_type), intent(in) :: intervals
            !! 時間軸の微小区間の幅
        !&>
        type(time_into_intervals_type) :: new_time_into_interval
            !! 時間軸と微小区間の幅をまとめた型

        new_time_into_interval%time = time
        allocate (new_time_into_interval%intervals%intervals, &
                  source=intervals%intervals)
    end function construct_time_into_intervals
end module discretization_op_into
