!>時間離散化に関する派生型を提供する．
!>
!>派生型には，離散化する時間軸と時間軸を分割する微小区間の幅を
!>まとめて取り扱うための派生型が含まれる．
!>
module discretization_op_vars_time_intoIntervals
    use :: time_vars_axis
    use :: discretization_vars_intervals
    implicit none
    private

    !>離散化する時間軸と軸方向の微小区間の幅を
    !>まとめて取り扱うための型．<br>
    !>名前は2項演算`time .into. intervals(dt)`に由来．
    type, public :: time_into_intervals_type
        type(time_axis_type) :: time
            !! 離散化する時間軸
        type(intervals_type) :: intervals
            !! 時間軸の微小区間の幅
    end type time_into_intervals_type
end module discretization_op_vars_time_intoIntervals
