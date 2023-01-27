!>離散化された時間に関係する手続を提供する．
!>
!>手続には，離散化された時間を表す派生型から成分を取得する手続が含まれる．
!>
module discreteTime_op_value
    use, intrinsic :: iso_fortran_env
    use :: discreteTime_vars_discreteTime
    use :: discreteTime_op_value_vars_components
    implicit none
    private
    public :: operator(.value.)
    public :: of_time_interval

    !>ユーザ定義演算子`.value.`を定義するインタフェース
    interface operator(.value.)
        procedure :: get_time_interval
    end interface
contains
    !>計算時間間隔を返す．
    function get_time_interval(discrete_time, time_interval) result(time_interval_)
        implicit none
        !&<
        type(discrete_time_type), intent(in) :: discrete_time
            !! 離散化された時間
        type(time_interval_type), intent(in) :: time_interval
            !! 計算時間間隔を取得できるように，
            !! 呼び出す手続を決定するためのダミー変数
        !&>
        real(real64) :: time_interval_
            !! 計算時間間隔

        time_interval_ = discrete_time%get_time_interval()

        return
        if (same_type_as(time_interval, time_interval)) continue !! 未使用警告の抑制
    end function get_time_interval
end module discreteTime_op_value
