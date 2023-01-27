!>離散化された時間に関係する派生型を提供する．
!>
!>派生型には，離散化された時間を表す派生型から成分を取得する際に
!>演算子に所望の成分を認識させる仮の変数が含まれる．
!>
module discreteTime_op_value_vars_components
    implicit none
    private

    !>計算時間間隔を指定するための派生型．
    type, public :: time_interval_type
    end type time_interval_type

    type(time_interval_type), public, parameter :: &
        of_time_interval = time_interval_type()
        !! 演算子に計算時間間隔を認識させるための定数
end module discreteTime_op_value_vars_components
