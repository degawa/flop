!>2次元等間隔のスタガード格子に関係する派生型を提供する．
!>
!>派生型には，スタガード格子を表す派生型から成分を取得する際に
!>演算子に所望の成分を認識させる仮の変数が含まれる．
!>
module grid_uniform_stg_op_cust_value_vars_components
    implicit none
    private

    !>格子幅の最小値を指定するための派生型．
    type, public :: minimum_grid_interval_type
    end type minimum_grid_interval_type

    type(minimum_grid_interval_type), public, parameter :: &
        of_minimum_interval = minimum_grid_interval_type()
        !! 演算子に格子幅の最小値を認識させるための定数
end module grid_uniform_stg_op_cust_value_vars_components
