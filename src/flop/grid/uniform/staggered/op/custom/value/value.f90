!>2次元等間隔のスタガード格子に関係する手続を提供する．
!>
!>手続には，スタガード格子を表す派生型から成分を取得する手続が含まれる．
!>
module grid_uniform_stg_op_cust_value
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_stg_2d
    use :: grid_uniform_stg_op_cust_value_vars_components
    implicit none
    public :: operator(.value.)
    public :: of_minimum_interval

    !>ユーザ定義演算子`.value.`を定義するインタフェース
    interface operator(.value.)
        procedure :: get_minimum_interval
    end interface

contains
    !>格子幅の最小値を返す．
    function get_minimum_interval(grid, minimum_interval) result(minimum_interval_)
        implicit none
        !&<
        type(staggered_uniform_grid_2d_type), pointer   , intent(in) :: grid
            !! 格子
        type(minimum_grid_interval_type)                , intent(in) :: minimum_interval
            !! 呼び出す手続を決定するためのダミー変数
        !&>
        real(real64) :: minimum_interval_
            !! 格子幅の最小値

        minimum_interval_ = minval(grid%get_interval())

        return
        if (same_type_as(minimum_interval, minimum_interval)) continue ! 変数未使用警告の抑制
    end function get_minimum_interval
end module grid_uniform_stg_op_cust_value
