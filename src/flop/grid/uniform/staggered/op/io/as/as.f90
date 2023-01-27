!>物理量出力時の書式整形に関係する演算子を提供する．
!>
module grid_uniform_stg_op_io_as
    use :: grid_uniform_stg_op_io_as_csv
    use :: grid_uniform_stg_op_io_as_vtr
    implicit none
    private
    public :: operator(.as.)

    !>ユーザ定義演算子`.as.`を定義するインタフェース
    interface operator(.as.)
        ! variables as csv
        procedure :: as_scr2d_csvfmt
        procedure :: as_vec2d_csvfmt
        ! variables as vtr
        procedure :: as_scr2d_vtrfmt
        procedure :: as_vec2d_vtrfmt
    end interface
end module grid_uniform_stg_op_io_as
