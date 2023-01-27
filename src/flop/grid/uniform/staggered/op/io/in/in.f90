!>物理量をファイルから入力する際の書式整形に関係する演算子を提供する．
!>
module grid_uniform_stg_op_io_in
    use :: grid_uniform_stg_op_io_in_npy
    implicit none
    private
    public :: operator(.in.)

    !>ユーザ定義演算子`.in.`を定義するインタフェース
    interface operator(.in.)
        ! read file in npy format
        procedure :: in_scr2d_npyfmt
    end interface
end module grid_uniform_stg_op_io_in
