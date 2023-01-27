!>物理量をファイルから読み込む際のファイル名指定に関係する演算子を提供する．
!>
module grid_uniform_stg_op_io_from
    use :: grid_uniform_stg_op_io_from_npy
    implicit none
    private
    public :: operator(.from.)

    !>ユーザ定義演算子`.from.`を定義するインタフェース
    interface operator(.from.)
        ! input from a file in npy format
        procedure :: set_unit_number_from_filename_to_scr2dnpyreader
    end interface
end module grid_uniform_stg_op_io_from
