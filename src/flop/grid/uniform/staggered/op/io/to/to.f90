!>物理量出力時のファイル名指定に関係する演算子を提供する．
!>
module grid_uniform_stg_op_io_to
    use :: grid_uniform_stg_op_io_to_csv
    use :: grid_uniform_stg_op_io_to_vtr
    use :: grid_uniform_stg_op_io_to_npy
    implicit none
    private
    public :: operator(.to.)

    !>ユーザ定義演算子`.to.`を定義するインタフェース
    interface operator(.to.)
        ! output csv to a file
        procedure :: set_unit_number_to_scr2dcsvwriter
        procedure :: set_unit_number_to_vec2dcsvwriter
        procedure :: set_unit_number_from_filename_to_scr2dcsvwriter
        procedure :: set_unit_number_from_filename_to_vec2dcsvwriter
        ! output vtr to a file
        procedure :: set_unit_number_to_scr2dvtrwriter
        procedure :: set_unit_number_to_vec2dvtrwriter
        procedure :: set_unit_number_from_filename_to_scr2dvtrwriter
        procedure :: set_unit_number_from_filename_to_vec2dvtrwriter
        ! output npy to a file
        procedure :: set_unit_number_to_scr2dnpywriter
        procedure :: set_unit_number_to_vec2dnpywriter
        procedure :: set_unit_number_from_filename_to_scr2dnpywriter
        procedure :: set_unit_number_from_filename_to_vec2dnpywriter
    end interface
end module grid_uniform_stg_op_io_to
