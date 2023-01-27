!>物理量をファイルから読み込む際のファイル名指定に関係する手続を提供する．
!>
!>手続には，ファイル名に紐付いた装置番号を取得する手続が含まれる．
!>
module grid_uniform_stg_op_io_from_npy
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_stg_op_io_vars_scalar_reader_npy
    implicit none
    private
    public :: set_unit_number_from_filename_to_scr2dnpyreader

contains
    !>ファイル名に紐付いた装置番号が設定された，スカラ量に対するNPY入力子を返す．
    function set_unit_number_from_filename_to_scr2dnpyreader(scr_reader, filename) result(new_scr_reader)
        use :: grid_uniform_stg_op_io_unit
        use :: grid_uniform_stg_op_io_vars_format_npy
        implicit none
        !&<
        type(scalar_2d_npy_reader_type) , intent(in) :: scr_reader
            !! スカラ量のNPY形式入力子
        character(*)                    , intent(in) :: filename
            !! 入力ファイル名
        !&>
        type(scalar_2d_npy_reader_type) :: new_scr_reader
            !! 装置番号が設定された，スカラ量のNPY形式入力子

        new_scr_reader%scr = scr_reader%scr
        new_scr_reader%unit_number = unit(filename//npy_extension)
    end function set_unit_number_from_filename_to_scr2dnpyreader
end module grid_uniform_stg_op_io_from_npy
