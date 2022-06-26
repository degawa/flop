!>物理量出力時のファイル名指定に関係する手続を提供する．
!>
!>手続には，ファイル名に紐付いた装置番号を取得する手続が含まれる．
!>
module grid_uniform_staggered_op_io_to
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_staggered_op_io_vars_scalar_writer_csv
    use :: grid_uniform_staggered_op_io_vars_vector_writer_csv
    implicit none
    private
    public :: operator(.to.)
    public :: set_unit_number_to_vec2dcsvwriter

    !>ユーザ定義演算子`.to.`を定義するインタフェース
    interface operator(.to.)
        procedure :: set_unit_number_to_scr2dcsvwriter
        procedure :: set_unit_number_to_vec2dcsvwriter
        procedure :: set_unit_number_from_filename_to_scr2dcsvwriter
        procedure :: set_unit_number_from_filename_to_vec2dcsvwriter
    end interface

contains
    !>装置番号が設定された，スカラ量に対するCSV出力子を返す．
    function set_unit_number_to_scr2dcsvwriter(scr_writer, unit_number) result(new_scr_writer)
        implicit none
        !&<
        type(scalar_2d_csv_writer_type) , intent(in) :: scr_writer
            !! スカラ量のCSV形式出力子
        integer(int32)                  , intent(in) :: unit_number
            !! 出力装置番号
        !&>
        type(scalar_2d_csv_writer_type) :: new_scr_writer
            !! 装置番号が設定されたスカラ量のCSV形式出力子

        new_scr_writer%scr = scr_writer%scr
        new_scr_writer%unit_number = unit_number
    end function set_unit_number_to_scr2dcsvwriter

    !>装置番号が設定された，ベクトル量に対するCSV出力子を返す．
    function set_unit_number_to_vec2dcsvwriter(vec_writer, unit_number) result(new_vec_writer)
        implicit none
        !&<
        type(vector_2d_csv_writer_type) , intent(in) :: vec_writer
            !! ベクトル量のCSV形式出力子
        integer(int32)                  , intent(in) :: unit_number
            !! 出力装置番号
        !&>
        type(vector_2d_csv_writer_type) :: new_vec_writer
            !! 装置番号が設定されたベクトル量のCSV形式出力子

        new_vec_writer%vec = vec_writer%vec
        new_vec_writer%unit_number = unit_number
    end function set_unit_number_to_vec2dcsvwriter

    !>ファイル名に紐付いた装置番号が設定された，スカラ量に対するCSV出力子を返す．
    function set_unit_number_from_filename_to_scr2dcsvwriter(scr_writer, filename) result(new_scr_writer)
        use :: grid_uniform_staggered_op_io_unit
        implicit none
        !&<
        type(scalar_2d_csv_writer_type) , intent(in) :: scr_writer
            !! スカラ量のCSV形式出力子
        character(*)                    , intent(in) :: filename
            !! 出力ファイル名
        !&>
        type(scalar_2d_csv_writer_type) :: new_scr_writer
            !! 装置番号が設定されたスカラ量のCSV形式出力子

        new_scr_writer%scr = scr_writer%scr
        new_scr_writer%unit_number = unit(filename)
    end function set_unit_number_from_filename_to_scr2dcsvwriter

    !>ファイル名に紐付いた装置番号が設定された，ベクトル量に対するCSV出力子を返す．
    function set_unit_number_from_filename_to_vec2dcsvwriter(vec_writer, filename) result(new_vec_writer)
        use :: grid_uniform_staggered_op_io_unit
        implicit none
        !&<
        type(vector_2d_csv_writer_type) , intent(in) :: vec_writer
            !! ベクトル量のCSV形式出力子
        character(*)                    , intent(in) :: filename
            !! 出力ファイル名
        !&>
        type(vector_2d_csv_writer_type) :: new_vec_writer
            !! 装置番号が設定されたベクトル量のCSV形式出力子

        new_vec_writer%vec = vec_writer%vec
        new_vec_writer%unit_number = unit(filename)
    end function set_unit_number_from_filename_to_vec2dcsvwriter
end module grid_uniform_staggered_op_io_to
