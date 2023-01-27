!>物理量出力時のファイル名指定に関係する手続を提供する．
!>
!>手続には，ファイル名に紐付いた装置番号を取得する手続が含まれる．
!>
module grid_uniform_stg_op_io_to_npy
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_stg_op_io_vars_scalar_writer_npy
    use :: grid_uniform_stg_op_io_vars_vector_writer_npy
    implicit none
    private
    public :: set_unit_number_to_scr2dnpywriter
    public :: set_unit_number_to_vec2dnpywriter
    public :: set_unit_number_from_filename_to_scr2dnpywriter
    public :: set_unit_number_from_filename_to_vec2dnpywriter

contains
    !>装置番号が設定された，スカラ量に対するNPY出力子を返す．
    function set_unit_number_to_scr2dnpywriter(scr_writer, unit_number) result(new_scr_writer)
        implicit none
        !&<
        type(scalar_2d_npy_writer_type) , intent(in) :: scr_writer
            !! スカラ量のNPY形式出力子
        integer(int32)                  , intent(in) :: unit_number
            !! 出力装置番号
        !&>
        type(scalar_2d_npy_writer_type) :: new_scr_writer
            !! 装置番号が設定されたスカラ量のNPY形式出力子

        new_scr_writer%scr = scr_writer%scr
        new_scr_writer%unit_number = unit_number
    end function set_unit_number_to_scr2dnpywriter

    !>装置番号が設定された，ベクトル量に対するNPY出力子を返す．
    function set_unit_number_to_vec2dnpywriter(vec_writer, unit_number) result(new_vec_writer)
        implicit none
        !&<
        type(vector_2d_npy_writer_type) , intent(in) :: vec_writer
            !! ベクトル量のNPY形式出力子
        integer(int32)                  , intent(in) :: unit_number
            !! 出力装置番号
        !&>
        type(vector_2d_npy_writer_type) :: new_vec_writer
            !! 装置番号が設定されたベクトル量のNPY形式出力子

        new_vec_writer%vec = vec_writer%vec
        new_vec_writer%unit_number = unit_number
    end function set_unit_number_to_vec2dnpywriter

    !>ファイル名に紐付いた装置番号が設定された，スカラ量に対するNPY出力子を返す．
    function set_unit_number_from_filename_to_scr2dnpywriter(scr_writer, filename) result(new_scr_writer)
        use :: grid_uniform_stg_op_io_unit
        use :: grid_uniform_stg_op_io_vars_format_npy
        implicit none
        !&<
        type(scalar_2d_npy_writer_type) , intent(in) :: scr_writer
            !! スカラ量のNPY形式出力子
        character(*)                    , intent(in) :: filename
            !! 出力ファイル名
        !&>
        type(scalar_2d_npy_writer_type) :: new_scr_writer
            !! 装置番号が設定されたスカラ量のNPY形式出力子

        new_scr_writer%scr = scr_writer%scr
        new_scr_writer%unit_number = unit(filename//npy_extension)
    end function set_unit_number_from_filename_to_scr2dnpywriter

    !>ファイル名に紐付いた装置番号が設定された，ベクトル量に対するNPY出力子を返す．
    function set_unit_number_from_filename_to_vec2dnpywriter(vec_writer, filename) result(new_vec_writer)
        use :: grid_uniform_stg_op_io_unit
        use :: grid_uniform_stg_op_io_vars_format_npy
        implicit none
        !&<
        type(vector_2d_npy_writer_type) , intent(in) :: vec_writer
            !! ベクトル量のNPY形式出力子
        character(*)                    , intent(in) :: filename
            !! 出力ファイル名
        !&>
        type(vector_2d_npy_writer_type) :: new_vec_writer
            !! 装置番号が設定されたベクトル量のNPY形式出力子

        new_vec_writer%vec = vec_writer%vec
        new_vec_writer%unit_number = unit(filename//npy_extension)
    end function set_unit_number_from_filename_to_vec2dnpywriter
end module grid_uniform_stg_op_io_to_npy
