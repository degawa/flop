!>物理量出力時の書式整形に関係する手続を提供する．
!>
!>手続には，物理量と書式から物理量をCSV形式で出力する出力子を
!>作成する手続が含まれる．
!>
module grid_uniform_stg_op_io_as_csv
    use :: grid_uniform_stg_vars_scalar_2d
    use :: grid_uniform_stg_vars_vector_2d
    use :: grid_uniform_stg_op_io_vars_format
    use :: grid_uniform_stg_op_io_vars_scalar_writer_csv
    use :: grid_uniform_stg_op_io_vars_vector_writer_csv
    implicit none
    private
    public :: as_scr2d_csvfmt
    public :: as_vec2d_csvfmt

contains
    !>スカラ量をCSV形式に整形する出力子を返す．
    function as_scr2d_csvfmt(scr, csv_fmt) result(new_scr_writer)
        implicit none
        !&<
        type(scalar_2d_type)    , intent(in) :: scr
            !! 出力されるスカラ量
        type(csv_format_type)   , intent(in) :: csv_fmt
            !! CSV形式指標
        !&>
        type(scalar_2d_csv_writer_type) :: new_scr_writer
            !! スカラ量のCSV形式出力子

        new_scr_writer%scr = scr

        return
        if (same_type_as(csv_fmt, csv_fmt)) continue ! 変数未使用警告の抑制
    end function as_scr2d_csvfmt

    !>ベクトル量をCSV形式に整形する出力子を返す．
    function as_vec2d_csvfmt(vec, csv_fmt) result(new_vec_writer)
        implicit none
        !&<
        type(vector_2d_type)    , intent(in) :: vec
            !! 出力されるベクトル量
        type(csv_format_type)   , intent(in) :: csv_fmt
            !! CSV形式指標
        !&>
        type(vector_2d_csv_writer_type) :: new_vec_writer
            !! ベクトル量のCSV形式出力子

        new_vec_writer%vec = vec

        return
        if (same_type_as(csv_fmt, csv_fmt)) continue ! 変数未使用警告の抑制
    end function as_vec2d_csvfmt
end module grid_uniform_stg_op_io_as_csv
