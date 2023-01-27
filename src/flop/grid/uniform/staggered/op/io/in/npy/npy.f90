!>物理量をファイルから入力する際の書式整形に関係する手続を提供する．
!>
!>手続には，NPY形式で記述されたスカラ量を読み込む入力子を
!>作成する手続が含まれる．
!>
module grid_uniform_stg_op_io_in_npy
    use :: grid_uniform_stg_vars_scalar_2d
    use :: grid_uniform_stg_op_io_vars_format
    use :: grid_uniform_stg_op_io_vars_scalar_reader_npy
    implicit none
    private
    public :: in_scr2d_npyfmt

contains
    !>NPY形式でファイルを読み込む入力子を返す．
    function in_scr2d_npyfmt(scr_mold, npy_fmt) result(new_scr_reader)
        implicit none
        !&<
        type(scalar_2d_type)    , intent(in) :: scr_mold
            !! 入力されるスカラ量のひな形
        type(npy_format_type)   , intent(in) :: npy_fmt
            !! NPY形式指標
        !&>
        type(scalar_2d_npy_reader_type) :: new_scr_reader
            !! スカラ量のNPY形式入力子

        new_scr_reader%scr = scr_mold

        return
        if (same_type_as(npy_fmt, npy_fmt)) continue ! 変数未使用警告の抑制
    end function in_scr2d_npyfmt
end module grid_uniform_stg_op_io_in_npy
