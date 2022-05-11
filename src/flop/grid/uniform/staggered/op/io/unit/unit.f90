!>物理量出力時のファイル名指定に関係する手続を提供する．
!>
!>手続には，物理量の出力子に装置番号を渡す手続が含まれる．
!>
module grid_uniform_staggered_op_io_unit
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: unit

    !>総称名`unit`を設定するためのインタフェース
    interface unit
        procedure :: filename_to_unit
    end interface

contains
    !>ファイル名に紐付いた装置番号を返す．
    function filename_to_unit(filename) result(unit_number)
        implicit none
        character(*), intent(in) :: filename
            !! ファイル名

        integer(int32) :: unit_number
            !! ファイル名に紐付いた装置番号

        logical :: is_unit_opened

        ! まずはファイル名で装置が開いているかを確認
        ! 開かれていたらunit_numberが取得されているのでreturnする
        inquire (file=filename, opened=is_unit_opened, number=unit_number)
        if (is_unit_opened) return

        ! 開かれてなかったら開く
        open (newunit=unit_number, file=filename)
    end function filename_to_unit
end module grid_uniform_staggered_op_io_unit
