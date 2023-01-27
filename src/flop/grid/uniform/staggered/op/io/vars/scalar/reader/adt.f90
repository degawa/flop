!>物理量の入力に関係する抽象型を提供する．
!>
!>抽象型には，スカラ量を読み込む入力子の抽象型が含まれる．
!>
module grid_uniform_stg_op_io_vars_scalar_reader_adt
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_stg_vars_scalar_2d
    implicit none
    private

    !>2次元のスカラ量を装置から読み込む入力子の抽象型．
    !>
    !>@warning
    !>成分`unit_number`は一時的な実装であり，今後削除される場合がある．
    !>@endwarning
    type, public, abstract :: scalar_2d_reader_atype
        type(scalar_2d_type), public :: scr
            !! 読み込まれたスカラ量
        integer(int32), public :: unit_number
            !! 読み込むスカラ量の装置番号
    end type scalar_2d_reader_atype
end module grid_uniform_stg_op_io_vars_scalar_reader_adt
