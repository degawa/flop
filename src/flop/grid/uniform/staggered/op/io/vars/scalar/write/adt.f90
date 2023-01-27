!>物理量の出力に関係する抽象型を提供する．
!>
!>抽象型には，スカラ量を出力する出力子の抽象型が含まれる．
!>
module grid_uniform_stg_op_io_vars_scalar_writer_adt
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_stg_vars_scalar_2d
    implicit none
    private

    !>2次元のスカラ量を装置に出力する出力子の抽象型．
    !>
    !>@warning
    !>成分`unit_number`は一時的な実装であり，今後削除される場合がある．<br>
    !>ユーザ定義派生型IOを利用して
    !>`write(unit("p.txt"),*) p.as.csv`
    !>のように実行できるようにすると，
    !>ベクトル量のユーザ定義派生型IOを定義した際に
    !>Intel Fortran (Intel(R) 64 Compiler Classic, Version 2021.5.0 Build 20211109_000000)
    !>がcatastrophic errorを出力し，ビルドできない．
    !>対策として，`output`サブルーチンを設けて
    !>`call output(p.as.csv .to. "p.txt")`
    !>と実行するようにしており，その際に装置番号を受け取る必要がある．
    !>@endwarning
    type, public, abstract :: scalar_2d_writer_atype
        type(scalar_2d_type), public :: scr
            !! 出力されるスカラ量
        integer(int32), public :: unit_number
            !! スカラ量を出力する装置番号
    end type scalar_2d_writer_atype

end module grid_uniform_stg_op_io_vars_scalar_writer_adt
