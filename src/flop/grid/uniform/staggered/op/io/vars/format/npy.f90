!>物理量出力時の書式整形に関係する変数や派生型を提供する．
!>
!>派生型には，NPY形式で整形する指標として用いる派生型が含まれる．
!>
!>変数には，および式内で参照するための擬似的な変数が含まれる．
!>
!>@note
!>メインルーチンの中では明確に型宣言されない．
!>演算に使われる他の型の成分として宣言，演算子に渡す値として生成，
!>演算の結果として中間的に生成される．
!>@endnote
!>
module grid_uniform_stg_op_io_vars_format_npy
    implicit none
    private

    !>NPY形式に整形する指標となる派生型．
    type, public :: npy_format_type
    end type npy_format_type

    type(npy_format_type), public, parameter :: npy = npy_format_type()
        !! NPY形式に整形する指標を式内で参照するための変数

    character(*), public, parameter :: npy_extension = ".npy"
        !! NPY形式のファイル拡張子
end module grid_uniform_stg_op_io_vars_format_npy
