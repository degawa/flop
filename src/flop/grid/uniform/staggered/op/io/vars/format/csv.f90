!>物理量出力時の書式整形に関係する変数や派生型を提供する．
!>
!>派生型には，CSV形式で整形する指標として用いる派生型が含まれる．
!>
!>変数には，および式内で参照するための擬似的な変数が含まれる．
!>
module grid_uniform_staggered_op_io_format_csv
    implicit none
    private

    !>CSV形式に整形する指標となる派生型．
    type, public :: csv_format_type
    end type csv_format_type

    type(csv_format_type), public :: csv
        !! CSV形式に整形する指標を式内で参照するための変数

end module grid_uniform_staggered_op_io_format_csv
