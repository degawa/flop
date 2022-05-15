!>空間離散化に関する派生型を提供する．
!>
!>派生型には，離散化する空間と各方向のセル数を
!>まとめて取り扱うための派生型が含まれる．
!>
!>@note
!>メインルーチンの中では明確に型宣言されない．
!>演算に使われる他の型の成分として宣言，演算子に渡す値として生成，
!>演算の結果として中間的に生成される．
!>@endnote
!>
!>@note
!>モジュールおよび派生型の名前は，演算子指向で書いた空間離散化の処理
!>
!>```Fortran
!>x = x .set. [0d0, l]
!>y = y .set. [0d0, l]
!>space = space .set. Cartesian([x, y])
!>grid = .divide. (space.into.cells([40, 40]))
!>```
!>
!>に現れる2項演算`space.into.cells([40, 40])`に由来．
!>@endnote
!>
module discretization_op_vars_space_intoCells
    use :: space_vars_Cartesian
    use :: discretization_op_vars_cells
    implicit none
    private

    !>離散化するデカルト座標系と各軸方向のセル数を
    !>まとめて取り扱うための型．<br>
    !>名前は2項演算`space .into. cells([n,n])`に由来．
    type, public :: space_into_cells_type
        type(Cartesian_2d_type) :: space
            !! 離散化する空間座標系
        type(cells_type) :: cells
            !! 各軸方向のセル数
    end type space_into_cells_type
end module discretization_op_vars_space_intoCells
