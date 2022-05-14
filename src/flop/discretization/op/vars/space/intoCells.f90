!>空間離散化に関する派生型を提供する．
!>
!>派生型には，離散化する空間と各方向のセル数を
!>まとめて取り扱うための派生型が含まれる．
!>
module discretization_op_vars_space_intoCells
    use :: space_vars_Cartesian
    use :: discretization_vars_cells
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
