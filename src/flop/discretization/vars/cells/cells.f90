!>空間離散化に関する派生型や手続を提供する．
!>
!>派生型には，セル数をまとめた派生型が含まれる．
!>
!>手続には，セル数をまとめた派生型のコンストラクタが含まれる．
!>
module discretization_vars_cells
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: Cells

    !>セル数をまとめた型．
    type, public :: cells_type
        integer(int32), allocatable :: num_cells(:)
            !! セル数
    end type cells_type

    !>セル数をまとめた型のコンストラクタを`Cells`と呼べるようにするための
    !>インタフェース
    interface Cells
        procedure :: construct_cells
    end interface

contains
    !>各軸のセル数を受け取り，セル数をまとめた型を返す．
    function construct_cells(num_cells) result(new_cells)
        implicit none
        integer(int32), intent(in) :: num_cells(:)
            !! 各軸方向のセル数

        type(cells_type) :: new_cells
            !! 各軸方向のセル数をまとめた型

        new_cells%num_cells = num_cells
    end function construct_cells
end module discretization_vars_cells
