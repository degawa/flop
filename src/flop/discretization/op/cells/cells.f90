!>空間離散化に関する手続を提供する．
!>
!>手続には，離散化する空間にセル数の情報を渡すための手続が含まれる．
!>
module discretization_op_cells
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: cells

contains
    !>各軸のセル数を受け取り，セル数をまとめた配列を返す．
    function cells(num_cells) result(new_cells)
        implicit none
        integer(int32), intent(in) :: num_cells(:)
            !! 各軸方向のセル数

        integer(int32), allocatable :: new_cells(:)
            !! 各軸方向のセル数

        new_cells = num_cells
    end function cells
end module discretization_op_cells
