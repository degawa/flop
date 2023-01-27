!>物理量の出力に関係する派生型を提供する．
!>
!>派生型には，ベクトル量をNPY形式で出力する出力子型が含まれる．
!>
!>@note
!>メインルーチンの中では明確に型宣言されない．
!>演算に使われる他の型の成分として宣言，演算子に渡す値として生成，
!>演算の結果として中間的に生成される．
!>@endnote
!>
module grid_uniform_stg_op_io_vars_vector_writer_npy
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_stg_op_io_vars_vector_writer_adt
    use :: grid_uniform_stg_vars_vector_2d
    implicit none
    private
    public :: output

    !>ベクトル量をNPY形式で出力する出力子型．
    type, public, extends(vector_2d_writer_atype) :: vector_2d_npy_writer_type
    end type vector_2d_npy_writer_type

    !>サブルーチン`output`を設定するインタフェース．
    interface output
        procedure :: output_vector_2d_as_npy
    end interface
contains
    !>ベクトル量をNPY形式で出力する．
    !>`vector_2d_npy_writer`の派生型IO用のサブルーチン．
    subroutine write_vector_2d_as_npy(vector_2d_npy_writer, &
                                      unit, &
                                      io_status, io_message)
        implicit none
        !&<
        class(vector_2d_npy_writer_type), intent(in)    :: vector_2d_npy_writer
            !! NPY writer
        integer(int32)                  , intent(in)    :: unit
            !! 出力先装置番号
        integer(int32)                  , intent(out)   :: io_status
            !! 出力結果の状態（符号）
        character(*)                    , intent(inout) :: io_message
            !! 出力結果の状態（文字列）
        !&>

        ! 装置が開かれていない場合は，非ゼロの状態符号と
        ! 対応するメッセージを返す．
        ! ユーザ定義派生型IOとしてwrite文をオーバーロードする場合は不要だが，
        ! モジュール内の手続からサブルーチンとして呼ばれる場合を想定して実装．
        block
            logical :: is_unit_opened
            inquire (unit=unit, opened=is_unit_opened)
            if (.not. is_unit_opened) then
                io_status = -1
                io_message = 'output unit is not opened yet.'
                return
            end if
        end block

        block
            use :: grid_uniform_stg_2d
            use :: space_vars_Cartesian, only: &
                x_dir => x_dir_index, y_dir => y_dir_index
            integer(int32) :: Nx, Ny, i, j, ic, jc
            real(real64), allocatable :: u(:, :, :)
            type(staggered_uniform_grid_2d_type), pointer :: grid
            character(256) :: filename

            inquire (unit, name=filename)
            close (unit)

            grid => vector_2d_npy_writer%vec%get_base_grid()
            call grid%get_number_of_grid_points_to(Nx, Ny)

            allocate (u(1:Nx, 1:Ny, x_dir:y_dir))
            !&<
            do j = 1, Ny
            do i = 1, Nx
                jc = j
                u(i, j, x_dir) = (vector_2d_npy_writer%vec%x(i   , jc-1) + vector_2d_npy_writer%vec%x(i , jc))/2d0
            end do
            end do
            do j = 1, Ny
            do i = 1, Nx
                ic = i
                u(i, j, y_dir) = (vector_2d_npy_writer%vec%y(ic-1, j   ) + vector_2d_npy_writer%vec%y(ic, j ))/2d0
            end do
            end do
            !&>
            call output_vector2d_npy(trim(filename), u)

            deallocate (u)
        end block
    contains
        subroutine output_vector2d_npy(filename, u)
            use :: stdlib_io_npy
            implicit none
            !&<
            character(*)    , intent(in)    :: filename
                !! 出力ファイル名
            real(real64)    , intent(in)    :: u(:, :, :)
                !! 出力するベクトル量の\(x, y\)方向成分
            !&>

            call save_npy(filename, u)
        end subroutine output_vector2d_npy
    end subroutine write_vector_2d_as_npy

    !>ベクトル量出力子内のベクトル量をNPY形式で出力する．
    !>
    !>内部で`write_vector_2d_as_npy`を呼び出している．
    subroutine output_vector_2d_as_npy(vector_2d_npy_writer)
        implicit none
        type(vector_2d_npy_writer_type), intent(in) :: vector_2d_npy_writer
            !! NPY writer

        integer(int32) :: unit
            !! 出力先装置番号
        integer(int32) :: io_status
            !! 出力結果の状態（符号）
        character(256) :: io_message
            !! 出力結果の状態（文字列）

        unit = vector_2d_npy_writer%unit_number

        call write_vector_2d_as_npy(vector_2d_npy_writer, &
                                    unit, &
                                    io_status, io_message)

        ! unit `unit` is closed in write_vector_2d_as_npy
    end subroutine output_vector_2d_as_npy
end module grid_uniform_stg_op_io_vars_vector_writer_npy
