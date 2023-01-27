!>物理量の出力に関係する派生型を提供する．
!>
!>派生型には，スカラ量をNPY形式で出力する出力子型が含まれる．
!>
!>@note
!>メインルーチンの中では明確に型宣言されない．
!>演算に使われる他の型の成分として宣言，演算子に渡す値として生成，
!>演算の結果として中間的に生成される．
!>@endnote
!>
module grid_uniform_stg_op_io_vars_scalar_writer_npy
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_stg_op_io_vars_scalar_writer_adt
    use :: grid_uniform_stg_vars_scalar_2d
    implicit none
    private
    public :: output

    !>スカラ量をNPY形式で出力する出力子型．
    type, public, extends(scalar_2d_writer_atype) :: scalar_2d_npy_writer_type
    end type scalar_2d_npy_writer_type

    !>サブルーチン`output`を設定するインタフェース．
    interface output
        procedure :: output_scalar_2d_as_npy
    end interface

contains
    !>スカラ量出力子内のスカラ量をNPY形式で出力する．
    subroutine write_scalar_2d_as_npy(scalar_2d_npy_writer, &
                                      unit, &
                                      io_status, io_message)
        implicit none
        !&<
        class(scalar_2d_npy_writer_type), intent(in)    :: scalar_2d_npy_writer
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
                io_message = 'unit to output is not opened yet.'
                return
            end if
        end block

        block
            use :: grid_uniform_stg_2d
            integer(int32) :: Ncx, Ncy
            type(staggered_uniform_grid_2d_type), pointer :: grid
            character(256) :: filename

            inquire (unit, name=filename)
            close (unit)

            grid => scalar_2d_npy_writer%scr%get_base_grid()
            call grid%get_number_of_grid_center_to(Ncx, Ncy)

            ! 1行呼び出すだけだが，ベクトル量と合わせるために関数を作成
            call output_scalar2d_npy(trim(filename), scalar_2d_npy_writer%scr%val(1:Ncx, 1:Ncy))
        end block
    contains
        subroutine output_scalar2d_npy(filename, u)
            use :: stdlib_io_npy
            implicit none
            !&<
            character(*)    , intent(in)    :: filename
                !! 出力ファイル名
            real(real64)    , intent(in)    :: u(:,:)
                !! 出力するスカラ量
            !&>

            call save_npy(filename, u)
        end subroutine output_scalar2d_npy
    end subroutine write_scalar_2d_as_npy

    !>スカラ量出力子内のスカラ量をNPY形式で出力する．
    !>
    !>内部で`write_scalar_2d_as_npy`を呼び出している．
    subroutine output_scalar_2d_as_npy(scalar_2d_npy_writer)
        implicit none
        type(scalar_2d_npy_writer_type), intent(in) :: scalar_2d_npy_writer
            !! NPY writer

        integer(int32) :: unit
            !! 出力先装置番号
        integer(int32) :: io_status
            !! 出力結果の状態（符号）
        character(256) :: io_message
            !! 出力結果の状態（文字列）

        unit = scalar_2d_npy_writer%unit_number

        call write_scalar_2d_as_npy(scalar_2d_npy_writer, &
                                    unit, &
                                    io_status, io_message)

        ! unit `unit` is closed in write_scalar_2d_as_npy
    end subroutine output_scalar_2d_as_npy
end module grid_uniform_stg_op_io_vars_scalar_writer_npy
