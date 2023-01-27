!>物理量の入力に関係する派生型を提供する．
!>
!>派生型には，NPY形式で記述されたスカラ量を読み込むする入力子型が含まれる．
!>
!>@note
!>メインルーチンの中では明確に型宣言されない．
!>演算に使われる他の型の成分として宣言，演算子に渡す値として生成，
!>演算の結果として中間的に生成される．
!>@endnote
!>
module grid_uniform_stg_op_io_vars_scalar_reader_npy
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_stg_op_io_vars_scalar_reader_adt
    use :: grid_uniform_stg_vars_scalar_2d
    implicit none
    private
    public :: input
    public :: read_scalar_2d_in_npy_format

    !>NPY形式で記述されたスカラ量を読み込む入力子型．
    type, public, extends(scalar_2d_reader_atype) :: scalar_2d_npy_reader_type
    end type scalar_2d_npy_reader_type

    !>サブルーチン`input`を設定するインタフェース．
    interface input
        procedure :: input_scalar_2d_in_npy_format
    end interface

contains
    !>NPY形式でスカラ量を，指定のスカラ量型変数に読み込む．
    subroutine read_scalar_2d_in_npy_format(scalar_2d_npy_reader, &
                                            unit, &
                                            io_status, io_message, &
                                            scr)
        implicit none
        !&<
        class(scalar_2d_npy_reader_type), intent(in)    :: scalar_2d_npy_reader
            !! NPY reader
        integer(int32)                  , intent(in)    :: unit
            !! 入力装置番号
        integer(int32)                  , intent(out)   :: io_status
            !! 入力結果の状態（符号）
        character(*)                    , intent(inout) :: io_message
            !! 入力結果の状態（文字列）
        type(scalar_2d_type)            , intent(inout) :: scr
            !! ファイルから読み込まれたスカラ量
        !&>

        ! 装置が開かれていない場合は，非ゼロの状態符号と
        ! 対応するメッセージを返す．
        block
            logical :: is_unit_opened
            inquire (unit=unit, opened=is_unit_opened)
            if (.not. is_unit_opened) then
                io_status = -1
                io_message = 'input unit is not opened yet.'
                return
            end if
        end block

        block
            use :: grid_uniform_stg_2d
            integer(int32) :: Ncx, Ncy
            type(staggered_uniform_grid_2d_type), pointer :: grid
            character(256) :: filename
            real(real64), allocatable :: u(:, :)

            ! 装置番号からファイル名を取得
            inquire (unit, name=filename)

            ! load_npyの中で別途装置が開かれるので，
            ! 開かれている装置を閉じる
            close (unit)

            call input_scalar2d_npy(trim(filename), u)

            grid => scalar_2d_npy_reader%scr%get_base_grid()
            call grid%get_number_of_grid_center_to(Ncx, Ncy)

            scr = scalar_2d_npy_reader%scr
            scr%val(1:Ncx, 1:Ncy) = u(1:Ncx, 1:Ncy)
        end block
        io_status = 0
        io_message = ""
    contains
        subroutine input_scalar2d_npy(filename, u)
            use :: stdlib_io_npy
            implicit none
            character(*), intent(in) :: filename
            real(real64), allocatable, intent(inout) :: u(:, :)

            call load_npy(filename, u)
        end subroutine input_scalar2d_npy
    end subroutine read_scalar_2d_in_npy_format

    !>NPY形式で記述されたスカラ量を読み込み，
    !>`scalar_2d_type`を返す．
    function input_scalar_2d_in_npy_format(scalar_2d_npy_reader) result(new_scr)
        implicit none
        type(scalar_2d_npy_reader_type), intent(in) :: scalar_2d_npy_reader
            !! NPY reader

        type(scalar_2d_type) :: new_scr
            !! 入力されたスカラ量

        integer(int32) :: unit
        integer(int32) :: io_stat
        character(256) :: io_message

        unit = scalar_2d_npy_reader%unit_number

        call read_scalar_2d_in_npy_format(scalar_2d_npy_reader, unit, io_stat, io_message, &
                                          new_scr)
    end function input_scalar_2d_in_npy_format
end module grid_uniform_stg_op_io_vars_scalar_reader_npy
