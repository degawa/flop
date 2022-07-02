!>物理量の出力に関係する派生型を提供する．
!>
!>派生型には，スカラ量をCSV形式で出力する出力子型が含まれる．
!>
!>@note
!>メインルーチンの中では明確に型宣言されない．
!>演算に使われる他の型の成分として宣言，演算子に渡す値として生成，
!>演算の結果として中間的に生成される．
!>@endnote
!>
module grid_uniform_stg_op_io_vars_scalar_writer_csv
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_stg_op_io_vars_scalar_writer_adt
    use :: grid_uniform_stg_vars_scalar_2d
    implicit none
    private
    public :: output
    ! public :: write (formatted)

    !>スカラ量をCSV形式で出力する出力子型．
    type, public, extends(scalar_2d_writer_atype) :: scalar_2d_csv_writer_type
    end type scalar_2d_csv_writer_type

    ! !>ユーザ定義派生型IO設定用インタフェース．
    ! interface write (formatted)
    !    procedure :: write_scalar_2d_as_csv
    ! end interface

    !>サブルーチン`output`を設定するインタフェース．
    interface output
        procedure :: output_scalar_2d
    end interface

contains
    !>スカラ量出力子内のスカラ量をCSV形式で出力する．
    !>`scalar_2d_csv_writer`の派生型IO用のサブルーチン．
    subroutine write_scalar_2d_as_csv(scalar_2d_csv_writer, &
                                      unit, io_type, arg_list, &
                                      io_status, io_message)
        implicit none
        !&<
        class(scalar_2d_csv_writer_type), intent(in)    :: scalar_2d_csv_writer
            !! CSV writer
        integer(int32)                  , intent(in)    :: unit
            !! 出力先装置番号
        character(*)                    , intent(in)    :: io_type
            !! 書式指定子の型情報
        integer(int32)                  , intent(in)    :: arg_list(:)
            !! 書式指定子の桁情報など
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

        ! 書式指定の型情報が誤っている場合は，非ゼロの状態符号と
        ! 対応するメッセージを返す
        ! 型情報は`'DT"scalar_2d_csv_writer_type"'`
        if (io_type /= "LISTDIRECTED" .and. io_type /= "DTscalar_2d_csv_writer_type") then
            io_status = -1
            io_message = 'Error in user-defind type format specifier. &
                         &Confirm specifier style as DT"scalar_2d_csv_writer_type"'
            return
        end if

        block
            use :: grid_uniform_stg_2d
            integer(int32) :: Ncx, Ncy, ic, jc
            type(staggered_uniform_grid_2d_type), pointer :: grid

            grid => scalar_2d_csv_writer%scr%get_base_grid()
            call grid%get_number_of_grid_center_to(Ncx, Ncy)

            !&<
            do jc = 1, Ncy
            do ic = 1, Ncx
                write (unit, '(3(E28.16E3:,","))', iostat = io_status, iomsg = io_message) &
                    grid%xc(ic), grid%yc(jc), &
                    scalar_2d_csv_writer%scr%val(ic, jc)
                ! ユーザ定義派生型出力として用いる場合，書式指定子の末尾に改行`/`を付ける．
                ! ユーザ定義派生型出力では，改行が抑制されるためである．
                ! これは，write(*,*) "a", "b"を実行した際に
                ! "a"と"b"の間で改行されないことを想像してもらえれば判りやすい．
                ! また，同じ理由で，1行目にスペースが一つ入る．
                ! write (unit, '(E28.16E3,",",E28.16E3,",",E28.16E3/)', &
                !        iostat = io_status, iomsg = io_message) &
                !     grid%xc(ic), grid%yc(jc), &
                !     scalar_2d_csv_writer%scr%val(ic, jc)
            end do
            end do
            !&>
        end block

        return
        if (size(arg_list) == 0) continue ! 変数未使用警告の抑制
    end subroutine write_scalar_2d_as_csv

    !>スカラ量出力子内のスカラ量をCSV形式で出力する．
    !>
    !>内部で`write_scalar_2d_as_csv`を呼び出している．
    !>ユーザ定義派生型IOを導入するとIntel Fortranで
    !>コンパイルできなくなる対策として導入．
    subroutine output_scalar_2d(scalar_2d_csv_writer)
        implicit none
        type(scalar_2d_csv_writer_type), intent(in) :: scalar_2d_csv_writer
            !! CSV writer

        integer(int32) :: unit
            !! 出力先装置番号
        character(:), allocatable :: io_type
            !! 書式指定子の型情報
        integer(int32), allocatable :: arg_list(:)
            !! 書式指定子の桁情報など
        integer(int32) :: io_status
            !! 出力結果の状態（符号）
        character(256) :: io_message
            !! 出力結果の状態（文字列）

        ! ユーザ定義派生型IOとしてwrite_scalar_2d_as_csvが
        ! 呼ばれた状況（書式指定は`*`）を想定して値を設定．
        io_type = "LISTDIRECTED"
        io_message = ""
        allocate (arg_list(0))
        unit = scalar_2d_csv_writer%unit_number

        call write_scalar_2d_as_csv(scalar_2d_csv_writer, &
                                    unit, io_type, arg_list, &
                                    io_status, io_message)

        block
            logical :: opened
            inquire (unit, opened=opened)
            if (opened) close (unit)
        end block
    end subroutine output_scalar_2d
end module grid_uniform_stg_op_io_vars_scalar_writer_csv
