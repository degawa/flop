!>物理量の出力に関係する派生型を提供する．
!>
!>派生型には，ベクトル量をCSV形式で出力する出力子型が含まれる．
!>
!>@note
!>メインルーチンの中では明確に型宣言されない．
!>演算に使われる他の型の成分として宣言，演算子に渡す値として生成，
!>演算の結果として中間的に生成される．
!>@endnote
!>
module grid_uniform_staggered_op_io_vars_vector_writer_csv
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_staggered_op_io_vars_vector_writer_adt
    use :: grid_uniform_staggered_vars_vector_2d
    implicit none
    private
    public :: output
    ! public :: write (formatted)

    !>ベクトル量をCSV形式で出力する出力子型．
    type, public, extends(vector_2d_writer_atype) :: vector_2d_csv_writer_type
    end type vector_2d_csv_writer_type

    ! !>ユーザ定義派生型IO設定用インタフェース．
    ! interface write (formatted)
    !    procedure :: write_vector_2d_as_csv
    ! end interface

    !>サブルーチン`output`を設定するインタフェース．
    interface output
        procedure :: output_vector_2d
    end interface
contains
    !>ベクトル量をCSV形式で出力する．
    !>`vector_2d_csv_writer`の派生型IO用のサブルーチン．
    subroutine write_vector_2d_as_csv(vector_2d_csv_writer, &
                                      unit, io_type, arg_list, &
                                      io_status, io_message)
        implicit none
        !&<
        class(vector_2d_csv_writer_type), intent(in)    :: vector_2d_csv_writer
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
        ! 型情報は`'DT"vector_2d_csv_writer_type"'`
        if (io_type /= "LISTDIRECTED" .and. io_type /= "DTvector_2d_csv_writer_type") then
            io_status = -1
            io_message = 'Error in user-defind type format specifier. &
                         &Confirm specifier style as DT"vector_2d_csv_writer_type"'
            return
        end if

        block
            use :: grid_uniform_staggered_2d
            integer(int32) :: Nx, Ny, i, j, ic, jc
            type(staggered_uniform_grid_2d_type), pointer :: grid

            grid => vector_2d_csv_writer%vec%get_base_grid()
            call grid%get_number_of_grid_points_to(Nx, Ny)

            !&<
            do j = 1, Ny
            do i = 1, Nx
                ic = i
                jc = j
                write (unit, '(4(E28.16E3:,","))', iostat = io_status, iomsg = io_message) &
                    grid%x(i), grid%y(j), &
                    (vector_2d_csv_writer%vec%x(i   , jc-1) + vector_2d_csv_writer%vec%x(i , jc))/2d0, &
                    (vector_2d_csv_writer%vec%y(ic-1, j   ) + vector_2d_csv_writer%vec%y(ic, j ))/2d0
                ! ユーザ定義派生型出力として用いる場合，書式指定子の末尾に改行`/`を付ける．
                ! ユーザ定義派生型出力では，改行が抑制されるためである．
                ! これは，write(*,*) "a", "b"を実行した際に
                ! "a"と"b"の間で改行されないことを想像してもらえれば判りやすい．
                ! また，同じ理由で，1行目にスペースが一つ入る．
                ! write (unit, '(E28.16E3,",",E28.16E3,",",E28.16E3,",",E28.16E3/)', &
                !        iostat = io_status, iomsg = io_message) &
                !     grid%x(i), grid%y(j), &
                !     (vector_2d_csv_writer%vec%x(i   , jc-1) + vector_2d_csv_writer%vec%x(i , jc))/2d0, &
                !     (vector_2d_csv_writer%vec%y(ic-1, j   ) + vector_2d_csv_writer%vec%y(ic, j ))/2d0
            end do
            end do
            !&>
        end block

        return
        if (size(arg_list) == 0) continue ! 変数未使用警告の抑制
    end subroutine write_vector_2d_as_csv

    !>ベクトル量出力子内のベクトル量をCSV形式で出力する．
    !>
    !>内部で`write_vector_2d_as_csv`を呼び出している．
    !>ユーザ定義派生型IOを導入するとIntel Fortranで
    !>コンパイルできなくなる対策として導入．
    subroutine output_vector_2d(vector_2d_csv_writer)
        implicit none
        type(vector_2d_csv_writer_type), intent(in) :: vector_2d_csv_writer
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

        ! ユーザ定義派生型IOとしてwrite_vector_2d_as_csvが
        ! 呼ばれた状況（書式指定は`*`）を想定して値を設定．
        io_type = "LISTDIRECTED"
        io_message = ""
        allocate (arg_list(0))
        unit = vector_2d_csv_writer%unit_number

        call write_vector_2d_as_csv(vector_2d_csv_writer, &
                                    unit, io_type, arg_list, &
                                    io_status, io_message)
    end subroutine output_vector_2d
end module grid_uniform_staggered_op_io_vars_vector_writer_csv
