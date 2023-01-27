!>時空間離散化に関するオペレータを一括して`use`するためのモジュール
module discretization_operators
    use :: discretization_op_divide
    use :: discretization_op_into
    use :: discretization_op_vars_cells
    use :: discretization_op_vars_intervals
    use :: discretization_op_vars_space_intoCells
    use :: discretization_op_vars_time_intoIntervals
    public
end module discretization_operators
