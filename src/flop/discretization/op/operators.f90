!>時空間離散化に関するオペレータを一括して`use`するためのモジュール
module discretization_op
    use :: discretization_op_divide
    use :: discretization_op_into
    use :: discretization_op_vars_space_intoCells
    use :: discretization_op_vars_time_intoIntervals
    public
end module discretization_op
