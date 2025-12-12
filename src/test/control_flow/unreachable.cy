use meta

fn while_inf() -> int:
    while:
        return 123

fn if_stmt() -> int:
    if true:
        return 123
    else:
        return 234

fn ct_if_stmt() -> int:
    #if true:
        return 123
    #else:
        return 234

fn switch_stmt() -> int:
    switch 1:
        case 1:
            return 123
        else:
            return 234

type Number enum:
    case one void
    case two void

fn ct_switch_stmt_choice() -> int:
    #switch Number.one:
        #case .one:
            return 123
        #case .two:
            return 234

fn ct_switch_stmt_enum() -> int:
    #switch Number.Tag.one:
        #case .one:
            return 123
        #case .two:
            return 234

fn switch_for_case(n &Number) -> int:
    switch meta.choice_tag(n.*):
        #for meta.enum_values(Number.Tag) |Tag|:
            case Tag:
                return 123

fn panic_stmt() -> int:
    panic('boom')

--cytest: pass