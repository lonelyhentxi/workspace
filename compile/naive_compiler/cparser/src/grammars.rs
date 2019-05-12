use clr1::Grammar;

pub fn gen_cgrammars() -> Grammar {
    let mut cases: Vec<(&str, Vec<&str>)> = vec![];
    macro_rules! add_case_group {
        ($head:expr$(,$tail:expr)*) => {
            $(
            cases.push(($head,$tail));
            )*
        }
    }
    add_case_group!(
        "primary_expression",
        vec!["IDENTIFIER"],
        vec!["CONSTANT"],
        vec!["STRING_LITERAL"],
        vec!["(", "expression", ")"]
    );
    add_case_group!(
        "postfix_expression",
        vec!["postfix_expression", "[", "expression", "]"],
        vec!["postfix_expression", "(", ")"],
        vec!["postfix_expression", "(", "argument_expression_list", ")"],
        vec!["postfix_expression", ".", "IDENTIFIER"],
        vec!["postfix_expression", "PTR_OP", "IDENTIFIER"],
        vec!["postfix_expression", "INC_OP"],
        vec!["postfix_expression", "DEC_OP"]
    );

    add_case_group!(
        "argument_expression_list",
        vec!["assignment_expression"],
        vec!["argument_expression_list", ",", "assignment_expression"]
    );
    add_case_group!(
        "unary_expression",
        vec!["postfix_expression"],
        vec!["INC_OP", "unary_expression"],
        vec!["DEC_OP", "unary_expression"],
        vec!["unary_operator", "cast_expression"],
        vec!["SIZEOF", "unary_expression"],
        vec!["SIZEOF", "(", "type_name", ")"]
    );
    add_case_group!(
        "unary_operator",
        vec!["&"],
        vec!["*"],
        vec!["+"],
        vec!["-"],
        vec!["~"],
        vec!["!"]
    );
    add_case_group!(
        "cast_expression",
        vec!["unary_expression"],
        vec!["(", "type_name", ")", "cast_expression"]
    );
    add_case_group!(
        "multiplicative_expression",
        vec!["cast_expression"],
        vec!["multiplicative_expression", "*", "cast_expression"],
        vec!["multiplicative_expression", "/", "cast_expression"],
        vec!["multiplicative_expression", "%", "cast_expression"]
    );
    add_case_group!(
        "additive_expression",
        vec!["multiplicative_expression"],
        vec!["additive_expression", "+", "multiplicative_expression"],
        vec!["additive_expression", "-", "multiplicative_expression"]
    );
    add_case_group!(
        "shift_expression",
        vec!["additive_expression"],
        vec!["shift_expression", "LEFT_OP", "additive_expression"],
        vec!["shift_expression", "RIGHT_OP", "additive_expression"]
    );
    add_case_group!(
        "relational_expression",
        vec!["shift_expression"],
        vec!["relational_expression", "<", "shift_expression"],
        vec!["relational_expression", ">", "shift_expression"],
        vec!["relational_expression", "LE_OP", "shift_expression"],
        vec!["relational_expression", "GE_OP", "shift_expression"]
    );
    add_case_group!(
        "equality_expression",
        vec!["relational_expression"],
        vec!["equality_expression", "EQ_OP", "relational_expression"],
        vec!["equality_expression", "NE_OP", "relational_expression"]
    );
    add_case_group!(
        "and_expression",
        vec!["equality_expression"],
        vec!["and_expression", "&", "equality_expression"]
    );
    add_case_group!(
        "exclusive_or_expression",
        vec!["and_expression"],
        vec!["exclusive_or_expression", "^", "and_expression"]
    );
    add_case_group!(
        "inclusive_or_expression",
        vec!["exclusive_or_expression"],
        vec!["inclusive_or_expression", "|", "exclusive_or_expression"]
    );
    add_case_group!(
        "logical_and_expression",
        vec!["inclusive_or_expression"],
        vec![
            "logical_and_expression",
            "AND_OP",
            "inclusive_or_expression"
        ]
    );
    add_case_group!(
        "logical_or_expression",
        vec!["logical_and_expression"],
        vec!["logical_or_expression", "OR_OP", "logical_and_expression"]
    );
    add_case_group!(
        "conditional_expression",
        vec!["logical_or_expression"],
        vec![
            "logical_or_expression",
            "?",
            "expression",
            ":",
            "conditional_expression"
        ]
    );
    add_case_group!(
        "assignment_expression",
        vec!["conditional_expression"],
        vec![
            "unary_expression",
            "assignment_operator",
            "assignment_expression"
        ]
    );
    add_case_group!(
        "assignment_operator",
        vec!["="],
        vec!["MUL_ASSIGN"],
        vec!["DIV_ASSIGN"],
        vec!["MOD_ASSIGN"],
        vec!["ADD_ASSIGN"],
        vec!["SUB_ASSIGN"],
        vec!["LEFT_ASSIGN"],
        vec!["RIGHT_ASSIGN"],
        vec!["AND_ASSIGN"],
        vec!["XOR_ASSIGN"],
        vec!["OR_ASSIGN"]
    );
    add_case_group!(
        "expression",
        vec!["assignment_expression"],
        vec!["expression", ",", "assignment_expression"]
    );
    add_case_group!("constant_expression", vec!["conditional_expression"]);
    add_case_group!(
        "declaration",
        vec!["declaration_specifiers", ";"],
        vec!["declaration_specifiers", "init_declarator_list", ";"]
    );
    add_case_group!(
        "declaration_specifiers",
        vec!["storage_class_specifier"],
        vec!["storage_class_specifier", "declaration_specifiers"],
        vec!["type_specifier"],
        vec!["type_specifier", "declaration_specifiers"],
        vec!["type_qualifier"],
        vec!["type_qualifier", "declaration_specifiers"]
    );
    add_case_group!(
        "init_declarator_list",
        vec!["init_declarator"],
        vec!["init_declarator_list", ",", "init_declarator"]
    );
    add_case_group!(
        "init_declarator",
        vec!["declarator"],
        vec!["declarator", "=", "initializer"]
    );
    add_case_group!(
        "storage_class_specifier",
        vec!["TYPEDEF"],
        vec!["EXTERN"],
        vec!["STATIC"],
        vec!["AUTO"],
        vec!["REGISTER"]
    );
    add_case_group!(
        "type_specifier",
        vec!["VOID"],
        vec!["CHAR"],
        vec!["SHORT"],
        vec!["INT"],
        vec!["LONG"],
        vec!["FLOAT"],
        vec!["DOUBLE"],
        vec!["SIGNED"],
        vec!["UNSIGNED"],
        vec!["struct_or_union_specifier"],
        vec!["enum_specifier"],
        vec!["TYPE_NAME"]
    );
    add_case_group!(
        "struct_or_union_specifier",
        vec![
            "struct_or_union",
            "IDENTIFIER",
            "{",
            "struct_declaration_list",
            "}"
        ],
        vec!["struct_or_union", "{", "struct_declaration_list", "}"],
        vec!["struct_or_union", "IDENTIFIER"]
    );
    add_case_group!("struct_or_union", vec!["STRUCT"], vec!["UNION"]);
    add_case_group!(
        "struct_declaration_list",
        vec!["struct_declaration"],
        vec!["struct_declaration_list", "struct_declaration"]
    );
    add_case_group!(
        "struct_declaration",
        vec!["specifier_qualifier_list", "struct_declarator_list", ";"]
    );
    add_case_group!(
        "specifier_qualifier_list",
        vec!["type_specifier", "specifier_qualifier_list"],
        vec!["type_specifier"],
        vec!["type_qualifier", "specifier_qualifier_list"],
        vec!["type_qualifier"]
    );
    add_case_group!(
        "struct_declarator_list",
        vec!["struct_declarator"],
        vec!["struct_declarator_list", ",", "struct_declarator"]
    );
    add_case_group!(
        "struct_declarator",
        vec!["declarator"],
        vec![":", "constant_expression"],
        vec!["declarator", ":", "constant_expression"]
    );
    add_case_group!(
        "enum_specifier",
        vec!["ENUM", "{", "enumerator_list", "}"],
        vec!["ENUM", "IDENTIFIER", "{", "enumerator_list", "}"],
        vec!["ENUM", "IDENTIFIER"]
    );
    add_case_group!(
        "enumerator_list",
        vec!["enumerator"],
        vec!["enumerator_list", ",", "enumerator"]
    );
    add_case_group!(
        "enumerator",
        vec!["IDENTIFIER"],
        vec!["IDENTIFIER", "=", "constant_expression"]
    );
    add_case_group!("type_qualifier", vec!["CONST"], vec!["VOLATILE"]);
    add_case_group!(
        "declarator",
        vec!["pointer", "direct_declarator"],
        vec!["direct_declarator"]
    );
    add_case_group!(
        "direct_declarator",
        vec!["IDENTIFIER"],
        vec!["(", "declarator", ")"],
        vec!["direct_declarator", "[", "constant_expression", "]"],
        vec!["direct_declarator", "[", "]"],
        vec!["direct_declarator", "(", "parameter_type_list", ")"],
        vec!["direct_declarator", "(", "identifier_list", ")"],
        vec!["direct_declarator", "(", ")"]
    );
    add_case_group!(
        "pointer",
        vec!["*"],
        vec!["*", "type_qualifier_list"],
        vec!["*", "pointer"],
        vec!["*", "type_qualifier_list", "pointer"]
    );
    add_case_group!(
        "type_qualifier_list",
        vec!["type_qualifier"],
        vec!["type_qualifier_list", "type_qualifier"]
    );
    add_case_group!(
        "parameter_type_list",
        vec!["parameter_list"],
        vec!["parameter_list", ",", "ELLIPSIS"]
    );
    add_case_group!(
        "parameter_list",
        vec!["parameter_declaration"],
        vec!["parameter_list", ",", "parameter_declaration"]
    );
    add_case_group!(
        "parameter_declaration",
        vec!["declaration_specifiers", "declarator"],
        vec!["declaration_specifiers", "abstract_declarator"],
        vec!["declaration_specifiers"]
    );
    add_case_group!(
        "identifier_list",
        vec!["IDENTIFIER"],
        vec!["identifier_list", ",", "IDENTIFIER"]
    );
    add_case_group!(
        "type_name",
        vec!["specifier_qualifier_list"],
        vec!["specifier_qualifier_list", "abstract_declarator"]
    );
    add_case_group!(
        "abstract_declarator",
        vec!["pointer"],
        vec!["direct_abstract_declarator"],
        vec!["pointer", "direct_abstract_declarator"]
    );
    add_case_group!(
        "direct_abstract_declarator",
        vec!["(", "abstract_declarator", ")"],
        vec!["[", "]"],
        vec!["[", "constant_expression", "]"],
        vec!["direct_abstract_declarator", "[", "]"],
        vec![
            "direct_abstract_declarator",
            "[",
            "constant_expression",
            "]"
        ],
        vec!["(", ")"],
        vec!["(", "parameter_type_list", ")"],
        vec!["direct_abstract_declarator", "(", ")"],
        vec![
            "direct_abstract_declarator",
            "(",
            "parameter_type_list",
            ")"
        ]
    );
    add_case_group!(
        "initializer",
        vec!["assignment_expression"],
        vec!["{", "initializer_list", "}"],
        vec!["{", "initializer_list", ",", "}"]
    );
    add_case_group!(
        "initializer_list",
        vec!["initializer"],
        vec!["initializer_list", ",", "initializer"]
    );
    add_case_group!(
        "statement",
        vec!["labeled_statement"],
        vec!["compound_statement"],
        vec!["expression_statement"],
        vec!["selection_statement"],
        vec!["iteration_statement"],
        vec!["jump_statement"]
    );
    add_case_group!(
        "labeled_statement",
        vec!["IDENTIFIER", ":", "statement"],
        vec!["CASE", "constant_expression", ":", "statement"],
        vec!["DEFAULT", ":", "statement"]
    );
    add_case_group!(
        "compound_statement",
        vec!["{", "}"],
        vec!["{", "statement_list", "}"],
        vec!["{", "declaration_list", "}"],
        vec!["{", "declaration_list", "statement_list", "}"]
    );
    add_case_group!(
        "declaration_list",
        vec!["declaration"],
        vec!["declaration_list", "declaration"]
    );
    add_case_group!(
        "statement_list",
        vec!["statement"],
        vec!["statement_list", "statement"]
    );
    add_case_group!("expression_statement", vec![";"], vec!["expression", ";"]);
    add_case_group!(
        "selection_statement",
        vec!["IF", "(", "expression", ")", "statement"],
        vec![
            "IF",
            "(",
            "expression",
            ")",
            "statement",
            "ELSE",
            "statement"
        ],
        vec!["SWITCH", "(", "expression", ")", "statement"]
    );
    add_case_group!(
        "iteration_statement",
        vec!["WHILE", "(", "expression", ")", "statement"],
        vec!["DO", "statement", "WHILE", "(", "expression", ")", ";"],
        vec![
            "FOR",
            "(",
            "expression_statement",
            "expression_statement",
            ")",
            "statement"
        ],
        vec![
            "FOR",
            "(",
            "expression_statement",
            "expression_statement",
            "expression",
            ")",
            "statement"
        ]
    );
    add_case_group!(
        "jump_statement",
        vec!["GOTO", "IDENTIFIER", ";"],
        vec!["CONTINUE", ";"],
        vec!["BREAK", ";"],
        vec!["RETURN", ";"],
        vec!["RETURN", "expression", ";"]
    );
    add_case_group!(
        "translation_unit",
        vec!["external_declaration"],
        vec!["translation_unit", "external_declaration"]
    );
    add_case_group!(
        "external_declaration",
        vec!["function_definition"],
        vec!["declaration"]
    );
    add_case_group!(
        "function_definition",
        vec![
            "declaration_specifiers",
            "declarator",
            "declaration_list",
            "compound_statement"
        ],
        vec!["declaration_specifiers", "declarator", "compound_statement"],
        vec!["declarator", "declaration_list", "compound_statement"],
        vec!["declarator", "compound_statement"]
    );
    let mut non_terminals = vec![];
    for (ref head, ref _tail) in &cases {
        non_terminals.push(*head);
    }
    let g = Grammar::from_str("translation_unit", non_terminals, cases);
    g
}
