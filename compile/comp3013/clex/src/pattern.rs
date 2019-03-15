use std::rc::Rc;
use std::iter::FromIterator;
use regex_engine::reg;
use regex_engine::core::*;
use regex_engine::ext::*;
use std::collections::HashSet;

pub fn gen_digit_reg() -> Rc<CharSetExpr> {
    let digit_set: HashSet<char> = HashSet::from_iter(['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'].iter().cloned());
    reg!(CharSetExpr,digit_set)
}

pub fn gen_letter_reg() -> Rc<CharSetExpr> {
    let mut letter_set = HashSet::new();
    for i in ('a' as i32)..=('z' as i32) {
        letter_set.insert(i as u8 as char);
    }
    for i in ('A' as i32)..=('Z' as i32) {
        letter_set.insert(i as u8 as char);
    }
    letter_set.insert('_');
    reg!(CharSetExpr,letter_set)
}

pub fn gen_hex_digit_reg(digit_reg: Rc<RegexExpr>) -> Rc<AltExpr> {
    let mut letter_set = HashSet::new();
    for i in ('a' as i32)..=('f' as i32) {
        letter_set.insert(i as u8 as char);
    }
    for i in ('A' as i32)..=('F' as i32) {
        letter_set.insert(i as u8 as char);
    }
    reg!(AltExpr, reg!(CharSetExpr,letter_set),digit_reg)
}

pub fn gen_exp_reg(digit_reg: Rc<RegexExpr>) -> Rc<SeqExpr> {
    reg!(SeqExpr,
        vec![
            reg!(AltExpr,
                reg!(MatchExpr,'E'),
                reg!(MatchExpr,'e')
                ),
            reg!(OptionalExpr, reg!(AltExpr,
                reg!(MatchExpr,'+'),
                reg!(MatchExpr,'-')
                )),
            reg!(PlusExpr, digit_reg)
            ]
        )
}

pub fn gen_fs_expr() -> Rc<CharSetExpr> {
    let fs_set: HashSet<char> = HashSet::from_iter(['f', 'F', 'l', 'L'].iter().cloned());
    reg!(CharSetExpr,fs_set)
}

pub fn gen_is_expr() -> Rc<RepeatExpr> {
    let is_sub_set: HashSet<char> = HashSet::from_iter(['u', 'U', 'l', 'L'].iter().cloned());
    reg!(RepeatExpr,reg!(CharSetExpr,is_sub_set))
}

pub fn gen_hex_const_expr(hex_digit_expr: Rc<RegexExpr>, is_expr: Rc<RegexExpr>) -> Rc<SeqExpr> {
    reg!(SeqExpr,
        vec![
            reg!(MatchExpr,'0'),
            reg!(AltExpr,
                reg!(MatchExpr, 'x'),
                reg!(MatchExpr, 'X')
                ),
            reg!(PlusExpr,hex_digit_expr),
            reg!(OptionalExpr,is_expr)
        ])
}

pub fn gen_oct_const_expr(digit_expr: Rc<RegexExpr>, is_expr: Rc<RegexExpr>) -> Rc<SeqExpr> {
    reg!(SeqExpr,
        vec![
            reg!(MatchExpr,'0'),
            reg!(PlusExpr,digit_expr),
            reg!(OptionalExpr,is_expr)
        ])
}

macro_rules! add_reg_return {
    ( $y:ident , $x: expr, $z: expr ) => {
        ($y).push( ( ($y).len(),  $x , Rc::new(|_| { Some( ($z).to_string() ) } ) ) )
    }
}

pub fn gen_patterns() -> Vec<(usize ,Rc<dyn RegexExpr>, Rc<Fn(&str) -> Option<String>>)> {
    // D: [0-9]
    let digit_expr: Rc<CharSetExpr> = gen_digit_reg();
    // L : [a-zA-Z_]
    let letter_expr: Rc<CharSetExpr> = gen_letter_reg();
    // H : [a-fA-F0-9]
    let hex_digit_expr: Rc<AltExpr> = gen_hex_digit_reg(digit_expr.clone());
    // E : [Ee][+-]?{D}+
    let exp_expr: Rc<SeqExpr> = gen_exp_reg(digit_expr.clone());
    // FS : (f|F|l|L)
    let fs_expr: Rc<CharSetExpr> = gen_fs_expr();
    // IS : (u|U|l|L)*
    let is_expr: Rc<RepeatExpr> = gen_is_expr();

    let mut res: Vec<(usize, Rc<RegexExpr>, Rc<Fn(&str) -> Option<String>>)> = Vec::new();

    let kw_auto: Rc<StringMatchExpr> = reg!(StringMatchExpr,"auto");
    add_reg_return!(res,kw_auto,"auto");

    let kw_break: Rc<StringMatchExpr> = reg!(StringMatchExpr,"break");
    add_reg_return!(res,kw_break,"break");

    let kw_case: Rc<StringMatchExpr> = reg!(StringMatchExpr,"case");
    add_reg_return!(res,kw_case,"case");

    let kw_char: Rc<StringMatchExpr> = reg!(StringMatchExpr,"char");
    add_reg_return!(res,kw_char,"char");

    let kw_const: Rc<StringMatchExpr> = reg!(StringMatchExpr,"const");
    add_reg_return!(res,kw_const,"const");

    let kw_continue: Rc<StringMatchExpr> = reg!(StringMatchExpr,"continue");
    add_reg_return!(res,kw_continue,"continue");

    let kw_default: Rc<StringMatchExpr> = reg!(StringMatchExpr,"default");
    add_reg_return!(res,kw_default,"default");

    let kw_do: Rc<StringMatchExpr> = reg!(StringMatchExpr,"do");
    add_reg_return!(res,kw_do,"do");

    let kw_double: Rc<StringMatchExpr> = reg!(StringMatchExpr,"double");
    add_reg_return!(res,kw_double,"double");

    let kw_else: Rc<StringMatchExpr> = reg!(StringMatchExpr,"else");
    add_reg_return!(res,kw_else,"else");

    let kw_enum: Rc<StringMatchExpr> = reg!(StringMatchExpr,"enum");
    add_reg_return!(res,kw_enum,"enum");

    let kw_extern: Rc<StringMatchExpr> = reg!(StringMatchExpr,"extern");
    add_reg_return!(res,kw_extern,"extern");

    let kw_float: Rc<StringMatchExpr> = reg!(StringMatchExpr,"float");
    add_reg_return!(res,kw_float,"float");

    let kw_for: Rc<StringMatchExpr> = reg!(StringMatchExpr,"for");
    add_reg_return!(res,kw_for,"for");

    let kw_goto: Rc<StringMatchExpr> = reg!(StringMatchExpr,"goto");
    add_reg_return!(res,kw_goto,"goto");

    let kw_if: Rc<StringMatchExpr> = reg!(StringMatchExpr,"if");
    add_reg_return!(res,kw_if,"if");

    let kw_int: Rc<StringMatchExpr> = reg!(StringMatchExpr,"int");
    add_reg_return!(res,kw_int,"int");

    let kw_long: Rc<StringMatchExpr> = reg!(StringMatchExpr,"long");
    add_reg_return!(res,kw_long,"long");

    let kw_register: Rc<StringMatchExpr> = reg!(StringMatchExpr,"register");
    add_reg_return!(res,kw_register,"register");

    let kw_return: Rc<StringMatchExpr> = reg!(StringMatchExpr,"return");
    add_reg_return!(res,kw_return,"return");

    let kw_short: Rc<StringMatchExpr> = reg!(StringMatchExpr,"short");
    add_reg_return!(res,kw_short,"short");

    let kw_signed: Rc<StringMatchExpr> = reg!(StringMatchExpr,"signed");
    add_reg_return!(res,kw_signed,"signed");

    let kw_sizeof: Rc<StringMatchExpr> = reg!(StringMatchExpr,"sizeof");
    add_reg_return!(res,kw_sizeof,"sizeof");

    let kw_static: Rc<StringMatchExpr> = reg!(StringMatchExpr,"static");
    add_reg_return!(res,kw_static,"static");

    let kw_struct: Rc<StringMatchExpr> = reg!(StringMatchExpr,"struct");
    add_reg_return!(res,kw_struct,"struct");

    let kw_switch: Rc<StringMatchExpr> = reg!(StringMatchExpr,"switch");
    add_reg_return!(res,kw_switch,"switch");

    let kw_typedef: Rc<StringMatchExpr> = reg!(StringMatchExpr,"typedef");
    add_reg_return!(res,kw_typedef,"typedef");

    let kw_union: Rc<StringMatchExpr> = reg!(StringMatchExpr,"union");
    add_reg_return!(res,kw_union,"union");

    let kw_unsigned: Rc<StringMatchExpr> = reg!(StringMatchExpr,"unsigned");
    add_reg_return!(res,kw_unsigned,"unsigned");

    let kw_void: Rc<StringMatchExpr> = reg!(StringMatchExpr,"void");
    add_reg_return!(res,kw_void,"void");

    let kw_volatile: Rc<StringMatchExpr> = reg!(StringMatchExpr,"volatile");
    add_reg_return!(res,kw_volatile,"volatile");

    let kw_while: Rc<StringMatchExpr> = reg!(StringMatchExpr,"while");
    add_reg_return!(res,kw_while,"while");

    // {L}({L}|{D})*
    let id_expr: Rc<ConcatExpr> = reg!(ConcatExpr,
        letter_expr.clone(),
        reg!(RepeatExpr, reg!(AltExpr,letter_expr.clone(),digit_expr.clone()))
        );
    add_reg_return!(res,id_expr,"identifier");

    // 0[xX]{H}+{IS}?
    let hex_const_expr: Rc<SeqExpr> = gen_hex_const_expr(hex_digit_expr.clone(), is_expr.clone());
    add_reg_return!(res,hex_const_expr,"constant");

    // 0{D}+{IS}?
    let oct_const_expr: Rc<SeqExpr> = gen_oct_const_expr(digit_expr.clone(), is_expr.clone());
    add_reg_return!(res,oct_const_expr.clone(),"constant");

    // {D}+{IS}?
    let dec_const_expr: Rc<ConcatExpr> = reg!(ConcatExpr,
                                              reg!(PlusExpr,digit_expr.clone()),
                                              reg!(OptionalExpr,is_expr.clone())
                                            );
    add_reg_return!(res,dec_const_expr,"constant");

    // {D}+{E}{FS}?
    let defs_const_expr: Rc<SeqExpr> = reg!(SeqExpr,vec![
                                            reg!(PlusExpr,digit_expr.clone()),
                                            exp_expr.clone(),
                                            reg!(OptionalExpr,fs_expr.clone())]);
    add_reg_return!(res,defs_const_expr,"constant");

    // {D}*.{D}+({E})?{FS}?
    let drpdefs_const_expr: Rc<SeqExpr> =
        reg!(SeqExpr,
            vec![
                reg!(RepeatExpr,digit_expr.clone()),
                reg!(MatchExpr,'.'),
                reg!(PlusExpr,digit_expr.clone()),
                reg!(OptionalExpr,exp_expr.clone()),
                reg!(OptionalExpr,fs_expr.clone())
            ]);
    add_reg_return!(res,drpdefs_const_expr,"constant");

    // {D}+.{D}+({E})?{FS}?
    let dppdefs_const_expr: Rc<SeqExpr> =
        reg!(SeqExpr,
            vec![
                reg!(PlusExpr,digit_expr.clone()),
                reg!(MatchExpr,'.'),
                reg!(RepeatExpr,digit_expr.clone()),
                reg!(OptionalExpr,exp_expr.clone()),
                reg!(OptionalExpr,fs_expr.clone())
            ]);
    add_reg_return!(res,dppdefs_const_expr,"constant");

    // L?'(\\.|[^'])+'
    let char_literal: Rc<SeqExpr> =
        reg!(SeqExpr,
            vec![
                reg!(OptionalExpr,letter_expr.clone()),
                reg!(MatchExpr,'\''),
                reg!(RepeatExpr,
                  reg!(AltExpr,
                    reg!(ConcatExpr,reg!(StringMatchExpr,"\\"),reg!(AnyExpr)),
                    reg!(NotInCharSetExpr,HashSet::from_iter(['\'','\\'].iter().cloned()))
                    )
                ),
                reg!(MatchExpr,'\'')
            ]);
    add_reg_return!(res,char_literal,"constant");

    // L?"(\\.|[^"\\])*"
    let string_literal: Rc<SeqExpr> =
        reg!(SeqExpr,
            vec![
                reg!(OptionalExpr,letter_expr.clone()),
                reg!(MatchExpr,'"'),
                reg!(RepeatExpr,
                  reg!(AltExpr,
                    reg!(ConcatExpr,reg!(StringMatchExpr,"\\"),reg!(AnyExpr)),
                    reg!(NotInCharSetExpr,HashSet::from_iter(['"','\\'].iter().cloned()))
                    )
                ),
                reg!(MatchExpr,'"')
            ]);
    add_reg_return!(res,string_literal.clone(),"string_literal");

    let op_ellipsis: Rc<StringMatchExpr> = reg!(StringMatchExpr,"...");
    add_reg_return!(res,op_ellipsis,"ellipsis");

    let op_right_assign: Rc<StringMatchExpr> = reg!(StringMatchExpr,">>=");
    add_reg_return!(res,op_right_assign,"right_assign");

    let op_left_assign: Rc<StringMatchExpr> = reg!(StringMatchExpr,"<<=");
    add_reg_return!(res,op_left_assign,"left_assign");

    let op_add_assign: Rc<StringMatchExpr> = reg!(StringMatchExpr,"+=");
    add_reg_return!(res,op_add_assign,"add_assign");

    let op_sub_assign: Rc<StringMatchExpr> = reg!(StringMatchExpr,"-=");
    add_reg_return!(res,op_sub_assign,"sub_assign");

    let op_mut_assign: Rc<StringMatchExpr> = reg!(StringMatchExpr,"*=");
    add_reg_return!(res,op_mut_assign,"mut_assign");

    let op_div_assign: Rc<StringMatchExpr> = reg!(StringMatchExpr,"/=");
    add_reg_return!(res,op_div_assign,"div_assign");

    let op_mod_assign: Rc<StringMatchExpr> = reg!(StringMatchExpr,"%=");
    add_reg_return!(res,op_mod_assign,"mod_assign");

    let op_and_assign: Rc<StringMatchExpr> = reg!(StringMatchExpr,"&=");
    add_reg_return!(res,op_and_assign,"and_assign");

    let op_xor_assign: Rc<StringMatchExpr> = reg!(StringMatchExpr,"^=");
    add_reg_return!(res,op_xor_assign,"xor_assign");

    let op_or_assign: Rc<StringMatchExpr> = reg!(StringMatchExpr,"|=");
    add_reg_return!(res,op_or_assign,"or_assign");

    let op_right_right: Rc<StringMatchExpr> = reg!(StringMatchExpr,">>");
    add_reg_return!(res,op_right_right,"right_right");

    let op_left_left: Rc<StringMatchExpr> = reg!(StringMatchExpr,"<<");
    add_reg_return!(res,op_left_left,"left_left");

    let op_inc: Rc<StringMatchExpr> = reg!(StringMatchExpr,"++");
    add_reg_return!(res,op_inc,"inc");

    let op_dec: Rc<StringMatchExpr> = reg!(StringMatchExpr,"--");
    add_reg_return!(res,op_dec,"dec");

    let op_ptr: Rc<StringMatchExpr> = reg!(StringMatchExpr,"->");
    add_reg_return!(res,op_ptr,"ptr");

    let op_and: Rc<StringMatchExpr> = reg!(StringMatchExpr,"&&");
    add_reg_return!(res,op_and,"and");

    let op_or: Rc<StringMatchExpr> = reg!(StringMatchExpr,"||");
    add_reg_return!(res,op_or,"or");

    let op_le: Rc<StringMatchExpr> = reg!(StringMatchExpr,"<=");
    add_reg_return!(res,op_le,"le");

    let op_ge: Rc<StringMatchExpr> = reg!(StringMatchExpr,">=");
    add_reg_return!(res,op_ge,"ge");

    let op_eq: Rc<StringMatchExpr> = reg!(StringMatchExpr,"==");
    add_reg_return!(res,op_eq,"eq");

    let op_ne: Rc<StringMatchExpr> = reg!(StringMatchExpr,"!=");
    add_reg_return!(res,op_ne,"ne");

    let op_semicolon: Rc<MatchExpr> = reg!(MatchExpr,';');
    add_reg_return!(res,op_semicolon,"semicolon");

    let op_left_curly_brace: Rc<AltExpr> = reg!(AltExpr,reg!(MatchExpr,'{'),reg!(StringMatchExpr,"<%"));
    add_reg_return!(res,op_left_curly_brace,"left_curly_brace");

    let op_right_curly_brace: Rc<AltExpr> = reg!(AltExpr,reg!(MatchExpr,'}'),reg!(StringMatchExpr,"%>"));
    add_reg_return!(res,op_right_curly_brace,"right_curly_brace");

    let op_comma: Rc<MatchExpr> = reg!(MatchExpr,',');
    add_reg_return!(res,op_comma,"comma");

    let op_colon: Rc<MatchExpr> = reg!(MatchExpr,':');
    add_reg_return!(res,op_colon,"colon");

    let op_assign: Rc<MatchExpr> = reg!(MatchExpr,'=');
    add_reg_return!(res,op_assign,"assign");

    let op_left_brace: Rc<MatchExpr> = reg!(MatchExpr,'(');
    add_reg_return!(res,op_left_brace,"left_brace");

    let op_right_brace: Rc<MatchExpr> = reg!(MatchExpr,')');
    add_reg_return!(res,op_right_brace,"right_brace");

    let op_left_square_bracket: Rc<AltExpr> = reg!(AltExpr,reg!(MatchExpr,'['),reg!(StringMatchExpr,"<:"));
    add_reg_return!(res,op_left_square_bracket,"left_square_bracket");

    let op_left_square_bracket: Rc<AltExpr> = reg!(AltExpr,reg!(MatchExpr,']'),reg!(StringMatchExpr,":>"));
    add_reg_return!(res,op_left_square_bracket,"left_square_bracket");

    let op_point: Rc<MatchExpr> = reg!(MatchExpr,'.');
    add_reg_return!(res,op_point,"point");

    let op_excl: Rc<MatchExpr> = reg!(MatchExpr,'!');
    add_reg_return!(res,op_excl,"excl");

    let op_assign: Rc<MatchExpr> = reg!(MatchExpr,'=');
    add_reg_return!(res,op_assign,"assign");

    let op_address: Rc<MatchExpr> = reg!(MatchExpr,'&');
    add_reg_return!(res,op_address,"address");

    let op_dash: Rc<MatchExpr> = reg!(MatchExpr,'-');
    add_reg_return!(res,op_dash,"dash");

    let op_wave: Rc<MatchExpr> = reg!(MatchExpr,'~');
    add_reg_return!(res,op_wave,"wave");

    let op_add: Rc<MatchExpr> = reg!(MatchExpr,'+');
    add_reg_return!(res,op_add,"add");

    let op_star: Rc<MatchExpr> = reg!(MatchExpr,'*');
    add_reg_return!(res,op_star,"star");

    let op_left_slash: Rc<MatchExpr> = reg!(MatchExpr,'/');
    add_reg_return!(res,op_left_slash,"left_slash");

    let op_per: Rc<MatchExpr> = reg!(MatchExpr,'%');
    add_reg_return!(res,op_per,"per");

    let op_left: Rc<MatchExpr> = reg!(MatchExpr,'<');
    add_reg_return!(res,op_left,"left");

    let op_right: Rc<MatchExpr> = reg!(MatchExpr,'>');
    add_reg_return!(res,op_right,"right");

    let op_up: Rc<MatchExpr> = reg!(MatchExpr,'^');
    add_reg_return!(res,op_up,"up");

    let op_split: Rc<MatchExpr> = reg!(MatchExpr,'|');
    add_reg_return!(res,op_split,"split");

    let op_question: Rc<MatchExpr> = reg!(MatchExpr,'?');
    add_reg_return!(res,op_question,"question");

    let space: Rc<CharSetExpr> = reg!(CharSetExpr,HashSet::from_iter(['\t', '\n','\r',' '].iter().cloned()));
    res.push((res.len(), space, Rc::new(|_| None)));

    res
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_digit_set() {
        let digit_reg = gen_digit_reg();
        for i in 0..10 {
            assert_eq!(reg_match(digit_reg.clone(), &format!("{}", i)), true);
        }
    }

    #[test]
    fn test_string_literal() {
        let string_literal_reg: Rc<SeqExpr> =
            reg!(SeqExpr,
            vec![
                reg!(OptionalExpr,gen_letter_reg().clone()),
                reg!(MatchExpr,'"'),
                reg!(RepeatExpr,
                  reg!(AltExpr,
                    reg!(ConcatExpr,reg!(StringMatchExpr,"\\"),reg!(AnyExpr)),
                    reg!(NotInCharSetExpr,HashSet::from_iter(['"','\\'].iter().cloned()))
                    )
                ),
                reg!(MatchExpr,'"')
            ]);
        assert_eq!(reg_match(string_literal_reg,"\"hello, \\\"world!\""),true);
    }
}