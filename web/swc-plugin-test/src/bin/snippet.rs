fn main() {}

#[cfg(test)]
mod tests {
  use swc_common::{ DUMMY_SP, Span };
  use swc_core::{
    ecma::{
      ast::*,
      parser::{EsConfig, Syntax},
      transforms::testing::test_transform,
      utils::{member_expr, quote_ident, quote_str},
      visit::{noop_fold_type, Fold, FoldWith},
    },
    quote, quote_expr,
  };
  use swc_atoms::{ js_word, JsWord };

  #[test]
  fn test_quote() {
    let generated = quote!("const $name = \"123\";" as Stmt, name = quote_ident!("ref"));
    let expected = Stmt::Decl(Decl::Var(Box::new(VarDecl {
      span: DUMMY_SP,
      kind: VarDeclKind::Const,
      declare: false,
      decls: vec![VarDeclarator {
        span: DUMMY_SP,
        name: Pat::Ident(BindingIdent {
          id: quote_ident!("ref"),
          type_ann: None,
        }),
        init: Some(Box::new(Expr::Lit(Lit::Str(Str {
          span: DUMMY_SP,
          value: "123".into(),
          raw: Some("\"123\"".into()),
        })))),
        definite: false,
      }],
    })));

    assert_eq!(generated, expected)
  }

  #[test]
  fn test_quote_expr () {
    let generated = quote_expr!("console.log(\"123\")");
    let expected = Box::new(quote!("console.log(\"123\")" as Expr));

    assert_eq!(generated, expected);
  }

  #[test]
  fn test_utils_quotes () {
    // quote_str
    assert_eq!(quote_str!("hahaha"), Str { span: DUMMY_SP, value: "hahaha".into(), raw: None });
    // quote_ident
    assert_eq!(quote_ident!("hahaha"), Ident { span: DUMMY_SP, sym: "hahaha".into(), optional: false });
    // member_expr
    assert_eq!(member_expr!(DUMMY_SP, Function.prototype.bind), Box::new(
      Expr::Member(MemberExpr { 
        span: DUMMY_SP, 
        obj: Box::new(
          Expr::Member(MemberExpr { span: DUMMY_SP, obj: 
            Box::new(Expr::Ident(quote_ident!("Function"))), 
            prop: MemberProp::Ident(quote_ident!("prototype")) 
          })
        ), 
        prop: MemberProp::Ident(quote_ident!("bind")) 
      })
    ));
    // quote_expr
    assert_eq!(
      swc_core::ecma::utils::quote_expr!(DUMMY_SP, null),
      Expr::Lit(Lit::Null (Null { span: DUMMY_SP }))
    )
  }

  #[test]
  fn test_js_word () {
    let word = js_word!("undefined");
    assert_eq!(JsWord::from("undefined"), word);
  }

  #[test]
  fn test_utils_factory_methods () {
    use swc_core::ecma::utils::ExprFactory;

    {
      let first = Lit::Num(Number { span: DUMMY_SP, value: 0.0, raw: None });
      assert_eq!(
        first.clone().as_arg(),
        ExprOrSpread {
          spread: None,
          expr: Box::new(Expr::Lit(first))
        }
      )
    }
  }
}
