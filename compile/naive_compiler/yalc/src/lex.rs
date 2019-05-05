use std::borrow::Cow;


#[derive(Debug,Clone,PartialEq,Eq,Serialize,Deserialize)]
#[serde(tag="type")]
pub enum StringLiteral<'a> {
    Double {
        content: Cow<'a,str>,
    },
    Single {
        content: Cow<'a,str>,
    },
    LongTerm {
        content: Cow<'a,str>,
        depth: u32,
    }
}