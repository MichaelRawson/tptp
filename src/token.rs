/// Tokens obtained by lexical analysis
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub enum Token {
    /// `(`
    LParen,
    /// `)`
    RParen,
    /// `[`
    LBrack,
    /// `]`
    RBrack,
    /// `,`
    Comma,
    /// `.`
    Period,
    /// `:`
    Colon,
    /// `&`
    Ampersand,
    /// `|`
    Pipe,
    /// `~`
    Tilde,
    /// `~|`
    TildePipe,
    /// `~&`
    TildeAmpersand,
    /// `=>`
    LeftArrow,
    /// `<=`
    RightArrow,
    /// `<=>`
    BothArrow,
    /// `<~>`
    TildeBothArrow,
    /// `=`
    Equals,
    /// `!=`
    NotEquals,
    /// `!`
    Exclamation,
    /// `?`
    Question,
    /// e.g. `$true`
    Defined(String),
    /// e.g. `X`
    Upper(String),
    /// e.g. `lower_Cased2`
    Lower(String),
    /// e.g. `'quoted'`
    SingleQuoted(String),
    /// e.g. `"quoted"`,
    DoubleQuoted(String),
    /// e.g. `123`
    Integer(String),
    /// (end of file)
    EOF,
}
