macro_rules! expect_token {
    ($ts:ident, $token:ident$(($pat:pat) : $ty:ty)?) => {
        {
            use ParseError::*;
            use Token::*;
            (match $ts.next() {
                None => Err(MissingToken {
                    expected: $token$((<$ty as Default>::default().into()))?,
                }),
                Some($token$((v @ $pat))?) => Ok(($(<$ty>::from(v))?)),
                Some(t) => Err(UnexpectedToken {
                    expected: $token$((<$ty as Default>::default().into()))?,
                    actual: t,
                }),
            })?
        }
    };
}
pub(super) use expect_token;

/// Tries multiple functions, returning the when the first returns OK, otherwise returning a
/// default value.
macro_rules! try_parse {
    ($ts:ident, $default:expr, $(
        |$iter:ident| $tt:tt ,
    )*) => {
        {
            $(
            let mut $iter = $ts.clone();
            if let Ok(out) = (|| -> Result<_, ParseError> $tt)() {
                *$ts = $iter;
                return Ok(out);
            }
            )*
            $default
        }
    };
}
pub(super) use try_parse;

/// This is an extremely nasty macro. Unfortunately, it's borne out of necessity: we need to be
/// able to generate all struct definitions at once if we want to use `#[functional_macros::ast]`
/// on them.
macro_rules! nodes {
    ($(
        $node:ident
        $(
            // Multiplication: concatenate all nodes
            ( $(
                *
                $($m_token:ident)?
                $(<$m_sname:ident : $m_subnode:ident>)?
                $({$m_cname:ident : $m_ctoken:ident ($m_pat:pat = $m_ty:ty) })?
            )* )
        )?
        $(
            // Addition: choose between nodes
            ( $(
                +
                $($a_token:ident)?
                $(<$a_subnode:ident>)?
                $({$a_ctoken:ident ($a_pat:pat = $a_ty:ty) })?
            )* )
        )?
        $(
            // Star: repeat node
            [$s_subnode:ident]
        )?
        $(
            // Other: just emit normally
            enum $oe_tt:tt
        )?
    ; )*) => {
        #[functional_macros::ast]
        mod ast {
        use super::*;
        $(
        $(
        // Multiplication
        #[derive(Debug)]
        #[cfg_attr(test, derive(PartialEq))]
        pub struct $node {$(
            $(pub $m_sname: $m_subnode,)?
            $(pub $m_cname: $m_ty,)?
        )*}

        impl FromTokens for $node {
            fn from_tokens(ts: &mut (impl Iterator<Item = Token> + Clone)) -> Result<Self, ParseError> {
                $(
                    $(expect_token!(ts, $m_token);)?
                    $(let $m_sname = $m_subnode::from_tokens(ts)?;)?
                    $(let $m_cname = expect_token!(ts, $m_ctoken($m_pat): $m_ty);)?
                )*
                Ok($node {$(
                    $($m_sname,)?
                    $($m_cname,)?
                )*})
            }
        }

        impl ToTokens for $node {
            fn to_tokens(self) -> impl Iterator<Item = Token> {
                let $node {$(
                    $($m_sname,)?
                    $($m_cname,)?
                )*} = self;

                ::core::iter::empty()
                $(.chain(
                    $(::core::iter::once(Token::$m_token))?
                    $($m_sname.to_tokens())?
                    $(::core::iter::once(Token::$m_ctoken($m_cname.into())))?
                ))*
            }
        }
        )?

        $(
        // Addition
        #[derive(Debug)]
        #[cfg_attr(test, derive(PartialEq))]
        pub enum $node {$(
            $($a_token,)?
            $($a_subnode($a_subnode),)?
            $($a_ctoken($a_ty),)?
        )*}

        impl FromTokens for $node {
            fn from_tokens(ts: &mut (impl Iterator<Item = Token> + Clone)) -> Result<Self, ParseError> {
                $(
                let mut iter = ts.clone();
                if let Ok(out) = (|| -> Result<Self, ParseError> {
                    $(let out = {
                        expect_token!(iter, $a_token);
                        $node::$a_token
                    };)?
                    $(let out = $node::$a_subnode($a_subnode::from_tokens(&mut iter)?);)?
                    $(let out = $node::$a_ctoken(expect_token!(iter, $a_ctoken($a_pat): $a_ty));)?
                    Ok(out)
                })() {
                    *ts = iter;
                    return Ok(out);
                }
                )*

                Err(ParseError::NoMatches)
            }
        }

        impl ToTokens for $node {
            fn to_tokens(self) -> impl Iterator<Item = Token> {
                use $node::*;
                let out: Box<dyn Iterator<Item = Token>> = match self {$(
                    $($a_token => Box::new(::core::iter::once(Token::$a_token)),)?
                    $($a_subnode(s) => Box::new(s.to_tokens()),)?
                    $($a_ctoken(c) => Box::new(::core::iter::once(Token::$a_ctoken(c.into()))),)?
                )*};
                out
            }
        }
        )?

        $(
        // Star
        #[derive(Debug)]
        #[cfg_attr(test, derive(PartialEq))]
        pub struct $node(pub Vec<$s_subnode>);

        impl FromTokens for $node {
            fn from_tokens(ts: &mut (impl Iterator<Item = Token> + Clone)) -> Result<Self, ParseError> {
                let mut out = Vec::<$s_subnode>::new();
                while let Ok(subnode) = $s_subnode::from_tokens(ts) {
                    out.push(subnode);
                }
                Ok($node(out))
            }
        }

        impl ToTokens for $node {
            fn to_tokens(self) -> impl Iterator<Item = Token> {
                self.0.into_iter().flat_map(ToTokens::to_tokens)
            }
        }
        )?
        $(
        // Other
        #[derive(Debug)]
        #[cfg_attr(test, derive(PartialEq))]
        pub enum $node $oe_tt
        )?
        )* }
        pub use ast::*;
    };
}
pub(super) use nodes;
