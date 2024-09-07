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

macro_rules! node {
    // Multiplication: concatenate all nodes
    ($node:ident ( $(
        *
        $($itoken:ident)?
        $(<$sname:ident : $subnode:ident>)?
        $({$cname:ident : $ctoken:ident ($pat:pat = $ty:ty) })?
    )* ) ) => {
        #[derive(Debug)]
        #[cfg_attr(test, derive(PartialEq))]
        pub struct $node {$(
            $(pub $sname: $subnode,)?
            $(pub $cname: $ty,)?
        )*}

        impl FromTokens for $node {
            fn from_tokens(ts: &mut (impl Iterator<Item = Token> + Clone)) -> Result<Self, ParseError> {
                $(
                    $(expect_token!(ts, $itoken);)?
                    $(let $sname = $subnode::from_tokens(ts)?;)?
                    $(let $cname = expect_token!(ts, $ctoken($pat): $ty);)?
                )*
                Ok($node {$(
                    $($sname,)?
                    $($cname,)?
                )*})
            }
        }

        impl ToTokens for $node {
            fn to_tokens(self) -> impl Iterator<Item = Token> {
                let $node {$(
                    $($sname,)?
                    $($cname,)?
                )*} = self;

                ::core::iter::empty()
                $(.chain(
                    $(::core::iter::once(Token::$itoken))?
                    $($sname.to_tokens())?
                    $(::core::iter::once(Token::$ctoken($cname.into())))?
                ))*
            }
        }
    };

    // Addition: choose between notes
    ($node:ident ( $(
        +
        $($itoken:ident)?
        $(<$subnode:ident>)?
        $({$ctoken:ident ($pat:pat = $ty:ty) })?
    )* ) ) => {
        #[derive(Debug)]
        #[cfg_attr(test, derive(PartialEq))]
        pub enum $node {$(
            $($itoken,)?
            $($subnode($subnode),)?
            $($ctoken($ty),)?
        )*}

        impl FromTokens for $node {
            fn from_tokens(ts: &mut (impl Iterator<Item = Token> + Clone)) -> Result<Self, ParseError> {
                use $node::*;
                $(
                let mut iter = ts.clone();
                if let Ok(out) = (|| -> Result<Self, ParseError> {
                    $(let out = {
                        expect_token!(iter, $itoken);
                        $itoken
                    };)?
                    $(let out = $subnode($subnode.from_tokens(&mut iter)?);)?
                    $(let out = $ctoken(expect_token!(iter, $ctoken($pat): $ty));)?
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
                match self {$(
                    $($itoken => Box::new(::core::iter::once(Token::$itoken)) as Box<dyn Iterator<Item = Token>>,)?
                    $($subnode(s) => Box::new(s.to_tokens()) as Box<dyn Iterator<Item = Token>>,)?
                    $($ctoken(c) => Box::new(::core::iter::once(Token::$ctoken(c.into()))) as Box<dyn Iterator<Item = Token>>,)?
                )*}
            }
        }
    };

    // Addition and multiplication nested in a way that allows recursion
    ($node:ident ( $(
        +
        $($case:ident ( $(
            *
            $($itoken:ident)?
            $(<$sname:ident : $subnode:ident>)?
            $([$bname:ident : Box<Self>])?
            $({$cname:ident : $ctoken:ident ($pat:pat = $ty:ty) })?
        )* ) )?
        $( (
            $(* $pretoken:ident)*
            * [Self]
            $(* $posttoken:ident)*
        ) )?
    )+ ) ) => {
        #[derive(Debug)]
        #[cfg_attr(test, derive(PartialEq))]
        pub enum $node {$(
            $($case {$(
                $($sname: $subnode,)?
                $($bname: Box<$node>,)?
                $($cname: $ty,)?
            )*},)?
        )+}

        impl FromTokens for $node {
            fn from_tokens(ts: &mut (impl Iterator<Item = Token> + Clone)) -> Result<Self, ParseError> {
                use $node::*;
                $(

                $(let mut iter = ts.clone();
                if let Ok(out) = (|| -> Result<Self, ParseError> {
                    $(
                        $(expect_token!(iter, $itoken);)?
                        $(let $sname = $subnode::from_tokens(&mut iter)?;)?
                        $(let $bname = Box::new($node::from_tokens(&mut iter)?);)?
                        $(let $cname = expect_token!(iter, $ctoken($pat): $ty);)?
                    )*
                    Ok($case {$(
                        $($sname,)?
                        $($bname,)?
                        $($cname,)?
                    )*})
                })() {
                    *ts = iter;
                    return Ok(out);
                })?

                $(let mut iter = ts.clone();
                if let Ok(out) = (|| -> Result<Self, ParseError> {
                    $(expect_token!(iter, $pretoken);)*
                    let out = $node::from_tokens(&mut iter)?;
                    $(expect_token!(iter, $posttoken);)*
                    Ok(out)
                })() {
                    *ts = iter;
                    return Ok(out);
                })?

                )+

                Err(ParseError::NoMatches)
            }
        }


        impl ToTokens for $node {
            fn to_tokens(self) -> impl Iterator<Item = Token> {
                use $node::*;
                let out: Box<dyn Iterator<Item = Token>> = match self {$(
                    $($case {$(
                        $($sname,)?
                        $($bname,)?
                        $($cname,)?
                    )*} => Box::new(::core::iter::empty()$(
                        $(.chain(::core::iter::once(Token::$itoken)))?
                        $(.chain($sname.to_tokens()))?
                        $(.chain($bname.to_tokens()))?
                        $(.chain(::core::iter::once(Token::$ctoken($cname.into()))))?
                    )*),)?
                )+};
                out
            }
        }
    }
}
pub(super) use node;

macro_rules! nodes {
    ($($node:ident $tt:tt;)*) => {
        $(node!($node $tt);)*
    };
}
pub(super) use nodes;
