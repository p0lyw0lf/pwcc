use functional::Foldable;
use functional::FoldableMut;
use functional::Functor;
use functional::TryFunctor;
use pwcc_util::parser::FromTokens;
use pwcc_util::parser::ToTokens;
use pwcc_util::span::Span;
use pwcc_util::span::Spanned;

use crate::lexer::Token;
use crate::parser::ParseError;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct CommaDelimited<T>(pub Vec<T>);

impl<T: FromTokens<Token, ParseError>> FromTokens<Token, ParseError> for CommaDelimited<T> {
    fn from_tokens<'a>(
        ts: &mut impl pwcc_util::parser::CloneableIterator<Item = (&'a Token, Span)>,
    ) -> Result<Self, ParseError>
    where
        Token: 'a,
    {
        let mut iter = ts.clone();
        let mut args = Vec::new();

        // To start with, we don't have to parse an argument (can be the empty list).
        // However, as soon as we se a comma, we must parse the next argument (no trailing commas
        // allowed).
        let mut must_parse_arg = false;

        loop {
            let result = T::from_tokens(&mut iter);
            match (must_parse_arg, result) {
                (_, Ok(arg)) => args.push(arg),
                (true, Err(err)) => return Err(err),
                (false, Err(_)) => break,
            };

            let mut peek_iter = iter.clone();
            match peek_iter.next() {
                Some((Token::Comma, _)) => {
                    // Parse the next argument
                    iter = peek_iter;
                    must_parse_arg = true;
                }
                _ => {
                    // Reached end of arguments
                    break;
                }
            }
        }

        *ts = iter;
        Ok(Self(args))
    }
}

impl<T: ToTokens<Token>> ToTokens<Token> for CommaDelimited<T> {
    fn to_tokens(self) -> impl Iterator<Item = Token> {
        self.0
            .into_iter()
            .flat_map(|arg| std::iter::once(Token::Comma).chain(arg.to_tokens()))
            .skip(1)
    }
}

impl<T: Spanned> Spanned for CommaDelimited<T> {
    fn span(&self) -> Span {
        self.0.span()
    }
}

impl<T, Output> Functor<Output> for CommaDelimited<T>
where
    T: Functor<Output>,
{
    type Input = T::Input;
    type Mapped = CommaDelimited<T::Mapped>;

    fn fmap_impl(
        self,
        f: &mut impl FnMut(Self::Input) -> Output,
        how: functional::RecursiveCall,
    ) -> Self::Mapped {
        CommaDelimited(self.0.fmap_impl(f, how))
    }
}

impl<T, Output> TryFunctor<Output> for CommaDelimited<T>
where
    T: TryFunctor<Output>,
{
    fn try_fmap_impl<E: functional::Semigroup + functional::ControlFlow>(
        self,
        f: &mut impl FnMut(Self::Input) -> Result<Output, E>,
        how: functional::RecursiveCall,
    ) -> Result<Self::Mapped, E> {
        Ok(CommaDelimited(self.0.try_fmap_impl(f, how)?))
    }
}

impl<T, A> Foldable<A> for CommaDelimited<T>
where
    T: Foldable<A>,
{
    fn foldl_impl<'s, B>(
        &'s self,
        f: &mut impl FnMut(B, &'s A) -> B,
        acc: B,
        how: functional::RecursiveCall,
    ) -> B
    where
        A: 's,
    {
        self.0.foldl_impl(f, acc, how)
    }
}

impl<T, A> FoldableMut<A> for CommaDelimited<T>
where
    T: FoldableMut<A>,
{
    fn foldl_mut_impl<B>(
        &mut self,
        f: &mut impl FnMut(B, &mut A) -> B,
        acc: B,
        how: functional::RecursiveCall,
    ) -> B {
        self.0.foldl_mut_impl(f, acc, how)
    }
}
