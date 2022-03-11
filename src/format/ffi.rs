use super::interp::{json, Context, CowValue, Value};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Expected {0} argument(s), got {1}")]
    Arity(usize, usize),
    #[error("Type error: expected {0}, got {}", json(.1))]
    TypeError(&'static str, Value),
    #[error("Expected a pipe input, got nothing")]
    NoTopic,
    #[error("Unexpected pipe input")]
    ExtraTopic,
    #[error("An internal error occurred")]
    Internal(#[from] anyhow::Error),
}

pub type Result<T> = std::result::Result<T, Error>;
pub type Output<'a> = Result<CowValue<'a>>;
pub type Function = for<'a> fn(Input<'a>) -> Output<'a>;

#[allow(missing_debug_implementations)]
pub struct Input<'a> {
    ctx: &'a Context,
    topic: Option<CowValue<'a>>,
    args: Vec<CowValue<'a>>,
}

trait Marshal<'a>: Sized {
    const ARITY: usize;

    fn marshal<I: IntoIterator<IntoIter = J>, J: ExactSizeIterator<Item = CowValue<'a>>>(
        iter: I,
    ) -> Result<Self> {
        let iter = iter.into_iter();

        match iter.len() {
            l if l == Self::ARITY => Self::marshal_unchecked(iter),
            len => Err(Error::Arity(Self::ARITY, len)),
        }
    }

    fn marshal_unchecked<I: ExactSizeIterator<Item = CowValue<'a>>>(iter: I) -> Result<Self>;
}

impl<'a> Marshal<'a> for () {
    const ARITY: usize = 0;

    fn marshal_unchecked<I: ExactSizeIterator<Item = CowValue<'a>>>(mut iter: I) -> Result<Self> {
        if cfg!(debug_assertions) && iter.next().is_some() {
            unreachable!()
        }

        Ok(())
    }
}

impl<'a, H: TryFrom<CowValue<'a>, Error = Error>, T: Marshal<'a>> Marshal<'a> for (H, T) {
    const ARITY: usize = T::ARITY + 1;

    fn marshal_unchecked<I: ExactSizeIterator<Item = CowValue<'a>>>(mut iter: I) -> Result<Self> {
        Ok((
            iter.next().unwrap_or_else(|| unreachable!()).try_into()?,
            T::marshal_unchecked(iter)?,
        ))
    }
}

trait MarshalTopic<'a>: TryFrom<Option<CowValue<'a>>, Error = Error> {}

impl<'a> Input<'a> {
    #[inline]
    pub fn new(ctx: &'a Context, topic: Option<CowValue<'a>>, args: Vec<CowValue<'a>>) -> Self {
        Self { ctx, topic, args }
    }
}

impl<'a, T, A> TryFrom<Input<'a>> for (&'a Context, T, A)
where
    T: MarshalTopic<'a>,
    A: Marshal<'a>,
{
    type Error = Error;

    fn try_from(Input { ctx, topic, args }: Input<'a>) -> Result<Self> {
        Ok((ctx, topic.try_into()?, A::marshal(args)?))
    }
}

pub struct NoTopic;
#[repr(transparent)]
pub struct Topic<T>(pub T);

impl<'a> MarshalTopic<'a> for NoTopic {}
impl<'a, T: TryFrom<CowValue<'a>, Error = Error>> MarshalTopic<'a> for Topic<T> {}

impl<'a> TryFrom<Option<CowValue<'a>>> for NoTopic {
    type Error = Error;

    fn try_from(topic: Option<CowValue<'a>>) -> Result<Self> {
        match topic {
            Some(_) => Err(Error::ExtraTopic),
            None => Ok(Self),
        }
    }
}

impl<'a, T: TryFrom<CowValue<'a>, Error = Error>> TryFrom<Option<CowValue<'a>>> for Topic<T> {
    type Error = Error;

    fn try_from(topic: Option<CowValue<'a>>) -> Result<Self> {
        match topic {
            Some(t) => Ok(Self(t.try_into()?)),
            None => Err(Error::NoTopic),
        }
    }
}

#[repr(transparent)]
pub struct Any<'a>(pub CowValue<'a>);

impl<'a> TryFrom<CowValue<'a>> for Any<'a> {
    type Error = Error;

    fn try_from(val: CowValue<'a>) -> Result<Self> { Ok(Self(val)) }
}
