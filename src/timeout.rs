use std::future::Future;

use tokio::time;

#[derive(Debug, thiserror::Error)]
pub enum Error<E> {
    #[error("{0}")]
    Elapsed(#[from] time::error::Elapsed),
    #[error("{0}")]
    Other(#[source] E),
}

#[derive(Debug, Clone, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Timeout<T>(T);

impl<T> From<T> for Timeout<T> {
    fn from(value: T) -> Self {
        Self(value)
    }
}

impl<T> Timeout<T> {
    #[inline]
    pub fn block<F: FnOnce(&'_ T) -> R, R>(&self, f: F) -> R {
        f(&self.0)
    }

    #[inline]
    pub unsafe fn smuggle<'a, F: FnOnce(&'a T) -> R, R>(&'a self, f: F) -> R {
        f(&self.0)
    }

    #[inline]
    pub async fn run<'a, F: FnOnce(&'a T) -> FR, FR: Future + 'a>(
        &'a self,
        duration: time::Duration,
        f: F,
    ) -> Result<FR::Output, time::error::Elapsed>
    where
        T: 'a,
    {
        time::timeout(duration, f(&self.0)).await
    }

    #[inline]
    pub async fn try_run<'a, F: FnOnce(&'a T) -> FR, FR: Future<Output = Result<R, E>> + 'a, R, E>(
        &'a self,
        duration: time::Duration,
        f: F,
    ) -> Result<R, Error<E>>
    where
        T: 'a,
    {
        self.run(duration, f).await?.map_err(Error::Other)
    }
}
