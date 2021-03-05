use anyhow::{Context, Error};
use dbus::nonblock;
use dbus_tokio::connection;
use structopt::StructOpt;
use tokio::{
    runtime::Builder as RtBuilder,
    select,
    signal::{unix, unix::SignalKind},
    sync::oneshot,
};

type Result<T = (), E = Error> = std::result::Result<T, E>;

#[derive(StructOpt)]
enum Opts {
    Server,
    Client,
}

fn main() {
    let result = run();

    match result {
        Ok(()) => (),
        Err(e) => eprintln!("ERROR: {:?}", e),
    }
}

fn run() -> Result {
    let rt = RtBuilder::new_current_thread()
        .enable_all()
        .build()
        .context("failed to start runtime")?;

    let opts = Opts::from_args();

    match opts {
        Opts::Server => rt.block_on(run_server()),
        Opts::Client => rt.block_on(run_client()),
    }
}

async fn run_server() -> Result {
    let (res, conn) = connection::new_session_sync().context("failed to connect to D-Bus")?;
    let (close_tx, close_rx) = oneshot::channel();

    tokio::spawn(async {
        close_tx
            .send(Error::from(res.await).context("D-Bus disconnected"))
            .ok();
    });

    enum Event {
        MprisMessage,
        ClientRequest,
        Stop(Option<Error>),
    }

    use Event::*;

    let mut hup = unix::signal(SignalKind::hangup()).context("failed to hook SIGHUP")?;
    let mut int = unix::signal(SignalKind::interrupt()).context("failed to hook SIGINT")?;
    let mut quit = unix::signal(SignalKind::quit()).context("failed to hook SIGQUIT")?;
    let mut term = unix::signal(SignalKind::terminate()).context("failed to hook SIGTERM")?;

    loop {
        match select!(
            Some(()) = hup.recv() => Stop(None),
            Some(()) = int.recv() => Stop(None),
            Some(()) = quit.recv() => Stop(None),
            Some(()) = term.recv() => Stop(None),
            res = close_rx => Stop(res.context("failed to listen for stop").err()),
        ) {
            MprisMessage => todo!(),
            ClientRequest => todo!(),
            Stop(e) => {
                if let Some(e) = e {
                    eprintln!("ERROR: {:?}", e);
                }

                eprintln!("Shutting down...");

                break;
            },
        }
    }

    Ok(())
}

async fn run_client() -> Result {
    println!("Hello, servers!");

    Ok(())
}
