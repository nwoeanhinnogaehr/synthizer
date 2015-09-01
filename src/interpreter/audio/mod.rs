use super::tokens::Number;
use super::compiler::Compiler;

use std::thread;
use std::sync::mpsc::{sync_channel, Receiver};
use std::slice;

mod stream;
mod filewriter;

//TODO prefered buffer size, num threads, etc..
fn render_samples(compiler: &Compiler, sample_rate: u32) -> Option<Receiver<Vec<f32>>> {
    compiler.get_init_fn()(());
    let main_fn: extern fn(Number) -> Number = unsafe { match compiler.get_fn("main") {
        Some(f) => f,
        None => return None,
    }};

    const POOL_SIZE: usize = 8;
    const CHUNK_SIZE: usize = 256;
    const BUF_SIZE: usize = CHUNK_SIZE*POOL_SIZE;
    let (tx, rx) = sync_channel(8);

    thread::spawn(move || {
        for buf_id in 0.. {
            let mut buffer = vec![0f32; BUF_SIZE];
            {
                // this is necessary because the compiler can't reason that the threads are done with
                // the buffer after they are joined below.
                let buffer = unsafe { slice::from_raw_parts_mut(buffer.as_mut_ptr(), buffer.len()) };
                let mut threads = Vec::new();
                for (chunk_id, chunk) in buffer.chunks_mut(CHUNK_SIZE).enumerate() {
                    threads.push(thread::spawn(move || {
                        for i in 0..CHUNK_SIZE {
                            let time = (buf_id*BUF_SIZE + chunk_id*CHUNK_SIZE + i) as Number / sample_rate as Number;
                            chunk[i] = main_fn(time) as f32;
                            if !chunk[i].is_finite() && i > 0 {
                                chunk[i] = chunk[i-1];
                            }
                        }
                    }));
                }
                for thread in threads {
                    thread.join().unwrap();
                }
            }
            match tx.send(buffer) {
                Ok(_) => { },
                Err(_) => return,
            }
        }
    });

    Some(rx)
}

pub use self::stream::play_stream;
pub use self::filewriter::write_wav;
