use super::super::compiler::Compiler;
use super::super::tokens::Number;

use sound_stream::{CallbackFlags, CallbackResult, SoundStream, Settings, StreamParams};
use std::thread;
use std::sync::mpsc::sync_channel;
use std::slice;

pub fn play_stream(compiler: &Compiler) {
    compiler.get_init_fn()(());
    let main_fn: extern fn(Number) -> Number = unsafe { match compiler.get_fn("main") {
        Some(f) => f,
        None => return,
    }};

    const POOL_SIZE: usize = 8;
    const CHUNK_SIZE: usize = 256;
    const BUF_SIZE: usize = CHUNK_SIZE*POOL_SIZE;
    let (tx, rx) = sync_channel(8);

    thread::spawn(move || {
        for buf_id in 0.. {
            let mut buffer = vec![0f32; BUF_SIZE];
            let tx = tx.clone();
            {
                // this is necessary because the compiler can't reason that the threads are done with
                // the buffer after they are joined below.
                let buffer = unsafe { slice::from_raw_parts_mut(buffer.as_mut_ptr(), buffer.len()) };
                let mut threads = Vec::new();
                for (chunk_id, chunk) in buffer.chunks_mut(CHUNK_SIZE).enumerate() {
                    threads.push(thread::spawn(move || {
                        for i in 0..CHUNK_SIZE {
                            let time = (buf_id*BUF_SIZE + chunk_id*CHUNK_SIZE + i) as Number / 44100.0;
                            chunk[i] = main_fn(time) as f32;
                        }
                    }));
                }
                for thread in threads {
                    thread.join().unwrap();
                }
            }
            tx.send(buffer).unwrap();
        }
    });

    let mut buf_ptr = 0usize;
    let mut buffer = rx.recv().unwrap();
    let callback = Box::new(move |output: &mut[f32], settings: Settings, _: f64, _: CallbackFlags| {
        let mut max = 0f32;
        for frame in output.chunks_mut(settings.channels as usize) {
            let amp = buffer[buf_ptr];
            if amp > max {
                max = amp;
            }
            for channel in frame {
                *channel = amp;
            }
            buf_ptr += 1;
            if buf_ptr >= CHUNK_SIZE*POOL_SIZE {
                buf_ptr = 0;
                buffer = rx.recv().unwrap();
            }
        }
        //if max < 0.0001 { CallbackResult::Complete } else { CallbackResult::Continue }
        CallbackResult::Continue
    });

    // Construct the default, non-blocking output stream and run our callback.
    let params = StreamParams::new().suggest_latency(0.05);
    let stream = SoundStream::new().output(params).run_callback(callback).unwrap();

    while let Ok(true) = stream.is_active() {}
}
