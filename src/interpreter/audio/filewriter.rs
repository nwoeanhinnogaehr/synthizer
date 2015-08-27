use super::super::compiler::EntryPoints;
use super::super::tokens::Number;

use hound;
use std::thread;
use std::sync::mpsc::sync_channel;
use std::slice;

pub fn write_wav(ep: &EntryPoints, filename: String, length: f32) {
    (ep.init)(());
    let main_fn = match ep.main {
        Some(f) => f,
        None => return,
    };

    let spec = hound::WavSpec {
        channels: 1,
        sample_rate: 44100,
        bits_per_sample: 16
    };

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
                            let time = (buf_id*BUF_SIZE + chunk_id*CHUNK_SIZE + i) as Number / spec.sample_rate as Number;
                            chunk[i] = main_fn(time) as f32;
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

    let mut writer = hound::WavWriter::create(filename, spec).unwrap();
    let mut buffer = rx.recv().unwrap();
    let mut buf_ptr = 0;
    for _ in 0..(length*spec.sample_rate as f32) as usize {
        let sample = buffer[buf_ptr].max(-1.0).min(1.0);
        let amplitude = ::std::i16::MAX as f32;
        writer.write_sample((sample * amplitude) as i16).unwrap();
        buf_ptr += 1;
        if buf_ptr >= buffer.len() {
            buffer = rx.recv().unwrap();
            buf_ptr = 0;
        }
    }
    writer.finalize().unwrap();
}
