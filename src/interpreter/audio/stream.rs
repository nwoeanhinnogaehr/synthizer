use super::super::compiler::Compiler;
use super::render_samples;

use sound_stream::{CallbackFlags, CallbackResult, SoundStream, Settings, StreamParams};

pub fn play_stream(compiler: &Compiler) {
    let rx = render_samples(compiler, 48000).unwrap();
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
            if buf_ptr >= buffer.len() {
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
