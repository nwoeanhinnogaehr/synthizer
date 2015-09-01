use super::super::compiler::Compiler;
use super::render_samples;

use hound;

pub fn write_wav(compiler: &Compiler, filename: String, length: f32) {
    let spec = hound::WavSpec {
        channels: 1,
        sample_rate: 44100,
        bits_per_sample: 16
    };
    let rx = render_samples(compiler, spec.sample_rate).unwrap();

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
