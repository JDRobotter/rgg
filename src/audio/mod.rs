extern crate cpal;
use cpal::traits::{DeviceTrait,HostTrait,EventLoopTrait};
use cpal::{StreamId,StreamDataResult,SampleRate,SampleFormat,Format,StreamData,UnknownTypeOutputBuffer};

use std::sync::{Mutex,Arc};
use std::thread;

#[derive(Copy,Clone)]
struct ToneGeneratorParameters {
    active: bool,
    frequency: f64,
    amplitude: f64,
}

impl ToneGeneratorParameters {
    fn new() -> ToneGeneratorParameters {
        ToneGeneratorParameters {
            active: false,
            frequency: 0.0,
            amplitude: 1000.0,
        }
    }
}

struct AudioSynthParameters {
    sample_rate_hz: f64,
    sample_time: f64,

    tone_generators: [ToneGeneratorParameters; 3],
}

impl AudioSynthParameters {
    pub fn new(sample_rate_hz:f64) -> AudioSynthParameters {
        AudioSynthParameters {
            sample_rate_hz: sample_rate_hz,
            sample_time: 0.0,

            tone_generators: [ToneGeneratorParameters::new(); 3],
        }
    }

    pub fn set_tone_active(&mut self, n:usize, b:bool) {
        self.tone_generators[n].active = b;
    }

    pub fn set_tone_frequency(&mut self, n:usize, f:f64) {
        self.tone_generators[n].frequency = f;
    }

    pub fn set_tone_amplitude(&mut self, n:usize, a:f64) {
        self.tone_generators[n].amplitude = a;
    }
    
    fn square(x:f64) -> f64 {
        let mx = x % 1.0;
        if mx > 0.5 {
            1.0
        }
        else {
            -1.0
        }
    }

    fn next_sample(&mut self) -> i16 {

        let mut sample:f64 = 0.0;
        // increment time
        let step_s = 1.0 / self.sample_rate_hz;
        self.sample_time += step_s;

        // iterate over tone generators
        for i in 0..3 {
            let tgp = &self.tone_generators[i];
            
            if tgp.active {
                let x = tgp.frequency * self.sample_time;
                let y = tgp.amplitude*AudioSynthParameters::square(x);

                sample += y;
            }
        }

        sample as i16
    }
}

pub struct AudioSynth {

    parameters: Arc<Mutex<AudioSynthParameters>>,
}

impl AudioSynth {

    pub fn new() -> AudioSynth {

        let host = cpal::default_host();
        let device = host
            .default_output_device()
            .expect("failed to find a default audio output device");
     
        let format = Format {
            channels: 1,
            sample_rate: SampleRate(44100),
            data_type: SampleFormat::I16
        };

        let event_loop = host.event_loop();
        let sid = event_loop.build_output_stream(&device, &format)
                    .expect("failed to ::build_output_stream");

        let parameters = Arc::new(Mutex::new(AudioSynthParameters::new(44100.0)));

        println!("playing");
        event_loop.play_stream(sid).expect("failed to ::play_stream");

        // spawn a thread running cpal event loop
        let tp = parameters.clone();
        thread::spawn(move || {
            event_loop.run(move |stream_id,stream_result| {
                let asp = &mut tp.lock().unwrap();
                AudioSynth::evloop(stream_id, stream_result, asp);
            });
        });

        AudioSynth {
            parameters: parameters.clone(),
        }
    }

    pub fn set_tone_active(&mut self, n:usize, b:bool) {
        let mut asp = self.parameters.lock().unwrap();
        asp.set_tone_active(n,b);
    }

    pub fn set_tone_frequency(&mut self, n:usize, f:f64) {
        let mut asp = self.parameters.lock().unwrap();
        asp.set_tone_frequency(n,f);
    }

    pub fn set_tone_amplitude(&mut self, n:usize, a:f64) {
        let mut asp = self.parameters.lock().unwrap();
        asp.set_tone_amplitude(n,a);
    }

    fn evloop(stream_id:StreamId, stream_result:StreamDataResult, parameters: &mut AudioSynthParameters) {

        let stream_data = match stream_result {
            Ok(data) => data,
            Err(err) => {
                eprintln!("an error occured on stream {:?}: {}",
                    stream_id, err);
                return;
            }
        };

        match stream_data {
            StreamData::Output { buffer: UnknownTypeOutputBuffer::I16(mut buffer) } => {
                for e in buffer.iter_mut() {
                    *e = parameters.next_sample();
                }
            },
            _ => (),
        }

    }
}
