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
            amplitude: 0.0,
        }
    }
}

#[derive(Copy,Clone)]
struct NoiseGeneratorParameters {
    amplitude: f64,
}

impl NoiseGeneratorParameters {
    fn new() -> NoiseGeneratorParameters {
        NoiseGeneratorParameters {
            amplitude: 0.0,
        }
    }
}

struct AudioSynthParameters {
    sample_rate_hz: f64,
    sample_time: f64,

    tone_generators: [ToneGeneratorParameters; 3],

    noise_generator: NoiseGeneratorParameters,
}

impl AudioSynthParameters {
    pub fn new(sample_rate_hz:f64) -> AudioSynthParameters {
        AudioSynthParameters {
            sample_rate_hz: sample_rate_hz,
            sample_time: 0.0,

            tone_generators: [ToneGeneratorParameters::new(); 3],
            
            noise_generator: NoiseGeneratorParameters::new(),
        }
    }

    pub fn set_tone_active(&mut self, n:usize, b:bool) {
        self.tone_generators[n].active = b;
    }

    pub fn set_tone_frequency(&mut self, n:usize, f:f64) {
        self.tone_generators[n].frequency = f;
    }

    pub fn get_tone_frequency(&self, n:usize) -> f64 {
        self.tone_generators[n].frequency
    }

    pub fn set_tone_amplitude(&mut self, n:usize, a:f64) {
        self.tone_generators[n].amplitude = a;
    }
 
    pub fn get_tone_amplitude(&self, n:usize) -> f64 {
        self.tone_generators[n].amplitude
    }

    pub fn set_noise_amplitude(&mut self, a:f64) {
        self.noise_generator.amplitude = a;
    }
 
    pub fn get_noise_amplitude(&self) -> f64 {
        self.noise_generator.amplitude
    }

    fn square(x:f64) -> f64 {
        /*
        let mx = x % 1.0;
        if mx > 0.5 {
            1.0
        }
        else {
            -1.0
        }
        */

        // use square function Fourier expansion to "soften" the sound
        const N: usize = 15;
        let mut y = 0.0;
        for k in 0..N {
            const TWO_PI:f64 = 2.0*std::f64::consts::PI;
            let a = 2.0*(k as f64) - 1.0;
            y += (TWO_PI * a * x).sin() / a;
        }

        (4.0*y) / std::f64::consts::PI
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

        // general gain
        sample *= 0.05;

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

    pub fn get_tone_frequency(&mut self, n:usize) -> f64 {
        let mut asp = self.parameters.lock().unwrap();
        asp.get_tone_frequency(n)
    }

    pub fn set_tone_amplitude(&mut self, n:usize, a:f64) {
        let mut asp = self.parameters.lock().unwrap();
        asp.set_tone_amplitude(n,a);
    }

    pub fn get_tone_amplitude(&mut self, n:usize) -> f64 {
        let mut asp = self.parameters.lock().unwrap();
        asp.get_tone_amplitude(n)
    }

    pub fn set_noise_synchronous(&mut self, b:bool) {
        let mut asp = self.parameters.lock().unwrap();
        asp.set_noise_synchronous(b);
    }   

    pub fn get_noise_synchronous(&mut self) -> bool {
        let mut asp = self.parameters.lock().unwrap();
        asp.get_noise_synchronous()
    } 

    pub fn set_noise_frequency(&mut self, f:f64) {
        let mut asp = self.parameters.lock().unwrap();
        asp.set_noise_frequency(f);
    }   

    pub fn get_noise_frequency(&mut self) -> f64 {
        let mut asp = self.parameters.lock().unwrap();
        asp.get_noise_frequency()
    } 

    pub fn set_noise_amplitude(&mut self, a:f64) {
        let mut asp = self.parameters.lock().unwrap();
        asp.set_noise_amplitude(a);
    }   

    pub fn get_noise_amplitude(&mut self) -> f64 {
        let mut asp = self.parameters.lock().unwrap();
        asp.get_noise_amplitude()
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
