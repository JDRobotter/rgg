extern crate cpal;
use cpal::traits::{HostTrait,EventLoopTrait};
use cpal::{StreamId,StreamDataResult,SampleRate,SampleFormat,Format,StreamData,UnknownTypeOutputBuffer};

use std::sync::{Mutex,Arc};
use std::thread;

use std::sync::mpsc::{Sender, Receiver};
use std::sync::mpsc;

pub struct LFSR {
    register: u16,
    state: bool,
    output: f64,
}

impl LFSR {
    pub fn new() -> LFSR {
        LFSR {
            register: 0x8000,
            state: false,
            output: 0.0,
        }
    }

    fn parity(inr:u16) -> bool {
        let inr = inr ^ (inr >> 8);
        let inr = inr ^ (inr >> 4);
        let inr = inr ^ (inr >> 2);
        let inr = inr ^ (inr >> 1);
        (inr & 1) != 0
    }

    pub fn reset(&mut self) {
        self.register = 0x8000;
    }

    pub fn output(&self) -> f64 {
        self.output
    }

    pub fn shift(&mut self) {
        // apply feedback on register
        if LFSR::parity(self.register) {
            self.register |= 0x8000;
        }
        
        self.output = if self.register & 0x0001 != 0x0000 { 1.0 } else { 0.0 };
        
        self.register = self.register >> 1;
    }

    fn update(&mut self, x:f64) -> f64 {
        if x > 0.5 {
            if self.state == false {
                // on rising edge
                self.shift();

                self.state = true;
            }
        }
        else {
            self.state = false;
        }

        self.output
    }
}

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
    coupled: bool,
    frequency: f64,
    feedback: bool,
}

impl NoiseGeneratorParameters {
    fn new() -> NoiseGeneratorParameters {
        NoiseGeneratorParameters {
            amplitude: 0.0,
            coupled: false,
            frequency: 0.0,
            feedback: false,
        }
    }
}

pub enum AudioSynthCommand {
    SetToneActive(u64, usize, bool),
    SetToneAmplitude(u64, usize, f64),
    SetToneFrequency(u64, usize, f64),
    SetNoiseAmplitude(u64, f64),
    SetNoiseFrequency(u64, bool, f64),
    SetNoiseFeedback(u64, bool),
}

struct AudioSynthGenerator {
    queue: Receiver<AudioSynthCommand>,

    sample_rate_hz: f64,
    sample_time: f64,

    tone_generators: [ToneGeneratorParameters; 3],

    noise_generator: NoiseGeneratorParameters,
    noise_lfsr: LFSR,
}

impl AudioSynthGenerator {
    pub fn new(rx:Receiver<AudioSynthCommand>, sample_rate_hz:f64) -> AudioSynthGenerator {
        AudioSynthGenerator {
            queue: rx,

            sample_rate_hz: sample_rate_hz,
            sample_time: 0.0,

            tone_generators: [ToneGeneratorParameters::new(); 3],
            
            noise_generator: NoiseGeneratorParameters::new(),
            noise_lfsr: LFSR::new(),
        }
    }

    pub fn pop_command(&mut self) {


    }

    fn apply_command(&mut self, command:AudioSynthCommand) {
        match command {

        }
    }
/*
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

    pub fn set_noise_feedback(&mut self, b:bool) {
        self.noise_generator.feedback = b
    }

    pub fn get_noise_feedback(&self) -> bool {
        self.noise_generator.feedback
    }

    pub fn set_noise_frequency(&mut self, b:bool, f:f64) {
        self.noise_generator.frequency = f;
        self.noise_generator.coupled = b;
    }

    pub fn get_noise_frequency(&self) -> f64 {
        self.noise_generator.frequency
    }

    pub fn reset_noise(&mut self) {
        self.noise_lfsr.reset();
    }
*/

    fn square(x:f64) -> f64 {
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

                let y = if tgp.frequency == 0.0 {
                    // output DC value
                    tgp.amplitude
                }
                else {
                    let x = tgp.frequency * self.sample_time;
                    tgp.amplitude * AudioSynthGenerator::square(x)
                };

                sample += y;
            }
        }

        //noise generator
        /*
        let nf = if self.noise_generator.coupled {
            self.tone_generators[2].frequency
        }
        else {
            self.noise_generator.frequency
        };
        let x = nf * self.sample_time;
        let ny = self.noise_generator.amplitude * self.noise_lfsr.update(x % 1.0);
        sample += ny;
        */

        // general gain
        sample *= 0.2;

        sample as i16
    }
}


pub struct AudioSynth {
    queue: Sender<AudioSynthCommand>,
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

        // instanciate mt channel
        let (tx,rx): (Sender<AudioSynthCommand>, Receiver<AudioSynthCommand>) = mpsc::channel();

        event_loop.play_stream(sid).expect("failed to ::play_stream");

        // spawn a thread running cpal event loop
        thread::spawn(move || {

            let mut generator = AudioSynthGenerator::new(rx, 44100.0);

            event_loop.run(move |stream_id,stream_result| {
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
                            *e = generator.next_sample();
                        }
                    },
                    _ => (),
                }
            });
        });

        AudioSynth {
            queue:tx,
        }
    }

    pub fn push(&mut self, command:AudioSynthCommand) {
        self.queue.send(command).unwrap();
    }

}
