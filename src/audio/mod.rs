extern crate cpal;
use cpal::traits::{HostTrait,EventLoopTrait};
use cpal::{StreamId,StreamDataResult,SampleRate,SampleFormat,Format,StreamData,UnknownTypeOutputBuffer};

use std::sync::{Mutex,Arc};
use std::thread;

use std::sync::mpsc::{Sender, Receiver};
use std::sync::mpsc;

use crate::cpu::Z80;

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

#[derive(Copy,Clone,Debug)]
pub enum AudioSynthAction {
    SyncTiming,
    SetToneActive(usize, bool),
    SetToneAmplitude(usize, f64),
    SetToneFrequency(usize, f64),
    SetNoiseAmplitude(f64),
    SetNoiseFrequency(bool, f64),
    SetNoiseFeedback(bool),
}
use AudioSynthAction as ASA;

#[derive(Copy,Clone)]
pub struct AudioSynthCommand {
    cycle: i64,
    action: AudioSynthAction,
}

impl AudioSynthCommand {

    pub fn new(cycle: i64, action: AudioSynthAction) -> AudioSynthCommand {
        AudioSynthCommand {
            cycle: cycle,
            action: action,
        }
    }
}

struct AudioSynthGenerator {
    queue: Receiver<AudioSynthCommand>,

    sample_rate_hz: i64,
    sample_time_sps: i64,

    tone_generators: [ToneGeneratorParameters; 3],

    noise_generator: NoiseGeneratorParameters,
    noise_lfsr: LFSR,

    next_command: Option<AudioSynthCommand>,

    sync_timing_us: Option<i64>,
}

impl AudioSynthGenerator {
    pub fn new(rx:Receiver<AudioSynthCommand>, sample_rate_hz:i64) -> AudioSynthGenerator {
        AudioSynthGenerator {
            queue: rx,

            sample_rate_hz: sample_rate_hz,
            sample_time_sps: 0,

            tone_generators: [ToneGeneratorParameters::new(); 3],
            
            noise_generator: NoiseGeneratorParameters::new(),
            noise_lfsr: LFSR::new(),

            next_command: None,
            sync_timing_us: None,
        }
    }

    fn apply_action(&mut self, action:AudioSynthAction, cycle:i64) {
        match action {
            ASA::SyncTiming =>           { self.sync_timing(cycle); }
            ASA::SetToneActive(n,b) =>      { self.tone_generators[n].active = b; },
            ASA::SetToneAmplitude(n,v) =>   { self.tone_generators[n].amplitude = v; },
            ASA::SetToneFrequency(n,f) =>   { self.tone_generators[n].frequency = f; },
            ASA::SetNoiseAmplitude(v) =>    { self.noise_generator.amplitude = v; },
            ASA::SetNoiseFeedback(b) =>     { self.noise_generator.feedback = b; },
            ASA::SetNoiseFrequency(b,f) =>  {
                self.noise_generator.frequency = f;
                self.noise_generator.coupled = b;
            },
        }
    }

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

    fn sync_timing(&mut self, cycles:i64) {
        let audio_time_us = 1_000_000 * self.sample_time_sps / self.sample_rate_hz;
        let time_us = 1_000_000 * cycles / Z80::clock_frequency_hz();

        self.sync_timing_us = Some(audio_time_us - time_us);
    }

    fn pop_commands(&mut self) {

        // convert to audio time in us
        let audio_time_us = 1_000_000 * self.sample_time_sps / self.sample_rate_hz;
        loop {
            // pop next command if needed
            if self.next_command.is_none() {
                self.next_command = self.queue.try_recv().ok();
            }

            // execute next command when scheduleded
            if let Some(command) = self.next_command {
                // time in CPU cycles at which command was emited
                let rcycle = command.cycle;
                // convert to time in microseconds
                let time_us = 1_000_000 * rcycle / Z80::clock_frequency_hz();

                let dt = (audio_time_us - time_us) - self.sync_timing_us.unwrap_or(0);
                if dt > 0 {
                    self.apply_action(command.action, command.cycle);
                    // reset command
                    self.next_command = None
                }
                else {
                    // next command is not scheduled to apply now
                    return;
                }
            }
            else {
                // no more command to execute
                return;
            }
        }//loop
    }
    fn next_sample(&mut self) -> i16 {

        // increment sample time
        self.sample_time_sps += 1;

        let mut sample:f64 = 0.0;
        // increment time
        let sample_time = self.sample_time_sps as f64 / self.sample_rate_hz as f64;

        // pop next commands
        self.pop_commands();

        // iterate over tone generators
        for i in 0..3 {
            let tgp = &self.tone_generators[i];
            
            if tgp.active {

                let y = if tgp.frequency == 0.0 {
                    // output DC value
                    tgp.amplitude
                }
                else {
                    let x = tgp.frequency * sample_time;
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
        sample *= 0.6;

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
            sample_rate: SampleRate(48000),
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

            let mut generator = AudioSynthGenerator::new(rx, 48000);

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
