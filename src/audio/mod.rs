extern crate cpal;
use cpal::{SampleRate,SampleFormat};
use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};

use std::thread;

use std::sync::mpsc::{Sender, Receiver};
use std::sync::mpsc;

use crate::cpu::Z80;

use std::time::Instant;

pub struct LFSR {
    register: u16,
    state: bool,
    output: f64,
    feedback: bool,
}

impl LFSR {
    pub fn new() -> LFSR {
        LFSR {
            register: 0x8000,
            state: false,
            output: 0.0,
            feedback: false,
        }
    }

    fn set_feedback(&mut self, b:bool) {
        self.feedback = b;
    }

    pub fn reset(&mut self) {
        self.register = 0x8000;
    }

    pub fn shift(&mut self) {

        if self.feedback {
            // https://www.smspower.org/Development/SN76489?from=Development.PSG
            // For the SMS (1 and 2), Genesis and Game Gear,
            // the tapped bits are bits 0 and 3 ($0009), fed back into bit 15.

            let b0: bool = self.register & (1<<0) != 0;
            let b3: bool = self.register & (1<<3) != 0;
            let nb = b0 ^ b3;

            // fed bit back to register
            self.register = if nb { 0x8000 } else {0x0000} | self.register >> 1;
        
            self.output = if b0 { 1.0 } else { -1.0 };
        }
        else {
            // periodic noise mode
            let bit0 = self.register & 1;
            self.register = self.register >> 1 | bit0 << 15;

            self.output = if bit0 != 0 { 1.0 } else { -1.0 };
        }
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
    ResetNoiseRegister,
    SetNoiseAmplitude(f64),
    SetNoiseFrequency(bool, f64),
    SetNoiseFeedback(bool),
}
use AudioSynthAction as ASA;

#[derive(Copy,Clone)]
pub struct AudioSynthCommand {
    timestamp: Instant,
    z80_cycle: i64,
    action: AudioSynthAction,
}

impl AudioSynthCommand {

    pub fn new(cycle: i64, action: AudioSynthAction) -> AudioSynthCommand {
        AudioSynthCommand {
            timestamp: Instant::now(),
            z80_cycle: cycle,
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
    pub fn new(rx:Receiver<AudioSynthCommand>, sample_rate_hz:u32) -> AudioSynthGenerator {
        AudioSynthGenerator {
            queue: rx,

            sample_rate_hz: sample_rate_hz as i64,
            sample_time_sps: 0,

            tone_generators: [ToneGeneratorParameters::new(); 3],
            
            noise_generator: NoiseGeneratorParameters::new(),
            noise_lfsr: LFSR::new(),

            next_command: None,
            sync_timing_us: None,
        }
    }

    fn apply_action(&mut self, action:AudioSynthAction) {
        match action {
            ASA::SetToneActive(n,b) =>      {
                self.tone_generators[n].active = b;
            },
            ASA::SetToneAmplitude(n,v) =>   {
                self.tone_generators[n].amplitude = v;
            },
            ASA::SetToneFrequency(n,f) =>   {
                self.tone_generators[n].frequency = f;
            },
            ASA::ResetNoiseRegister => {
                self.noise_lfsr.reset();
            },
            ASA::SetNoiseAmplitude(v) =>    {
                self.noise_generator.amplitude = v;
            },
            ASA::SetNoiseFeedback(b) =>     {
                self.noise_generator.feedback = b;
            },
            ASA::SetNoiseFrequency(b,f) =>  {
                self.noise_generator.frequency = f;
                self.noise_generator.coupled = b;
            },
            _ => {},
        }
    }

    fn square(x:f64) -> f64 {
        // use square function Fourier expansion to "soften" the sound
        const N: usize = 10;
        let mut y = 0.0;
        for k in 0..N {
            const TWO_PI:f64 = 2.0*std::f64::consts::PI;
            let a = 2.0*(k as f64) - 1.0;
            y += (TWO_PI * a * x).sin() / a;
        }
        
        (4.0*y) / std::f64::consts::PI
    }

    fn pop_commands(&mut self) {

        // current audio time in microseconds
        let audio_time_us = 1_000_000 * self.sample_time_sps / self.sample_rate_hz;

        loop {
            // pop next command if needed
            if self.next_command.is_none() {
                self.next_command = self.queue.try_recv().ok();
            }

            // execute next command when scheduled
            if let Some(command) = self.next_command {

                // CPU time in microseconds
                let cpu_time_us = 1_000_000 * command.z80_cycle / Z80::clock_frequency_hz();

                // synchronisation action is not scheduled and executed ASAP
                match command.action {
                    ASA::SyncTiming => {
                        // compute time it took to unpack queue
                        let unpack_time_us = command.timestamp.elapsed().as_micros() as i64;
                        // correct audio time to estimate what time is was when action was pushed
                        let push_audio_time_us = audio_time_us - unpack_time_us;

                        // compute latency between audio thread clock and Z80 cpu clock
                        self.sync_timing_us = Some(push_audio_time_us - cpu_time_us);

                        // clear next command
                        self.next_command = None;
                    }

                    _ => {
                        // time in CPU cycles at which command was emited
                        let cpu_cycle = command.z80_cycle;
                        // convert to time in microseconds
                        let cpu_time_us = 1_000_000 * cpu_cycle / Z80::clock_frequency_hz();

                        let dt_us = (audio_time_us - cpu_time_us) - self.sync_timing_us.unwrap_or(0);

                        // execute audio commands with a 5ms latency
                        if dt_us > 5000 {
                            self.apply_action(command.action);
                            // reset command
                            self.next_command = None
                        }
                        else {
                            // next command is not scheduled to apply now
                            return;
                        }
                    }
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

        // noise generator
        let nf = if self.noise_generator.coupled {
            self.tone_generators[2].frequency
        }
        else {
            self.noise_generator.frequency
        };
        self.noise_lfsr.set_feedback(self.noise_generator.feedback);
        let x = nf * sample_time;
        let ny = self.noise_generator.amplitude * self.noise_lfsr.update(x % 1.0);
        sample += ny;

        // general gain
        sample *= 0.1;

        sample as i16
    }
}


pub struct AudioSynth {
    queue: Sender<AudioSynthCommand>,
    stream: cpal::Stream,
}

impl AudioSynth {


    fn find_config(configs:cpal::SupportedOutputConfigs, rate:cpal::SampleRate, fmt:cpal::SampleFormat, channels:cpal::ChannelCount) 
        -> Option<cpal::StreamConfig> {
    
        for config in configs {
            let min_rate = config.min_sample_rate();
            let max_rate = config.max_sample_rate();
            if min_rate < rate && rate < max_rate {
                let ssc = config.with_sample_rate(rate);
                if ssc.sample_format() == fmt && ssc.channels() == channels {
                    return Some(ssc.config());
                }
            }
        };
        None
    }

    pub fn new() -> AudioSynth {

        let host = cpal::default_host();
        let device = host
            .default_output_device()
            .expect("failed to find a default audio output device");

        let configs = device
            .supported_output_configs()
            .expect("failed to obtain a list of supported configurations");

        let sample_rate_hz = 44100;


        let config = Self::find_config(configs, cpal::SampleRate(44100), cpal::SampleFormat::I16, 1)
                    .expect("unable to configure output device with required parameters");

        // instanciate mt channel
        let (tx,rx): (Sender<AudioSynthCommand>, Receiver<AudioSynthCommand>) = mpsc::channel();
        
        // instanciate audio generator
        let mut generator = AudioSynthGenerator::new(rx, sample_rate_hz);

        let err_fn = |err| eprintln!("Error building output sound stream: {}", err);

        let data_fn = move |data:&mut[i16], _:&cpal::OutputCallbackInfo| {

            for i in 0..data.len() {
                data[i] = generator.next_sample();
            }
        };
        let stream = device.build_output_stream(&config, data_fn, err_fn)
                    .expect("failed to build_output_stream()");

        stream.play()
            .expect("failed to play() stream");

        std::thread::sleep(std::time::Duration::from_millis(1000));

        AudioSynth {
            queue:tx,
            stream,
        }
    }

    pub fn push(&mut self, command:AudioSynthCommand) {
        self.queue.send(command).unwrap();
    }

}
