
use crate::audio::AudioSynth;

use std::option;
use std::thread;

#[derive(Clone,Copy)]
struct ToneGeneratorRegister {
    divider: u16,
    attenuation: u8,
}

#[derive(Clone,Copy)]
struct NoiseGeneratorRegister {
    attenuation: u8,
    feedback: bool,
    divider: u8,
}

enum RegisterAddress {
    ToneDivider(u8),
    ToneAttenuation(u8),
    NoiseDivider,
    NoiseAttenuation,
    None
}

/// Sega Game Gear PSG chip
/// https://www.smspower.org/uploads/Development/SN76489-20030421.txt
pub struct PSG {
    audio_synth: AudioSynth,

    latched_tone_generator_registers: [ToneGeneratorRegister; 3],
    latched_noise_generator_register: NoiseGeneratorRegister,
    last_written_register: RegisterAddress,
}

impl PSG {

    pub fn new() -> PSG {

        PSG {
            audio_synth: AudioSynth::new(),
            latched_tone_generator_registers: [ToneGeneratorRegister {divider:0, attenuation:0}; 3],
            latched_noise_generator_register: NoiseGeneratorRegister {attenuation:0, feedback:false, divider:0},
            last_written_register: RegisterAddress::None,
        }
    }

    pub fn debug_get_tone_amplitude(&mut self, n:usize) -> f64 {
        self.audio_synth.get_tone_amplitude(n)
    }

    pub fn debug_get_tone_frequency(&mut self, n:usize) -> f64 {
        self.audio_synth.get_tone_frequency(n)
    }

    fn frequency_from_divider(div:u16) -> f64 {
        if div == 0 {
            return 0.0
        }
        const N:u32 = 3579454;
        let f = N / (32 * div as u32);
        f as f64
    }

    fn amplitude_from_attenuation(a:u8) -> f64 {

        if a == 0x0f {
            // $F is a special case where tone generator is OFF
            0.0
        }
        else {
            // attenuation in db
            let att_db = 2.0 * a as f64;
            32767.0 * (10.0f64).powf(-0.1*att_db)
        }
    }

    pub fn write(&mut self, byte: u8) {

        // first byte (latched)
        // 1  R1 R0 T  F3 F2 F1 F0
        //
        // R1R0 : register address
        // T : 1: volume / 0: tone
        //
        // second byte
        // 0  x  F9 F8 F7 F6 F5 F4a
        //
        // R : register address
        // F : data

        println!("W {:02x}", byte);
        if byte & 0x80 != 0 {
            // LATCH/DATA byte
            
            // register address
            let ra = (byte >> 5) & 0x03;
            // volume / tone selector
            let t = byte & 0x10 != 0;
            if ra == 3 {
                // noise register
                if t {
                    println!("PSG NZ ATT {}", byte & 0x0f);
                    // attenuation
                    self.latched_noise_generator_register.attenuation = byte & 0x0f;
                    self.last_written_register = RegisterAddress::NoiseAttenuation;
                }
                else {
                    println!("PSG NZ SHIFT {} {}", (byte & 0x04) == 0, (byte & 0x03));
                    // shift
                    self.latched_noise_generator_register.feedback = (byte & 0x04) == 0;
                    self.latched_noise_generator_register.divider = byte & 0x03;
                    self.last_written_register = RegisterAddress::NoiseDivider;
                }
            }
            else {
                // tone registers
                if t {
                    println!("PSG TN {} ATT {}", ra, byte & 0x0f);
                    // attenuation
                    self.latched_tone_generator_registers[ra as usize].attenuation = byte & 0x0f;
                    self.last_written_register = RegisterAddress::ToneAttenuation(ra);
                }
                else {
                    println!("PSG TN {} DIV {}", ra, byte & 0x0f);
                    // tone divider
                    self.latched_tone_generator_registers[ra as usize].divider = (byte & 0x0f) as u16;
                    self.last_written_register = RegisterAddress::ToneDivider(ra);
                }
            }

        }
        else {
            // DATA byte

            // complement latched register
            match self.last_written_register {

                RegisterAddress::ToneDivider(ri) => {
                    let div = &mut self.latched_tone_generator_registers[ri as usize].divider;
                    
                    *div = (*div & 0x0f) | ((byte & 0x3f) as u16) << 4;
                },

                RegisterAddress::ToneAttenuation(ri) => {
                    self.latched_tone_generator_registers[ri as usize].attenuation = byte & 0x0f;
                },

                RegisterAddress::NoiseDivider => {
                    // nothing to do
                },

                RegisterAddress::NoiseAttenuation => {
                    self.latched_noise_generator_register.attenuation = byte & 0x0f;
                },

                RegisterAddress::None => { 
                    // nothing to do
                },
            }

            self.last_written_register = RegisterAddress::None;
            
            println!("PSG LATCH");
            // apply latched tone registers
            for ri in 0..3 {
                let att = self.latched_tone_generator_registers[ri].attenuation;
                let div = self.latched_tone_generator_registers[ri].divider;
                self.audio_synth.set_tone_active(ri, div != 0);
                self.audio_synth.set_tone_frequency(ri, PSG::frequency_from_divider(div));
                self.audio_synth.set_tone_amplitude(ri, PSG::amplitude_from_attenuation(att));
            }

            // apply latched noise registers
            let att = self.latched_noise_generator_register.attenuation;
            let f = match self.latched_noise_generator_register.divider {
                _ =>    { self.audio_synth.set_noise_frequency(false, PSG::frequency_from_divider(16)) },
                0x01 => { self.audio_synth.set_noise_frequency(false, PSG::frequency_from_divider(32)) },
                0x02 => { self.audio_synth.set_noise_frequency(false, PSG::frequency_from_divider(64)) },
                // link to tone generator 3
                0x03 => { self.audio_synth.set_noise_frequency(true, 0.0) },
            };
            let fb = self.latched_noise_generator_register.feedback;
            self.audio_synth.set_noise_amplitude(PSG::amplitude_from_attenuation(att));
            self.audio_synth.set_noise_feedback(fb);

        }
    }


}
